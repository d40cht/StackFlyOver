package processing

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}
import org.scalaquery.ql.Ordering._

import net.liftweb.json.{JsonParser, DefaultFormats}

import scala.collection.{mutable, immutable}

import controllers.{CriticalMassTables, Dispatch, SODispatch, DBUtil}

object Parameters
{
    val minUserRepForDetail     = 60L
    val reputationTagId         = -1L
    val itemScoreId             = -2L
    val overallLocationId       = -1L
    
    val numTagsToInclude        = 400
    val numTopTagsPerUser       = 8
    
    
    val stackOverflowKey        = "5FUHVgHRGHWbz9J5bEy)Ng(("
}


case class Badges(
    val gold : Int,
    val silver : Int,
    val bronze : Int )

case class FullUser(
    val user_id : Long,
    val display_name : String,
    val creation_date : Long,
    val last_access_date : Long,
    val reputation : Long,
    val age : Option[Int],
    val accept_rate : Option[Int],
    val website_url : Option[String],
    val location : Option[String],
    val badge_counts : Badges,
    val profile_image : Option[String] )
    
case class UserTagCounts(
    val tag_name            : String,
    val answer_count        : Long,
    val answer_score        : Long,
    val question_count      : Long,
    val question_score      : Long )
{
    def combine( other : UserTagCounts ) =
    {
        assert( tag_name == other.tag_name )
        
        new UserTagCounts(
            tag_name,
            answer_count + other.answer_count,
            answer_score + other.answer_score,
            question_count + other.question_count,
            question_score + other.question_score )
    }
}
    

    
import play.api.Logger

case class RankRow( val name : String, val rankings : List[(Int, Int)] )
case class UserRankings( headings : List[String], rankings : List[RankRow] )
{
    override def toString =
        rankings.map( r => r.name + ": " + (headings zip r.rankings).mkString(",") ).mkString("\n")
}


object MarkerClusterer
{
    val endRange = 13
    val startRange = 0
    
    // http://www.movable-type.co.uk/scripts/latlong.html
    def distfn( lon1 : Double, lat1 : Double, lon2 : Double, lat2 : Double ) : Double =
    {
        def toRad( v : Double ) : Double = 2.0 * math.Pi * (v / 360.0)
        
        val R = 6371.0; // Radius of the Earth in km
        val dLat = toRad(lat2-lat1)
        val dLon = toRad(lon2-lon1)
        val dlat1 = toRad(lat1)
        val dlat2 = toRad(lat2)

        val a = math.sin(dLat/2.0) * math.sin(dLat/2.0) +
                math.sin(dLon/2.0) * math.sin(dLon/2.0) * math.cos(dlat1) * math.cos(dlat2)
        val c = 2.0 * math.atan2(math.sqrt(a), math.sqrt(1.0-a))
        val d = R * c;
        
        d
    }
}

object RankGenerator
{
    def hexDigest( v : String ) : String =
    {
        val sh = org.apache.commons.codec.digest.DigestUtils.getSha1Digest()
        sh.update( v.getBytes )
        
        org.apache.commons.codec.binary.Hex.encodeHex( sh.digest() ).mkString("")
    }
    
    def recalculateRanks( db : Database ) =
    {
        val calculateTime = new java.sql.Timestamp( (new java.util.Date()).getTime )
     
        db withSession
        {   
            // Cache the results of this query in the Tags table using a trigger?
            Logger.debug( "Fetching top %d tags".format( Parameters.numTagsToInclude ) )
            val topTags = (for (
                tn <- CriticalMassTables.Tags;
                t <- CriticalMassTables.UserTags if tn.id === t.tag_id;
                _ <- Query groupBy tn.id ) yield tn.name ~ tn.id ~ t.count.sum ).elements.filter( _._3.isDefined ).map( x => (x._1, x._2, x._3.get) ).toList.sortWith( _._3 > _._3 ).take( Parameters.numTagsToInclude )
                
            topTags.foreach( println(_) )
            
            var dbWrites = 0
            
            def getYHLocation( baseName : String, fullyQualifiedName : String, extent : Int ) : Option[Long] =
            {
                import org.apache.commons.codec.digest.DigestUtils
                
                def scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                
                if ( baseName == "" ) None
                else
                {
                    val placeHash = hexDigest( fullyQualifiedName )
                    
                    val already = ( for (ylh <- CriticalMassTables.YahooLocationHierarchyIdentifier if ylh.placeHash === placeHash) yield ylh.id).list
                    if ( !already.isEmpty )
                    {
                        Some(already.head)
                    }
                    else
                    {
                        val ylhT = CriticalMassTables.YahooLocationHierarchyIdentifier
                        
                        (ylhT.name ~ ylhT.placeHash ~ ylhT.extent).insert( (baseName + " rank", placeHash, extent) )
                        
                        Some(Query(scopeIdentity).first)
                    }
                }
            }
            
            
            // Get the top tags per user
            val allUsers = (for ( u <- CriticalMassTables.Users ) yield u.user_id).elements
            
            val reputationTag = -1L
            
            Logger.debug( "Fetching top %d tags per user".format(Parameters.numTopTagsPerUser) )
            val userTopTags = allUsers.map
            { uid =>
            
                val topTags = (for ( t <- CriticalMassTables.UserTags if t.user_id === uid; _ <- Query orderBy(Desc(t.count)) ) yield t.tag_id).elements.take(Parameters.numTopTagsPerUser).toSet
                
                (uid, topTags + reputationTag)
            }.toMap
            
            
            def uniqueCountMap( s : Set[Long] ) = s.toList.map( n => (n, 0) ).toMap
            
            case class RankResult( val count : Long, val user_id : Long, val cityIdO : Option[Long], val stateIdO : Option[Long], val countryIdO : Option[Long] )
            
            class LocationRankTracker( val userTopTags : Map[Long, Set[Long]], val tagId : Long, locationIds : Set[Long] )
            {
                var overallRank = 0
                var locationRankings = uniqueCountMap( locationIds )
                
                def update( rr : RankResult ) =
                {
                    def updateRankMap( locId : Long, m : Map[Long, Int] ) : Map[Long, Int] =
                    {
                        val next = m(locId) + 1
                        
                        (m + (locId->next))
                    }
                    
                    overallRank += 1
                    rr.cityIdO.foreach( id => locationRankings = updateRankMap( id, locationRankings ) )
                    rr.stateIdO.foreach( id => locationRankings = updateRankMap( id, locationRankings ) )
                    rr.countryIdO.foreach( id => locationRankings = updateRankMap( id, locationRankings ) )
                    
                    if ( userTopTags(rr.user_id) contains tagId )
                    {
                        CriticalMassTables.UserRanks.insert( (
                            rr.user_id,
                            tagId,
                            Parameters.itemScoreId,
                            rr.count.toInt,
                            calculateTime ) )
                            
                        CriticalMassTables.UserRanks.insert( (
                            rr.user_id,
                            tagId,
                            Parameters.overallLocationId,
                            overallRank,
                            calculateTime ) )
                        
                        rr.cityIdO.foreach
                        { id =>
                            CriticalMassTables.UserRanks.insert( (
                                rr.user_id,
                                tagId,
                                id,
                                locationRankings(id),
                                calculateTime ) )
                        }
                        
                        rr.stateIdO.foreach
                        { id =>
                            CriticalMassTables.UserRanks.insert( (
                                rr.user_id,
                                tagId,
                                id,
                                locationRankings(id),
                                calculateTime ) )
                        }
                        
                        rr.countryIdO.foreach
                        { id =>
                            CriticalMassTables.UserRanks.insert( (
                                rr.user_id,
                                tagId,
                                id,
                                locationRankings(id),
                                calculateTime ) )
                        }
                            
                        dbWrites += 1
                    }
                }
            }
            
            // Calculate reputation ranks
            {
                Logger.debug( "Calculating ranks for reputation" )
                val reputationRanks = ( for (
                    u <- CriticalMassTables.Users;
                    l <- CriticalMassTables.Location if u.location_name_id === l.name_id )
                    yield u.reputation ~ u.user_id ~ l.city ~ l.state ~ l.country )
                    .list
                    .map
                    { case( rep, uid, city, state, country) =>
                    
                        val cityId = getYHLocation( city, city + state + country, 1 )
                        val stateId = getYHLocation( state, state + country, 2 )
                        val countryId = getYHLocation( country, country, 3 )
                        RankResult( rep, uid, cityId, stateId, countryId )
                        
                    }.sortWith( _.count > _.count )
                    
                    
                val allLocationIds =
                    reputationRanks.flatMap( _.cityIdO ).toSet ++
                    reputationRanks.flatMap( _.stateIdO ).toSet ++
                    reputationRanks.flatMap( _.countryIdO ).toSet
                
                val ltm = new LocationRankTracker( userTopTags, Parameters.reputationTagId, allLocationIds )
                
                reputationRanks.foreach( ltm.update(_) )
                
                Logger.info( "  db writes: " + dbWrites )
            }
            
            // Calculate tag ranks
            for ( ((name, tagId, count), i) <- topTags.zipWithIndex )
            {
                val scoresByTag = ( for (
                    u <- CriticalMassTables.Users;
                    l <- CriticalMassTables.Location if u.location_name_id === l.name_id;
                    t <- CriticalMassTables.UserTags if u.user_id === t.user_id && t.tag_id === tagId )
                    yield t.count ~ u.user_id ~ l.city ~ l.state ~ l.country )
                    .list
                    .map
                    { case( rep, uid, city, state, country) =>
                    
                        val cityId = getYHLocation( city, city + state + country, 1 )
                        val stateId = getYHLocation( state, state + country, 2 )
                        val countryId = getYHLocation( country, country, 3 )
                        RankResult( rep, uid, cityId, stateId, countryId )
                        
                    }.sortWith( _.count > _.count )
                    
                val allLocationIds =
                    scoresByTag.flatMap( _.cityIdO ).toSet ++
                    scoresByTag.flatMap( _.stateIdO ).toSet ++
                    scoresByTag.flatMap( _.countryIdO ).toSet
                
                val ltm = new LocationRankTracker( userTopTags, tagId, allLocationIds )
                Logger.debug( "Calculating ranks for " + name + " - " + scoresByTag.size + " (" + i + ")" )                
                
                scoresByTag.foreach
                {
                    // TODO: Only update if this tag is within the users' top N
                    ltm.update(_)
                }
                Logger.info( "  db writes: " + dbWrites )
            }
            
            
        }
    }
}


object HierarchyVisitor
{
    // Closest: select POWER("longitude"-13.5, 2.0) + POWER("latitude"-43.7, 2.0) as "dist", "longitude", "latitude"  from "DataHierarchy" where "level"=10 ORDER BY "dist" ASC limit 10;
    
    case class DHPoint( id : Long, dist : Double, lon : Double, lat : Double )
    {
        def dist( lonOther : Double, latOther : Double ) = MarkerClusterer.distfn( lon, lat, lonOther, latOther )
    }
    
    def apply( db : Database, longitude : Double, latitude : Double, visitFn : (Int, Double, DHPoint) => Unit )
    {
        val dhTimestamp = org.seacourt.global.Instance().getDHTimestamp.get
        
        def closest( scale : Int, limit : Int ) : List[DHPoint] =
        {
            db withSession
            {
                import org.scalaquery.simple.{StaticQuery}
                
                val q = StaticQuery[(Double, Double, Int, java.sql.Timestamp, Int), (Long, Double, Double, Double)] +
                    "SELECT \"id\", POWER(\"longitude\" - ?, 2.0) + POWER(\"latitude\" - ?, 2.0) as \"dist\", \"longitude\", \"latitude\" " +
                    "FROM \"DataHierarchy\" WHERE \"level\"=? AND \"created\"=? ORDER BY \"dist\" ASC LIMIT ?"
                
                val res : List[DHPoint] = q( (longitude, latitude, scale, dhTimestamp, limit) ).list.map( r => Function.tupled( DHPoint.apply _ )(r) )
                res
            }
        }
        
        for ( level <- MarkerClusterer.endRange to MarkerClusterer.startRange by -1)
        {
            def nearestFew = closest( level, 20 )
            
            val (ndist, nobj) = nearestFew.map( p => (p.dist( longitude, latitude), p) ).sortBy( _._1 ).head
            
            visitFn( level, ndist, nobj )
        }
    }
}



class MarkerClusterer( val db : Database )
{
    val levelRange : List[(Int, Double)] = (MarkerClusterer.endRange to MarkerClusterer.startRange by -1).zipWithIndex.map
    { case (l, i) =>
        (l, 1.6 * math.pow(2.0, i.toDouble))
    }.toList
    
    
    // Returns a list of tuples of (hierarchy level, nearest hierarchy element id)
    def getClosestElements( lon : Double, lat : Double ) : List[(Int, Long)] =
    {
        // No geo-query currently available natively in H2, so pull data out
        // using an appropriate bounding box
        levelRange.map
        { case (l, maxMergeDist) =>
            (l, 0L)
        }
    }
    
    def run( statusFn : (Double, String) => Unit ) =
    {        
        case class Cluster( val lon : Double, val lat : Double, val locDatum : LocationData )
        {
            def dist( other : Cluster ) = MarkerClusterer.distfn( lon, lat, other.lon, other.lat )
            def merge( other : Cluster) : Cluster =
            {
                val size = locDatum.userData.size.toDouble
                val sizeOther = other.locDatum.userData.size.toDouble
                val total = size + sizeOther
                val newLon = (size/total) * lon + (sizeOther/total) * other.lon
                val newLat = (size/total) * lat + (sizeOther/total) * other.lat
                
                new Cluster( newLon, newLat, locDatum combine other.locDatum ) 
            }
            
            def coords = Array( lon, lat )
            def tupleCoords = (lon, lat)
        }
        
        case class LocationData( val userData : List[(Int, Int)], val maxRep : Int, val tagData : Map[Int, Int], val institutionIds : Set[Int] )
        {
            def combine( other : LocationData ) =
            {
                var combinedTags = tagData
                for ( (tid, count) <- other.tagData )
                {
                    val prevCount = combinedTags.getOrElse( tid, 0 )
                    combinedTags += (tid -> (prevCount + count))
                }
                new LocationData( userData ++ other.userData, maxRep max other.maxRep, combinedTags, institutionIds ++ other.institutionIds )
            }
            
            def userCount = userData.size
            def topUserIds = userData.sortWith( _._2 > _._2 ).map( _._1 ).take(1000)
            def topTagData = tagData.toList.sortWith( _._2 > _._2 ).take(100)
        }
        
        class Progress( val size : Int, val message : String )
        {
            var lastPercent = -1
            
            def update( count : Int )
            {
                val thisPercent = (100.0 * count.toDouble / size.toDouble).toInt
                if ( thisPercent != lastPercent )
                {
                    statusFn( thisPercent, message )
                    lastPercent = thisPercent
                }
            }
        }
        
        var totalInstitutions = 0
        val allData = db withSession
        {
            statusFn( 0.0, "Pulling in locations for clustering" )
            val locations = ( for ( ln <- CriticalMassTables.Location if ln.radius >= 0.0 && ln.radius < 100000.0 )
                yield ln.name_id ~ ln.longitude ~ ln.latitude ).list
        
            val p = new Progress( locations.size, "Pulling data for locations" )
            val allData = for ( (loc, i) <- locations.zipWithIndex ) yield
            {
                val (id, lon, lat) = loc
                
                p.update(i)
                
                val institutions = (for (r <- CriticalMassTables.UserRole if r.location_name_id === id ) yield r.institution_id).list.toSet
                
                totalInstitutions += institutions.size
                val users = (for (r <- CriticalMassTables.Users if r.location_name_id === id ) yield r.user_id ~ r.reputation).list
                val tags = (for (
                    r <- CriticalMassTables.Users;
                    t <- CriticalMassTables.UserTags;
                    _ <- Query groupBy t.tag_id;
                    if r.user_id === t.user_id && r.location_name_id === id ) yield t.tag_id ~ t.count.sum).list
                val locData = new LocationData( users.map( x => (x._1.toInt, x._2.toInt) ), users.map(_._2.toInt).foldLeft(0)(_ max _), tags.map( x => (x._1.toInt, x._2.getOrElse(0L).toInt) ).toMap, institutions.map(_.toInt) )
                
                (loc, locData)
            }
            
            allData
        }

        statusFn( 0.0, "pulled in %d locations, %d institutions".format( allData.size, totalInstitutions ) )
        
        // Run through the google map scales, merging as appropriate
        var mergeSet = immutable.HashMap[(Double, Double), Cluster]()
        var debugCount = 0
        for ( ((loc_id, lon, lat), locDatum) <- allData )
        {
            val c = new Cluster( lon, lat, locDatum )
            val cn = if ( mergeSet contains c.tupleCoords )
            {
                val original = mergeSet(c.tupleCoords)
                mergeSet -= c.tupleCoords
                debugCount -= 1
                new Cluster( c.lon, c.lat, locDatum combine original.locDatum )
            }
            else c
            
            debugCount += 1
            mergeSet += (cn.tupleCoords -> cn)
        }
        
        assert( debugCount == mergeSet.size )
        
        val mergeTree = new edu.wlu.cs.levy.CG.KDTree[Cluster](2)
        for ( (c, u) <- mergeSet ) mergeTree.insert( u.coords, u )
        
        // In metres?
        val updateTimestamp = new java.sql.Timestamp( (new java.util.Date()).getTime )
        for ( (level, maxMergeDistance) <- levelRange )
        {
            statusFn( 0.0, "Merge distance: %f %d (map scale: %d)".format( maxMergeDistance, mergeSet.size, level ) )
            
            var finished = false
            
            while ( !finished )
            {
                // Choose a min distance cluster to merge
                var minDist : Option[(Double, Cluster, Cluster)] = None
                for ( (coords, c) <- mergeSet )
                {
                    import scala.collection.JavaConversions._
                    
                    val nearest = mergeTree.nearest( c.coords, 2 ).filter( _.tupleCoords != c.tupleCoords ).head
                    assert( nearest.tupleCoords != c.tupleCoords )
                    val d = c.dist(nearest)
                    
                    if ( d <= maxMergeDistance )
                    {
                        minDist match
                        {
                            case None =>
                            {
                                minDist = Some( (d, c, nearest) )
                            }
                            case Some( (dother, _, _) ) =>
                            {
                                if ( d < dother ) minDist = Some( (d, c, nearest) )
                            }
                        }
                    }
                }
                
                minDist match
                {
                    case Some( (d, c1, c2) ) =>
                    {
                        val merged = c1.merge(c2)
                        mergeSet -= c1.tupleCoords
                        mergeSet -= c2.tupleCoords
                        mergeTree.delete( c1.coords )
                        mergeTree.delete( c2.coords )
                        mergeSet += (merged.tupleCoords -> merged)
                        mergeTree.insert( merged.coords, merged )
                    }
                    case None => finished = true
                }
                
                if ( mergeSet.size < 50 ) finished = true
            }
            
            statusFn( 0.0, "after merge: %d".format( mergeSet.size ) )
            
            
            
            db withSession
            {
                val p = new Progress( mergeSet.size, "Loading hierarchy data into db" )
                for ( ((coords, cluster), i) <- mergeSet.toList.zipWithIndex )
                {
                    p.update(i)
                    
                    val dh = CriticalMassTables.DataHierarchy
                    
                    val collapsed = cluster.locDatum
                    
                    val userCount = collapsed.userCount
                    val maxRep = collapsed.maxRep
                    val maxRepUid = 0L
                    
                    val label = "Users: %d, max rep: %d".format( userCount, maxRep )
                    
                    // Load this dh point plus all the relevant users
                    db withTransaction
                    {
                        val scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                        (dh.level ~ dh.longitude ~ dh.latitude ~ dh.count ~ dh.maxRep ~ dh.maxRepUid ~ dh.label ~ dh.created) insert
                            ( (level, cluster.lon, cluster.lat, userCount, maxRep, maxRepUid, label, updateTimestamp) )
                        
                        
                        val dhId = Query(scopeIdentity).first
                        
                        for ( uid <- collapsed.topUserIds )
                        {
                            CriticalMassTables.UserMap insert ( (dhId, uid.toLong) )
                        }
                        
                        for ( (tagId, count) <- collapsed.topTagData )
                        {
                            if ( count > 1 )
                            {
                                val logCount = scala.math.log(count.toLong.toDouble)
                                CriticalMassTables.TagMap insert( (dhId, tagId.toLong, logCount.toLong) )
                            }
                        }
                        
                        for ( instId <- collapsed.institutionIds )
                        {
                            CriticalMassTables.InstitutionMap insert( (dhId, instId.toLong) )
                        }
                    }
                }
            }
            
            //maxMergeDistance *= 2.0
        }
        
        // Point to the latest DH hierarchy data
        org.seacourt.global.Instance().setDHTimestamp(updateTimestamp)
        
        // Clear out any old data
        Logger.info("Clearing out previous hierarchy data.")
        db withSession
        {
            import org.scalaquery.simple.{StaticQuery}
            
            val q = StaticQuery[java.sql.Timestamp] +
                "DELETE FROM DataHierarchy WHERE \"created\" <> ?"
                
            q( updateTimestamp )
        }
        Logger.info("Clearout complete.")
    }
}

class LocationUpdater( val db : Database )
{
    
    def run( statusFn : (Double, String) => Unit )
    {
        // Get most recent user from StackOverflow
        db withSession
        {
            // Fetch all locations left un-geocoded and geocode via Yahoo
            //val locations = for ( Join(u, l) <- CriticalMassTables.Users leftJoin CriticalMassTables.Locations on(_.location is _.name) if ((l.name isNull) && (u.location != "")) ) yield u.location
            val locations = for ( Join(ln, l) <-
                CriticalMassTables.LocationName leftJoin
                CriticalMassTables.Location on (_.id is _.name_id)
                if ((l.name_id isNull) && (ln.name != "")) ) yield ln.id ~ ln.name
            
            val allLocs = locations.list
            val allNonEmptyLocs = allLocs.filter( _ != "" )
            val uniques = allNonEmptyLocs.toSet
            
            Logger.debug( "All locations: %d, all non-empty locations: %d, unique locations: %d".format( allLocs.size, allNonEmptyLocs.size, uniques.size ) )
            
            for ( ((id, addr), count) <- uniques.toList.zipWithIndex )
            {
                // TODO: This should really be done after pulling data from StackExchange. But why
                // is this data XML encoded anyway?
                val decodedAddr = org.apache.commons.lang3.StringEscapeUtils.unescapeXml( addr )
                val locations = controllers.YahooGeocoder( decodedAddr )
                
                //Logger.debug( "%d: %s".format( count, addr) )
                if ( !locations.isEmpty )
                {
                    val l = locations.head
                    //Logger.debug( "    %s".format( l ) )
                    
                    CriticalMassTables.Location.insert( (
                        id,
                        l.longitude.toDouble,
                        l.latitude.toDouble,
                        l.radius.toDouble,
                        l.quality.toDouble,
                        l.neighborhood,
                        l.city,
                        l.county,
                        l.state,
                        l.country ) )
                }
                else
                {
                    // Enter a null location
                    CriticalMassTables.Location.insert( (
                        id,
                        0.0,
                        0.0,
                        -1.0,
                        -1.0,
                        "", "", "", "", "" ) )
                }
                
                statusFn( count.toDouble / uniques.size.toDouble, "New location: %s".format( decodedAddr ) )
                Thread.sleep(4000)
            }
        }
    }
}

object FetchProfileImages
{
    implicit val formats = DefaultFormats
    
    def run( db : Database )
    {
        db withSession
        {
            // Now fetch tag stats for each user left without tags
            val allUntaggedHighRepUsers = ( for (
                u <- CriticalMassTables.Users if (u.profileImage isNull);
                _ <- Query orderBy(Desc(u.reputation)) ) yield u.user_id ).take( 900000 ).list
                
            val groups = allUntaggedHighRepUsers.grouped(100)
            
            for ( userGroup <- groups )
            {
                val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%s".format(userGroup.mkString(";")), List(
                    ("site", "stackoverflow"),
                    ("pagesize", "100"),
                    ("key", Parameters.stackOverflowKey) ) )

                val users = (json \ "items").children.map( _.extract[FullUser] )
                
                for ( u <- users )
                {
                    val uru = ( for ( ur <- CriticalMassTables.Users if ur.user_id === u.user_id ) yield ur.profileImage )
                    
                    uru.update( Some(u.profile_image.getOrElse("")) )
                }
            }
        }
    }
}


class UserScraper( val db : Database )
{
    
    
    import org.scalaquery.ql.Ordering.Desc
    import org.scalaquery.ql.extended.H2Driver.Implicit._
    
    def run( statusFn : (Double, String) => Unit )
    {
        implicit val formats = DefaultFormats
        
        // Get most recent user from StackOverflow
        db withSession
        {
            // Scrape in additional users from SO and add to db
            try
            {
                val maxUserId =
                {
                    val userPull = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users", List(
                        ("order", "desc"),
                        ("sort", "creation"),
                        ("site", "stackoverflow"),
                        ("pagesize","1"),
                        ("key", Parameters.stackOverflowKey) ) )
                        
                    val mostRecentUser = (userPull \ "items").children.head.extract[FullUser]
                
                    mostRecentUser.user_id
                }
                
                // Get most recent user id from db
                val startUserId =
                {
                    val users = for ( u <- CriticalMassTables.Users ) yield u.user_id.max
                    
                    users.list.head.getOrElse( -1L ) + 1
                }
            
            
                for ( i <- startUserId until maxUserId by 100L )
                {
                    val j = (i until i+100L)

                    val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%s".format(j.mkString(";")), List(
                        ("site", "stackoverflow"),
                        ("pagesize", "100"),
                        ("key", Parameters.stackOverflowKey) ) )

                    val users = (json \ "items").children.map( _.extract[FullUser] )
                    
                    var count = 0
                    for ( u <- users )
                    {
                        val locationNameId =
                        {
                            val lname = u.location.getOrElse("")
                            
                            val query = ( for ( l <- CriticalMassTables.LocationName if l.name === lname ) yield l.id ).list
                            if ( !query.isEmpty ) query.head
                            else
                            {
                                CriticalMassTables.LocationName.name insert (lname)
                                
                                def scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                                Query(scopeIdentity).first
                            }
                        }
                        
                        val now = new java.sql.Timestamp( (new java.util.Date()).getTime )
                        
                        CriticalMassTables.Users insert (
                            u.user_id,
                            u.display_name,
                            u.creation_date,
                            u.last_access_date,
                            u.reputation,
                            u.age.getOrElse(-1),
                            u.accept_rate.getOrElse(-1),
                            u.website_url.getOrElse(""),
                            locationNameId,
                            u.badge_counts.gold,
                            u.badge_counts.silver,
                            u.badge_counts.bronze,
                            None,
                            now,
                            false,
                            Some(u.profile_image.getOrElse(""))
                        )
                        count += 1
                    }
                    statusFn( 0.0, "New users: %d".format( count ) )
                    
                    Thread.sleep(500)
                }
            }
            catch
            {
                case e : java.lang.AssertionError =>
                {
                    Logger.error( "Assertion failed in user scrape", e )
                }
            }
                
            // Now fetch tag stats for each user left without tags
            val allUntaggedHighRepUsers = (for ( Join(user, tags) <-
                CriticalMassTables.Users leftJoin
                CriticalMassTables.UserTags on(_.user_id is _.user_id) if (tags.user_id isNull) && user.reputation >= Parameters.minUserRepForDetail;
                _ <- Query orderBy(Desc(user.reputation)) ) yield user.user_id ~ user.display_name ~ user.reputation).list
                
            Logger.debug( "number of untagged users remaining to scrape: " + allUntaggedHighRepUsers.size )
                     
            for ( (uid, name, reputation) <- allUntaggedHighRepUsers )
            {
                val answerTags =
                {
                    val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%d/top-answer-tags".format(uid), List(
                        ("site", "stackoverflow"),
                        ("pagesize", "100"),
                        ("order", "desc"),
                        ("sort", "answer_score"),
                        ("key", Parameters.stackOverflowKey) ) )
                        
                    (json \ "items").children.map( _.extract[UserTagCounts] )
                }
                
                val questionTags =
                {
                    val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%d/top-question-tags".format(uid), List(
                        ("site", "stackoverflow"),
                        ("pagesize", "100"),
                        ("order", "desc"),
                        ("sort", "answer_score"),
                        ("key", Parameters.stackOverflowKey) ) )
                        
                    (json \ "items").children.map( _.extract[UserTagCounts] )
                }
                
                var tags = (answerTags ++ questionTags)
                    .groupBy( _.tag_name )
                    .map( _._2.reduce(_ combine _) )
                
                if ( tags.isEmpty )
                {
                    tags = List( new UserTagCounts( "notag", 0, 0, 0, 0 ) )
                }
                
                Logger.debug( "Tags for: %s (%d), rep: %d".format(name, uid, reputation) )
                threadLocalSession withTransaction
                {
                    for ( tag <- tags )
                    {
                        val checkTag = (for ( t <- CriticalMassTables.Tags if t.name === tag.tag_name ) yield t.id).list
                        
                        val tagId = if ( checkTag.isEmpty )
                        {
                            CriticalMassTables.Tags.name insert (tag.tag_name)
                            
                            val scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                            Query(scopeIdentity).first
                        }
                        else checkTag.head
                        
                        CriticalMassTables.UserTags insert (tagId, uid, tag.answer_score)
                    }
                }
                
                statusFn( 0.0, "Tags for user: %s".format( name ) )
                Thread.sleep(200)
            }
            
            if ( true )
            {
                val l = new LocationUpdater( db )
                l.run( statusFn )
            }
        }
    }
}


