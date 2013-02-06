package processing

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import net.liftweb.json.{JsonParser, DefaultFormats}

import scala.collection.{mutable, immutable}

import controllers.{CriticalMassTables, Dispatch, SODispatch}

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
    val badge_counts : Badges )
    
case class UserTagCounts(
    val name                : String,
    val count               : Long,
    val is_required         : Boolean,
    val is_moderator_only   : Boolean,
    val user_id             : Long,
    val has_synonyms        : Boolean
)
    
case class YahooLocation(
    latitude    : String,
    longitude   : String,
    radius      : String )
    

class MarkerClusterer( val db : Database )
{
    // http://www.movable-type.co.uk/scripts/latlong.html
    private def distfn( lon1 : Double, lat1 : Double, lon2 : Double, lat2 : Double ) : Double =
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
    
    def run( statusFn : (Double, String) => Unit ) =
    {
        println( "Deleting old data" )
        db withSession
        {
            import org.scalaquery.ql.extended.{ExtendedTable => Table}
            
            def rowCount[T]( table : Table[T] ) = Query( ( for ( r <- table ) yield r ).count ).first
            
            ( for ( r <- CriticalMassTables.DataHierarchy ) yield r ).mutate( _.delete )
            ( for ( r <- CriticalMassTables.TagMap ) yield r ).mutate( _.delete )
            ( for ( r <- CriticalMassTables.UserMap ) yield r ).mutate( _.delete )
            ( for ( r <- CriticalMassTables.InstitutionMap ) yield r ).mutate( _.delete )


            //delete from "DataHierarchy"; delete from "InstitutionMap"; delete from "TagMap"; delete from "UserMap";
            // The foreign key constraint should clear these out. But currently don't. Why?
            assert( rowCount( CriticalMassTables.DataHierarchy ) == 0 )
            assert( rowCount( CriticalMassTables.TagMap ) == 0 )
            assert( rowCount( CriticalMassTables.UserMap ) == 0 )
            assert( rowCount( CriticalMassTables.InstitutionMap ) == 0 )
        }
        
        case class Cluster( val lon : Double, val lat : Double, val locDatum : LocationData )
        {
            def dist( other : Cluster ) = distfn( lon, lat, other.lon, other.lat )
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
        var maxMergeDistance = 1.6
        for ( level <- 13 to 0 by -1 )
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
                        (dh.level ~ dh.longitude ~ dh.latitude ~ dh.count ~ dh.maxRep ~ dh.maxRepUid ~ dh.label) insert ( (level, cluster.lon, cluster.lat, userCount, maxRep, maxRepUid, label) )
                        
                        
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
            
            maxMergeDistance *= 2.0
        }
    }
}

class UserScraper( val db : Database )
{
    val stackOverflowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val yahooAPIKey = "50EgoNvV34HOEN8sYfWvUqVqpOfapxOSGBiRb7VjwbdsfYwolMb4XdFPhuuz"
    
    def run( statusFn : (Double, String) => Unit )
    {
        implicit val formats = DefaultFormats
        
        // Get most recent user from StackOverflow
        db withSession
        {
            val maxUserId =
            {
                val userPull = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users", List(
                    ("order", "desc"),
                    ("sort", "creation"),
                    ("site", "stackoverflow"),
                    ("pagesize","1"),
                    ("key", stackOverflowKey) ) )
                    
                val mostRecentUser = (userPull \ "items").children.head.extract[FullUser]
            
                mostRecentUser.user_id
            }
            
            // Get most recent user id from db
            val startUserId =
            {
                val users = for ( u <- CriticalMassTables.Users ) yield u.user_id.max
                
                users.list.head.getOrElse( -1L ) + 1
            }
            
            // Scrape in additional users from SO and add to db
            if ( true )
            {
            for ( i <- startUserId until maxUserId by 100L )
            {
                val j = (i until i+100L)

                val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%s".format(j.mkString(";")), List(
                    ("site", "stackoverflow"),
                    ("pagesize", "100"),
                    ("key", stackOverflowKey) ) )

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
                        u.badge_counts.bronze
                    )
                    count += 1
                }
                statusFn( 0.0, "New users: %d".format( count ) )
                
                Thread.sleep(500)
            }
            }
            
            if ( true )
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
                
                println( allLocs.size, allNonEmptyLocs.size, uniques.size )
                
                for ( ((id, addr), count) <- uniques.toList.zipWithIndex )
                {
                    val locationJ = Dispatch.pullJSON( "http://where.yahooapis.com/geocode", List(
                        ("flags", "J"),
                        ("q", addr),
                        ("appid", yahooAPIKey) ) )
                        
                    val locations = (locationJ \ "ResultSet" \ "Results").children.map( _.extract[YahooLocation] ).sortWith( _.radius < _.radius )
                    
                    println( "%d: %s".format( count, addr) )
                    if ( !locations.isEmpty )
                    {
                        val l = locations.head
                        println( "    %s".format( l ) )
                        
                        CriticalMassTables.Location insert (
                            id,
                            l.longitude.toDouble,
                            l.latitude.toDouble,
                            l.radius.toDouble )
                    }
                    else
                    {
                        // Enter a null location
                        CriticalMassTables.Location insert (
                            id,
                            0.0,
                            0.0,
                            -1.0 )
                    }
                    
                    statusFn( 0.0, "New location: %s".format( addr ) )
                    Thread.sleep(500)
                }
            }
                
            // Now fetch tag stats for each user left without tags
            val allUntaggedHighRepUsers = (for ( Join(user, tags) <-
                CriticalMassTables.Users leftJoin
                CriticalMassTables.UserTags on(_.user_id is _.user_id) if (tags.user_id isNull) && user.reputation > 120L ) yield user.user_id ~ user.display_name).list
                
            println( "number of untagged users remaining to scrape: ", allUntaggedHighRepUsers.size )
                     
            for ( (uid, name) <- allUntaggedHighRepUsers )
            {
                val json = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%d/tags".format(uid), List(
                    ("site", "stackoverflow"),
                    ("pagesize", "100"),
                    ("order", "desc"),
                    ("sort", "popular"),
                    ("key", stackOverflowKey) ) )
                    
                var tags = (json \ "items").children.map( _.extract[UserTagCounts] )
                
                if ( tags.isEmpty )
                {
                    tags = List( new UserTagCounts( "notag", 0, false, false, uid, false ) )
                }
                
                println( "Tags for: %s".format(name) )
                threadLocalSession withTransaction
                {
                    for ( tag <- tags )
                    {
                        val checkTag = (for ( t <- CriticalMassTables.Tags if t.name === tag.name ) yield t.id).list
                        
                        val tagId = if ( checkTag.isEmpty )
                        {
                            CriticalMassTables.Tags.name insert (tag.name)
                            
                            val scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                            Query(scopeIdentity).first
                        }
                        else checkTag.head
                        
                        CriticalMassTables.UserTags insert (tagId, uid, tag.count)
                    }
                }
                
                statusFn( 0.0, "Tags for user: %s".format( name ) )
                Thread.sleep(200)
            }
        }
    }
}


