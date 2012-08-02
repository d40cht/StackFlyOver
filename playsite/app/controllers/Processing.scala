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
            ( for ( r <- CriticalMassTables.DataHierarchy ) yield r ).mutate( _.delete )
            ( for ( r <- CriticalMassTables.TagMap ) yield r ).mutate( _.delete )
            ( for ( r <- CriticalMassTables.UserMap ) yield r ).mutate( _.delete )
            //( for ( r <- CriticalMassTables.InstitutionMap ) yield r ).mutate( _.delete )
        }
        
        case class Cluster( val lon : Double, val lat : Double, val nameIds : List[Long] )
        {
            def dist( other : Cluster ) = distfn( lon, lat, other.lon, other.lat )
            def merge( other : Cluster) : Cluster =
            {
                val size = nameIds.size.toDouble
                val sizeOther = other.nameIds.size.toDouble
                val total = size + sizeOther
                val newLon = (size/total) * lon + (sizeOther/total) * other.lon
                val newLat = (size/total) * lat + (sizeOther/total) * other.lat
                
                new Cluster( newLon, newLat, nameIds ++ other.nameIds ) 
            }
            
            def coords = Array( lon, lat )
            def tupleCoords = (lon, lat)
        }
        
        val locations = ( for ( ln <- CriticalMassTables.Location if ln.radius >= 0.0 && ln.radius < 100000.0 )
            yield ln.name_id ~ ln.longitude ~ ln.latitude ).list
                
        /*class UserTag( val id : Long, val name : String, val count : Long )        
        class UserData( val uid : Long, val reputation : Long, val lon : Double, val lat : Double, val tags : List[UserTag] )
        
        // Pull all the user data out into Scala
        println( "Pulling in all users with location data and tag data" )
        val allUsers = db withSession
        {
            val allUsers = (for ( Join(user, loc) <- 
                CriticalMassTables.Users innerJoin
                CriticalMassTables.Locations on(_.location is _.name)
                    if loc.radius >= 0.0 && loc.radius < 100000.0 && user.reputation >= 2L )
                yield user.display_name ~ user.user_id ~ user.reputation ~ loc.longitude ~ loc.latitude ).list
            
            val userData = mutable.ArrayBuffer[UserData]()
            val allUserData = for ( (name, uid, rep, lon, lat) <- allUsers ) yield
            {
                val userTagData = (for ( Join(userTag, tagData) <-
                    CriticalMassTables.UserTags innerJoin
                    CriticalMassTables.Tags on (_.tag_id is _.id )
                    if userTag.user_id === uid )
                    yield userTag.tag_id ~ tagData.name ~ userTag.count ).list
                    
                new UserData( uid, rep, lon, lat, userTagData.map( t => new UserTag( t._1, t._2, t._3 ) ) )
            }
            
            allUserData.toList
        }
        
        println( "   pulled in %d".format( allUsers.size ) )
        
        // Then run kdTree.query( new com.vividsolutions.jts.geom.Envelope( lon1, lat1, lon2, lat2 ) ) to get list
        // of points in range
        println( "  done" )*/
        
        statusFn( 0.0, "pulled in %d locations".format( locations.size ) )
        
        // Run through the google map scales, merging as appropriate
        var mergeSet = immutable.HashMap[(Double, Double), Cluster]()
        var debugCount = 0
        for ( (loc_id, lon, lat) <- locations )
        {
            val c = new Cluster( lon, lat, List(loc_id) )
            val cn = if ( mergeSet contains c.tupleCoords )
            {
                val original = mergeSet(c.tupleCoords)
                mergeSet -= c.tupleCoords
                debugCount -= 1
                new Cluster( c.lon, c.lat, loc_id :: original.nameIds )
            }
            else c
            
            debugCount += 1
            mergeSet += (cn.tupleCoords -> cn)
        }
        
        assert( debugCount == mergeSet.size )
        
        val mergeTree = new edu.wlu.cs.levy.CG.KDTree[Cluster](2)
        for ( (c, u) <- mergeSet ) mergeTree.insert( u.coords, u )
        
        // In metres
        var maxMergeDistance = 0.2
        for ( level <- 16 to 0 by -1 )
        {
            statusFn( 0.0, "Merge distance: %f %d".format( maxMergeDistance, mergeSet.size ) )
            
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
                        //println( "      %s (%d) (%s, %s, %s, %s)".format( d.toString, mergeSet.size, c1.lon.toString, c1.lat.toString, c2.lon.toString, c2.lat.toString ) )
                    }
                    case None => finished = true
                }
                
                if ( mergeSet.size < 50 ) finished = true
            }
            
            statusFn( 0.0, "after merge: %d".format( mergeSet.size ) )
            
            /*db withSession
            {
                for ( (c, u) <- mergeSet )
                {
                    val dh = CriticalMassTables.DataHierarchy
                    
                    val numUsers = u.users.size
                    var tagSummary = immutable.Map[(String, Long), Double]()
                    for ( user <- u.users ) yield
                    {
                        val totalCount = user.tags.foldLeft(0L)( _ + _.count ).toDouble
                        
                        if ( totalCount > 5 )
                        {
                            for ( tag <- user.tags )
                            {
                                val key = (tag.name, tag.id)
                                val oldScore = tagSummary.getOrElse(key, 0.0)
                                //val newScore = 100.0 * (tag.count.toDouble / totalCount.toDouble)
                                val newScore = scala.math.log( tag.count.toDouble )
                                
                                tagSummary += (key -> (newScore+oldScore))
                            }
                        }
                    }
                    
                    val sorted = tagSummary.toList.sortWith( _._2 > _._2 ).take(30)
                    val topAveTags = sorted.take(5)
                    val maxRepUser = u.users.sortWith( _.reputation > _.reputation ).head
                    val maxRep = maxRepUser.reputation.toInt
                    val maxRepUid = maxRepUser.uid
                    
                    val label = "%d: %s".format( numUsers, topAveTags.map(_._1._1).mkString(" ") )
                    
                    // Load this dh point plus all the relevant users
                    db withTransaction
                    {
                        val scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                        (dh.level ~ dh.longitude ~ dh.latitude ~ dh.count ~ dh.maxRep ~ dh.maxRepUid ~ dh.label) insert ( (level, u.lon, u.lat, numUsers, maxRep, maxRepUid, label) )
                        
                        
                        val dhId = Query(scopeIdentity).first
                        
                        for ( user <- u.users )
                        {
                            CriticalMassTables.UserMap insert ( (dhId, user.uid) )
                        }
                        
                        for ( ((tagName, tagId), count) <- sorted )
                        {
                            CriticalMassTables.TagMap insert( (dhId, tagId, count.toLong) )
                        }
                    }
                }
            }*/
            
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
                statusFn( 0.0, "New users: %d".format( count.toString ) )
                
                Thread.sleep(500)
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


