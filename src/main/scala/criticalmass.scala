package org.seacourt.criticalmass

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import net.liftweb.json.{JsonParser, DefaultFormats}

import scala.collection.{mutable, immutable}

object CriticalMassTables
{
    import org.scalaquery.ql.extended.{ExtendedTable => Table}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._
    
    object SectorTags extends Table[(Long, String)]("SectorTags")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    object Institution extends Table[(Long, String)]("Institutions")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    object UserRole extends Table[(Long, Long, Long, String, String, String)]("UserRole")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def user_id             = column[Long]("user_id")
        def institution_id      = column[Long]("institution_id")
        def department          = column[String]("department")
        def url                 = column[String]("url")
        def location            = column[String]("location")
        
        def * = id ~ user_id ~ institution_id ~ department ~ url ~ location
    }
    
    object RoleSOTags extends Table[(Long, Long)]("RoleSOTags")
    {
        def role_id             = column[Long]("role_id")
        def tag_id              = column[Long]("tag_id")
        
        def * = role_id ~ tag_id
    }
    
    object RoleSectorTags extends Table[(Long, Long)]("RoleSectorTags")
    {
        def role_id             = column[Long]("role_id")
        def tag_id              = column[Long]("tag_id")
        
        def * = role_id ~ tag_id
    }
    
    object Tags extends Table[(Long, String)]("Tags")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    // Top tags for a user, including counts
    object UserTags extends Table[(Long, Long, Long)]("UserTags")
    {
        def tag_id              = column[Long]("tag_id")
        def user_id             = column[Long]("user_id")
        def count               = column[Long]("count")
        
        def * = tag_id ~ user_id ~ count
    }

    object InstitutionMap extends Table[(Long, Long)]("InstitutionMap")
    {
        def dh_id               = column[Long]("dh_id")
        def institution_id      = column[Long]("institution_id")

        def * = dh_id ~ institution_id
    }
    
    // Top tags for a hierarchy area, including counts
    object TagMap extends Table[(Long, Long, Long)]("TagMap")
    {
        def dh_id               = column[Long]("dh_id")
        def tag_id              = column[Long]("tag_id")
        def count               = column[Long]("count")
        
        def * = dh_id ~ tag_id ~ count
    }
    
    // Users for a hierarchy area
    object UserMap extends Table[(Long, Long)]("UserMap")
    {
        def dh_id               = column[Long]("dh_id")
        def user_id             = column[Long]("user_id")
        
        def * = dh_id ~ user_id
    }
    
    object DataHierarchy extends Table[(Long, Int, Double, Double, Int, Int, Long, String)]("DataHierarchy")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def level               = column[Int]("level")
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def count               = column[Int]("count")
        def maxRep              = column[Int]("maxRep")
        def maxRepUid           = column[Long]("maxRepUid")
        def label               = column[String]("label")
        
        def * = id ~ level ~ longitude ~ latitude ~ count ~ maxRep ~ maxRepUid ~ label
    }
    
    
    // A sensible radius threshold seems to be 40km (40,000)
    object Locations extends Table[(String, Double, Double, Double)]("Locations")
    {
        def name                = column[String]("name", O PrimaryKey)
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def radius              = column[Double]("radius")
        
        def * = name ~ longitude ~ latitude ~ radius
    }
    
    object Users extends Table[(Long, String, Long, Long, Long, Int, Int, String, String, Int, Int, Int)]("Users")
    {
        def user_id             = column[Long]("user_id", O PrimaryKey)
        def display_name        = column[String]("display_name")
        def creation_date       = column[Long]("creation_date")
        def last_access_date    = column[Long]("last_access_date")
        def reputation          = column[Long]("reputation")
        def age                 = column[Int]("age")
        def accept_rate         = column[Int]("accept_rate")
        def website_url         = column[String]("website_url")
        def location            = column[String]("location")
        def badge_gold          = column[Int]("badge_gold")
        def badge_silver        = column[Int]("badge_silver")
        def badge_bronze        = column[Int]("badge_bronze")
        
        def * = user_id ~ display_name ~ creation_date ~ last_access_date ~ reputation ~
                age ~ accept_rate ~ website_url ~ location ~ badge_gold ~ badge_silver ~ badge_bronze
    }
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
    
    def run() =
    {   
        println( "Deleting old data" )
        db withSession
        {
            println("1")
            ( for ( r <- CriticalMassTables.DataHierarchy ) yield r ).mutate( _.delete )
            println("2")
            ( for ( r <- CriticalMassTables.TagMap ) yield r ).mutate( _.delete )
            println("3")
            ( for ( r <- CriticalMassTables.UserMap ) yield r ).mutate( _.delete )
            println("4")
            //( for ( r <- CriticalMassTables.InstitutionMap ) yield r ).mutate( _.delete )
            println("5")
        }
                
        class UserTag( val id : Long, val name : String, val count : Long )        
        class UserData( val uid : Long, val reputation : Long, val lon : Double, val lat : Double, val tags : List[UserTag] )
        
        // Pull all the user data out into Scala
        println( "Pulling in all users with location data and tag data" )
        val allUsers = db withSession
        {
            val allUsers = (for ( Join(user, loc) <- 
                CriticalMassTables.Users innerJoin
                CriticalMassTables.Locations on(_.location is _.name)
                    if loc.radius < 100000.0 && user.reputation >= 2L )
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
        println( "  done" )
        
        // Run through the google map scales, merging as appropriate
        class UserCluster( val lon : Double, val lat : Double, val users : List[UserData] )
        {
            def this( ud : UserData ) = this( ud.lon, ud.lat, List(ud) )
            def dist( other : UserCluster ) = distfn( lon, lat, other.lon, other.lat )
            def merge( other : UserCluster) : UserCluster =
            {
                val size = users.size.toDouble
                val sizeOther = other.users.size.toDouble
                val total = size + sizeOther
                val newLon = (size/total) * lon + (sizeOther/total) * other.lon
                val newLat = (size/total) * lat + (sizeOther/total) * other.lat
                
                new UserCluster( newLon, newLat, users ++ other.users ) 
            }
            
            def coords = Array( lon, lat )
            def tupleCoords = (lon, lat)
        }
        
        println( "Building merge tree" )
        var mergeSet = immutable.HashMap[(Double, Double), UserCluster]()
        var debugCount = 0
        for ( ud <- allUsers )
        {
            val u = new UserCluster(ud)
            val nu = if ( mergeSet contains u.tupleCoords )
            {
                val original = mergeSet(u.tupleCoords)
                mergeSet -= u.tupleCoords
                debugCount -= 1
                new UserCluster( u.lon, u.lat, ud :: original.users )
            }
            else u
            
            debugCount += 1
            mergeSet += (nu.tupleCoords -> nu)
        }
        
        assert( debugCount == mergeSet.size )
        
        val mergeTree = new edu.wlu.cs.levy.CG.KDTree[UserCluster](2)
        for ( (c, u) <- mergeSet ) mergeTree.insert( u.coords, u )
        
        // In metres
        var maxMergeDistance = 0.2
        for ( level <- 16 to 0 by -1 )
        {
            println( "Merge distance: %f %d".format( maxMergeDistance, mergeSet.size ) ) 
            
            var finished = false
            
            while ( !finished )
            {
                // Choose a min distance cluster to merge
                var minDist : Option[(Double, UserCluster, UserCluster)] = None
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
            
            println( "  after merge: %d".format( mergeSet.size ) )
            
            db withSession
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
            }
            
            maxMergeDistance *= 2.0
        }
    }
}

class UserScraper( val db : Database )
{
    val stackOverflowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val yahooAPIKey = "50EgoNvV34HOEN8sYfWvUqVqpOfapxOSGBiRb7VjwbdsfYwolMb4XdFPhuuz"
    
    def run()
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
                    CriticalMassTables.Users insert (
                        u.user_id,
                        u.display_name,
                        u.creation_date,
                        u.last_access_date,
                        u.reputation,
                        u.age.getOrElse(-1),
                        u.accept_rate.getOrElse(-1),
                        u.website_url.getOrElse(""),
                        u.location.getOrElse("").toLowerCase,
                        u.badge_counts.gold,
                        u.badge_counts.silver,
                        u.badge_counts.bronze
                    )
                    count += 1
                }
                println( "                       ", count )
                Thread.sleep(500)
            }
            
            if ( true )
            {
                // Fetch all locations left un-geocoded and geocode via Yahoo
                val locations = for ( Join(u, l) <- CriticalMassTables.Users leftJoin CriticalMassTables.Locations on(_.location is _.name) if ((l.name isNull) && (u.location != "")) ) yield u.location
                
                val allLocs = locations.list
                val allNonEmptyLocs = allLocs.filter( _ != "" )
                val uniques = allNonEmptyLocs.toSet
                
                println( allLocs.size, allNonEmptyLocs.size, uniques.size )
                
                for ( (addr, count) <- uniques.toList.zipWithIndex )
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
                        
                        CriticalMassTables.Locations insert (
                            addr,
                            l.longitude.toDouble,
                            l.latitude.toDouble,
                            l.radius.toDouble )
                    }
                    else
                    {
                        // Enter a null location
                        CriticalMassTables.Locations insert (
                            addr,
                            0.0,
                            0.0,
                            -1.0 )
                    }
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
                
                Thread.sleep(200)
            }
        }
    }
}



class AboutMeParser
{
    val stackOverflowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    
    case class AboutMe( val user_id : Long, val display_name : String, val reputation : Long, val about_me : Option[String] )    

    // bzcat aboutmes.txt.bz2 | egrep -i "(developer|engineer|architect|manager|work|working|scientist|consultant|employed|lead) (at|for)" | wc

    // http://nlp.stanford.edu/software/stanford-dependencies.shtml
    def run()
    {
        implicit val formats = DefaultFormats
        
        val output = new java.io.FileWriter("aboutmes.txt")
        var descCount = 0
        var totalCount = 0
        for ( i <- 0 until 8000 )
        {
            println( "********* " + i + ": " + descCount + ", " + totalCount + " ********" )
            val userPull = SODispatch.pullJSON( "http://api.stackexchange.com/2.0/users", List(
                ("page", (i+1).toString),
                ("order", "desc"),
                ("sort", "reputation"),
                ("site", "stackoverflow"),
                ("pagesize", "100"),
                ("filter", "!9hnGsshlI"),
                ("key", stackOverflowKey) ) )
                
            
            val users = (userPull \ "items").children.map( _.extract[AboutMe] )
            totalCount += users.size
            
            for ( am <- users if am.about_me != None )
            {
                val aboutMeText = org.jsoup.Jsoup.parse(am.about_me.get).text().replace("\n", " ")
                output.write( am.user_id + " " + am.reputation + " " + aboutMeText + "\n" )
                descCount += 1
            }
            
            
            // Sentence detect using opennlp then pass through to stanford parser using:
            //java -cp "./*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser -sentences newline -outputFormat "oneline" edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz -
            //Or man-up and envelope it programatically.
            
            /*val sentenceModel = new opennlp.tools.sentdetect.SentenceModel( new java.io.FileInputStream( "./data/en-sent.bin" ) )
            val tokenModel = new opennlp.tools.tokenize.TokenizerModel( new java.io.FileInputStream( "./data/en-token.bin" ) )
            val orgModel = new opennlp.tools.namefind.TokenNameFinderModel( new java.io.FileInputStream("./data/en-ner-organization.bin") )
            
            val detector = new opennlp.tools.sentdetect.SentenceDetectorME(sentenceModel)
            val tokenizer = new opennlp.tools.tokenize.TokenizerME( tokenModel )
            val orgFinder = new opennlp.tools.namefind.NameFinderME( orgModel )
            
            for ( am <- users if am.about_me != None )
            {
                val aboutMeText = org.jsoup.Jsoup.parse(am.about_me.get).text()
                
                println( am.display_name, am.reputation )
                for ( s <- detector.sentDetect(aboutMeText) )
                {
                    println( "  ", s )
                    val tokens = tokenizer.tokenize(s)
                    val spans = orgFinder.find(tokens)
                    
                    for ( span <- spans )   
                    {
                        println( "  >>  ", tokens.slice(span.getStart(), span.getEnd()).toList )
                    }
                }
                
                orgFinder.clearAdaptiveData()
            }*/
            
            Thread.sleep(5000)
        }
        output.close()
    }
}

object Main extends App
{
    override def main( args : Array[String] ) =
    {
        val dbName = "stack_users"
        //val db = Database.forURL("jdbc:h2:tcp://localhost/%s;DB_CLOSE_DELAY=-1".format(dbName), driver = "org.h2.Driver")
        val db = Database.forURL("jdbc:h2:file:%s;DB_CLOSE_DELAY=-1".format(dbName), driver = "org.h2.Driver")
        
        if ( !new java.io.File("%s.h2.db".format(dbName)).exists() )
        {
            db withSession
            {
                (   CriticalMassTables.Users.ddl ++
                    CriticalMassTables.Locations.ddl ++
                    CriticalMassTables.Tags.ddl ++
                    CriticalMassTables.UserTags.ddl ++
                    CriticalMassTables.DataHierarchy.ddl ++ 
                    CriticalMassTables.TagMap.ddl ++
                    CriticalMassTables.SectorTags.ddl ++
                    CriticalMassTables.Institution.ddl ++
                    CriticalMassTables.UserRole.ddl ++
                    CriticalMassTables.RoleSOTags.ddl ++
                    CriticalMassTables.RoleSectorTags.ddl ) create
            }
        }
        //val qs = new UserScraper(db)
        //qs.run()
        //val mc = new MarkerClusterer(db)
        //mc.run()
        val ap = new AboutMeParser()
        ap.run()
    }
}
       
