package org.seacourt.criticalmass

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import net.liftweb.json.{JsonParser, DefaultFormats}

object CriticalMassTables
{
    import org.scalaquery.ql.extended.{ExtendedTable => Table}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._

    object Tags extends Table[(Long, String)]("Tags")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    object UserTags extends Table[(Long, Long, Long)]("UserTags")
    {
        def tag_id              = column[Long]("tag_id")
        def user_id             = column[Long]("user_id")
        def count               = column[Long]("count")
        
        def * = tag_id ~ user_id ~ count
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
    

class UserScraper( val db : Database )
{
    val stackOverflowKey = "b*DdyNcsAlQOQak6IvFH*w(("
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
            
            if ( true )
            {
                val ukUsers = for ( Join(user, loc) <- CriticalMassTables.Users
                    innerJoin CriticalMassTables.Locations on(_.location is _.name)
                    if ( loc.longitude >= -12.5 && loc.longitude <= 2.7 &&
                         loc.latitude >= 49.9 && loc.latitude <= 59.7 &&
                         loc.radius < 100000.0 && user.reputation >= 2L ) ) yield user.display_name ~ loc.longitude ~ loc.latitude

                var count = 0
                for ( (name, lon, lat) <- ukUsers.list )
                {
                    println( "{ name : \"%s\", lon : \"%f\", lat : \"%f\" },".format( name, lon, lat ) )
                    count += 1
                }
                println( count )
                
                assert( false )
            }
            
            if ( false )
            {
                // Fetch all locations
                val locations = for ( u <- CriticalMassTables.Users ) yield u.location
                
                val allLocs = locations.list
                val allNonEmptyLocs = allLocs.filter( _ != "" )
                val uniques = allNonEmptyLocs.toSet
                
                //println( uniques.toList )
                println( allLocs.size, allNonEmptyLocs.size, uniques.size )
                
                for ( (addr, count) <- uniques.toList.zipWithIndex )
                {
                    val checkTable = for ( l <- CriticalMassTables.Locations if l.name === addr ) yield l.name
                    
                    if ( checkTable.list.isEmpty )
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
                
                // Now fetch tag stats for each user
                val ukUsers = for ( Join(user, loc) <- CriticalMassTables.Users
                    innerJoin CriticalMassTables.Locations on(_.location is _.name)
                    if ( loc.longitude >= -12.5 && loc.longitude <= 2.7 &&
                         loc.latitude >= 49.9 && loc.latitude <= 59.7 &&
                         loc.radius < 100000.0 && user.reputation >= 2L ) ) yield user.user_id ~ user.display_name
                         
                for ( (uid, name) <- ukUsers.list )
                {
                    val checkTable = for ( t <- CriticalMassTables.UserTags if t.user_id === uid ) yield t.user_id
                    
                    if ( checkTable.list.isEmpty )
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
                        
                        println( name, tags )
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
                            
                        //println( "%s -> %s".format(name, location) )
                        
                        Thread.sleep(200)
                    }
                }
                
                assert(false)
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

                // Show locations using the Google MarkerManager (https://developers.google.com/maps/articles/toomanymarkers)

                
            }
        }
    }
}

// Tags and synonyms: /tags, /tags/synonyms 

// Then user tags: /2.0/users/{1,2,3}/tags?order=desc&sort=popular&site=stackoverflow
            /*
            {
      "name": "windows",
      "count": 20,
      "is_required": false,
      "is_moderator_only": false,
      "user_id": 123,
      "has_synonyms": true
    }*/


object Main extends App
{
    override def main( args : Array[String] ) =
    {
        val dbName = "stack_users"
        val db = Database.forURL("jdbc:h2:file:%s;DB_CLOSE_DELAY=-1".format(dbName), driver = "org.h2.Driver")
        
        if ( !new java.io.File("%s.h2.db".format(dbName)).exists() )
        {
            db withSession
            {
                (CriticalMassTables.Users.ddl ++ CriticalMassTables.Locations.ddl ++ CriticalMassTables.Tags.ddl ++ CriticalMassTables.UserTags.ddl) create
            }
        }
        
        val qs = new UserScraper(db)
        qs.run()
    }
}

/*
{
            val items = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/users/1;2;3;4;5;6;7;8;9;10?order=desc&sort=reputation&site=stackoverflow" )
            
            val items = (json \ "items").children
            
            for ( user <- items )
            {
                println( user \ "display_name" )
                println( "  " + user \ "reputation" )
                println( "  " + user \ "location" )
            }
            
            // Geocode using the google api (max 15000 a day): https://developers.google.com/maps/documentation/geocoding/
            // http://maps.googleapis.com/maps/api/geocode/json?address=San+Diego,CA&sensor=false
            //
            // and p
            
            //println( res )
            //println( json.toString )
        }
        
         
            // Geocode using the google api (max 15000 a day): https://developers.google.com/maps/documentation/geocoding/
            // http://maps.googleapis.com/maps/api/geocode/json?address=San+Diego,CA&sensor=false
            


{
   "results" : [
      {
         "address_components" : [
            {
               "long_name" : "San Diego",
               "short_name" : "San Diego",
               "types" : [ "locality", "political" ]
            },
            {
               "long_name" : "San Diego",
               "short_name" : "San Diego",
               "types" : [ "administrative_area_level_2", "political" ]
            },
            {
               "long_name" : "California",
               "short_name" : "CA",
               "types" : [ "administrative_area_level_1", "political" ]
            },
            {
               "long_name" : "United States",
               "short_name" : "US",
               "types" : [ "country", "political" ]
            }
         ],
         "formatted_address" : "San Diego, CA, USA",
         "geometry" : {
            "bounds" : {
               "northeast" : {
                  "lat" : 33.1142490,
                  "lng" : -116.908160
               },
               "southwest" : {
                  "lat" : 32.5348560,
                  "lng" : -117.28216650
               }
            },
            "location" : {
               "lat" : 32.71532920,
               "lng" : -117.15725510
            },
            "location_type" : "APPROXIMATE",
            "viewport" : {
               "northeast" : {
                  "lat" : 32.86540990,
                  "lng" : -116.90113630
               },
               "southwest" : {
                  "lat" : 32.56499550,
                  "lng" : -117.41337390
               }
            }
         },
         "types" : [ "locality", "political" ]
      }
   ],
   "status" : "OK"
*/
       
