package org.seacourt.criticalmass

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._

import net.liftweb.json.{JsonParser, DefaultFormats}

object CriticalMassTables
{
    import org.scalaquery.ql.basic.{BasicTable}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._

    // Tags, then user -> tag count
    // Then location string -> long/lat, area
    /*object Tags extends BasicTable[(Long, String)]("Tags")
    {
    }
    
    object UserTags extends BasicTable[(Long, Long, Long)]
    {
    }*/ 
    
    object Users extends BasicTable[(Long, String, Long, Long, Long, Int, Int, String, String, Int, Int, Int)]("Users")
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
    val user_id             : Long
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
            
            if ( false )
            {
                val locations = for ( u <- CriticalMassTables.Users ) yield u.location
                
                /*var uniques = Set[String]()
                for ( l <- locations.list )
                {
                    uniques += l
                }*/
                val allLocs = locations.list.map( _.toLowerCase )
                val allNonEmptyLocs = allLocs.filter( _ != "" )
                val uniques = allNonEmptyLocs.toSet
                
                //println( uniques.toList )
                println( allLocs.size, allNonEmptyLocs.size, uniques.size )
                
                for ( addr <- uniques )
                {
                    val locationJ = Dispatch.pullJSON( "http://where.yahooapis.com/geocode", List(
                        ("flags", "J"),
                        ("q", addr),
                        ("appid", yahooAPIKey) ) )
                        
                    //println( locationJ )
                    val locations = (locationJ \ "ResultSet" \ "Results").children
                    for ( l <- locations )
                    {
                        println( addr, l.extract[YahooLocation] )
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
                        u.location.getOrElse(""),
                        u.badge_counts.gold,
                        u.badge_counts.silver,
                        u.badge_counts.bronze
                    )
                    count += 1
                }
                println( "                       ", count )
                Thread.sleep(500)
                
                // Look locations up with http://developer.yahoo.com/geo/placefinder/ API, store centre and radius
                //
                // http://where.yahooapis.com/geocode?flags=J&q=Appleton+Oxford+UK&appid=
                // {"ResultSet":{"version":"1.0","Error":0,"ErrorMessage":"No error","Locale":"us_US","Quality":50,"Found":1,"Results":[{"quality":50,"latitude":"51.710590","longitude":"-1.363550","offsetlat":"51.710590","offsetlon":"-1.363550","radius":2700,"name":"","line1":"","line2":"Appleton, Abingdon","line3":"OX13 5","line4":"United Kingdom","house":"","street":"","xstreet":"","unittype":"","unit":"","postal":"OX13 5","neighborhood":"Appleton","city":"Abingdon","county":"Oxfordshire","state":"England","country":"United Kingdom","countrycode":"GB","statecode":"ENG","countycode":"OXF","uzip":"OX13 5","hash":"","woeid":10893,"woetype":7}]}}
                //
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
                (CriticalMassTables.Users.ddl) create
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
       
