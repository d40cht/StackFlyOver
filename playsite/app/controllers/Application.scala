package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

object Dispatch
{
    import dispatch._
    
    lazy val h = new Http
    
    def pullJSON( baseUrl : String, params : List[(String, String)] ) =
    {
        import java.net.URLEncoder.encode
        
        val fullUrl = baseUrl + "?" + params.map( x => encode(x._1, "utf-8") + "=" + encode(x._2, "utf-8") ).mkString("&")
        val u = url( fullUrl )
        val res = h(u as_str)   
        val j = JsonParser.parse(res)
        
        
        
        j
    }
}

object CriticalMassTables
{
    import org.scalaquery.ql.extended.{ExtendedTable => Table}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._
    
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
    
    object Locations extends Table[(String, Double, Double, Double)]("Locations")
    {
        def name                = column[String]("name", O PrimaryKey)
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def radius              = column[Double]("radius")
        
        def * = name ~ longitude ~ latitude ~ radius
    }
    
    object UserMap extends Table[(Long, Long)]("UserMap")
    {
        def dh_id               = column[Long]("dh_id")
        def user_id             = column[Long]("user_id")
        
        def * = dh_id ~ user_id
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
    
    val dbUri="jdbc:h2:file:./stack_users;DB_CLOSE_DELAY=-1"
}

object Application extends Controller
{
    import play.api.cache.Cache
    import play.api.Play.current
    
    case class Pos( val name : String, val lon : Double, val lat : Double )
    case class UserData( val accessToken : String, val expiry : Int, val uid : Int, val name : String )
    
    val stackOverFlowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val stackOverFlowSecretKey = "aL1DlUG5A7M96N48t2*k0w(("
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    def index = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            Ok(views.html.index(Cache.getAs[UserData]("user")))
        }
    }
    
    def mapData( loc : String ) = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        println( swlat, swlon, nelat, nelon, zoom )
        
        db withSession
        {
            val points = for 
            {
                dh <- CriticalMassTables.DataHierarchy if
                dh.level === (zoom.toInt) &&
                dh.longitude >= swlon && dh.longitude <= nelon &&
                dh.latitude >= swlat && dh.latitude <= nelat
            } yield dh.count ~ dh.longitude ~ dh.latitude ~ dh.label ~ dh.maxRep ~ dh.maxRepUid ~ dh.id
            
            val json = render( points.list.map( x => ("name" -> x._4) ~ ("lon" -> x._2.toString) ~ ("lat" -> x._3.toString) ~ ("count" -> x._1) ~ ("maxRep" -> x._5 ) ~ ("maxRepUid" -> x._6 ) ~ ("dh_id" -> x._7) ) )
            
            Ok(compact(json))
        }
    }
    
    def markerUsers( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            val users = (for
            {
                Join(userMap, users) <-
                CriticalMassTables.UserMap innerJoin
                CriticalMassTables.Users on (_.user_id is _.user_id)
                if userMap.dh_id === dh_id
                _ <- Query orderBy(Desc(users.reputation))
            } yield users.reputation ~ users.display_name ~ users.user_id ~ users.location)
            
            val firstN = users take 100
            
            val json = render( "aaData" -> firstN.list.map( x =>
                ("reputation" -> x._1) ~
                ("name" -> "<a href=\"http://stackoverflow.com/users/%d\">%s</a>".format(x._3, x._2)) ~
                ("location" -> x._4) ) )
            
            Ok(compact(json))
        }
    }
    
    def logout() = Action
    {
        Cache.set("user", None)
        Redirect(routes.Application.index)
    }
    
    def authenticate( code : String ) = Action
    {
        import akka.util.Timeout
        import akka.dispatch.Await
        import play.api.libs.ws.WS
        import akka.util.duration._
        
        implicit val formats = net.liftweb.json.DefaultFormats
        
        // Post the code back to try to get an access token
        println( "Requesting access token" )
        val timeout = Timeout(5.seconds)
        val url = WS.url("https://stackexchange.com/oauth/access_token")
        val promiseRes = url.post( Map(
            "client_id"     -> Seq("498"),
            "code"          -> Seq(code),
            "redirect_uri"  -> Seq("http://www.stackflyover.com/authenticate"),
            "client_secret" -> Seq(stackOverFlowSecretKey) ) )
        val post = promiseRes.await(5000).get.body


        val fields = post.split("&").map
        { el =>
            val Array( key, value ) = el.split("=") 
            (key, value)
        } toMap

        // Got an access token
        val accessToken = fields("access_token")
        val expires = fields("expires").toInt
        
        println( "Received access token: %s (expires: %f hours)".format( accessToken, (expires/3600.0) ) )
        
        println( "Getting details for authenticated user" )
        val uidurlRes = Dispatch.pullJSON("https://api.stackexchange.com/2.0/me",
            List(
            ("site",            "stackoverflow"),
            ("access_token",    accessToken),
            ("key",             stackOverFlowKey) ) )
            
        val response = uidurlRes.children.head

        val meuid = (response \ "user_id").extract[Int]
        val mename = (response \ "display_name").extract[String]

	println( "User authenticated: ", meuid, mename )
                
        // Get user_id and display_name and stick them in the cache
        Cache.set("user", new UserData(accessToken, expires, meuid, mename ) )
        
        println( "User is %s (%d)".format( mename, meuid ) )
        
        // TODO: If this is their first login, ask for more details
        // namely finer location, company name
        
        Redirect(routes.Application.index)
    }
  
}
