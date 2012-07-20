package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._

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
    val dbUri="jdbc:h2:file:./stack_users;DB_CLOSE_DELAY=-1"
}

object Application extends Controller
{
    import play.api.cache.Cache
    import play.api.Play.current
    
    case class Pos( val name : String, val lon : Double, val lat : Double )
    case class UserData( val accessToken : String, val expiry : String, val uid : Int, val name : String )
    
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
        
        // http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m4.png
        // Then ClusterIcon from here: http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/src/markerclusterer.js
        // which manages text over the top of the icon
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        println( swlat, swlon, nelat, nelon, zoom )
        //val json = render(data.map( x => ("name" -> x.name) ~ ("lon" -> x.lon.toString) ~ ("lat" -> x.lat.toString) ))
        
        db withSession
        {
            val points = for 
            {
                dh <- CriticalMassTables.DataHierarchy if
                dh.level === (zoom.toInt) &&
                dh.longitude >= swlon && dh.longitude <= nelon &&
                dh.latitude >= swlat && dh.latitude <= nelat
            } yield dh.count ~ dh.longitude ~ dh.latitude ~ dh.label ~ dh.maxRep ~ dh.maxRepUid
            
            val json = render( points.list.map( x => ("name" -> x._4) ~ ("lon" -> x._2.toString) ~ ("lat" -> x._3.toString) ~ ("count" -> x._1) ~ ("maxRep" -> x._5 ) ~ ("maxRepUid" -> x._6 ) ) )
            
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
        val expires = fields("expires")
        
        val uidurlRes = Dispatch.pullJSON("https://api.stackexchange.com/2.0/me",
            List(
            ("site",       "stackoverflow"),
            ("access_token", accessToken),
            ("key",          stackOverFlowKey) ) )
            
        val response = uidurlRes.children.head

        val meuid = (response \ "user_id").extract[Int]
        val mename = (response \ "display_name").extract[String]
        
        Cache.set("user", new UserData(accessToken, expires, meuid, mename ) )
        
        // Get user_id and display_name and stick them in the cache
        
        Redirect(routes.Application.index)
    }
  
}
