package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

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
    case class Pos( val name : String, val lon : Double, val lat : Double )
    
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    def index = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            Ok(views.html.index("Your new application is ready."))
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
  
}
