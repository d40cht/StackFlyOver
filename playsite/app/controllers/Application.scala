package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._



object Application extends Controller
{
    case class Pos( val name : String, val lon : Double, val lat : Double )
    
    lazy val data = List(
        new Pos( "braklet", -0.127144, 51.506325 ),
        new Pos( "John", -1.407319, 50.909940 ),
        new Pos( "Mat", -1.464544, 53.383110 ),
        new Pos( "saniul", -0.127144, 51.506325 ),
        new Pos( "Peter Coulton", -1.237634, 54.573055 ),
        new Pos( "Luke Girvin", 1.294900, 52.622495 ),
        new Pos( "Stephen Hendry", -0.127144, 51.506325 ),
        new Pos( "Rob Burke", -6.248274, 53.348070 ),
        new Pos( "Darren Greaves", -0.127144, 51.506325 ),
        new Pos( "Arnold Zokas", -0.127144, 51.506325 ),
        new Pos( "UberAlex", -6.248274, 53.348070 ),
        new Pos( "Sebastjan Trepča", -0.127144, 51.506325 ),
        new Pos( "robaker", -2.441765, 52.676662 ),
        new Pos( "Brendan", -2.591564, 51.453730 ),
        new Pos( "Dan", -0.127144, 51.506325 ),
        new Pos( "Ray", -0.127144, 51.506325 ),
        new Pos( "Scott Cowan", -0.127144, 51.506325 ),
        new Pos( "roryf", -3.202774, 55.954155 ),
        new Pos( "Binarytales", -0.134429, 50.828481 ),
        new Pos( "ZombieSheep", -1.546579, 53.794493 )
    )
    
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    def index = Action
    {
        val db = Database.forURL("jdbc:h2:file:/home/alex/Devel/AW/CriticalMass/stack_users;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
        
        db withSession
        {
            Ok(views.html.index("Your new application is ready."))
        }
    }
    
    def mapData( loc : String ) = Action
    {
        // http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m4.png
        // Then ClusterIcon from here: http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/src/markerclusterer.js
        // which manages text over the top of the icon
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        println( swlat, swlon, nelat, nelon, zoom )
        val json = render(data.map( x => ("name" -> x.name) ~ ("lon" -> x.lon.toString) ~ ("lat" -> x.lat.toString) ))
        
        Ok(compact(json))
    }
  
}
