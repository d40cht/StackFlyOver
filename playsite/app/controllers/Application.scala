package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._


object Application extends Controller
{
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    def index = Action
    {
        val db = Database.forURL("jdbc:h2:file:/home/alex/Devel/AW/CriticalMass/stack_users;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
        
        db withSession
        {
            Ok(views.html.index("Your new application is ready."))
        }
    }
  
}
