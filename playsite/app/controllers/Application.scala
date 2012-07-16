package controllers

import play.api._
import play.api.mvc._

object Application extends Controller
{
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    def index = Action
    {
        Ok(views.html.index("Your new application is ready."))
    }
  
}
