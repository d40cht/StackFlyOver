import play.api._
import play.api.db._

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.info("Application has started")
    
    implicit val ipapp = app
    // Clear out any jobs from a previous run...
    val db = Database.forDataSource(DB.getDataSource())
    db.withSession
    {
        ( for ( r <- controllers.CriticalMassTables.Jobs ) yield r ).mutate( _.delete )
    }
  }  
  
  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
  }  
    
}

