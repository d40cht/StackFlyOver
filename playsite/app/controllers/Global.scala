package org.seacourt.global

import play.api._
import play.api.db._

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import utils.QuartzScheduler


object Global extends GlobalSettings
{
    private var dhTimestamp : Option[java.sql.Timestamp] = None
    
    def setDHTimestamp( newTs : java.sql.Timestamp ) = this.synchronized
    {
        dhTimestamp = Some(newTs)
    }
    def getDHTimestamp = dhTimestamp

    override def onStart(app: Application)
    {
        import play.api.cache.Cache
        
        Logger.info("Application has started")

        implicit val ipapp = app

        // Clear out any jobs from a previous run...
        val db = Database.forDataSource(DB.getDataSource())
        db.withSession
        {
            ( for ( r <- controllers.CriticalMassTables.Jobs ) yield r ).mutate( _.delete )
        }
        
        // Find the current oldest data hierarchy
        setDHTimestamp( db withSession
        {
            import org.scalaquery.simple.{StaticQuery}
            
            val q = StaticQuery[java.sql.Timestamp] +
                "SELECT DISTINCT \"created\" FROM ORDER BY \"created\" ASC LIMIT 1"
            
            q.first
        } )
        
        // Start the Quartz job scheduler
        Logger.info( "Starting the Quartz scheduler" )
        QuartzScheduler.start()
        //QuartzScheduler schedule("job 1 ", foo) every (5 seconds)
        //QuartzScheduler schedule("job 2 ", bar) at "0 0 3 * * ? *"

        // 10:15:00 on any day of any month, any day of the week on any year
        QuartzScheduler schedule( "Data scrape",
        {
            controllers.JobRegistry.submit( "User scrape job",
            { statusFn =>
                
                val db = Database.forDataSource(DB.getDataSource())
                val userFetch = new processing.UserScraper(db)
                userFetch.run( statusFn )
            } )
        } ) at "0 15 23 * * ?"
    }  

    override def onStop(app: Application)
    {
        // Stopping the Quartz scheduler
        Logger.info( "Stopping the Quartz scheduler" )
        QuartzScheduler.stop()
        Logger.info( "Application shutdown..." )
    }  
    
}

