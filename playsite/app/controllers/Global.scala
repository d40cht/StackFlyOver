

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
        Logger.info( "Setting DH timestamp to: " + newTs )
        dhTimestamp = Some(newTs)
    }
    def getDHTimestamp = dhTimestamp

    override def onStart(app: Application)
    {
        import play.api.cache.Cache
        
        Logger.info("Application has started")

        implicit val ipapp = app

        Logger.info("Clearing out any jobs from a previous run")
        val db = Database.forDataSource(DB.getDataSource())
        db.withSession
        {
            ( for ( r <- controllers.CriticalMassTables.Jobs ) yield r ).mutate( _.delete )
        }
        
        // Find the current oldest data hierarchy
        db withSession
        {
            import org.scalaquery.simple.{StaticQuery}
            
            val q = StaticQuery[java.sql.Timestamp] +
                "SELECT DISTINCT \"created\" FROM \"DataHierarchy\" ORDER BY \"created\" DESC LIMIT 1"
            
            q.firstOption match
            {
                case Some(t)    => setDHTimestamp(t)
                case None       =>
            }
        }
        
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
                
                processing.FetchProfileImages.run(db)
                
                {
                    val l = new processing.LocationUpdater( db )
                    l.run( statusFn )
                }
                
                {
                    val userFetch = new processing.UserScraper(db)
                    userFetch.run( statusFn )
                }
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

package org.seacourt.global
{
    object Instance
    {
        def apply() = Global
    }
}

