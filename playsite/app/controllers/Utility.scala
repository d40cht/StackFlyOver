package controllers

import play.api._
import play.api.mvc._
import play.api.db._

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}
import org.scalaquery.ql.Ordering._

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

object SODispatch
{
    implicit val formats = DefaultFormats
    
    def pullJSON( baseUrl : String, params : List[(String, String)] ) =
    {
        val j = Dispatch.pullJSON( baseUrl, params )
        
        val backoff = (j \ "backoff").extract[Option[Int]]
        val quota_remaining = j \ "quota_remaining"
        Logger.debug( "Backoff: %s %s".format(backoff.toString, quota_remaining.toString) )
        
        backoff match
        {
            case Some( time ) => Thread.sleep( time * 1000 )
            case None =>
        }

        Thread.sleep( 1000 )
        
        assert( quota_remaining.extract[Int] > 1 )
        j
    }
}

object WithDbSession
{
    def apply[T]( block : => T )( implicit app : Application ) : T =
    {
        val db = Database.forDataSource(DB.getDataSource())

        db.withSession(block)
    }
}

object DBUtil
{
    def clearTable( db : Database, tableName : String ) =
    {
        db withSession
        { session : org.scalaquery.session.Session =>
        
            val ps = session.prepareStatement( "DELETE FROM \"" + tableName + "\"" )
            ps.execute()
        }
    }
}

case class YahooLocation(
    latitude        : String,
    longitude       : String,
    radius          : String,
    quality	        : String,
    // New
    neighborhood    : String,
    city            : String,
    county          : String,
    state           : String,
    country         : String )

object YahooGeocoder
{
    import net.liftweb.json.{JsonParser, DefaultFormats}
    implicit val formats = DefaultFormats
    
    val yahooAPIKey = "50EgoNvV34HOEN8sYfWvUqVqpOfapxOSGBiRb7VjwbdsfYwolMb4XdFPhuuz"
    
    def apply( addr : String ) =
    {
        val locationJ = Dispatch.pullJSON( "http://where.yahooapis.com/geocode", List(
            ("flags", "J"),
            ("q", addr),
            ("appid", yahooAPIKey) ) )
            
        val locations = (locationJ \ "ResultSet" \ "Results").children.map( _.extract[YahooLocation] ).sortWith( _.quality.toDouble > _.quality.toDouble )
        
        locations
    }
}

object JobRegistry
{
    class JobStatus(
        val id : String,
        val name : String,
        val progress : Double,
        val status : String,
        val startTime : java.sql.Timestamp,
        val endTime : java.sql.Timestamp )
    
    def getJobs( implicit app : Application ) : List[JobStatus] = WithDbSession
    {
        WithDbSession
        {
            ( for ( r <- CriticalMassTables.Jobs ) yield r ).list.map( x => new JobStatus( x._1, x._2, x._3, x._4, x._5, x._6 ) )
        }
    }
    
    def submit( name : String, workFn : ((Double, String) => Unit) => Unit )( implicit app : Application ) =
    {
        import play.api.libs.concurrent.Akka
        
        // Register job in db
        val uuid = java.util.UUID.randomUUID().toString
        WithDbSession
        {
            val now = new java.sql.Timestamp( (new java.util.Date()).getTime )
            CriticalMassTables.Jobs insert (uuid, name, 0.0, "Submitted", now, now )
        }
        
        def reportProgressFn( progress : Double, status : String ) =
        {
            Logger.debug( progress.toString + ": " + status )
            WithDbSession
            {
                val job = ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r )
                
                job.mutate ( m =>
                {
                    m.row = m.row.copy(_3 = progress, _4 = status )
                } )
            }
        }
        
        // Submit future
        Akka.future
        {
            // Call the work function, passing in a callback to update progress and status
            try
            {
                workFn( reportProgressFn )
               
                reportProgressFn( 100.0, "Complete" )
                // Set status to complete
                /*WithDbSession
                {
                    ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r ).mutate( _.delete )
                }*/
            }
            catch
            {
                case e : Throwable =>
                {
                    Logger.error( "Error in job", e )
                    // Set status to error
                    WithDbSession
                    {
                        val job = ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r )
                         
                        try
                        {
                            job.mutate ( m =>
                            {
                                val message = e.toString + ": " + e.getStackTrace.map(_.toString).mkString(";")
                                m.row = m.row.copy(_3 = 0.0, _4 = message.take(255) )
                            } )
                        }
                        catch
                        {
                            case e : Throwable => Logger.error( "Error setting job error status", e )
                        }
                    }
                }
                case e =>
                {
                    Logger.error( "Uncaught exception in job", e )
                }
            }
            
            
        }
        
        uuid
    }
}


object Testing
{
    import controllers.CriticalMassTables
    
    private lazy val db =
    {
        val dbName = "stack_users"
        Database.forURL("jdbc:h2:file:./%s;DB_CLOSE_DELAY=-1;TRACE_LEVEL_FILE=3;TRACE_MAX_FILE_SIZE=50".format(dbName), driver = "org.h2.Driver")
    }
    
    
    def recalculateRanks = processing.RankGenerator.recalculateRanks( db )
    
    
    /*def userRanking( userId : Long ) =
    {
        db withSession
        {
            val urg = new processing.UserRankingsGenerator( db )
            
            urg.run( userId )
        }
    }*/
    
    def locationStats() =
    {
        db withSession
        {
            println( "Num relevant users: " + (for ( u <- CriticalMassTables.Users if u.reputation > processing.Parameters.minUserRepForDetail ) yield u.user_id ).elements.size )
            println( "Num cities: " + (for ( l <- CriticalMassTables.Location; _ <- Query groupBy l.city ) yield l.city).elements.size )
            println( "Num states: " + (for ( l <- CriticalMassTables.Location; _ <- Query groupBy l.state ) yield l.state).elements.size )
            println( "Num countries: " + (for ( l <- CriticalMassTables.Location; _ <- Query groupBy l.country ) yield l.country).elements.size )
        }
    }
}



