package controllers

import play.api._
import play.api.mvc._
import play.api.db._

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

object SODispatch
{
    implicit val formats = DefaultFormats
    
    def pullJSON( baseUrl : String, params : List[(String, String)] ) =
    {
        val j = Dispatch.pullJSON( baseUrl, params )
        
        val backoff = (j \ "backoff").extract[Option[Int]]
        val quota_remaining = j \ "quota_remaining"
        println( "Backoff: ", backoff, quota_remaining )
        
        backoff match
        {
            case Some( time ) => Thread.sleep( time * 1000 )
            case None =>
        }
        
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
        
        // Submit future
        Akka.future
        {
            // Call the work function, passing in a callback to update progress and status
            try
            {
                workFn( (progress : Double, status : String ) =>
                {
                    WithDbSession
                    {
                        val job = ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r )
                        
                        job.mutate ( m =>
                        {
                            m.row = m.row.copy(_3 = progress, _4 = status )
                        } )
                    }
                } )
                
                // Set status to complete
                WithDbSession
                {
                    ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r ).mutate( _.delete )
                }
            }
            catch
            {
                case e : Throwable =>
                {
                    println( "Boom1")
                    println( e.toString )
                    println( "Boom2" )
                    println( e.getStackTrace.map(_.toString).mkString(";") )
                    println( "Boom3")
                    // Set status to error
                    WithDbSession
                    {
                        println( "Boom4" )
                        val job = ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r )
                         
                        println( "Boom5" )
                        job.mutate ( m =>
                        {
                            val message = e.toString + ": " + e.getStackTrace.map(_.toString).mkString(";")
                            m.row = m.row.copy(_3 = 0.0, _4 = message )
                        } )
                    }
                    println( "Boom6" )
                }
                case e =>
                {
                    println( "Uncaught exception: " + e.toString )
                }
            }
            
            
        }
        
        uuid
    }
}