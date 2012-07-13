import dispatch._
import net.liftweb.json.{JsonParser, DefaultFormats}

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._

import swing._
import swing.event.{TableRowsSelected, TableEvent, TableColumnsSelected, ButtonClicked}

import scala.collection.{mutable, immutable}


object Dispatch
{
    lazy val h = new Http
    
    def pullJSON( urlStr : String ) =
    {
        val u = url( urlStr )
        val res = h(u as_str)   
        JsonParser.parse(res)
    }
}

object DatabaseTables
{
    import org.scalaquery.ql.basic.{BasicTable}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._

    object Users extends BasicTable[(Long, String, Int)]("Users")
    {
        def user_id         = column[Long]("user_id", O PrimaryKey)
        def display_name    = column[String]("display_name")
        def reputation      = column[Int]("reputation")
        
        def * = user_id ~ display_name ~ reputation
    }
    
    object Questions extends BasicTable[(Long, String, Long, Int, Int, Int, String, String, Long)]("Questions")
    {
        def question_id     = column[Long]("question_id", O PrimaryKey)
        def title           = column[String]("title")
        def creation_date   = column[Long]("creation_date")
        def score           = column[Int]("score")
        def answer_count    = column[Int]("answer_count")
        def view_count      = column[Int]("view_count")
        def tags            = column[String]("tags")
        def link            = column[String]("link")
        def owner_id        = column[Long]("owner_id")
        
        def * = question_id ~ title ~ creation_date ~ score ~ answer_count ~ view_count ~ tags ~ link ~ owner_id
    }

}

case class User(
    val user_id : Long,
    val display_name : String,
    val reputation : Int )

case class Question(
    val question_id  : Long,
    val title : String,
    val creation_date : Long,
    val score : Int,
    val answer_count : Int,
    val view_count : Int,
    val tags : List[String],
    val link : String,
    val owner : User )


//JObject(List(JField(user_id,JInt(988)), JField(user_type,JString(registered)), JField(creation_date,JInt(1218456875)), JField(display_name,JString(staffan)), JField(profile_image,JString(http://www.gravatar.com/avatar/1bc901d89082a6481d8611d79ff8b33f?d=identicon&r=PG)), JField(reputation,JInt(1875)), JField(reputation_change_day,JInt(0)), JField(reputation_change_week,JInt(10)), JField(reputation_change_month,JInt(35)), JField(reputation_change_quarter,JInt(35)), JField(reputation_change_year,JInt(257)), JField(age,JInt(38)), JField(last_access_date,JInt(1341852024)), JField(last_modified_date,JInt(1332302157)), JField(is_employee,JBool(false)), JField(link,JString(http://stackoverflow.com/users/988/staffan)), JField(website_url,JString(http://none)), JField(location,JString(Sweden)), JField(account_id,JInt(758)), JField(badge_counts,JObject(List(JField(gold,JInt(1)), JField(silver,JInt(9)), JField(bronze,JInt(22))))), JField(accept_rate,JInt(86))))

case class Badges(
    val gold : Int,
    val silver : Int,
    val bronze : Int )

case class FullUser(
    val user_id : Long,
    val display_name : String,
    val creation_date : Long,
    val last_access_date : Long,
    val reputation : Long,
    val age : Option[Int],
    val accept_rate : Option[Int],
    val link : String,
    val website_url : Option[String],
    val location : Option[String],
    val badge_counts : Badges )
    

class UserScraper
{
    val key = "b*DdyNcsAlQOQak6IvFH*w(("
    
    def run()
    {
        for ( i <- 0 until 1000 by 100 )
        {
            val j = (i until i+100)
            implicit val formats = DefaultFormats
            
            val json = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/users/%s?site=stackoverflow&key=%s".format( j.mkString(";"), key) )
            val items = (json \ "items").children.map( _.extract[FullUser] )
            
            for ( i <- items )
            {
                println( i )
            }
            
            // Look locations up with google geocoder API, then check viewport area to validate the size
            // of the area described
        }
    }
}


class QuestionScraper( db : Database )
{
    //private var questionMap = Map[Long, Database.Question]()
    
    val key = "b*DdyNcsAlQOQak6IvFH*w(("
    
    def run()
    {
        import org.scalaquery.session._
        import org.scalaquery.session.Database.threadLocalSession
        import org.scalaquery.ql.basic.BasicDriver.Implicit._
        
        while ( true )
        {
            implicit val formats = DefaultFormats
            
            val json = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/questions?site=stackoverflow&pagesize=100&key=%s".format(key) )
            val items = (json \ "items").children
            
            val questions = items.map( _.extract[Question] )
            
            println( "Tick" )
            for ( q <- questions )
            {
                db withSession
                {
                    val existingUser = for ( u <- DatabaseTables.Users if u.user_id === q.owner.user_id ) yield u.reputation
                    existingUser.firstOption match
                    {
                        case Some(row) =>
                        {
                            existingUser.update( q.owner.reputation )
                        }
                        case None =>
                        {
                            DatabaseTables.Users insert ( q.owner.user_id, q.owner.display_name, q.owner.reputation )
                        }
                    }
                
                    val existingQuestion = for ( u <- DatabaseTables.Questions if u.question_id === q.question_id ) yield u.score ~ u.answer_count ~ u.view_count
                    existingQuestion.firstOption match
                    {
                        case Some(row) =>
                        {
                            existingQuestion.update( q.score, q.answer_count, q.view_count )
                            println( "Update: %d %d %d: %s".format( q.question_id, q.score, q.answer_count, q.view_count, q.title ) )
                        }
                        case None =>
                        {
                            DatabaseTables.Questions insert(
                                q.question_id,
                                q.title,
                                q.creation_date,
                                q.score,
                                q.answer_count,
                                q.view_count,
                                q.tags.mkString(";"),
                                q.link,
                                q.owner.user_id )
 
                            println( "New:    %d %d %d: %s".format( q.question_id, q.score, q.answer_count, q.title ) )
                        }
                    }
                }
            }
            
            val backoff = json \ "backoff"
            println( "Backoff: " + backoff )
            Thread.sleep(30 * 1000)
        }
    }
}

class QuestionView( db : Database ) extends SimpleSwingApplication
{
    setSystemLookAndFeel()

    var rows = mutable.ArrayBuffer[Array[Any]]()
    
    def top = new MainFrame
    {
        title = "StackOverflow Questions"
        
        db withSession
        {
            val qupdate = for ( q <- DatabaseTables.Questions ) yield q.score ~ q.answer_count ~ q.view_count ~ q.title ~ q.tags
            for ( (s, a, v, t, tags) <- qupdate.list )
            {
                rows.append( Array[Any](s, a, v, t, tags) )
            }
        }
        
        contents = ui
    }
    
    lazy val ui = new BoxPanel(Orientation.Vertical)
    {
        val headers = List( "Score", "Answers", "Views", "Title", "Tags" )
        val table = new swing.Table(rows.toArray, headers)
        
        contents += new ScrollPane(table)
    }
    
    def setSystemLookAndFeel()
    {
        import javax.swing.UIManager
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    }
}

object Main extends App
{
    override def main( args : Array[String] ) =
    {
        val dbName = "stack_questions"
        val db = Database.forURL("jdbc:h2:file:%s;DB_CLOSE_DELAY=-1".format(dbName), driver = "org.h2.Driver")
        
        if ( !new java.io.File("%s.h2.db".format(dbName)).exists() )
        {
            db withSession
            {
                (DatabaseTables.Users.ddl ++ DatabaseTables.Questions.ddl) create
            }
        }
        
        val qs = new UserScraper()
        qs.run()
        
        /*if ( false )
        {
            val qs = new QuestionScraper(db)    
            qs.run()
        }
        else
        {
            val qv = new QuestionView(db)
            qv.main( args )
        }*/
    }
}

