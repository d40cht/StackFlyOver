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
    
    object Questions extends BasicTable[(Long, String, Long, Int, Int, String, String, Long)]("Questions")
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

class QuestionScraper( db : Database )
{
    //private var questionMap = Map[Long, Database.Question]()
    
    def run()
    {
        import org.scalaquery.session._
        import org.scalaquery.session.Database.threadLocalSession
        import org.scalaquery.ql.basic.BasicDriver.Implicit._
        
        while ( true )
        {
            implicit val formats = DefaultFormats
            
            val json = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/questions?site=stackoverflow&pagesize=100" )
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

    def top = new MainFrame
    {
        title = "StackOverflow Questions"
        contents = ui
    }
    
    val headers = List( "Score", "Answers", "Views", "Title", "Tags" )
    
    var rows = mutable.ArrayBuffer[Array[Any]]()
    db withSession
    {
        for ( q <- DatabaseTables.Questions ) yield q.score ~ q.answer_count ~ q.view_count ~ q.title ~ q.tags
        
        for ( (s, a, v, t, tags) <- q )
        {
            rows.append( Array(s.toString, a.toString, v.toString, t, tags) )
        }
    }
    val rowData = rows.toArray
    
    lazy val ui = new BoxPanel(Orientation.Vertical)
    {
        val table = new swing.Table(rowData, headers)
        
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
        
        if ( true )
        {
            val qs = new QuestionScraper(db)    
            qs.run()
        }
        else
        {
            val qv = new QuestionView(db)
            qv.main( args )
        }
    }
}

