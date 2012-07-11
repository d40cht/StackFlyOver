import dispatch._
import net.liftweb.json.{JsonParser, DefaultFormats}


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

case class User(
    val user_id : String,
    val display_name : String,
    val reputation : Long )

case class Question(
    val title : String,
    val creation_date : Long,
    val score : Int,
    val question_id  : Long,
    val answer_count : Int,
    val tags : List[String],
    val link : String,
    val owner : User )
    
class QuestionScraper
{
    private var questionMap = Map[Long, Question]()
    
    def run()
    {
        while ( true )
        {
            implicit val formats = DefaultFormats
            
            val json = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/questions?site=stackoverflow&pagesize=100" )
            val items = (json \ "items").children
            
            val questions = items.map( _.extract[Question] )
            
            println( "Tick" )
            for ( q <- questions )
            {
                if ( !(questionMap contains q.question_id) )
                {
                    println( q.title )
                }
                questionMap += (q.question_id -> q)
            }
            
            val backoff = json \ "backoff"
            println( "Backoff: " + backoff )
            Thread.sleep(10 * 1000)
        }
    }
}

object Main extends App
{
    override def main( args : Array[String] ) =
    {
        val qs = new QuestionScraper()
            
        qs.run()
    }
}

