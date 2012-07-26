package controllers

import play.api._
import play.api.mvc._

import org.scalaquery.session.Database
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

object CriticalMassTables
{
    import org.scalaquery.ql.extended.{ExtendedTable => Table}
    import org.scalaquery.ql.TypeMapper._
    import org.scalaquery.ql._
    
    object SectorTags extends Table[(Long, String)]("SectorTags")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    object Institution extends Table[(Long, String, String)]("Institutions")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        def url                 = column[String]("url")
        
        def * = id ~ name ~ url
    }
    
    object UserRole extends Table[(Long, Long, Long, String)]("UserRole")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def user_id             = column[Long]("user_id")
        def institution_id      = column[Long]("institution_id")
        def work_location       = column[String]("work_location")
        
        def * = id ~ user_id ~ institution_id ~ work_location
    }
    
    object RoleSOTags extends Table[(Long, Long)]("RoleSOTags")
    {
        def role_id             = column[Long]("role_id")
        def tag_id              = column[Long]("tag_id")
        
        def * = role_id ~ tag_id
    }
    
    object RoleSectorTags extends Table[(Long, Long)]("RoleSectorTags")
    {
        def role_id             = column[Long]("role_id")
        def tag_id              = column[Long]("tag_id")
        
        def * = role_id ~ tag_id
    }
    
    object Tags extends Table[(Long, String)]("Tags")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    // Top tags for a user, including counts
    object UserTags extends Table[(Long, Long, Long)]("UserTags")
    {
        def tag_id              = column[Long]("tag_id")
        def user_id             = column[Long]("user_id")
        def count               = column[Long]("count")
        
        def * = tag_id ~ user_id ~ count
    }
    
    // Top tags for a hierarchy area, including counts
    object TagMap extends Table[(Long, Long, Long)]("TagMap")
    {
        def dh_id               = column[Long]("dh_id")
        def tag_id              = column[Long]("tag_id")
        def count               = column[Long]("count")
        
        def * = dh_id ~ tag_id ~ count
    }
    
    // Users for a hierarchy area
    object UserMap extends Table[(Long, Long)]("UserMap")
    {
        def dh_id               = column[Long]("dh_id")
        def user_id             = column[Long]("user_id")
        
        def * = dh_id ~ user_id
    }
    
    object DataHierarchy extends Table[(Long, Int, Double, Double, Int, Int, Long, String)]("DataHierarchy")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def level               = column[Int]("level")
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def count               = column[Int]("count")
        def maxRep              = column[Int]("maxRep")
        def maxRepUid           = column[Long]("maxRepUid")
        def label               = column[String]("label")
        
        def * = id ~ level ~ longitude ~ latitude ~ count ~ maxRep ~ maxRepUid ~ label
    }
    
    
    // A sensible radius threshold seems to be 40km (40,000)
    object Locations extends Table[(String, Double, Double, Double)]("Locations")
    {
        def name                = column[String]("name", O PrimaryKey)
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def radius              = column[Double]("radius")
        
        def * = name ~ longitude ~ latitude ~ radius
    }
    
    object Users extends Table[(Long, String, Long, Long, Long, Int, Int, String, String, Int, Int, Int)]("Users")
    {
        def user_id             = column[Long]("user_id", O PrimaryKey)
        def display_name        = column[String]("display_name")
        def creation_date       = column[Long]("creation_date")
        def last_access_date    = column[Long]("last_access_date")
        def reputation          = column[Long]("reputation")
        def age                 = column[Int]("age")
        def accept_rate         = column[Int]("accept_rate")
        def website_url         = column[String]("website_url")
        def location            = column[String]("location")
        def badge_gold          = column[Int]("badge_gold")
        def badge_silver        = column[Int]("badge_silver")
        def badge_bronze        = column[Int]("badge_bronze")
        
        def * = user_id ~ display_name ~ creation_date ~ last_access_date ~ reputation ~
                age ~ accept_rate ~ website_url ~ location ~ badge_gold ~ badge_silver ~ badge_bronze
    }

    val dbUri="jdbc:h2:file:./stack_users;DB_CLOSE_DELAY=-1"
}

case class SupplementaryData(
    val workLocation : String,
    val institutionName : String,
    val institutionURL : String,
    val soTags : String,
    val sectorTags : String )

object Application extends Controller
{
    import play.api.cache.Cache
    import play.api.Play.current
    import play.api.data._
    import play.api.data.Forms._
    
    case class Pos( val name : String, val lon : Double, val lat : Double )
    case class UserData( val accessToken : String, val expiry : Int, val uid : Int, val name : String )
    case class UserRole( val institutionName : String, val url : String, val location : String, val soTags : List[String], val sectorTags : List[String] )
    
    val stackOverFlowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val stackOverFlowSecretKey = "aL1DlUG5A7M96N48t2*k0w(("
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    
    def index = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            Ok(views.html.index(Cache.getAs[UserData]("user")))
        }
    }
    
    
    val userForm = Form(
        mapping(
            "WorkLocation"      -> text,
            "InstitutionName"   -> text,
            "InstitutionURL"    -> text,
            "SOTags"            -> text,
            "SectorTags"        -> text
        )(SupplementaryData.apply)(SupplementaryData.unapply)
    )
    
    def refineUserAccept = Action
    { implicit request =>
        userForm.bindFromRequest.fold(
            errors => BadRequest,
            {
                case (data) =>
                {
                    println( "Received user data: " + data.toString )
                    
                    val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
                    db withSession
                    {
                        db withTransaction
                        {
                            def scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                            
                            def isId(v:String) = v.foldLeft(true)( (acc,c) => acc && (c >= '0' && c <= '9') )
                            
                            val currUser = Cache.getAs[UserData]("user").get
                            
                            val institutionId = if ( isId(data.institutionName) ) data.institutionName.toLong
                            else
                            {
                                val i = CriticalMassTables.Institution
                                (i.name ~ i.url) insert ((data.institutionName, data.institutionURL))
                                
                                Query(scopeIdentity).first
                            }
                            
                            val roleId =
                            {
                                val r = CriticalMassTables.UserRole
                                (r.user_id ~ r.institution_id ~ r.work_location) insert ((currUser.uid.toLong, institutionId, data.workLocation))
                                
                                Query(scopeIdentity).first
                            }
                            
                            for ( tag <- data.soTags.split(";") )
                            {
                                val tagId = tag.toLong
                                
                                CriticalMassTables.RoleSOTags insert ((roleId, tagId))
                            }
                            
                            for ( tag <- data.sectorTags.split(";") )
                            {
                                val tagId = if ( isId(tag) ) tag.toLong
                                else
                                {
                                    CriticalMassTables.SectorTags.name insert tag.toLowerCase
                                    
                                    Query(scopeIdentity).first
                                }
                                
                                CriticalMassTables.RoleSectorTags insert ((roleId, tagId))
                            }
                            
                            Redirect(routes.Application.index)
                        }
                    }
                }
            }
        )
    }
    
    
        
    def refineUser() = Action
    {        
        val currUser = Cache.getAs[UserData]("user").get
        Ok(views.html.refineuser(currUser, userForm))
    }
    
    def userHome() = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        val user = Cache.getAs[UserData]("user").get
        
        db withSession
        {
            // Get roles with location and two types of tags
            val roles = ( for ( Join(role, institution) <-
                CriticalMassTables.UserRole innerJoin
                CriticalMassTables.Institution
                on (_.institution_id is _.id)
                if role.user_id === user.uid.toLong )
                yield role.id ~ role.work_location ~ institution.name ~ institution.url ).list

            val res = for ( (rid, loc, instname, insturl) <- roles ) yield
            {
                val soTags = ( for ( Join(roleTags, tags) <-
                    CriticalMassTables.RoleSOTags innerJoin
                    CriticalMassTables.Tags
                    on (_.tag_id is _.id)
                    if roleTags.role_id === rid ) yield tags.name ).list
                    
                val sectorTags = ( for ( Join(roleTags, tags) <-
                    CriticalMassTables.RoleSectorTags innerJoin
                    CriticalMassTables.SectorTags
                    on (_.tag_id is _.id)
                    if roleTags.role_id === rid ) yield tags.name ).list
                
                new UserRole( instname, insturl, loc, soTags, sectorTags )   
            }

            Ok(views.html.userhome(user, res.toList))
        }
    }

    
    def mapData( loc : String ) = Action
    {
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        println( swlat, swlon, nelat, nelon, zoom )
        
        
        // If logged in, additionally join on the institution table and give
        // local insitution summaries, e.g. institution to location, SO rep, SO tags, Sector tags
        db withSession
        {
            val points = for 
            {
                dh <- CriticalMassTables.DataHierarchy if
                dh.level === (zoom.toInt) &&
                dh.longitude >= swlon && dh.longitude <= nelon &&
                dh.latitude >= swlat && dh.latitude <= nelat
            } yield dh.count ~ dh.longitude ~ dh.latitude ~ dh.label ~ dh.maxRep ~ dh.maxRepUid ~ dh.id
            
            val json = render( points.list.map( x => ("name" -> x._4) ~ ("lon" -> x._2.toString) ~ ("lat" -> x._3.toString) ~ ("count" -> x._1) ~ ("maxRep" -> x._5 ) ~ ("maxRepUid" -> x._6 ) ~ ("dh_id" -> x._7) ) )
            
            Ok(compact(json))
        }
    }
    
    def markerUsers( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            val users = (for
            {
                Join(userMap, users) <-
                CriticalMassTables.UserMap innerJoin
                CriticalMassTables.Users on (_.user_id is _.user_id)
                if userMap.dh_id === dh_id && users.reputation > 150L
                _ <- Query orderBy(Desc(users.reputation))
            } yield users.reputation ~ users.display_name ~ users.user_id ~ users.location)
            
            val topN = (users take 100).list
            
            // Inefficient - cache in Users tables
            val userTags = for ( u <- topN ) yield
            {
                val topTags = (for (Join(userTags, tag) <-
                    CriticalMassTables.UserTags innerJoin
                    CriticalMassTables.Tags on (_.tag_id is _.id)
                    if userTags.user_id === u._3) yield tag.name).take(5).list.mkString(" ")
                topTags
            }
            
            

            val userTableData = (topN zip userTags).map { case (x, t) =>
                ("reputation" -> x._1) ~
                ("name" -> "<a href=\"http://stackoverflow.com/users/%d\">%s</a>".format(x._3, x._2)) ~
                ("location" -> x._4) ~
                ("tags" -> t) }
    
            
            val json = ("aaData" -> userTableData)
            Ok(compact(render(json)))
        }
    }
    
    def markerTags( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        
        db withSession
        {
            val topTags = (for (Join(tagMap, tags) <-
                CriticalMassTables.TagMap innerJoin
                CriticalMassTables.Tags on (_.tag_id is _.id)
                if tagMap.dh_id === dh_id) yield tags.name ~ tagMap.count).list

            val tagData = topTags.map( t => ("tag" -> t._1) ~ ("count" -> t._2) )

            Ok(compact(render(tagData)))
        }
    }
    
    def logout() = Action
    {
        Cache.set("user", None)
        Redirect(routes.Application.index)
    }
    
    def authenticate( code : String ) = Action
    {
        import akka.util.Timeout
        import akka.dispatch.Await
        import play.api.libs.ws.WS
        import akka.util.duration._
        
        implicit val formats = net.liftweb.json.DefaultFormats
        
        // Post the code back to try to get an access token
        println( "Requesting access token" )
        val timeout = Timeout(5.seconds)
        val url = WS.url("https://stackexchange.com/oauth/access_token")
        val promiseRes = url.post( Map(
            "client_id"     -> Seq("498"),
            "code"          -> Seq(code),
            "redirect_uri"  -> Seq("http://www.stacknative.com/authenticate"),
            "client_secret" -> Seq(stackOverFlowSecretKey) ) )
        val post = promiseRes.await(5000).get.body


        val fields = post.split("&").map
        { el =>
            val Array( key, value ) = el.split("=") 
            (key, value)
        } toMap

        // Got an access token
        val accessToken = fields("access_token")
        val expires = fields("expires").toInt
        
        println( "Received access token: %s (expires: %f hours)".format( accessToken, (expires/3600.0) ) )
        
        println( "Getting details for authenticated user" )
        val uidurlRes = Dispatch.pullJSON("https://api.stackexchange.com/2.0/me",
            List(
            ("site",            "stackoverflow"),
            ("access_token",    accessToken),
            ("key",             stackOverFlowKey) ) )
            
        val response = uidurlRes.children.head

        val meuid = (response \ "user_id").extract[Int]
        val mename = (response \ "display_name").extract[String]

	    println( "User authenticated: ", meuid, mename )
                
        // Get user_id and display_name and stick them in the cache
        Cache.set("user", new UserData(accessToken, expires, meuid, mename ) )
        
        println( "User is %s (%d)".format( mename, meuid ) )
        
        // TODO: If this is their first login, ask for more details
        // namely finer location, company name
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        db withSession
        {
            val checkRoles = ( for ( r <- CriticalMassTables.UserRole if r.user_id === meuid.toLong ) yield r.id ).list
            if ( checkRoles.isEmpty )
            {
                Redirect(routes.Application.refineUser)
            }
            else
            {
                Redirect(routes.Application.userHome)
            }
        }
    }
    
    def locationBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        db withSession
        {
            val similar = ( for ( t <- CriticalMassTables.Locations if t.name like q.toLowerCase() + "%" ) yield t.name ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
    
    def workSectorBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        db withSession
        {
            val similar = ( for ( t <- CriticalMassTables.SectorTags if t.name like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
    
    def institutionBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        db withSession
        {
            val similar = ( for ( t <- CriticalMassTables.Institution if t.name like q + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
    
    
    def tagBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val db = Database.forURL(CriticalMassTables.dbUri, driver = "org.h2.Driver")
        db withSession
        {
            val similar = ( for ( t <- CriticalMassTables.Tags if t.name like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
  
}
