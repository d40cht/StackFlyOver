package controllers

import play.api._
import play.api.mvc._
import play.api.db._

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
    
    object Institution extends Table[(Long, String)]("Institutions")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
    
    object UserRole extends Table[(Long, Long, Long, String, String, String)]("UserRole")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def user_id             = column[Long]("user_id")
        def institution_id      = column[Long]("institution_id")
        def department          = column[String]("department")
        def url                 = column[String]("url")
        def location            = column[String]("location")
        
        def * = id ~ user_id ~ institution_id ~ department ~ url ~ location
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
    
    object Jobs extends Table[(String, String, Double, String)]("Jobs")
    {
        def job_id              = column[String]("job_id", O PrimaryKey)
        def name                = column[String]("name")
        def progress            = column[Double]("progress")
        def status              = column[String]("status")
        
        def * = job_id ~ name ~ progress ~ status
    }
}





case class SupplementaryData(
    val workLocation : String,
    val institutionName : String,
    val institutionURL : String,
    val institutionDepartment : String,
    val soTags : String,
    val sectorTags : String,
    val anonymize : Boolean )

object Application extends Controller
{
    import play.api.cache.Cache
    import play.api.Play.current
    import play.api.data._
    import play.api.data.Forms._
    
    
    object JobRegistry
    {
        def initialise =
        {
            // Clear out any jobs from a previous run...
            ( for ( r <- CriticalMassTables.Jobs ) yield r ).mutate( _.delete )
        }
        
        def getJobs = withDbSession
        {
            ( for ( r <- CriticalMassTables.Jobs ) yield r.name ~ r.progress ~ r.status ).list
        }
        
        def submit( name : String, workFn : ((Double, String) => Unit) => Unit ) =
        {
            import play.api.libs.concurrent.Akka
            
            // Register job in db
            val uuid = java.util.UUID.randomUUID().toString
            withDbSession
            {
                CriticalMassTables.Jobs insert (uuid, name, 0.0, "Submitted")
            }
            
            // Submit future
            Akka.future
            {
                // Call the work function, passing in a callback to update progress and status
                workFn( (progress : Double, status : String ) =>
                {
                    withDbSession
                    {
                        val job = ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r.progress ~ r.status )
                        
                        job.mutate ( m =>
                        {
                            m.row = m.row.copy(_1 = progress, _2 = status )
                        } )
                    }
                } )
                
                // Set status to complete
                ( for ( r <- CriticalMassTables.Jobs if r.job_id === uuid ) yield r ).mutate( _.delete )
            }
            
            uuid
        }
    }

    case class Pos( val name : String, val lon : Double, val lat : Double )
    case class UserData( val accessToken : String, val expiry : Int, val uid : Int, val name : String )
    {
        def isAdmin = (uid == 415313)
    }

    case class UserRole( val institutionName : String, val url : String, val department : String, val location : String, val soTags : List[String], val sectorTags : List[String], val anonymize : Boolean )
    
    val stackOverFlowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val stackOverFlowSecretKey = "aL1DlUG5A7M96N48t2*k0w(("
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    
    class SessionCache( val uuid : String )
    {
        import play.api.cache.CacheAPI
        import play.api.cache.CachePlugin
        
        def get( key : String ) = Cache.get( uuid+key )
        def getAs[T]( key : String )(implicit app: Application, m: ClassManifest[T]) = Cache.getAs[T]( uuid+key )(app, m)
        def set( key : String, value : Any ) = Cache.set( uuid+key, value )
        def contains( key : String ) = Cache.get( uuid+key) != None
        def remove( key : String )(implicit app: Application) = Cache.set( uuid+key, None, 1 )
    }
    
    private def withDbSession[T]( block : => T ) : T =
    {
        val db = Database.forDataSource(DB.getDataSource())

        db.withSession(block)
    }
    
    
    
    // An action wrapped to pass through the cache for this session
    object SessionCacheAction
    {
        def apply[T]( requireLogin : Boolean, requireAdmin : Boolean = false )( block : (Request[AnyContent], SessionCache) => SimpleResult[T] ) =
        {
            Action( request =>
            {
                val session = request.session
                val uuid = session.get("uuid") match
                {
                    case Some(uuid) => uuid
                    case _ => java.util.UUID.randomUUID().toString()
                }
                
                val sessionCache = new SessionCache( uuid )
                
                var redirect = false
                if ( requireLogin )
                {
                    val user = sessionCache.getAs[UserData]("user")
                    user match
                    {
                        case Some(ud) =>
                        {
                            if ( requireAdmin && ! ud.isAdmin ) redirect = true
                        }
                        case _ => redirect = true
                    }
                }
                
                if ( !redirect )
                {
                    block(request, sessionCache).withSession( session + ("uuid" -> uuid ) )
                }
                else
                {
                    Redirect(routes.Application.index)
                }
            } )
        }
    }

    
    def index = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache) =>
        
        Ok(views.html.index(sessionCache.getAs[UserData]("user")))
    }
    
    def admin = SessionCacheAction(requireLogin=false, requireAdmin=true)
    {
        (request, sessionCache) =>
        
        withDbSession
        {
            val res = (for ( Join(role, user) <-
                CriticalMassTables.UserRole innerJoin
                CriticalMassTables.Users on (_.user_id is _.user_id) ) yield user.display_name ~ role.url).list
            Ok(views.html.admin(sessionCache.getAs[UserData]("user"), res))
        }
    }
    
    
    val userForm = Form(
        mapping(
            "WorkLocation"      -> text,
            "InstitutionName"   -> text,
            "InstitutionURL"    -> text,
            "InstitutionDepartment"	-> text,
            "SOTags"            -> text,
            "SectorTags"        -> text,
            "Anonymize"         -> boolean
        )(SupplementaryData.apply)(SupplementaryData.unapply)
    )
    
    def refineUserAccept = SessionCacheAction(requireLogin=true)
    {
        (requestEx, sessionCache) =>
        
        implicit val request = requestEx
        
        userForm.bindFromRequest.fold(
            errors => BadRequest,
            {
                case (data) =>
                {
                    println( "Received user data: " + data.toString )
                    
                    withDbSession
                    {
                        threadLocalSession withTransaction
                        {
                            def scopeIdentity = SimpleFunction.nullary[Long]("scope_identity")
                            
                            def isId(v:String) = v.foldLeft(true)( (acc,c) => acc && (c >= '0' && c <= '9') )
                            
                            val currUser = sessionCache.getAs[UserData]("user").get
                            
                            val institutionId = if ( isId(data.institutionName) ) data.institutionName.toLong
                            else
                            {
                                val i = CriticalMassTables.Institution
                                i.name insert ((data.institutionName))
                                
                                Query(scopeIdentity).first
                            }
                            
                            val roleId =
                            {
                                val r = CriticalMassTables.UserRole
                                (r.user_id ~ r.institution_id ~ r.department ~ r.url ~ r.location) insert ((currUser.uid.toLong, institutionId, data.institutionDepartment, data.institutionURL, data.workLocation))
                                
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
                            
                            Redirect(routes.Application.userHome)
                        }
                    }
                }
            }
        )
    }
    
    def exampleJob = Action
    {
        val uuid = JobRegistry.submit( "Test job",
        { statusFn =>
            
            for ( i <- 0 until 10 )
            {
                statusFn( i.toDouble / 10.0, "Status: " + i )
                Thread.sleep(1000)
            }
        } )
        Ok( "Submitted: " + uuid )
    }
    
    def listJobs = Action
    {
        val jobs = JobRegistry.getJobs
        
        Ok(compact(render(jobs.map( x => ("name" -> x._1) ~ ("progress" -> x._2) ~ ("status" -> x._3) ))))
    }
        
    def refineUser() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        Ok(views.html.refineuser(currUser, userForm))
    }
    
    def userHome() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache) =>
        
        val user = sessionCache.getAs[UserData]("user").get
        
        withDbSession
        {
            // Get roles with location and two types of tags
            val roles = ( for ( Join(role, institution) <-
                CriticalMassTables.UserRole innerJoin
                CriticalMassTables.Institution
                on (_.institution_id is _.id)
                if role.user_id === user.uid.toLong )
                yield role.id ~ institution.name ~ role.department ~ role.url ~ role.location ).list

            val res = for ( (rid, instname, dept, insturl, loc) <- roles ) yield
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
                
                new UserRole( instname, insturl, dept, loc, soTags, sectorTags, false )   
            }

            Ok(views.html.userhome(user, res.toList))
        }
    }

    
    def mapData( loc : String ) = Action
    {
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        println( swlat, swlon, nelat, nelon, zoom )
        
        
        // If logged in, additionally join on the institution table and give
        // local insitution summaries, e.g. institution to location, SO rep, SO tags, Sector tags
        withDbSession
        {
            def getPoints( swlat : Double, swlon : Double, nelat : Double, nelon : Double, zoom : Double ) =
            {
                import org.scalaquery.ql.extended.H2Driver.Implicit._
                ( for 
                {
                    dh <- CriticalMassTables.DataHierarchy if
                    dh.level === (zoom.toInt) &&
                    dh.longitude >= swlon && dh.longitude <= nelon &&
                    dh.latitude >= swlat && dh.latitude <= nelat
                } yield dh.count ~ dh.longitude ~ dh.latitude ~ dh.label ~ dh.maxRep ~ dh.maxRepUid ~ dh.id ).take(100)
            }
            
            val points = if ( swlon > nelon )
            {
                getPoints( swlat, swlon, nelat, nelon, zoom ).list
            }
            else
            {
                val p1 = getPoints( swlat, swlon, nelat, 180.0, zoom )
                val p2 = getPoints( swlat, 180.0, nelat, nelon, zoom )
                p1.list ++ p2.list
            }
            
            
            val json = render( points.map( x => ("name" -> x._4) ~ ("lon" -> x._2.toString) ~ ("lat" -> x._3.toString) ~ ("count" -> x._1) ~ ("maxRep" -> x._5 ) ~ ("maxRepUid" -> x._6 ) ~ ("dh_id" -> x._7) ) )
            
            Ok(compact(json))
        }
    }
    
    def markerUsers( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        withDbSession
        {
            println( "Marker users for: " + dh_id.toString )
            val users = (for
            {
                Join(userMap, users) <-
                CriticalMassTables.UserMap innerJoin
                CriticalMassTables.Users on (_.user_id is _.user_id)
                if userMap.dh_id === dh_id && users.reputation > 150L
                _ <- Query orderBy(Desc(users.reputation))
            } yield users.reputation ~ users.display_name ~ users.user_id ~ users.location)
            
            val topN = (users take 100).list

            println( "Got %d users".format(topN.size) )
            
            // Inefficient - cache in Users tables
            val userTags = for ( u <- topN ) yield
            {
                val topTags = (for (Join(userTags, tag) <-
                    CriticalMassTables.UserTags innerJoin
                    CriticalMassTables.Tags on (_.tag_id is _.id)
                    if userTags.user_id === u._3) yield tag.name).take(5).list.mkString(" ")
                topTags
            }

            println( "Got tags" )
            
            

            val userTableData = (topN zip userTags).map { case (x, t) =>
                ("reputation" -> x._1) ~
                ("name" -> "<a href=\"http://stackoverflow.com/users/%d\">%s</a>".format(x._3, x._2)) ~
                ("location" -> x._4) ~
                ("tags" -> t) }
    
            println( "Zipped users and tags" )
            
            val json = compact(render("aaData" -> userTableData))

            println( "Built json" )
            Ok(json)
        }
    }
    
    def markerTags( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        withDbSession
        {
            val topTags = (for (Join(tagMap, tags) <-
                CriticalMassTables.TagMap innerJoin
                CriticalMassTables.Tags on (_.tag_id is _.id)
                if tagMap.dh_id === dh_id) yield tags.name ~ tagMap.count).list

            val tagData = topTags.map( t => ("tag" -> t._1) ~ ("count" -> t._2) )

            Ok(compact(render(tagData)))
        }
    }
    
    def logout() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache) =>
        
        sessionCache.remove("user")
        Redirect(routes.Application.index)
    }
    
    def authenticate( code : String ) = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache) =>
        
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
        sessionCache.set("user", new UserData(accessToken, expires, meuid, mename ) )
        
        println( "User is %s (%d)".format( mename, meuid ) )
        
        // TODO: If this is their first login, ask for more details
        // namely finer location, company name
        
        withDbSession
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
    
    def workSectorBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        withDbSession
        {
            val similar = ( for ( t <- CriticalMassTables.SectorTags if t.name like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
    
    def institutionBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.Column
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        val lowerSFn = SimpleFunction[String]("lower")
        def lowerFn(c : Column[String]) = lowerSFn(Seq(c))
        
        withDbSession
        {
            val similar = ( for ( t <- CriticalMassTables.Institution if lowerFn(t.name) like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }   
 
    def instLocationBySuffix( instId : Long, q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._

        println( instId, q )
        
        if ( instId > 0 )
        {
            withDbSession
            {
                val similar = ( for ( role <- CriticalMassTables.UserRole if role.institution_id === instId ) yield role.location ).list.toSet
                
                Ok(compact(render(similar.toList.map( x => ("id" -> x) ~ ("name" -> x) ))))
            }
        }
        else Ok(compact(render(List())))
    }
    
    def instDepartmentBySuffix( instId : Long, q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        if ( instId > 0 )
        {
            withDbSession
            {
                val similar = ( for ( role <- CriticalMassTables.UserRole if role.institution_id === instId ) yield role.department ).list.toSet
                
                Ok(compact(render(similar.toList.map( x => ("id" -> x) ~ ("name" -> x) ))))
            }
        }
        else Ok(compact(render(List())))
    }
    
    def instURLBySuffix( instId : Long, q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        if ( instId > 0 )
        {
            withDbSession
            {
                val similar = ( for ( role <- CriticalMassTables.UserRole if role.institution_id === instId ) yield role.url ).list.toSet
                
                Ok(compact(render(similar.toList.map( x => ("id" -> x) ~ ("name" -> x) ))))
            }
        }
        else Ok(compact(render(List())))
    }
    
    
    
    def tagBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        withDbSession
        {
            val similar = ( for ( t <- CriticalMassTables.Tags if t.name like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
  
}
