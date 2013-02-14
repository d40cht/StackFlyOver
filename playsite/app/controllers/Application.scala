package controllers

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.{Join, SimpleFunction, Query}

import net.liftweb.json._
import net.liftweb.json.JsonDSL._


case class SupplementaryData(
    val workLocation : String,
    val institutionName : String,
    val institutionURL : String,
    val institutionDepartment : String,
    val soTags : String,
    val sectorTags : String )
    


object Application extends Controller
{
    import play.api.cache.Cache
    import play.api.Play.current
    import play.api.data._
    import play.api.data.Forms._
    import play.api.mvc.{Flash}

    case class Pos( val name : String, val lon : Double, val lat : Double )
    
    case class GlobalData( val baseUrl : String, val numUsers : Int, val numLocations : Int, val numRoles : Int, numInstitutions : Int )
    
    case class UserAuth( val accessToken : String, val expiry : Int )
    
    case class UserData( val uid : Int, val name : String, val auth : Option[UserAuth], val email : Option[String] )
    {
        def isLocallyAuthenticated = !auth.isEmpty
        def isAdmin = isLocallyAuthenticated && (uid == 415313)
    }

    case class UserRole( val id : Long, val institutionName : String, val url : String, val department : String, val location : String, val soTags : List[String], val sectorTags : List[String] )
    
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
    
    // An action wrapped to pass through the cache for this session
    object SessionCacheAction
    {
        def apply[T]( requireLogin : Boolean, requireAdmin : Boolean = false, requireUserId : Option[Long] = None )( block : (Request[AnyContent], SessionCache, GlobalData, Flash) => PlainResult ) =
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
                
                val globalData =
                {
                    sessionCache.getAs[GlobalData]("globalData") match
                    {
                        case Some(sd)   => sd
                        case None       =>
                        {
                            WithDbSession
                            {
                                import org.scalaquery.ql.extended.H2Driver.Implicit._
                                import org.scalaquery.ql.extended.{ExtendedTable => Table}
                                
                                def rowCount[T]( table : Table[T] ) = Query( ( for ( r <- table ) yield r ).count ).first
                        
                                val baseUrl = Play.application.configuration.getString("application.baseUrl").get
                                val numUsers = rowCount(CriticalMassTables.Users)
                                val numRoles = rowCount(CriticalMassTables.UserRole)
                                val numInstitutions = rowCount(CriticalMassTables.Institution)
                                val numLocations = rowCount(CriticalMassTables.Location)
                                
                                val newSD = GlobalData( baseUrl, numUsers, numLocations, numRoles, numInstitutions )
                                sessionCache.set("globalData", newSD )
                                newSD
                            }
                        }
                    }
                }
                
                var redirect = false
                if ( requireLogin )
                {
                    val user = sessionCache.getAs[UserData]("user")
                    user match
                    {
                        case Some(ud) =>
                        {
                            if ( requireAdmin && ! ud.isAdmin ) redirect = true
                            if ( requireUserId != None && requireUserId != Some( ud.uid ) ) redirect = true
                        }
                        case _ => redirect = true
                    }
                }
                
                if ( !redirect )
                {
                    block(request, sessionCache, globalData, flash(request)).withSession( session + ("uuid" -> uuid) )
                }
                else
                {
                    Redirect(routes.Application.index).flashing( "failure" -> "Please log in for full access" )
                }
            } )
        }
    }

    
    def index = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache, globalData, flash) =>
        
        Ok(views.html.index(globalData, sessionCache.getAs[UserData]("user"), flash))
    }
    
    def admin = SessionCacheAction(requireLogin=true, requireAdmin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val jobs = JobRegistry.getJobs.sortWith( (x, y) => x.startTime.after( y.startTime ) )
        
        WithDbSession
        {
            import org.scalaquery.ql.extended.H2Driver.Implicit._
            
            val nativeUsers = (for ( Join(nu, u) <-
                CriticalMassTables.NativeUser innerJoin
                CriticalMassTables.Users on (_.userId is _.user_id) ) yield u.user_id ~ u.display_name ~ nu.registrationDate ~ nu.lastLogin ~ nu.loginCount ).take(100).list
            
            val userRoles = (for ( Join(role, user) <-
                CriticalMassTables.UserRole innerJoin
                CriticalMassTables.Users on (_.user_id is _.user_id) ) yield user.user_id ~ user.display_name ~ role.url).take(100).list
            Ok(views.html.admin(globalData, sessionCache.getAs[UserData]("user"), nativeUsers, userRoles, jobs, flash))
        }
    }
    
    
    val userForm = Form(
        mapping(
            "WorkLocation"      -> text,
            "InstitutionName"   -> text,
            "InstitutionURL"    -> text,
            "InstitutionDepartment"	-> text,
            "SOTags"            -> text,
            "SectorTags"        -> text
        )(SupplementaryData.apply)(SupplementaryData.unapply)
    )

    
    def exampleJob = Action
    {
        val uuid = JobRegistry.submit( "Test job",
        { statusFn =>
            
            for ( i <- 0 until 100 )
            {
                statusFn( i.toDouble / 100.0, "Status: " + i )
                Thread.sleep(1000)
            }
        } )
        Ok( "Submitted: " + uuid )
    }
    
    def listJobs = Action
    {
        val jobs = JobRegistry.getJobs
        
        Ok(compact(render(jobs.map( x => ("name" -> x.name) ~ ("progress" -> x.progress) ~ ("status" -> x.status) ))))
    }
        
    def refineUser() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        Ok(views.html.refineuser(globalData, currUser, userForm, None, flash))
    }
    
    def deleteUserRole( role_id : Long ) = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
       
        WithDbSession
        { 
            ( for ( r <- CriticalMassTables.UserRole if r.id === role_id ) yield r ).mutate( _.delete )
        }
        
        Redirect(routes.Application.userHome)
    }
    
    def editUserRole( role_id : Long ) = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        
        WithDbSession
        {
            // Get role from id with location and two types of tags
            val roleData = ( for (
                role <- CriticalMassTables.UserRole;
                institution <- CriticalMassTables.Institution if role.institution_id === institution.id;
                location <- CriticalMassTables.LocationName if role.location_name_id === location.id;
                if role.id === role_id )
                yield role.id ~ institution.name ~ role.department ~ role.url ~ location.name ).list.head

            val res =
            {
                val (rid, instname, dept, insturl, loc) = roleData
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
                
                new UserRole( rid, instname, insturl, dept, loc, soTags, sectorTags )   
            }
            
            val f = userForm.fill(new SupplementaryData( res.location, res.institutionName, res.url, res.department, res.soTags.mkString(";"), res.sectorTags.mkString(";") ))
            Ok(views.html.refineuser(globalData, currUser, f, Some(role_id), flash))
        }
    }
    
    def refineUserAccept = SessionCacheAction(requireLogin=true)
    {
        (requestEx, sessionCache, globalData, flash) =>
        
        implicit val request = requestEx
        
        userForm.bindFromRequest.fold(
            errors => BadRequest,
            {
                case (data) =>
                {
                    Logger.info( "Received user data: " + data.toString )
                    
                    WithDbSession
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
                            
                            val locationNameId =
                            {
                                val already = ( for (loc <- CriticalMassTables.LocationName if loc.name == data.workLocation) yield loc.id).list
                                if ( !already.isEmpty )
                                {
                                    already.head
                                }
                                else
                                {
                                    CriticalMassTables.LocationName.name insert (data.workLocation)
                                    
                                    Query(scopeIdentity).first
                                }     
                            }
                            
                            val roleId =
                            {
                                val r = CriticalMassTables.UserRole
                                val now = new java.sql.Timestamp( (new java.util.Date()).getTime )
                                
                                (r.user_id ~ r.institution_id ~ r.department ~ r.url ~ r.location_name_id ~ r.modified) insert ((currUser.uid.toLong, institutionId, data.institutionDepartment, data.institutionURL, locationNameId, now))
                                
                                Query(scopeIdentity).first
                            }
                            
                            for ( tag <- data.soTags.split(";").filter( _ != "" ) )
                            {
                                val tagId = tag.toLong
                                
                                CriticalMassTables.RoleSOTags insert ((roleId, tagId))
                            }
                            
                            for ( tag <- data.sectorTags.split(";").filter( _ != "" ) )
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
    
    def pullUsersJob = Action
    {
        val uuid = JobRegistry.submit( "User scrape job",
        { statusFn =>
            
            val db = Database.forDataSource(DB.getDataSource())
            val userFetch = new processing.UserScraper(db)
            userFetch.run( statusFn )
        } )
        Ok( "Submitted: " + uuid )
    }
    
    def rebuildHierarchyJob = Action
    {
        val uuid = JobRegistry.submit( "Location hierarchy rebuild",
        { statusFn =>
            
            val db = Database.forDataSource(DB.getDataSource())
            val userFetch = new processing.MarkerClusterer(db)
            userFetch.run( statusFn )
        } )
        Ok( "Submitted: " + uuid )
    }
    
    def rebuildLocationsJob = Action
    {
        val uuid = JobRegistry.submit( "Locations rebuild",
        { statusFn =>
            
            val db = Database.forDataSource(DB.getDataSource())
            
            statusFn( 0.0, "Deleting existing locations" )
            WithDbSession
            {
                //( for ( r <- CriticalMassTables.Location ) yield r ).mutate( _.delete )
            }
            
            statusFn( 0.0, "Re-scraping all locations" )
            val l = new processing.LocationUpdater( db )
            l.run( statusFn )
        } )
        Ok( "Submitted: " + uuid )
    }
    
    
    def backupDbJob = Action
    {
        val uuid = JobRegistry.submit( "Location hierarchy rebuild",
        { statusFn =>
            
            val db = DB.getDataSource()
            
            val backupFileName = "db_" + org.joda.time.LocalDateTime.now().toString().replace( ":", "_" ).replace( ".", "_" ) + ".zip"
            statusFn( 0.0, "Starting db backup to: " + backupFileName )
            val con = db.getConnection
            val ps = con.prepareStatement( "BACKUP TO ?" )
            ps.setString( 1, backupFileName )
            ps.execute()
            con.close()
            statusFn( 100.0, "Backup complete to: " + backupFileName )
        } )
        Ok( "Submitted: " + uuid )
    }
    
    private def getUserDetails( user_id : Long )( implicit sessionCache : SessionCache, dbSession : org.scalaquery.session.Session ) =
    {
        val inSession = sessionCache.getAs[UserData]("user")
        
        inSession match
        {
            case Some(ud) if ud.uid == user_id => ud
            case _ => WithDbSession
            {
                val userDetails = (for (u <- CriticalMassTables.Users if u.user_id === user_id) yield u.display_name ~ u.email).first
                
                new UserData( user_id.toInt, userDetails._1, None, userDetails._2 )
            }
        }
    }
    
    def addEmail = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val emailAddress : Option[String] = request.queryString.get("email").flatMap(_.headOption)

        val ud = sessionCache.getAs[UserData]("user").get
        WithDbSession
        {
            val em = for ( u <- CriticalMassTables.Users if u.user_id === ud.uid.toLong ) yield u.email
            
            Logger.info( "Updating: " + ud.uid + " with " + emailAddress )
            em.update( emailAddress )
            
            sessionCache.set("user", ud.copy(email=emailAddress) )
        }
        
        
        Redirect(routes.Application.userPage(ud.uid))
    }
    
    
    def userHome = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val ud = sessionCache.getAs[UserData]("user").get
        Redirect(routes.Application.userPage(ud.uid))
    }
    
    def userPage( user_id : Long ) = SessionCacheAction(requireLogin=true, requireUserId=Some(user_id))
    {
        (request, sessionCache, globalData, flash) =>
        
        implicit val sc = sessionCache
        WithDbSession
        {
            val user = getUserDetails( user_id )
            
            // Get roles with location and two types of tags
            val roles = ( for ( 
                role <- CriticalMassTables.UserRole;
                institution <- CriticalMassTables.Institution if role.institution_id === institution.id;
                location <- CriticalMassTables.LocationName if role.location_name_id === location.id;
                if role.user_id === user.uid.toLong )
                yield role.id ~ institution.name ~ role.department ~ role.url ~ location.name ).list

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
                
                new UserRole( rid, instname, insturl, dept, loc, soTags, sectorTags )   
            }

            Ok(views.html.userhome(globalData, user, res.toList, flash))
        }
    }

    
    def mapData( loc : String ) = Action
    {
        val Array( swlat, swlon, nelat, nelon, zoom ) = loc.split(",").map(_.toDouble)
        Logger.debug( "Serving: %f, %f, %f, %f, %f".format(swlat, swlon, nelat, nelon, zoom) )
        
        
        // If logged in, additionally join on the institution table and give
        // local insitution summaries, e.g. institution to location, SO rep, SO tags, Sector tags
        WithDbSession
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
                } yield dh.count ~ dh.longitude ~ dh.latitude ~ dh.label ~ dh.maxRep ~ dh.maxRepUid ~ dh.id )//.take(100)
            }
            
            val points = if ( swlon < nelon )
            {
                getPoints( swlat, swlon, nelat, nelon, zoom ).list
            }
            else
            {
                val p1 = getPoints( swlat, swlon, nelat, 180.0, zoom )
                val p2 = getPoints( swlat, -180.0, nelat, nelon, zoom )
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
        
        WithDbSession
        {
            Logger.debug( "Marker users for: " + dh_id.toString )
            val users = (for
            {
                userMap <- CriticalMassTables.UserMap;
                users <- CriticalMassTables.Users if userMap.user_id === users.user_id;
                location <- CriticalMassTables.LocationName if users.location_name_id === location.id
                if userMap.dh_id === dh_id && users.reputation > 50L
                _ <- Query orderBy(Desc(users.reputation))
            } yield users.reputation ~ users.display_name ~ users.user_id ~ location.name)
            
            val topN = (users take 100).list

            Logger.debug( "Got %d users".format(topN.size) )
            
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
                
            //userTableData.foreach( t => Logger.debug( t.toString ) )
            
            val json = compact(render("aaData" -> userTableData))
            
            Ok(json)
        }
    }
    
    def markerTags( dh_id : Long ) = Action
    {
        import org.scalaquery.ql.Ordering.Desc
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        WithDbSession
        {
            Logger.debug( "Requesting tags for: " + dh_id )
            val topTags = (for (Join(tagMap, tags) <-
                CriticalMassTables.TagMap innerJoin
                CriticalMassTables.Tags on (_.tag_id is _.id)
                if tagMap.dh_id === dh_id;
                _ <- Query orderBy(Desc(tagMap.count)) ) yield tags.name ~ tagMap.count).take(50).list

            val tagData = topTags.sortWith(_._2 > _._2).map( t => ("tag" -> t._1) ~ ("count" -> t._2) )
            
            //tagData.foreach( t => Logger.debug( t.toString ) )

            Ok(compact(render(tagData)))
        }
    }
    
    def markerInstitutions( dh_id : Long ) = Action
    {
        Logger.debug( "Requesting institutions for: " + dh_id )
        WithDbSession
        {
            val roleData = (for
            {
                userMap <- CriticalMassTables.UserMap;
                userRole <- CriticalMassTables.UserRole if userMap.user_id === userRole.user_id;
                locationName <- CriticalMassTables.LocationName if userRole.location_name_id === locationName.id;
                institution <- CriticalMassTables.Institution if userRole.institution_id === institution.id
                if userMap.dh_id === dh_id
            } yield institution.name ~ userRole.url ~ locationName.name ~ userRole.id ~ userRole.user_id ).list
            
            class InstData
            {
                var userCount = 0
                private var urlMap = Map[String, Int]()
                private var locMap = Map[String, Int]()
                private var soTagMap = Map[String, Int]()
                private var sectorTagMap = Map[String, Int]()
                
                private def updateMap( m : Map[String, Int], s : String, c : Int ) =
                {
                    val prev = m.getOrElse(s, 0)
                    m + (s -> (prev+c))
                }
                
                def update( url : String, loc : String, soTags : List[(String, Int)], sectorTags : List[String] )
                {
                    userCount += 1
                    urlMap = updateMap(urlMap, url, 1)
                    locMap = updateMap(locMap, loc, 1)
                    soTags.foreach( t => { soTagMap = updateMap(soTagMap, t._1, t._2) } )
                    sectorTags.foreach( t => { sectorTagMap = updateMap(sectorTagMap, t, 1) } )
                }
                
                def location = locMap.toList.sortWith(_._2 > _._2).head._1
                def url = urlMap.toList.sortWith(_._2 > _._2).head._1
                def soTags = soTagMap.toList.sortWith(_._2 > _._2).take(5).map(_._1).mkString( " " )
                def sectorTags = sectorTagMap.toList.sortWith(_._2 > _._2).take(5).map(_._1).mkString( " " )
            }
            
            var instData = Map[String, InstData]()
            for ( (name, url, loc, role_id, user_id) <- roleData )
            {
                val d = if ( instData contains name )
                {
                    instData(name)
                }
                else
                {
                    val nd = new InstData()
                    instData += name -> nd
                    nd
                }
                
                // Scrape tags for this user
                val roleSOTags = (for
                {
                    roleTag <- CriticalMassTables.RoleSOTags;
                    tag <- CriticalMassTables.Tags if roleTag.tag_id === tag.id
                    if roleTag.role_id === role_id
                } yield tag.name ).list
                
                val roleSectorTags = (for (Join(roleSectorTags, tag) <-
                    CriticalMassTables.RoleSectorTags innerJoin
                    CriticalMassTables.SectorTags on (_.tag_id is _.id)
                    if roleSectorTags.role_id === role_id) yield tag.name).list
                
                d.update( url, loc, roleSOTags.map(t => (t, 1)), roleSectorTags )
            }
            val instDataList = instData.toList.sortWith(_._2.userCount > _._2.userCount)
           
            //instDataList.foreach( t => Logger.debug( t.toString ) )
           
            Ok(compact(render("aaData" -> instDataList.map( t =>
                ("count" -> t._2.userCount) ~
                ("name" -> "<a href=\"http://%s\">%s</a>".format( t._2.url, t._1 ) ) ~
                ("location" -> t._2.location) ~
                ("SOTags" -> t._2.soTags) ~
                ("SectorTags" -> t._2.sectorTags)
            ) ) ) )
        }
    }
    
    def logout() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val ud = sessionCache.getAs[UserData]("user").get
        val uname = ud.name
        
        Logger.info( "User: %d %s logout".format( ud.uid, ud.name ) )
        
        sessionCache.remove("user")
        Redirect(routes.Application.index).flashing( "success" -> "Goodbye %s".format(uname) )
    }
    
    def authenticate( code : String ) = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache, globalData, flash) =>
        
        import akka.util.Timeout
        import akka.dispatch.Await
        import play.api.libs.ws.WS
        import akka.util.duration._
        
        implicit val formats = net.liftweb.json.DefaultFormats

        
        val baseUrl = globalData.baseUrl
        
        // Post the code back to try to get an access token
        Logger.debug( "Requesting access token" )
        val timeout = Timeout(5.seconds)
        val url = WS.url("https://stackexchange.com/oauth/access_token")
        val promiseRes = url.post( Map(
            "client_id"     -> Seq("498"),
            "code"          -> Seq(code),
            "redirect_uri"  -> Seq("%s/authenticate".format(baseUrl)),
            "client_secret" -> Seq(stackOverFlowSecretKey) ) )
        val post = promiseRes.await(5000).get.body
        
        Logger.debug( post )


        val fields = post.split("&").map
        { el =>
            val Array( key, value ) = el.split("=") 
            (key, value)
        } toMap

        // Got an access token
        val accessToken = fields("access_token")
        val expires = fields("expires").toInt
        
        Logger.debug( "Received access token: %s (expires: %f hours)".format( accessToken, (expires/3600.0) ) )
        
        Logger.debug( "Getting details for authenticated user" )
        val uidurlRes = Dispatch.pullJSON("https://api.stackexchange.com/2.0/me",
            List(
            ("site",            "stackoverflow"),
            ("access_token",    accessToken),
            ("key",             stackOverFlowKey) ) )
            
        val response = uidurlRes.children.head

        val meuid = (response \ "user_id").extract[Int]
        val mename = (response \ "display_name").extract[String]

	    Logger.debug( "User authenticated: %d %s".format(meuid, mename) )
	    val emailOption : Option[Option[String]] = WithDbSession
        {
            ( for ( r <- CriticalMassTables.Users if r.user_id === meuid.toLong ) yield r.email ).list.headOption
        }
        
        emailOption match
        {
            case None => Redirect(routes.Application.index).flashing
            {
                "Log in failed" -> "We're sorry, but we have not yet gleaned your details from Stack Overflow. If you have just registered with SO, please try again tomorrow."
            }
                
            case Some( email ) =>
            {
	         
                // Get user_id and display_name and stick them in the cache
                sessionCache.set("user", UserData( meuid, mename, Some( new UserAuth(accessToken, expires) ), email ) )
                
                Logger.debug( "User is %s (%d)".format( mename, meuid ) )
                
                WithDbSession
                {
                    val now = new java.sql.Timestamp( (new java.util.Date()).getTime )
                    
                    // Fill in or update the NativeUser table
                    val nu = CriticalMassTables.NativeUser
                    val checkFirstLogin = ( for ( r <- nu if r.userId === meuid.toLong ) yield r.loginCount ).list
                    if ( checkFirstLogin.isEmpty )
                    {
                        // userId, registrationDate, email, lastLogin, loginCount
                        nu.insert( (meuid.toLong, now, None, now, 1) )
                    }
                    else
                    {
                        val r = for ( r <- nu if r.userId === meuid.toLong ) yield r.lastLogin ~ r.loginCount
                        r.update( (now, checkFirstLogin.head + 1) )
                    }
                
                    // If they haven't registered any roles, ask for more details
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
        }
    }
    
    def workSectorBySuffix( q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        WithDbSession
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
        
        WithDbSession
        {
            val similar = ( for ( t <- CriticalMassTables.Institution if lowerFn(t.name) like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }   
 
    def instLocationBySuffix( instId : Long, q : String ) = Action
    {
        import org.scalaquery.ql.extended.H2Driver.Implicit._

        if ( instId > 0 )
        {
            WithDbSession
            {
                val similar = ( for (
                    role <- CriticalMassTables.UserRole;
                    location <- CriticalMassTables.LocationName if role.location_name_id === location.id;
                    if role.institution_id === instId ) yield location.name ).list.toSet
                
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
            WithDbSession
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
            WithDbSession
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
        
        WithDbSession
        {
            val similar = ( for ( t <- CriticalMassTables.Tags if t.name like q.toLowerCase() + "%" ) yield t.id ~ t.name ).take(10).list
            
            Ok(compact(render(similar.map( x => ("id" -> x._1) ~ ("name" -> x._2) ))))
        }
    }
  
}

