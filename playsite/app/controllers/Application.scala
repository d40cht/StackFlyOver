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
import org.scalaquery.ql.Ordering._

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
    
    case class GlobalData( val baseUrl : String, val numUsers : Int, val numLocations : Int, val numRoles : Int, numInstitutions : Int, numTags : Int )
    
    case class UserAuth( val accessToken : String, val expiry : Int )
    
    case class UserData(
        val uid : Int,
        val name : String,
        val auth : Option[UserAuth],
        val email : Option[String],
        val reputation : Long,
        val lastScanned : java.sql.Timestamp,
        val location : String,
        val city : String,
        val state : String,
        val country : String,
        val url : String,
        val profileImage : Option[String] )
    {
        def isLocallyAuthenticated = !auth.isEmpty
        def isAdmin = isLocallyAuthenticated && (uid == 415313)
    }
    
    case class NameAndId( name : String, id : Long )
    
    case class RepTable( headings : List[NameAndId], rankings : List[(NameAndId, List[Int])] )

    case class UserRole( val id : Long, val institutionName : String, val url : String, val department : String, val location : String, val soTags : List[String], val sectorTags : List[String] )
    
    val stackOverFlowKey = "5FUHVgHRGHWbz9J5bEy)Ng(("
    val stackOverFlowSecretKey = "aL1DlUG5A7M96N48t2*k0w(("
    val googleMapsKey = "AIzaSyA_F10Lcod9fDputQVMZOtM4cMMaFbJybU"
    
    private def analyticsEvent( category : String, action : String, label : String ) : (String, String) =
    {
        "script" -> "_gaq.push(['_trackEvent', '%s', '%s', '%s']);".format(category, action, label)
    }
    
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
                Logger.debug( "IP: " + request.remoteAddress )
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
                                val numTags = rowCount(CriticalMassTables.Tags)
                                
                                val newSD = GlobalData( baseUrl, numUsers, numLocations, numRoles, numInstitutions, numTags )
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
    //def admin = SessionCacheAction(requireLogin=false, requireAdmin=false)
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
                CriticalMassTables.Users on (_.user_id is _.user_id) ) yield user.user_id ~ user.display_name ~ role.url ~ role.institution_id).take(100).list
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
    
    def addEmail() = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        Ok(views.html.addEmail(globalData, currUser, flash))
    }
    
    def addWatch( companyId : Long, locId : Long ) = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        
        WithDbSession
        {
            val ci = CriticalMassTables.CompanyWatch
            
            (ci.user_id ~ ci.institution_id ~ ci.location_name_id).insert( (currUser.uid.toLong, companyId, locId) )
        }
        
        Redirect(routes.Application.companyPage( companyId )).flashing( "success" -> "Watch added" )
    }
    
    def removeWatch( watchId : Long ) = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val currUser = sessionCache.getAs[UserData]("user").get
        
        // TODO: Fix so authenticated users can't delete other users watches
        WithDbSession
        {
            ( for ( cw <- CriticalMassTables.CompanyWatch if cw.id === watchId ) yield cw ).mutate( _.delete )
            
        }
        
        Redirect(routes.Application.userPage( currUser.uid )).flashing( "success" -> "Watch removed" )
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
                                val already = ( for (loc <- CriticalMassTables.LocationName if loc.name === data.workLocation) yield loc.id).list
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
                            
                            // Add a job to add this role to the hierarchy table
                            val uuid = JobRegistry.submit( "Location hierarchy rebuild",
                            { statusFn =>
                                
                                val db = Database.forDataSource(DB.getDataSource())
                                
                                val yahooLocation = controllers.YahooGeocoder( data.workLocation ).head
                            
                                processing.HierarchyVisitor( db, yahooLocation.longitude.toDouble, yahooLocation.latitude.toDouble,
                                { case (level, dist, dhPoint) =>
                                
                                    Logger.debug( "Adding %d to %d (%f, %f, %d)".format( institutionId, dhPoint.id, dhPoint.lon, dhPoint.lat, level ) )
                                    db withSession
                                    {
                                        CriticalMassTables.InstitutionMap.insert( (dhPoint.id, institutionId) )
                                    }
                                } )
                            } )
                            
                            
                            
                            val newRoleEvent = analyticsEvent("Action", "newRole", currUser.name )
                            Redirect(routes.Application.userPage(currUser.uid)).flashing( newRoleEvent, "success" -> "Thank you very much" )
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
    
    def getProfileImagesJob = Action
    {
        val uuid = JobRegistry.submit( "Get profile images",
        { statusFn =>
            
            val db = Database.forDataSource(DB.getDataSource())
            
            statusFn( 0.0, "Getting profile images" )
            processing.FetchProfileImages.run(db)
        } )
        Ok( "Submitted: " + uuid )
    }
    
    
    def rebuildRanksJob = Action
    {
        val uuid = JobRegistry.submit( "Ranks rebuild",
        { statusFn =>
            
            val db = Database.forDataSource(DB.getDataSource())
            
            //statusFn( 0.0, "Deleting existing ranks" )
            
            statusFn( 0.0, "Recalculating ranks" )
            processing.RankGenerator.recalculateRanks( db )
            
            statusFn( 100.0, "Complete." )
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
    
    private def getUserDetails( user_id : Long, userAuth : Option[UserAuth] = None )( implicit sessionCache : SessionCache, dbSession : org.scalaquery.session.Session ) =
    {
        val inSession = sessionCache.getAs[UserData]("user")
        
        inSession match
        {
            case Some(ud) if ud.uid == user_id => ud
            case _ => WithDbSession
            {
                val userDetails = (for (
                    u <- CriticalMassTables.Users;
                    ln <- CriticalMassTables.LocationName if ln.id === u.location_name_id;
                    l <- CriticalMassTables.Location if ln.id === l.name_id
                    if u.user_id === user_id
                    ) yield
                        u.display_name ~ u.email ~ u.reputation ~ u.lastScanned ~
                        ln.name ~ l.city ~ l.state ~ l.country ~ u.website_url ~ u.profileImage).first
                
                new UserData(
                    user_id.toInt,
                    userDetails._1,
                    userAuth,
                    userDetails._2,
                    userDetails._3,
                    userDetails._4,
                    userDetails._5,
                    userDetails._6,
                    userDetails._7,
                    userDetails._8,
                    userDetails._9,
                    userDetails._10 )
            }
        }
    }
    
    def acceptEmail = SessionCacheAction(requireLogin=true)
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
        
        val eventEmail = analyticsEvent("Action", "email", ud.name )
        Redirect(routes.Application.userPage(ud.uid)).flashing( eventEmail, "success" -> "Thank you very much" )
    }
    
    
    def userHome = SessionCacheAction(requireLogin=true)
    {
        (request, sessionCache, globalData, flash) =>
        
        val ud = sessionCache.getAs[UserData]("user").get
        Redirect(routes.Application.userPage(ud.uid))
    }
    
    def rankingsPage( tagId : Long, ylhId : Long, startRank : Int ) = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache, globalData, flash) =>
        
        WithDbSession
        {
            val nearby =
            {
                for (
                    r <- CriticalMassTables.UserRanks;
                    u <- CriticalMassTables.Users if r.user_id === u.user_id;
                    if r.tag_id === tagId &&
                    r.yahoo_location_hierarchy_id === ylhId &&
                    r.rank > startRank - 20 && r.rank < startRank + 20;
                    _ <- Query orderBy(Desc(r.rank)) ) yield r.rank ~ u.display_name ~ u.user_id
            }
            
            nearby.foreach( println(_) )
        }
        
        Redirect(routes.Application.index).flashing( "failure" -> "This page is not yet implemented" )
    }
    
    def companyPage( companyId : Long ) = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache, globalData, flash) =>
        
        WithDbSession
        {
            val companyName =
            {
                for ( ct <- CriticalMassTables.Institution
                    if ct.id === companyId ) yield ct.name
            }.first
                
            val companyInstitutions =
            {
                for (
                    ct <- CriticalMassTables.UserRole;
                    ln <- CriticalMassTables.LocationName;
                    if ct.location_name_id === ln.id && ct.institution_id === companyId ) yield ct.department ~ ct.url ~ ln.name ~ ln.id ~ true
            }.list
            
            val user = sessionCache.getAs[UserData]("user")
            
            Ok(views.html.companyPage(globalData, user, companyId, companyName, companyInstitutions, flash))
        }
    }
    
    def userPage( user_id : Long ) = SessionCacheAction(requireLogin=false)
    {
        (request, sessionCache, globalData, flash) =>
        
        import org.scalaquery.ql.Ordering._
        import org.scalaquery.ql.extended.H2Driver.Implicit._
        
        implicit val sc = sessionCache
        
        WithDbSession
        {
            val meUser = sessionCache.getAs[UserData]("user")
            val viewUser = getUserDetails( user_id )
            
            // Get user tags
            val tags = ( for ( Join(tags, tagNames) <-
                CriticalMassTables.UserTags innerJoin
                CriticalMassTables.Tags
                on (_.tag_id is _.id)
                if (tags.user_id === viewUser.uid.toLong );
                _ <- Query orderBy(Desc(tags.count)) )
                yield tagNames.name ~ tags.count ).take(5).list
            
            val repRes = ( for (
                r <- CriticalMassTables.UserRanks;
                t <- CriticalMassTables.Tags if r.tag_id === t.id;
                yhl <- CriticalMassTables.YahooLocationHierarchyIdentifier if r.yahoo_location_hierarchy_id === yhl.id
                if r.user_id === viewUser.uid.toLong )
                yield t.name ~ t.id ~ yhl.extent ~ yhl.name ~ yhl.id ~ r.rank )
                .list
            
            // (Tag name, Seq(LocationName, Rank))
            val repGrouped : List[(NameAndId, List[(NameAndId, Int)])] = repRes
                // Group by tag name
                .groupBy( r => NameAndId(r._1, r._2) ).toList
                // Within each tag, sort by region extent
                .map( r => (r._1, r._2.sortBy( _._3 ).map( r => (NameAndId(r._4, r._5), r._6) ) ) )
                
            //case class RepTable( headings : List[String], rankings : List[(String, List[Int])] )
            val repTable = if ( !repGrouped.isEmpty )
            {
                // Check that all tags have the same locations set
                val headings = repGrouped.head._2.map( _._1 )
                assert( repGrouped.forall( r => r._2.map( _._1 ) sameElements headings ) )
                
                RepTable( headings, repGrouped.map( r => (r._1, r._2.map(_._2)) ) )
            }
            else RepTable( List(), List() )
                
            // Get company watches with locations
            val watches = ( for (
                watch <- CriticalMassTables.CompanyWatch;
                institution <- CriticalMassTables.Institution if watch.institution_id === institution.id;
                location <- CriticalMassTables.LocationName if watch.location_name_id === location.id;
                if watch.user_id === viewUser.uid.toLong )
                yield watch.id ~ institution.name ~ location.name ).list
            
            // Get roles with location and two types of tags
            val roles = ( for ( 
                role <- CriticalMassTables.UserRole;
                institution <- CriticalMassTables.Institution if role.institution_id === institution.id;
                location <- CriticalMassTables.LocationName if role.location_name_id === location.id;
                if role.user_id === viewUser.uid.toLong )
                yield role.id ~ institution.name ~ role.department ~ role.url ~ location.name ).list

            val roleData = ( for ( (rid, instname, dept, insturl, loc) <- roles ) yield
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
            } ).toList

            Ok(views.html.userhome(globalData, meUser, viewUser, meUser.isDefined && meUser.get.uid==viewUser.uid, repTable, watches, roleData, flash))
        }
    }

    
    def mapData( loc : String ) = Action
    { request =>

        val Array( swlat, swlon, nelat, nelon, zoomRaw ) = loc.split(",").map(_.toDouble)
        Logger.debug( "IP %s: %f, %f, %f, %f, %f".format(request.remoteAddress, swlat, swlon, nelat, nelon, zoomRaw) )
        
        val zoom = zoomRaw min processing.MarkerClusterer.endRange
        
        val dhTimestamp = org.seacourt.global.Instance().getDHTimestamp
        
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
                    dh.created === dhTimestamp &&
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
                val topTags = (for (
                    ut <- CriticalMassTables.UserTags;
                    tag <- CriticalMassTables.Tags if ut.tag_id === tag.id && ut.user_id === u._3;
                    _ <- Query orderBy(Desc(ut.count)) ) yield tag.name).take(5).list.mkString(" ")
                topTags
            }
            val userTableData = (topN zip userTags).map { case (x, t) =>
                ("reputation" -> x._1) ~
                ("name" -> "<a href=\"/userPage?user_id=%d\">%s</a>".format(x._3, x._2)) ~
                ("location" -> x._4) ~
                ("tags" -> t) }
                
            //userTableData.foreach( t => Logger.debug( t.toString ) )
            
            val json = compact(render("aaData" -> userTableData))
            
            Ok(json)
        }
    }
    
    def markerTags( dh_id : Long ) = Action
    {
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
    { implicit request =>
        Logger.debug( "Requesting institutions for: " + dh_id )
        WithDbSession
        {
            val roleData = (for
            {
                institutionMap <- CriticalMassTables.InstitutionMap;
                institution <- CriticalMassTables.Institution if institution.id === institutionMap.institution_id;
                userRole <- CriticalMassTables.UserRole if userRole.institution_id === institutionMap.institution_id;
                locationName <- CriticalMassTables.LocationName if userRole.location_name_id === locationName.id;
                if institutionMap.dh_id === dh_id
            } yield institution.id ~ institution.name ~ userRole.url ~ locationName.name ~ userRole.id ~ userRole.user_id ).list
            
            Logger.debug( "Role data retrieved" )
            
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
            
            Logger.debug( "Building statistics from user details" )
            var instData = Map[String, InstData]()
            for ( (inst_id, name, urlIgnore, loc, role_id, user_id) <- roleData )
            {
                val url = routes.Application.companyPage(inst_id).absoluteURL()
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
            
            Logger.debug( "Rendering to JSON" )
           
            Ok(compact(render("aaData" -> instDataList.map( t =>
                ("count" -> t._2.userCount) ~
                ("name" -> "<a href=\"%s\">%s</a>".format( t._2.url, t._1 ) ) ~
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
        
        implicit val sc = sessionCache
        
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
	    val emailOption : Option[(Option[String], Long)] = WithDbSession
        {
            ( for ( r <- CriticalMassTables.Users if r.user_id === meuid.toLong ) yield r.email ~ r.reputation ).list.headOption
        }
        
        emailOption match
        {
            case None => Redirect(routes.Application.index).flashing
            {
                "failure" -> "We're sorry, but we have not yet gleaned your details from Stack Overflow. If you have just registered with SO, please try again tomorrow."
            }
                
            case Some( (email, reputation) ) =>
            {
                WithDbSession
                {
                    // Get user_id and display_name and stick them in the cache
                    sessionCache.remove("user")
                    val auth = new UserAuth( accessToken, expires )
                    val userDetails = getUserDetails( meuid, Some(auth) )
                    sessionCache.set("user", userDetails )
                    
                    Logger.debug( "User is %s (%d)".format( mename, meuid ) )
                    
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
                    
                    val loginEvent = analyticsEvent( category="Action", action="Login", label=mename )

                    if ( email.isEmpty )
                    {
                        Redirect(routes.Application.addEmail).flashing( loginEvent, "success" -> ("Welcome: " + mename) )
                    }
                    else if ( checkRoles.isEmpty )
                    {
                        Redirect(routes.Application.refineUser).flashing( loginEvent, "success" -> ("Welcome: " + mename) )
                    }
                    else
                    {
                        Redirect(routes.Application.userPage(meuid)).flashing( loginEvent, "success" -> ("Welcome: " + mename) )
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

