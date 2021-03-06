package controllers

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
    
    object UserRole extends Table[(Long, Long, Long, String, String, Long, java.sql.Timestamp)]("UserRole")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def user_id             = column[Long]("user_id")
        def institution_id      = column[Long]("institution_id")
        def department          = column[String]("department")
        def url                 = column[String]("url")
        def location_name_id    = column[Long]("location_id")
        def modified            = column[java.sql.Timestamp]("modified")
        
        def * = id ~ user_id ~ institution_id ~ department ~ url ~ location_name_id ~ modified
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
    
    // Institutions for a hierarchy area
    object InstitutionMap extends Table[(Long, Long)]("InstitutionMap")
    {
        def dh_id               = column[Long]("dh_id")
        def institution_id      = column[Long]("institution_id")
        
        def * = dh_id ~ institution_id
    }
    
    object DataHierarchy extends Table[(Long, Int, Double, Double, Int, Int, Long, String, java.sql.Timestamp)]("DataHierarchy")
    {
        def id                  = column[Long]("id", O PrimaryKey, O AutoInc)
        def level               = column[Int]("level")
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def count               = column[Int]("count")
        def maxRep              = column[Int]("maxRep")
        def maxRepUid           = column[Long]("maxRepUid")
        def label               = column[String]("label")
        def created             = column[java.sql.Timestamp]("created")
        
        def * = id ~ level ~ longitude ~ latitude ~ count ~ maxRep ~ maxRepUid ~ label ~ created
    }
    
    
    // A sensible radius threshold seems to be 40km (40,000)
    object Location extends Table[(Long, Double, Double, Double, Double, String, String, String, String, String)]("Location")
    {
        def name_id             = column[Long]("name_id", O PrimaryKey)
        def longitude           = column[Double]("longitude")
        def latitude            = column[Double]("latitude")
        def radius              = column[Double]("radius")
        
        def quality             = column[Double]("quality")
        def neighborhood        = column[String]("neighborhood")
        def city                = column[String]("city")
        def county              = column[String]("county")
        def state               = column[String]("state")
        def country             = column[String]("country")
        
        def * = name_id ~ longitude ~ latitude ~ radius ~ quality ~ neighborhood ~ city ~ county ~ state ~ country
    }

    object LocationName extends Table[(Long, String)]("LocationName")
    {
        
        def id                  = column[Long]("id", O PrimaryKey)
        def name                = column[String]("name")
        
        def * = id ~ name
    }
     
    object CompanyWatch extends Table[(Long, Long, Long, Long)]("CompanyWatch")
    {
        def id                      = column[Long]("id", O PrimaryKey)
        def user_id                 = column[Long]("user_id")
        def institution_id          = column[Long]("institution_id")
        def location_name_id        = column[Long]("location_name_id")
        
        def * = id ~ user_id ~ institution_id ~ location_name_id
    }
    
    object Users extends Table[(Long, String, Long, Long, Long, Int, Int, String, Long, Int, Int, Int, Option[String], java.sql.Timestamp, Boolean, Option[String])]("Users")
    {
        def user_id             = column[Long]("user_id", O PrimaryKey)
        def display_name        = column[String]("display_name")
        def creation_date       = column[Long]("creation_date")
        def last_access_date    = column[Long]("last_access_date")
        def reputation          = column[Long]("reputation")
        def age                 = column[Int]("age")
        def accept_rate         = column[Int]("accept_rate")
        def website_url         = column[String]("website_url")
        def location_name_id    = column[Long]("location_id")
        def badge_gold          = column[Int]("badge_gold")
        def badge_silver        = column[Int]("badge_silver")
        def badge_bronze        = column[Int]("badge_bronze")
        def email               = column[Option[String]]("email")
        def lastScanned         = column[java.sql.Timestamp]("lastScanned")
        def detailFresh         = column[Boolean]("detailFresh")
        def profileImage        = column[Option[String]]("profileImage")
        
        def * = user_id ~ display_name ~ creation_date ~ last_access_date ~ reputation ~
                age ~ accept_rate ~ website_url ~ location_name_id ~ badge_gold ~ badge_silver ~
                badge_bronze ~ email ~ lastScanned ~ detailFresh ~ profileImage
    }
    
    object NativeUser extends Table[(Long, java.sql.Timestamp, Option[String], java.sql.Timestamp, Int)]("NativeUser")
    {
        def userId              = column[Long]("userId")
        def registrationDate    = column[java.sql.Timestamp]("registrationDate")
        def email               = column[Option[String]]("email")
        def lastLogin           = column[java.sql.Timestamp]("lastLogin")
        def loginCount          = column[Int]("loginCount")
        
        def * = userId ~ registrationDate ~ email ~ lastLogin ~ loginCount
    }
    
    // TODO: Add time submitted and completed column
    object Jobs extends Table[(String, String, Double, String, java.sql.Timestamp, java.sql.Timestamp)]("Jobs")
    {
        def job_id              = column[String]("job_id", O PrimaryKey)
        def name                = column[String]("name")
        def progress            = column[Double]("progress")
        def status              = column[String]("status")
        def start_time          = column[java.sql.Timestamp]("start_time")
        def end_time            = column[java.sql.Timestamp]("end_time")
        
        def * = job_id ~ name ~ progress ~ status ~ start_time ~ end_time
    }
    
    
    // To get a unique city id, concatenate city, county, state, country together then hash and lookup id below
    // to get a unique state, concatenate state, country and follow as above
    object YahooLocationHierarchyIdentifier extends Table[(Long, String, String, Int)]("YahooLocationHierarchyIdentifier")
    {
        def id                  = column[Long]("id", O PrimaryKey)
        def name                = column[String]("name")
        def placeHash           = column[String]("hash")
        def extent              = column[Int]("extent")
        
        def * = id ~ name ~ placeHash ~ extent
    }
    
    object UserRanks extends Table[(Long, Long, Long, Int, java.sql.Timestamp)]("UserRanks")
    {
        def user_id                     = column[Long]("user_id")
        def tag_id                      = column[Long]("tag_id")
        def yahoo_location_hierarchy_id = column[Long]("yahoo_location_hierarchy_id")
        def rank                        = column[Int]("rank")
        def created                     = column[java.sql.Timestamp]("created")
        
        def * = user_id ~ tag_id ~ yahoo_location_hierarchy_id ~ rank ~ created
    }
    
    object ClosestUsers extends Table[(Long, Long, Long, Double)]("ClosestUsers")
    {
        def user_id         = column[Long]("user_id")
        def other_user_id   = column[Long]("other_user_id")
        def ylh_id          = column[Long]("ylh_id")
        def distance        = column[Double]("distance")
        
        def * = user_id ~ other_user_id ~ ylh_id ~ distance
    }
}


