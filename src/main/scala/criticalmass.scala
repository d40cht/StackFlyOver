
/*
{
            val items = Dispatch.pullJSON( "http://api.stackexchange.com/2.0/users/1;2;3;4;5;6;7;8;9;10?order=desc&sort=reputation&site=stackoverflow" )
            
            val items = (json \ "items").children
            
            for ( user <- items )
            {
                println( user \ "display_name" )
                println( "  " + user \ "reputation" )
                println( "  " + user \ "location" )
            }
            
            // Geocode using the google api (max 15000 a day): https://developers.google.com/maps/documentation/geocoding/
            // http://maps.googleapis.com/maps/api/geocode/json?address=San+Diego,CA&sensor=false
            //
            // and p
            
            //println( res )
            //println( json.toString )
        }
        
         
            // Geocode using the google api (max 15000 a day): https://developers.google.com/maps/documentation/geocoding/
            // http://maps.googleapis.com/maps/api/geocode/json?address=San+Diego,CA&sensor=false
            


{
   "results" : [
      {
         "address_components" : [
            {
               "long_name" : "San Diego",
               "short_name" : "San Diego",
               "types" : [ "locality", "political" ]
            },
            {
               "long_name" : "San Diego",
               "short_name" : "San Diego",
               "types" : [ "administrative_area_level_2", "political" ]
            },
            {
               "long_name" : "California",
               "short_name" : "CA",
               "types" : [ "administrative_area_level_1", "political" ]
            },
            {
               "long_name" : "United States",
               "short_name" : "US",
               "types" : [ "country", "political" ]
            }
         ],
         "formatted_address" : "San Diego, CA, USA",
         "geometry" : {
            "bounds" : {
               "northeast" : {
                  "lat" : 33.1142490,
                  "lng" : -116.908160
               },
               "southwest" : {
                  "lat" : 32.5348560,
                  "lng" : -117.28216650
               }
            },
            "location" : {
               "lat" : 32.71532920,
               "lng" : -117.15725510
            },
            "location_type" : "APPROXIMATE",
            "viewport" : {
               "northeast" : {
                  "lat" : 32.86540990,
                  "lng" : -116.90113630
               },
               "southwest" : {
                  "lat" : 32.56499550,
                  "lng" : -117.41337390
               }
            }
         },
         "types" : [ "locality", "political" ]
      }
   ],
   "status" : "OK"
*/
       
