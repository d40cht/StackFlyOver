
      
      function refreshTable(theTable, source)
      {
        var oSettings = theTable.fnSettings();

        // Retrieve the new data with $.getJSON. You could use it ajax too
        $.getJSON(source, null, function( json )
        {
            _gaq.push(['_trackPageview', source]);
            theTable.fnClearTable();

            for (var i=0; i<json.aaData.length; i++)
            {
                theTable.oApi._fnAddData(oSettings, json.aaData[i]);
            }

            oSettings.aiDisplay = oSettings.aiDisplayMaster.slice();
            theTable.fnDraw();
        });
    }


      function mapInitialize()
      {
        var myOptions = {
          center: new google.maps.LatLng(20, 0),
          zoom: 2,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        
        var markersArray = [];
        var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
        
        var instTable = null;
        var userTable = null;
      
        google.maps.event.addListener(map, 'idle', function ()
        {
            var bounds = map.getBounds();
            var zoom = map.getZoom();
            var ne = bounds.getNorthEast();
            var sw = bounds.getSouthWest();
            
            var url = "/mapData?loc="+sw.lat() + "," + sw.lng() + "," + ne.lat() + "," + ne.lng() + "," + zoom;
            $.getJSON(url,
                function(data)
                {
                    _gaq.push(['_trackPageview', url]);
                    // Clear any existing markers
                    while ( markersArray[0] )
                    {
                        markersArray.pop().setMap(null);
                    }
                    
                    // Add the new markers in
                    $.each(data, function(i, item)
                    {
                        var icon = new google.maps.MarkerImage("/assets/images/blue-dot.png");
                        var count = parseInt(item.count);
                        var maxRep = parseInt(item.maxRep);
                        if ( count > 2000 )
                        {
                            icon = new google.maps.MarkerImage( "/assets/images/m5.png", null, null, new google.maps.Point( 45, 45 ) );
                        }
                        else if ( count > 500 )
                        {
                            icon = new google.maps.MarkerImage( "/assets/images/m4.png", null, null, new google.maps.Point( 39, 39 ) );
                        }
                        else if ( count > 20 )
                        {
                            icon = new google.maps.MarkerImage( "/assets/images/m3.png", null, null, new google.maps.Point( 33, 33 ) );
                        }
                        /*else if ( count > 20 )
                        {
                            icon = new google.maps.MarkerImage( "/assets/images/m2.png", null, null, new google.maps.Point( 28, 28 ) );
                        }*/
                        else if ( count > 1 )
                        {
                            icon = new google.maps.MarkerImage( "/assets/images/m1.png", null, null, new google.maps.Point( 26, 26 ) );
                        }

                        var marker = new google.maps.Marker( {
                          position: new google.maps.LatLng(item.lat, item.lon),
                          title : item.name,
                          url : "/markerUsers?dh_id=" + item.dh_id,
                          map: map,
                          icon : icon,
                          zIndex : 0
                        });
                        markersArray.push(marker);
                        
                        var repIcon = "";
                        
                        if ( maxRep > 50000 )
                        {
                            repIcon = "stars4.png";
                        }
                        else if ( maxRep > 10000 )
                        {
                            repIcon = "stars3.png";
                        }
                        else if ( maxRep > 5000 )
                        {
                            repIcon = "stars2.png";
                        }
                        else if ( maxRep > 1000 )
                        {
                            repIcon = "stars1.png";
                        }
                        
                        var markerClickFn = function() {
                          
                            // Reload the table with local users
                            if ( userTable == null )
                            {
                                userTable = $('#userData').dataTable( {
                                    // Disable sorting. Take what comes from the server
                                    "aaSorting": [],
                                    "sPaginationType": "full_numbers",
                                    "bProcessing": false,
                                    "bAutoWidth":true,
                                    "bFilter":false,
                                    "bInfo":false,
                                    "bLengthChange":false,
                                    "iDisplayLength":10,
                                    "sAjaxSource": "/markerUsers?dh_id=" + item.dh_id,
                                    "aoColumns": [
                                        { "mDataProp": "reputation" },
                                        { "mDataProp": "name" },
                                        { "mDataProp": "location" },
                                        { "mDataProp": "tags" },
                                    ]
                                } );
                            }
                            else
                            {
                                refreshTable( userTable, "/markerUsers?dh_id=" + item.dh_id );
                            }
                            
                            // Reload the table with local users
                            if ( instTable == null )
                            {
                                instTable = $('#instData').dataTable( {
                                    // Disable sorting. Take what comes from the server
                                    "aaSorting": [],
                                    "sPaginationType": "full_numbers",
                                    "bProcessing": false,
                                    "bAutoWidth":true,
                                    "bFilter":false,
                                    "bInfo":false,
                                    "bLengthChange":false,
                                    "iDisplayLength":10,
                                    "sAjaxSource": "/markerInstitutions?dh_id=" + item.dh_id,
                                    "aoColumns": [
                                        { "mDataProp": "count" },
                                        { "mDataProp": "name" },
                                        { "mDataProp": "location" },
                                        { "mDataProp": "SOTags" },
                                        { "mDataProp": "SectorTags" }
                                    ]
                                } );
                            }
                            else
                            {
                                refreshTable( instTable, "/markerInstitutions?dh_id=" + item.dh_id );
                            }
                            
                            $.getJSON("/markerTags?dh_id=" + item.dh_id,
                                function(tags)
                                {
                                    $("#tagcloud").tagCloud(tags);
                                }
                            );
                            
                        }
                        
                        google.maps.event.addListener(marker, 'click', markerClickFn );
                        
                        if ( repIcon != "" )
                        {
                            var infoContent = 
                                '<a href="http://stackoverflow.com/users/' + item.maxRepUid + '">' +
                                '<img src="http://stackoverflow.com/users/flair/' + item.maxRepUid +'.png" width="208" height="58">' +
                                '</a>';
                                
                            var infowindow = new google.maps.InfoWindow({
                                content: infoContent
                            });

                            var base = "/assets/images/";
                            var markerImageRep = new google.maps.MarkerImage( base + repIcon, null, null, new google.maps.Point( 16, 8 ) );
                            var smarker = new google.maps.Marker( {
                              position: new google.maps.LatLng(item.lat, item.lon),
                              title : item.name, 
                              map: map,
                              icon : markerImageRep,
                              zIndex : 1
                            });
                            
                            markersArray.push(smarker);
                            
                            google.maps.event.addListener(smarker, 'click', markerClickFn );
                        }
                        
                    } );
                } );
        });

      }
