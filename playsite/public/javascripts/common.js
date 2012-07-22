
      
      function refreshTable(oTable, source) {
        table = oTable.dataTable();
        oSettings = table.fnSettings();

        //Retrieve the new data with $.getJSON. You could use it ajax too
        $.getJSON(source, null, function( json ) {
            table.fnClearTable(this);

            for (var i=0; i<json.aaData.length; i++) {
                table.oApi._fnAddData(oSettings, json.aaData[i]);
            }

            oSettings.aiDisplay = oSettings.aiDisplayMaster.slice();
            table.fnDraw();
        });
    }


      function initialize() {
        var myOptions = {
          center: new google.maps.LatLng(54.0, -2.0),
          zoom: 5,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        
        var markersArray = [];
        var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
        
        var oTable = null;
      
        google.maps.event.addListener(map, 'idle', function ()
        {
            var bounds = map.getBounds();
            var zoom = map.getZoom();
            var ne = bounds.getNorthEast();
            var sw = bounds.getSouthWest();
            
            $.getJSON("/mapData?loc="+sw.lat() + "," + sw.lng() + "," + ne.lat() + "," + ne.lng() + "," + zoom,
                function(data)
                {
                    // Reload the table
                    if ( oTable == null )
                    {
                        oTable = $('#example').dataTable( {
                            "bProcessing": false,
                            "bAutoWidth":true,
                            "bFilter":false,
                            "bInfo":false,
                            "bLengthChange":false,
                            "sAjaxSource": "/assets/javascripts/tabledata.json",
                            "aoColumns": [
                                { "mDataProp": "reputation" },
                                { "mDataProp": "user_name" },
                                { "mDataProp": "top_tags" },
                                { "mDataProp": "location" }
                            ]
                        } );
                    }
                    else
                    {
                        refreshTable( oTable, "/assets/javascripts/tabledata.json" );
                    }
                    
                    // Clear any existing markers
                    while ( markersArray[0] )
                    {
                        markersArray.pop().setMap(null);
                    }
                    
                    // Add the new markers in
                    $.each(data, function(i, item)
                    {
                        var icon = new google.maps.MarkerImage("http://www.google.com/intl/en_us/mapfiles/ms/micons/blue-dot.png");
                        var count = parseInt(item.count);
                        var maxRep = parseInt(item.maxRep);
                        if ( count > 2000 )
                        {
                            icon = new google.maps.MarkerImage( "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m5.png", null, null, new google.maps.Point( 45, 45 ) );
                        }
                        else if ( count > 500 )
                        {
                            icon = new google.maps.MarkerImage( "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m4.png", null, null, new google.maps.Point( 39, 39 ) );
                        }
                        else if ( count > 20 )
                        {
                            icon = new google.maps.MarkerImage( "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m3.png", null, null, new google.maps.Point( 33, 33 ) );
                        }
                        /*else if ( count > 20 )
                        {
                            icon = new google.maps.MarkerImage( "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m2.png", null, null, new google.maps.Point( 28, 28 ) );
                        }*/
                        else if ( count > 1 )
                        {
                            icon = new google.maps.MarkerImage( "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/images/m1.png", null, null, new google.maps.Point( 26, 26 ) );
                        }

                        var marker = new google.maps.Marker( {
                          position: new google.maps.LatLng(item.lat, item.lon),
                          title : item.name, 
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
                            
                            google.maps.event.addListener(smarker, 'click', function() {
                              infowindow.open(map,smarker);
                            });
                            
                            markersArray.push(smarker);
                        }
                        
                    } );
                } );
        });

      }