
addGtfsLinks <- function(outputLocation="./test/",
                         nodes, 
                         links,
                         gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip", 
                         analysis_start = as.Date("2019-10-11","%Y-%m-%d"), 
                         analysis_end = as.Date("2019-10-17","%Y-%m-%d"),
                         studyRegion=NA){
  # outputLocation="./gtfs/"
  # nodes=networkDirected[[1]]
  # links=networkDirected[[2]]
  # gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip"
  # analysis_start = as.Date("2019-10-11","%Y-%m-%d")
  # analysis_end = as.Date("2019-10-17","%Y-%m-%d")
  # studyRegion=greaterMelbourne
  
  validRoadEdges <- links %>%
    st_drop_geometry() %>%
    filter(is_walk==1 & is_car==1 & is_cycle==1) %>%
    dplyr::select(from_id,to_id)
  validRoadIds <- c(validRoadEdges$from_id,validRoadEdges$to_id) %>% unique()
  # network nodes that can be reached via walking, cycling, and driving.  
  validRoadNodes <- nodes %>%
    filter(id %in% validRoadIds) %>%
    st_set_crs(28355)
  
  # process the GTFS feed and export relevant tables into a folder
  processGtfs(outputLocation = outputLocation,
              networkNodes = validRoadNodes,
              studyRegion = greaterMelbourne)
  # read the outputs
  stops <- st_read(paste0(outputLocation,"stops.sqlite"),quiet=T)
  stopTimes <- readRDS(paste0(outputLocation,"stopTimes.rds"))
  trips <- readRDS(paste0(outputLocation,"trips.rds"))
  routes <- readRDS(paste0(outputLocation,"routes.rds"))
  
  # return the edges in the PT network as well as write the
  # transitVehicles.xml and transitSchedule.xml files
  ptEdges <- exportGtfsSchedule(
    outputLocation,
    stops,
    stopTimes,
    trips,
    routes
  )
  edgesCombined <- bind_rows(links,ptEdges) %>%
    st_sf() %>%
    mutate(cycleway=as.character(cycleway))
  return(edgesCombined)
}

processGtfs <- function(outputLocation="./test/",
                        networkNodes,
                        gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip", 
                        analysis_start = as.Date("2019-10-11","%Y-%m-%d"), 
                        analysis_end = as.Date("2019-10-17","%Y-%m-%d"),
                        studyRegion=NA){
  # outputLocation="./gtfs/"
  # networkNodes = validRoadNodes
  # gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip"
  # analysis_start = as.Date("2019-10-11","%Y-%m-%d")
  # analysis_end = as.Date("2019-10-17","%Y-%m-%d")
  
  dir.create(outputLocation, showWarnings = FALSE)
  
  gtfs <- read_gtfs(gtfs_feed)
  
  validCalendar <- gtfs$calendar %>%
    filter(start_date<=analysis_end & end_date>=analysis_start) %>%
    filter(wednesday==1)
  
  # trips during within the time period
  validTrips <- gtfs$trips %>%
    filter(service_id %in% validCalendar$service_id) %>%
    dplyr::select(route_id,service_id,trip_id) %>%
    mutate(route_id=as.factor(route_id))
  
  # routes that are part of a valid trip
  validRoutes <- gtfs$routes %>%
    filter(route_id %in% validTrips$route_id) %>%
    mutate(service_type="null",
           service_type=ifelse(agency_id%in%c(3)   & route_type%in%c(0),  "tram" ,service_type),
           service_type=ifelse(agency_id%in%c(1,2) & route_type%in%c(1,2),"train",service_type),
           service_type=ifelse(agency_id%in%c(4,6) & route_type%in%c(3),  "bus"  ,service_type)) %>%
    filter(service_type!="null") %>%
    mutate(route_id=as.factor(route_id)) %>%
    mutate(service_type=as.factor(service_type)) %>%
    dplyr::select(route_id,service_type)
  
  # some trips won't have any valid routes, so they must be removed
  validTrips <- validTrips %>%
    filter(route_id %in% validRoutes$route_id)
  
  # stopTimes that are part of a valid trip
  system.time( # takes about 6 seconds
    validStopTimes <- gtfs$stop_times %>%
      mutate(trip_id=as.factor(trip_id),
             stop_id=as.factor(stop_id)) %>%
      filter(trip_id %in% validTrips$trip_id) %>%
      mutate(arrival_time=as.numeric(as_hms(arrival_time)),
             departure_time=as.numeric(as_hms(departure_time))) %>%
      dplyr::select(trip_id,arrival_time,departure_time,stop_id,stop_sequence) %>%
      filter(!is.na(arrival_time)) %>% # some of the schedule goes past 24 hours
      arrange(trip_id,stop_sequence)
  )
  
  
  # stops that have a valid stopTime
  validStops <- gtfs$stops %>%
    mutate(stop_id=as.factor(stop_id)) %>%
    filter(stop_id %in% validStopTimes$stop_id) %>%
    dplyr::select(stop_id,stop_lat,stop_lon) %>%
    st_as_sf(coords=c("stop_lon", "stop_lat"), crs=4326) %>%
    st_transform(28355) %>%
    st_snap_to_grid(1)
  
  # only want stops within the study region
  if(!is.na(studyRegion)){
    message("Cropping to study region")
    validStops <- validStops %>%
      filter(lengths(st_intersects(., studyRegion)) > 0)
  }
  # st_write(validStops,"stops.sqlite",delete_layer=TRUE)
  
  # snapping the stops to the nearest node in the road network
  networkNodes <- networkNodes %>%
    mutate(tmp_id=row_number())
  nearestNodeId <- st_nearest_feature(validStops,networkNodes)
  
  # subsetting the networkNodes and rearranging to match validStops
  nearestNode <- networkNodes[nearestNodeId,]
  
  # calculating the distance from each stop to the nearest node in the road network
  distanceToNetwork <- st_distance(validStops,nearestNode,by_element=TRUE) %>%
    as.numeric()
  
  
  validStopsSnapped <- nearestNode %>%
    mutate(stop_id=validStops$stop_id) %>%
    mutate(dist=distanceToNetwork) %>%
    filter(dist<=1000) %>%
    dplyr::select(stop_id,id,x,y) #'stop_id' is the gtfs id, 'id' is the network node id
  
  
  # st_write(validStopsSnapped,paste0(outputLocation,"stopsSnapped.sqlite"),delete_layer=TRUE)
  
  validStopTimesSnapped <- validStopTimes %>%
    inner_join(st_drop_geometry(validStopsSnapped),by="stop_id") %>% # IMPORTANT: this join also removes the stops outside of the region!
    arrange(trip_id,stop_sequence) %>%
    group_by(trip_id) %>%
    # when we use the snapped locations, two sequential stops may be at the same
    # location. If this is the case, we remove the later stop.
    filter(id!=lag(id) | row_number()==1) %>%
    mutate(stop_sequence=row_number()) %>%
    ungroup() %>%
    dplyr::select(trip_id,stop_sequence,arrival_time,departure_time,stop_id,id,x,y)
  
  # some trips will no longer be present
  validTripsSnapped <- validTrips %>%
    filter(trip_id %in% validStopTimesSnapped$trip_id)
  
  # some routes will no longer be present
  validRoutesSnapped <- validRoutes %>%
    filter(route_id %in% validTripsSnapped$route_id)
  
  
  # replace stop_id with id (i.e., use the network node id instead of the stop
  # id provided by the GTFS feed)
  validStopsSnappedFinal <- validStopsSnapped %>%
    dplyr::select(-stop_id) %>%
    distinct(id,x,y) %>%
    rename(stop_id=id)
  validStopTimesSnappedFinal <- validStopTimesSnapped %>%
    dplyr::select(-stop_id) %>%
    rename(stop_id=id)
  
  # writing the exports to file
  st_write(validStopsSnappedFinal,paste0(outputLocation,"stops.sqlite"),delete_layer=T)
  saveRDS(validStopTimesSnappedFinal, file=paste0(outputLocation,"stopTimes.rds"))
  saveRDS(validTripsSnapped, file=paste0(outputLocation,"trips.rds"))
  saveRDS(validRoutesSnapped, file=paste0(outputLocation,"routes.rds"))
}


exportGtfsSchedule <- function(outputLocation,
                               stops,
                               stopTimes,
                               trips,
                               routes){
  
  
  vehicleTripMatching <- trips %>%
    left_join(routes,by="route_id")
  
  # the public transport network
  ptNetwork <- stopTimes %>%
    dplyr::select(trip_id,arrival_time,departure_time,from_id=stop_id,from_x=x,from_y=y) %>%
    # filter(row_number()<200) %>%
    group_by(trip_id) %>%
    mutate(arrivalOffset=arrival_time-min(arrival_time)) %>%
    mutate(departureOffset=departure_time-min(arrival_time)) %>%
    mutate(to_id=lead(from_id),
           to_x=lead(from_x),
           to_y=lead(from_y)) %>%
    ungroup() %>%
    mutate(arrivalOffset=as.character(as_hms(arrivalOffset)),
           departureOffset=as.character(as_hms(departureOffset)),
           arrival_time=as.character(as_hms(arrival_time)),
           departure_time=as.character(as_hms(departure_time)))
  
  ptStops <- ptNetwork %>%
    dplyr::select(from_id,to_id) %>%
    distinct() %>%
    mutate(stop_id=paste0("stop ",row_number()))
  
  ptNetwork <- ptNetwork %>%
    left_join(ptStops,by=c("from_id","to_id")) %>%
    left_join(vehicleTripMatching,by="trip_id") %>%
    dplyr::select(route_id,service_id,service_type,trip_id,stop_id,arrival_time,departure_time,
                  arrivalOffset,departureOffset,from_id,to_id,from_x,from_y,to_x,to_y)
  
  ptNetworkDistinctEdges <- ptNetwork %>%
    dplyr::select(from_id,to_id,from_x,from_y,to_x,to_y) %>%
    distinct() %>%
    filter(!is.na(to_id)) %>% 
    mutate(geom=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
    st_as_sf(wkt = "geom", crs = 28355)
  
  # ptNetworkGeom <- ptNetwork %>%
  #   dplyr::select(service_type,from_id,to_id,from_x,from_y,to_x,to_y) %>%
  #   distinct() %>%
  #   mutate(GEOMETRY=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
  #   st_as_sf(wkt = "GEOMETRY", crs = 28355)
  # st_write(ptNetworkGeom,"data/ptNetwork.sqlite",delete_layer=TRUE)
  
  
  
  # making tables for XML
  
  # ./data/transitVehicles.xml: vehicle
  # id is just the trip_id. This means we can potentially have a different vehicle 
  # for each trip. Have also set the vehicle type here.
  vehicles <- trips %>%
    inner_join(routes,by="route_id") %>%
    mutate(type=NA,
           type=ifelse(service_type=="train",1,type),
           type=ifelse(service_type=="bus",2,type),
           type=ifelse(service_type=="tram",3,type)) %>%
    dplyr::select(id=trip_id,type) %>%
    arrange(id,type)
  
  # ./data/transitSchedule.xml: transitSchedule > transitStops
  transitStops <- ptNetwork %>%
    dplyr::select(from_id,to_id,from_x,from_y,stop_id) %>%
    # the id column is the issue, as it doesn't exist
    # unique()
    dplyr::distinct() %>%
    mutate(linkRefId=as.factor(paste0(from_id,"_",to_id))) %>%
    dplyr::select(stop_id,linkRefId,from_id,to_id,x=from_x,y=from_y) 
  
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > routeProfile
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > route
  # * trip_id is the transitRoute (i.e., each trip is its own route, with a single
  #   trip. This allows for longer offsets during peak traffic).
  # * route is the same as the refID column since we use a direct line between each stop.
  routeProfile <- ptNetwork %>%
    dplyr::select(trip_id,arrivalOffset,departureOffset,from_id,to_id) %>%
    inner_join(transitStops,by=c("from_id","to_id")) %>%
    # NOTE: route.link.refId is not the same as the refId column
    dplyr::select(trip_id,arrivalOffset,departureOffset,refId=stop_id,from_id,to_id)
  
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > departures
  # vehicleRefId is just the trip_id. This means we can potentially have a 
  # different vehicle for each trip. I have also set the vehicle type here.
  departures <- ptNetwork %>%
    # filter(row_number()<200) %>%
    group_by(trip_id) %>%
    slice(which.min(row_number())) %>%
    ungroup() %>%
    # mutate(departure_time=as.character(as.hms(departure_time))) %>%
    left_join(vehicles, by=c("trip_id"="id")) %>%
    dplyr::select(departureTime=departure_time,id=type,vehicleRefId=trip_id)
  
  # Types of vehicles to place in the network
  vehicleTypes <- tribble(
    ~id, ~description, ~seats, ~standingRoom, ~length, ~accessTime, ~egressTime, ~passengerCarEquivalents,
    1  , "train"     , 114   , 206          , 150    , "0.0"      , "0.0"      , 0.25                    ,
    2  , "bus"       , 25    , 13           , 15     , "0.0"      , "0.0"      , 0.25                    ,
    3  , "tram"      , 16    , 50           , 30     , "0.0"      , "0.0"      , 0.25
  )
  
  echo("writing transitVehicles.xml\n")
  outxml<-paste0(outputLocation,"transitVehicles.xml")
  # transitVehicles
  cat(
    "<?xml version=\"1.0\" ?>
<vehicleDefinitions xmlns=\"http://www.matsim.org/files/dtd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.matsim.org/files/dtd http://www.matsim.org/files/dtd/vehicleDefinitions_v1.0.xsd\">\n",
    file=outxml,append=FALSE)
  str<-""
  writeInterval=500
  processed
  for (i in 1:nrow(vehicleTypes)) {
    str<-paste0(str,"  <vehicleType id=\"",vehicleTypes[i,]$id,"\">\n")
    str<-paste0(str,"    <description>",vehicleTypes[i,]$description,"</description>\n")
    str<-paste0(str,"    <capacity>\n")
    str<-paste0(str,"      <seats persons=\"",vehicleTypes[i,]$seats,"\"/>\n")
    str<-paste0(str,"      <standingRoom persons=\"",vehicleTypes[i,]$standingRoom,"\"/>\n")
    str<-paste0(str,"    </capacity>\n")
    str<-paste0(str,"    <length meter=\"",vehicleTypes[i,]$length,"\"/>\n")
    str<-paste0(str,"    <accessTime secondsPerPerson=\"",vehicleTypes[i,]$accessTime,"\"/>\n")
    str<-paste0(str,"    <egressTime secondsPerPerson=\"",vehicleTypes[i,]$egressTime,"\"/>\n")
    str<-paste0(str,"    <passengerCarEquivalents pce=\"",vehicleTypes[i,]$passengerCarEquivalents,"\"/>\n")
    str<-paste0(str,"  </vehicleType>\n")
  }
  cat(str,file=outxml,append=TRUE)
  str<-""
  for (i in 1:nrow(vehicles)) {
    str<-paste0(str,"  <vehicle id=\"",vehicles[i,]$id,"\" type=\"",vehicles[i,]$type,"\"/>\n")
    if (i%%writeInterval==0 || i==nrow(vehicleTypes)) {
      cat(str,file=outxml,append=TRUE)
      str<-"" # clear the buffer after writing it out
    }
    # report progress
    if (i%%50==0 || i==nrow(vehicleTypes)) printProgress(i,nrow(vehicles),' Vehicles')
  }
  cat(paste0("</vehicleDefinitions>\n"),file=outxml,append=TRUE)
  
  
  echo("writing transitSchedule.xml\n")
  outxml<-paste0(outputLocation,"transitSchedule.xml")
  str<-""
  
  # transitSchedule
  cat(
    "<?xml version=\"1.0\" ?>
    <!DOCTYPE transitSchedule SYSTEM \"http://www.matsim.org/files/dtd/transitSchedule_v1.dtd\">
    <transitSchedule>
    <transitStops>\n",
    file=outxml,append=FALSE)
  
  echo("writing transitStops\n")
  for (i in 1:nrow(transitStops)) {
    # for (i in 1:100) {
    str<-paste0(str,
                "    <stopFacility id=\"",transitStops[i,]$stop_id,"\" isBlocking=\"false\" linkRefId=\"",
                transitStops[i,]$linkRefId,"\" x=\"",transitStops[i,]$x,"\" y=\"",transitStops[i,]$y,"\"/>\n")
    
    if (i%%writeInterval==0 || i==nrow(transitStops)) {
      cat(str,file=outxml,append=TRUE)
      str<-"" # clear the buffer after writing it out
    }
    # report progress
    if (i%%50==0 || i==nrow(transitStops)) printProgress(i,nrow(transitStops),' transitStops')
  }
  cat(paste0("  </transitStops>\n"),file=outxml,append=TRUE)
  cat(paste0("  <transitLine id=\"Melbourne\">\n"),file=outxml,append=TRUE)

  echo("writing vehicleTripMatching\n")
  str<-""
  
  # TODO Ask @Alan to check this part:
  for (i in 1:nrow(vehicleTripMatching)) {
    # for (i in 1:100) {
    routeProfileCurrent <- routeProfile%>%filter(trip_id==vehicleTripMatching[i,]$trip_id) 
    if(nrow(routeProfileCurrent)>0){ # I added this to drop those empty route profiles
      str<-paste0(str,"    <transitRoute id=\"",vehicleTripMatching[i,]$trip_id,"\">\n")
      str<-paste0(str,"      <description>",vehicleTripMatching[i,]$service_id,"</description>\n")
      str<-paste0(str,"      <transportMode>",vehicleTripMatching[i,]$service_type,"</transportMode>\n")
      str<-paste0(str,"      <routeProfile>\n")
      
      for (j in 1:nrow(routeProfileCurrent)) { 
        # first row: no arrival offset
        if (j == 1) str<-paste0(str,"        <stop awaitDeparture=\"true\" departureOffset=\"",
                                routeProfileCurrent[j,]$departureOffset,
                                "\" refId=\"",
                                routeProfileCurrent[j,]$refId,
                                "\"/>\n")
        # rows except first and last
        else if (j < nrow(routeProfileCurrent)) {
          str<-paste0(str,"        <stop arrivalOffset=\"",
                      routeProfileCurrent[j,]$arrivalOffset,
                      "\" awaitDeparture=\"true\" departureOffset=\"",
                      routeProfileCurrent[j,]$departureOffset,
                      "\" refId=\"",
                      routeProfileCurrent[j,]$refId,
                      "\"/>\n")
        }
        # last row: no departure offset
        else {
          str<-paste0(str,"        <stop arrivalOffset=\"",
                      routeProfileCurrent[j,]$arrivalOffset,
                      "\" refId=\"",
                      routeProfileCurrent[j,]$refId,
                      "\"/>\n")
        }
      }
      str<-paste0(str,"      </routeProfile>\n")
      str<-paste0(str,"      <route>\n")
      for (j in 1:nrow(routeProfileCurrent)) {
        str<-paste0(str,"        <link refId=\"",
                   routeProfileCurrent[j,]$from_id,"_",routeProfileCurrent[j,]$to_id,
                   "\"/>\n")
      }
      str<-paste0(str,"      </route>\n")
      
      str<-paste0(str,"      <departures>\n")
      departuresCurrent <- departures%>%filter(vehicleRefId==vehicleTripMatching[i,]$trip_id)
      for (k in 1:nrow(departuresCurrent)) {
        str<-paste0(str,"        <departure departureTime=\"",
                    departuresCurrent[k,]$departureTime,
                    "\" id=\"",
                    departuresCurrent[k,]$id,
                    "\" vehicleRefId=\"",
                    departuresCurrent[k,]$vehicleRefId,
                    "\"/>\n")
      }   
      str<-paste0(str,"      </departures>\n")
      str<-paste0(str,"    </transitRoute>\n")
    }
    
    if (i%%writeInterval==0 || i==nrow(vehicleTripMatching)) {
      cat(str,file=outxml,append=TRUE)
      str<-"" # clear the buffer after writing it out
    }
    # report progress
    if (i%%50==0 || i==nrow(vehicleTripMatching)) printProgress(i,nrow(vehicleTripMatching),' vehicleTripMatching')
  }
  cat(paste0("  </transitLine>\n"),file=outxml,append=TRUE)
  cat(paste0("</transitSchedule>\n"),file=outxml,append=TRUE)
  
  
  
  # routeProfile, stop
  # route, link
  # departures, departure
  ptNetworkMATSim <- ptNetworkDistinctEdges %>% 
    mutate(length=round(as.numeric(st_length(.)),3)) %>%
    mutate(highway="pt") %>% 
    mutate(freespeed=11.1) %>% 
    mutate(permlanes=1) %>% 
    mutate(capacity=600) %>% 
    mutate(is_oneway=1) %>% 
    mutate(cycleway=NA) %>% 
    mutate(is_cycle=0) %>% 
    mutate(is_walk=0) %>% 
    mutate(is_car=0) %>% 
    mutate(modes="pt") %>%
    dplyr::select(from_id, to_id, fromX=from_x, fromY=from_y, toX=to_x, toY=to_y,
                  length, freespeed, permlanes, capacity, highway, is_oneway,
                  cycleway, is_cycle, is_walk, is_car, modes)
  
  return(ptNetworkMATSim)
}
