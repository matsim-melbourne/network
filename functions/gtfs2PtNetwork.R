
addGtfsLinks <- function(nodes, 
                         links,
                         gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip", 
                         analysis_start = as.Date("2019-10-11","%Y-%m-%d"), 
                         analysis_end = as.Date("2019-10-17","%Y-%m-%d"),
                         studyRegion=NA){
  # gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip"
  # analysis_start = as.Date("2019-10-11","%Y-%m-%d")
  # analysis_end = as.Date("2019-10-17","%Y-%m-%d")
  # studyRegion=NA
  
  links_pt <- processGtfs(nodes, gtfs_feed, analysis_start, analysis_end, studyRegion) # ToDo studyRegion = st_union(st_convex_hull(nodes)) 
  links_pt <- links_pt %>% 
    mutate(oneway=1) %>% 
    dplyr::select(names(links)) 
  links <- rbind(links, as.data.frame(links_pt)) %>% distinct()
  
  return(links)
}


processGtfs <- function(outputLocation="./gtfs/",
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
    
    
  st_write(validStopsSnapped,"data/stopsSnapped.sqlite",delete_layer=TRUE)
  
  validStopTimesSnapped <- validStopTimes %>%
    inner_join(st_drop_geometry(validStopsSnapped),by="stop_id") %>% # IMPORTANT: this join also removes the stops outside of the region!
    arrange(trip_id,stop_sequence) %>%
    group_by(trip_id) %>%
    # when we use the snapped locations, two sequential stops may be at the same
    # location. If this is the case, we remove the later stop.
    filter(id!=lag(id)) %>%
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


exportGtfsSchedule <- function(stops,
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
    filter(!is.na(to_id)) %>% 
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
    mutate(GEOMETRY=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355)
  
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
  
  
  
  # transitVehicles
  cat(
    "<?xml version=\"1.0\" ?>
<vehicleDefinitions xmlns=\"http://www.matsim.org/files/dtd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.matsim.org/files/dtd http://www.matsim.org/files/dtd/vehicleDefinitions_v1.0.xsd\">\n",
    file="./data/transitVehicles.xml",append=FALSE)
  for (i in 1:nrow(vehicleTypes)) {
    cat(paste0("  <vehicleType id=\"",vehicleTypes[i,]$id,"\">\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <description>",vehicleTypes[i,]$description,"</description>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <capacity>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("      <seats persons=\"",vehicleTypes[i,]$seats,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("      <standingRoom persons=\"",vehicleTypes[i,]$standingRoom,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    </capacity>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <length meter=\"",vehicleTypes[i,]$length,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <accessTime secondsPerPerson=\"",vehicleTypes[i,]$accessTime,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <egressTime secondsPerPerson=\"",vehicleTypes[i,]$egressTime,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <passengerCarEquivalents pce=\"",vehicleTypes[i,]$passengerCarEquivalents,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("  </vehicleType>\n"),file="./data/transitVehicles.xml",append=TRUE)
  }
  for (i in 1:nrow(vehicles)) {
    cat(paste0("  <vehicle id=\"",vehicles[i,]$id,"\" type=\"",vehicles[i,]$type,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
  }
  cat(paste0("</vehicleDefinitions>\n"),file="./data/transitVehicles.xml",append=TRUE)
  
  
  
  
  # transitSchedule
  cat(
    "<?xml version=\"1.0\" ?>
<!DOCTYPE transitSchedule SYSTEM \"http://www.matsim.org/files/dtd/transitSchedule_v1.dtd\">
<transitSchedule>
  <transitStops>\n",
    file="./data/transitSchedule.xml",append=FALSE)
  
  for (i in 1:nrow(transitStops)) {
    # for (i in 1:100) {
    cat(paste0("    <stopFacility id=\"",transitStops[i,]$stop_id,"\" isBlocking=\"false\" linkRefId=\"",
               transitStops[i,]$linkRefId,"\" x=\"",transitStops[i,]$x,"\" y=\"",
               transitStops[i,]$y,"\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
  }
  cat(paste0("  </transitStops>\n"),file="./data/transitSchedule.xml",append=TRUE)
  cat(paste0("  <transitLine id=\"Melbourne\">\n"),file="./data/transitSchedule.xml",append=TRUE)
  
  # TODO Ask @Alan to check this part:
  # for (i in 1:nrow(vehicleTripMatching)) {
  for (i in 1:100) {
    routeProfileCurrent <- routeProfile%>%filter(trip_id==vehicleTripMatching[i,]$trip_id) 
    if(nrow(routeProfileCurrent)>0){ # I added this to drop those empty route profiles
      cat(paste0("    <transitRoute id=\"",vehicleTripMatching[i,]$trip_id,"\">\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <description>",vehicleTripMatching[i,]$service_id,"</description>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <transportMode>",vehicleTripMatching[i,]$service_type,"</transportMode>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <routeProfile>\n"),file="./data/transitSchedule.xml",append=TRUE)
      
      for (j in 1:nrow(routeProfileCurrent)) {
        cat(paste0("        <stop arrivalOffset=\"",
                   routeProfileCurrent[j,]$arrivalOffset,
                   "\" awaitDeparture=\"true\" departureOffset=\"",
                   routeProfileCurrent[j,]$departureOffset,
                   "\" refId=\"",
                   routeProfileCurrent[j,]$refId,
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }
      cat(paste0("      </routeProfile>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <route>\n"),file="./data/transitSchedule.xml",append=TRUE)
      for (j in 1:nrow(routeProfileCurrent)) {
        cat(paste0("        <link refId=\"",
                   paste0(routeProfileCurrent[j,]$from_id,"_",routeProfileCurrent[j,]$to_id),
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }
      cat(paste0("      </route>\n"),file="./data/transitSchedule.xml",append=TRUE)
      
      cat(paste0("      <departures>\n"),file="./data/transitSchedule.xml",append=TRUE)
      departuresCurrent <- departures%>%filter(vehicleRefId==vehicleTripMatching[j,]$trip_id)
      for (k in 1:nrow(departuresCurrent)) {
        cat(paste0("        <departure departureTime=\"",
                   departuresCurrent[k,]$departureTime,
                   "\" id=\"",
                   departuresCurrent[k,]$id,
                   "\" vehicleRefId=\"",
                   departuresCurrent[k,]$vehicleRefId,
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }   
      cat(paste0("      </departures>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("    </transitRoute>\n"),file="./data/transitSchedule.xml",append=TRUE)
    }
    
  }
  cat(paste0("  </transitLine>\n"),file="./data/transitSchedule.xml",append=TRUE)
  cat(paste0("</transitSchedule>\n"),file="./data/transitSchedule.xml",append=TRUE)
  
  
  
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



processGtfs <- function(n_df,
                        gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip", 
                        analysis_start = as.Date("2019-10-11","%Y-%m-%d"), 
                        analysis_end = as.Date("2019-10-17","%Y-%m-%d"),
                        studyRegion=NA){
  # n_df = validRoadNodes
  # gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip"
  # analysis_start = as.Date("2019-10-11","%Y-%m-%d")
  # analysis_end = as.Date("2019-10-17","%Y-%m-%d")
  # studyRegion=NA   # TODO studyRegion = st_union(st_convex_hull(nodes)) won't work

  gtfs <- read_gtfs(gtfs_feed)
  
  validCalendar <- gtfs$calendar %>%
    filter(start_date<=analysis_end & end_date>=analysis_start) %>%
    filter(wednesday==1)
  
  validTrips <- gtfs$trips %>%
    filter(service_id %in% validCalendar$service_id) %>%
    dplyr::select(route_id,service_id,trip_id) %>%
    mutate(route_id=as.factor(route_id))
  
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
  
  validTrips <- validTrips %>%
    filter(route_id %in% validRoutes$route_id)
  
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
  

  
  validStops <- gtfs$stops %>%
    mutate(stop_id=as.factor(stop_id)) %>%
    filter(stop_id %in% validStopTimes$stop_id) %>%
    dplyr::select(stop_id,stop_lat,stop_lon) %>%
    st_as_sf(coords=c("stop_lon", "stop_lat"), crs=4326) %>%
    st_transform(28355) %>%
    st_snap_to_grid(1)
  
  # TODO Cropping to study region is not removing the trips outside e.g. T0.3-86-mjp-1.2.H is outside GMElb area
  # only want stops within the study region
  if(!is.na(studyRegion)){
    message("Cropping to study region")
    validStops <- validStops %>%
      filter(lengths(st_intersects(., studyRegion)) > 0)
  }
  #validStops <- validStops %>%
  #  filter(lengths(st_intersects(., studyRegion)) > 0)
  # st_write(validStops,"stops.sqlite",delete_layer=TRUE)
  
  # now need to snap the stops to intersections in the simplified network
  # networkIntersections <- st_read("data/networkSimplified.sqlite",layer="nodes") %>%
  networkIntersections <- n_df %>%
    mutate(tmp_id=row_number())
  nearestIntersection <- st_nearest_feature(validStops,networkIntersections)
  networkIntersectionsClosest <- networkIntersections[nearestIntersection,]
  distanceToNetwork <- st_distance(validStops,
                                   networkIntersectionsClosest,
                                   by_element=TRUE) %>%
    as.numeric()
  

  
  validStopsSnapped <- validStops %>%
    st_drop_geometry() %>%
    mutate(tmp_id=nearestIntersection) %>%
    mutate(dist=distanceToNetwork) %>%
    left_join(networkIntersectionsClosest,by="tmp_id") %>%
    st_sf() %>%
    filter(dist<=1000) %>%
    dplyr::select(stop_id,id,x,y) #'stop_id' is the gtfs id, 'id' is the network node id
  st_write(validStopsSnapped,"data/stopsSnapped.sqlite",delete_layer=TRUE)
  
  validStopTimesSnapped <- validStopTimes %>%
    right_join(st_drop_geometry(validStopsSnapped),by="stop_id") %>% # IMPORTANT: this join also removes the stops outside of the region!
    arrange(trip_id,stop_sequence) %>%
    dplyr::select(trip_id,stop_sequence,arrival_time,departure_time,stop_id,id,x,y)
  
  vehicleTripMatching <- validTrips %>%
    left_join(validRoutes,by="route_id")
  
  # the public transport network
  ptNetwork <- validStopTimesSnapped %>%
    dplyr::select(trip_id,arrival_time,departure_time,from_id=id,from_x=x,from_y=y) %>%
    # filter(row_number()<200) %>%
    group_by(trip_id) %>%
    mutate(arrivalOffset=arrival_time-min(arrival_time)) %>%
    mutate(departureOffset=departure_time-min(arrival_time)) %>%
    mutate(to_id=lead(from_id),
           to_x=lead(from_x),
           to_y=lead(from_y)) %>%
    #mutate(to_id=ifelse(is.na(to_id),lag(from_id,n()-1),to_id),
    #       to_x=ifelse(is.na(to_x),lag(from_x,n()-1),to_x),
    #       to_y=ifelse(is.na(to_y),lag(from_y,n()-1),to_y)) %>%
    filter(!is.na(to_id)) %>% 
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
  
  
  ptNetworkGeom <- ptNetwork %>%
    dplyr::select(service_type,from_id,to_id,from_x,from_y,to_x,to_y) %>%
    distinct() %>%
    mutate(GEOMETRY=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355)
  st_write(ptNetworkGeom,"data/ptNetwork.sqlite",delete_layer=TRUE)
  
  ptNetworkFull <- ptNetwork %>%
    mutate(GEOMETRY=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355)
  st_write(ptNetworkFull,"data/ptNetworkFull.sqlite",delete_layer=TRUE)
  
  
  # making tables for XML
  
  # ./data/transitVehicles.xml: vehicle
  #id is just the trip_id. This means we can potentially have a different vehicle for each trip. I have also set the vehicle type here.
  vehicles <- validTrips %>%
    filter(trip_id%in%validStopTimesSnapped$trip_id) %>%
    left_join(validRoutes,by="route_id") %>%
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
  # trip_id is the transitRoute (i.e., each trip is its own route, with a single trip. This allows for longer offsets during peak traffic.)
  # route is the same as the refID column since we use a direct line between each stop.
  routeProfile <- ptNetwork %>%
    dplyr::select(trip_id,arrivalOffset,departureOffset,from_id,to_id) %>%
    inner_join(transitStops,by=c("from_id","to_id")) %>%
    dplyr::select(trip_id,arrivalOffset,departureOffset,refId=stop_id,from_id,to_id) # NOTE: route.link.refId is not the same as the refId column
  
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > departures
  #vehicleRefId is just the trip_id. This means we can potentially have a different vehicle for each trip. I have also set the vehicle type here.
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
  
  
  
  # transitVehicles
  cat(
    "<?xml version=\"1.0\" ?>
<vehicleDefinitions xmlns=\"http://www.matsim.org/files/dtd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.matsim.org/files/dtd http://www.matsim.org/files/dtd/vehicleDefinitions_v1.0.xsd\">\n",
    file="./data/transitVehicles.xml",append=FALSE)
  for (i in 1:nrow(vehicleTypes)) {
    cat(paste0("  <vehicleType id=\"",vehicleTypes[i,]$id,"\">\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <description>",vehicleTypes[i,]$description,"</description>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <capacity>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("      <seats persons=\"",vehicleTypes[i,]$seats,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("      <standingRoom persons=\"",vehicleTypes[i,]$standingRoom,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    </capacity>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <length meter=\"",vehicleTypes[i,]$length,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <accessTime secondsPerPerson=\"",vehicleTypes[i,]$accessTime,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <egressTime secondsPerPerson=\"",vehicleTypes[i,]$egressTime,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("    <passengerCarEquivalents pce=\"",vehicleTypes[i,]$passengerCarEquivalents,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
    cat(paste0("  </vehicleType>\n"),file="./data/transitVehicles.xml",append=TRUE)
  }
  for (i in 1:nrow(vehicles)) {
    cat(paste0("  <vehicle id=\"",vehicles[i,]$id,"\" type=\"",vehicles[i,]$type,"\"/>\n"),file="./data/transitVehicles.xml",append=TRUE)
  }
  cat(paste0("</vehicleDefinitions>\n"),file="./data/transitVehicles.xml",append=TRUE)
  
  
  
  
  # transitSchedule
  cat(
    "<?xml version=\"1.0\" ?>
<!DOCTYPE transitSchedule SYSTEM \"http://www.matsim.org/files/dtd/transitSchedule_v1.dtd\">
<transitSchedule>
  <transitStops>\n",
    file="./data/transitSchedule.xml",append=FALSE)
  
  for (i in 1:nrow(transitStops)) {
    # for (i in 1:100) {
    cat(paste0("    <stopFacility id=\"",transitStops[i,]$stop_id,"\" isBlocking=\"false\" linkRefId=\"",
               transitStops[i,]$linkRefId,"\" x=\"",transitStops[i,]$x,"\" y=\"",
               transitStops[i,]$y,"\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
  }
  cat(paste0("  </transitStops>\n"),file="./data/transitSchedule.xml",append=TRUE)
  cat(paste0("  <transitLine id=\"Melbourne\">\n"),file="./data/transitSchedule.xml",append=TRUE)
  
  # TODO Ask @Alan to check this part:
  # for (i in 1:nrow(vehicleTripMatching)) {
  for (i in 1:100) {
    routeProfileCurrent <- routeProfile%>%filter(trip_id==vehicleTripMatching[i,]$trip_id) 
    if(nrow(routeProfileCurrent)>0){ # I added this to drop those empty route profiles
      cat(paste0("    <transitRoute id=\"",vehicleTripMatching[i,]$trip_id,"\">\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <description>",vehicleTripMatching[i,]$service_id,"</description>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <transportMode>",vehicleTripMatching[i,]$service_type,"</transportMode>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <routeProfile>\n"),file="./data/transitSchedule.xml",append=TRUE)
      
      for (j in 1:nrow(routeProfileCurrent)) {
        cat(paste0("        <stop arrivalOffset=\"",
                   routeProfileCurrent[j,]$arrivalOffset,
                   "\" awaitDeparture=\"true\" departureOffset=\"",
                   routeProfileCurrent[j,]$departureOffset,
                   "\" refId=\"",
                   routeProfileCurrent[j,]$refId,
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }
      cat(paste0("      </routeProfile>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("      <route>\n"),file="./data/transitSchedule.xml",append=TRUE)
      for (j in 1:nrow(routeProfileCurrent)) {
        cat(paste0("        <link refId=\"",
                   paste0(routeProfileCurrent[j,]$from_id,"_",routeProfileCurrent[j,]$to_id),
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }
      cat(paste0("      </route>\n"),file="./data/transitSchedule.xml",append=TRUE)
      
      cat(paste0("      <departures>\n"),file="./data/transitSchedule.xml",append=TRUE)
      departuresCurrent <- departures%>%filter(vehicleRefId==vehicleTripMatching[j,]$trip_id)
      for (k in 1:nrow(departuresCurrent)) {
        cat(paste0("        <departure departureTime=\"",
                   departuresCurrent[k,]$departureTime,
                   "\" id=\"",
                   departuresCurrent[k,]$id,
                   "\" vehicleRefId=\"",
                   departuresCurrent[k,]$vehicleRefId,
                   "\"/>\n"),file="./data/transitSchedule.xml",append=TRUE)
      }   
      cat(paste0("      </departures>\n"),file="./data/transitSchedule.xml",append=TRUE)
      cat(paste0("    </transitRoute>\n"),file="./data/transitSchedule.xml",append=TRUE)
    }

  }
  cat(paste0("  </transitLine>\n"),file="./data/transitSchedule.xml",append=TRUE)
  cat(paste0("</transitSchedule>\n"),file="./data/transitSchedule.xml",append=TRUE)
  
  
  
  # routeProfile, stop
  # route, link
  # departures, departure
  ptNetworkMATSim <- ptNetworkFull %>% 
    mutate(length=unclass(st_length(.)))%>% 
    #mutate(osm_id=9999999) %>% 
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
    mutate(id=paste0(from_id,"_",to_id)) %>% 
    st_drop_geometry() %>% 
    #dplyr::select(osm_id, id, from_id, to_id, fromX=from_x, fromY=from_y, toX=to_x, toY=to_y, length, highway, freespeed, permlanes, capacity, cycleway, is_cycle, is_walk, is_car, modes) %>% 
    dplyr::select(id, from_id, to_id, fromX=from_x, fromY=from_y, toX=to_x, toY=to_y,
                  length, freespeed, permlanes, capacity, is_oneway,
                  cycleway,highway, is_cycle, is_walk, is_car, modes) %>% 
    distinct()
    
  return(ptNetworkMATSim)
}

