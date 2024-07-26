addGtfsLinks <- function(outputLocation,
                         nodes, 
                         links,
                         gtfs_feed, 
                         analysis_date,
                         studyRegion=NA,
                         outputCrs,
                         onroadBus,
                         city){
  
  # outputLocation = "./output/generated_network/pt/"
  # nodes = networkOneway[[1]]
  # links = networkOneway[[2]]
  # gtfs_feed = "./data/gtfs.zip"
  # analysis_date = as.Date("2023-11-15","%Y-%m-%d")
  # studyRegion = st_read(region, quiet=T) %>% st_buffer(regionBufferDist) %>% st_snap_to_grid(1)
  # outputCrs = 7899
  # onroadBus = T
  
  dir_create(outputLocation)
  
  validRoadEdges <- links %>%
    st_drop_geometry() %>%
    filter(is_walk==1 & is_car==1 & is_cycle==1) %>%
    dplyr::select(from_id,to_id)
  validRoadIds <- c(validRoadEdges$from_id,validRoadEdges$to_id) %>% unique()
  # network nodes that can be reached via walking, cycling, and driving.  
  validRoadNodes <- nodes %>%
    filter(id %in% validRoadIds) %>%
    st_set_crs(outputCrs)
  
  # process the GTFS feed
  processedGtfs <- processGtfs(outputLocation,
                               nodes,
                               links,
                               networkNodes = validRoadNodes,
                               gtfs_feed,
                               analysis_date,
                               studyRegion,
                               outputCrs,
                               onroadBus)
  
  # unpack the outputs
  stops <- processedGtfs[[1]]
  stopTimes <- processedGtfs[[2]]
  trips <- processedGtfs[[3]]
  routes <- processedGtfs[[4]]
  stopTable <- processedGtfs[[5]]
  shape.links <- processedGtfs[[6]]
  
  # We run into trouble if the geometry column is 'geom' instead of 'GEOMETRY'
  stops <- stops %>% st_set_geometry("geom")
  
  # return the edges in the PT network as well as write the
  # transitVehicles.xml and transitSchedule.xml files
  edgesCombined <- exportGtfsSchedule(
    links,
    outputLocation,
    stops,
    stopTimes,
    trips,
    routes,
    stopTable,
    shape.links,
    outputCrs,
    onroadBus,
    city
  )
  return(edgesCombined)
}

processGtfs <- function(outputLocation = "./output/generated_network/pt/",
                        nodes,
                        links,
                        networkNodes,
                        gtfs_feed, 
                        analysis_date,
                        studyRegion = NA,
                        outputCrs,
                        onroadBus){
  
  # outputLocation ="./output/generated_network/pt/"
  # networkNodes = validRoadNodes
  # gtfs_feed = "./data/gtfs.zip"
  # analysis_date = as.Date("2023-11-15","%Y-%m-%d")

  #dir.create(outputLocation, showWarnings = FALSE)
  
  gtfs <- read_gtfs(gtfs_feed)
  
  analysis_day <- tolower(weekdays(analysis_date))
  
  # if calendar uses integers for dates, convert dates (eg 2023-11-15 to 20231115)
  if (typeof(gtfs$calendar$start_date) == "integer") {
    analysis_date <- as.integer(gsub("-", "", analysis_date))
  }
  
  validCalendar <- gtfs$calendar %>%
    filter(start_date<=analysis_date & end_date>=analysis_date) %>%
    filter(.data[[analysis_day]] == 1)

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
           service_type=ifelse(agency_id%in%c(4,5,6) & route_type%in%c(3),  "bus"  ,service_type)) %>%
    filter(service_type!="null") %>%  # eg skybus
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
      # arrival and departure times as no. of seconds (keeping even those over 24 hours)
      mutate(arrival_time = as.numeric(str_sub(arrival_time, 1, 2)) * 60 * 60 +
               as.numeric(str_sub(arrival_time, 4, 5)) * 60 +
               as.numeric(str_sub(arrival_time, 7, 8)),
             departure_time = as.numeric(str_sub(departure_time, 1, 2)) * 60 * 60 +
               as.numeric(str_sub(departure_time, 4, 5)) * 60 +
               as.numeric(str_sub(departure_time, 7, 8))) %>%
      dplyr::select(trip_id,arrival_time,departure_time,stop_id,stop_sequence) %>%
      arrange(trip_id,stop_sequence)
  )
  
  # stops that have a valid stopTime
  validStops <- gtfs$stops %>%
    mutate(stop_id=as.factor(stop_id)) %>%
    filter(stop_id %in% validStopTimes$stop_id) %>%
    dplyr::select(stop_id,stop_lat,stop_lon) %>%
    st_as_sf(coords=c("stop_lon", "stop_lat"), crs=4326) %>%
    st_transform(outputCrs) %>%
    st_snap_to_grid(1)
  
  # only want stops within the study region
  if(!is.na(studyRegion)[1]) {
    message("Cropping GTFS to study region")
    validStops <- validStops %>%
      filter(lengths(st_intersects(., studyRegion)) > 0)
  }
  
  # remove any duplicate stop id's (which shouldn't exist); they may have
  # different geometries; just keep the first of any duplicates
  validStops <- validStops %>% group_by(stop_id) %>% slice(1) %>% ungroup()
  
  # add service type, by joining stop times, trips and routes (note: one stop
  # may have more than one service, eg combined train and vline bus)
  serviceTable <- validStopTimes %>%
    dplyr::select(stop_id, trip_id) %>%
    distinct() %>%
    left_join(., validTrips %>% dplyr::select(trip_id, route_id) %>% distinct(), 
              by = "trip_id") %>%
    left_join(., validRoutes, by = "route_id") %>%
    distinct(stop_id, service_type)
  
  validStops <- validStops %>%
    left_join(serviceTable, by = "stop_id")

  if (onroadBus & "shapes" %in% names(gtfs)) {
    
    echo("Finding potential bus stop locations within 100m of GTFS shapes\n")
    
    # make subnetwork of nodes and links within buffered shapes
    shape.subnetwork <- makeShapeSubnetwork(gtfs, 
                                            nodes, 
                                            links, 
                                            validRoutes,
                                            studyRegion,
                                            outputCrs)
    shape.nodes <- shape.subnetwork[[1]]
    shape.links <- shape.subnetwork[[2]]
    
    # # write shape.links to file for use when finding routes
    # saveRDS(shape.links, file=paste0(outputLocation, "shape_links.rds"))

    # only shape.nodes are used for snapping bus stops
    networkNodesBus <- networkNodes %>%
      filter(id %in% shape.nodes$id)
    networkLinksBus <- links %>%
      filter(from_id %in% networkNodesBus$id | to_id %in% networkNodesBus$id)
    
  } else {
    
    if (onroadBus & !"shapes" %in% names(gtfs)) {
      message("No shapes file present in GTFS feed, so unable to convert shapes to routes; will make pseudo links for bus instead")
     } 
    
    # all nodes can be used for snapping bus stops
    networkNodesBus <- networkNodes
    networkLinksBus <- links %>%
      filter(from_id %in% networkNodesBus$id | to_id %in% networkNodesBus$id)
    shape.links <- NA
  }
  
  # divide into bus and non-bus (but, if onroadBus = F, they will be processed the same way,
  # because 'networkNodesBus' will be the same as 'networkNodes')
  validStopsBus <- validStops %>% filter(service_type == "bus")
  validStopsOther <- validStops %>% filter(service_type != "bus")
  validStopIds <- as.character(c(validStopsBus$stop_id, validStopsOther$stop_id))
  serviceTypes <- as.character(c(validStopsBus$service_type, validStopsOther$service_type))

  # snapping the stops to the nearest node in the road network
  # for bus - nearest valid node on the nearest link
  # setup for parallel processing and progress reporting
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  pb <- txtProgressBar(max = max(nrow(validStopsBus), 2), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  echo(paste("Finding nearest node on nearest link for", nrow(validStopsBus), "bus stops;",
             "parallel processing with", cores, "cores\n"))
  
  # loop to find list of boundary points
  nearestNodeBus <-
    foreach(i = 1:nrow(validStopsBus),
            # foreach(i = 1:10,
            .combine = rbind,
            .packages = c("dplyr", "sf"),
            .options.snow = opts) %dopar% {
              
              stop <- validStopsBus[i,]
              # nearest link (which must contain a NetworkNode) to the stop
              nearest.link <- networkLinksBus[st_nearest_feature(stop, networkLinksBus), ]
              # the link's NetworkNodes
              eligible.nodes <- networkNodesBus %>%
                filter(id == nearest.link$from_id | id == nearest.link$to_id)
              # nearest of the link's NetworkNodes to the stop
              nearest.node <- networkNodesBus %>%
                filter(id == eligible.nodes$id[st_nearest_feature(stop, eligible.nodes)])
              
              return(nearest.node)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  # otherwise - nearest valid node
  nearestNodeIdOther <- st_nearest_feature(validStopsOther, networkNodes)
  nearestNodeOther <- networkNodes[nearestNodeIdOther, ]
  
  # placing in order to match validStops
  nearestNode <- rbind(nearestNodeBus, nearestNodeOther)
  
  # calculating the distance from each stop to the nearest node in the road network
  distanceToNetworkBus <- st_distance(validStopsBus, nearestNodeBus, by_element=TRUE) %>%
    as.numeric()
  distanceToNetworkOther <- st_distance(validStopsOther, nearestNodeOther, by_element=TRUE) %>%
    as.numeric()
  distanceToNetwork <- c(distanceToNetworkBus, distanceToNetworkOther)
  
  validStopsSnapped <- nearestNode %>%
    mutate(stop_id = validStopIds,
           service_type = serviceTypes,
           dist = distanceToNetwork) %>%
    filter(dist <= 1000) %>%
    dplyr::select(stop_id, id, x, y, service_type)  # 'stop_id' is the gtfs id, 'id' is the network node id

  validStopTimesSnapped <- validStopTimes %>%
    # join service details 
    left_join(validTrips %>% dplyr::select(trip_id, route_id) %>% distinct(), 
              by = "trip_id") %>%
    left_join(validRoutes, by = "route_id") %>%
    # join stop locations
    inner_join(st_drop_geometry(validStopsSnapped),
               by = c("stop_id", "service_type")) %>% # IMPORTANT: this join also removes the stops outside of the region!
    arrange(trip_id,stop_sequence) %>%
    group_by(trip_id) %>%
    # we want at least 2 stops for each trip
    filter(n() >= 2) %>%
    # when we use the snapped locations, two sequential stops may be at the same
    # location. If this is the case, we remove the later stop.
    filter(id!=lag(id) | row_number()==1) %>%
    mutate(stop_sequence=row_number()) %>%
    ungroup() %>%
    dplyr::select(trip_id,stop_sequence,arrival_time,departure_time,stop_id,id,x,y,service_type)
  
  # some trips will no longer be present
  validTripsSnapped <- validTrips %>%
    filter(trip_id %in% validStopTimesSnapped$trip_id)
  
  # some routes will no longer be present
  validRoutesSnapped <- validRoutes %>%
    filter(route_id %in% validTripsSnapped$route_id)
  
  # table of GTFS railway station stop IDs and node ids (for later matching to patronage data)
  stopTable <- validStopsSnapped %>%
    st_drop_geometry() %>%
    dplyr::select(stop_id, id, service_type) %>%
    rename(gtfs_stop_id = stop_id, node_id = id) %>% 
    left_join(., gtfs$stops, by = c("gtfs_stop_id" = "stop_id")) %>%
    dplyr::select(gtfs_stop_id, node_id, service_type, stop_name)
 
  # replace stop_id with id (i.e., use the network node id instead of the stop
  # id provided by the GTFS feed)
  validStopsSnappedFinal <- validStopsSnapped %>%
    dplyr::select(-stop_id) %>%
    group_by(id,x,y) %>%
    slice_head() %>%
    ungroup() %>%
    rename(stop_id=id)

  validStopTimesSnappedFinal <- validStopTimesSnapped %>%
    dplyr::select(-stop_id) %>%
    rename(stop_id=id)
  
  # return the exports as a list
  return(list(validStopsSnappedFinal,
              validStopTimesSnappedFinal,
              validTripsSnapped,
              validRoutesSnapped,
              stopTable,
              shape.links))
  
}


exportGtfsSchedule <- function(links,
                               outputLocation,
                               stops,
                               stopTimes,
                               trips,
                               routes,
                               stopTable,
                               shape.links,
                               outputCrs,
                               onroadBus,
                               city){
  
  # flag for whether buses are routed onroad (requires 'shape.links' to be created in 'processGtfs')
  if (onroadBus & !is.na(shape.links)[1]) {
    onroadBusRouting = T
  } else {
    if (onroadBus & is.na(shape.links)[1]) {
      message("Subnetwork of links for bus routing has not been created; will make pseudo links for bus instead")
    }
    onroadBusRouting = F
  }

  # duplicate stopTimes where arrrival time is at or after 24:00:00, so timetable contains early morning entries
  earlyMorningStopTimes <- stopTimes %>%
    filter(departure_time >= 86400) %>% # 86400 is the number of seconds in 24 hours
    mutate(arrival_time = arrival_time - 86400,
           departure_time = departure_time - 86400,
           trip_id_orig = trip_id, # original trip_id
           trip_id = paste0(trip_id, "_E"))  # add 'E' (early) to trip_id, so copy is distinguished in creating ptNetwork
  
  # add orig trip id field to stopTimes (so 'trip_id' will  distinguish duplicated 'early' trip id's, 
  # while 'orig_trip_id' will be used to join other tables containing only the original trip id's
  stopTimes <- stopTimes %>%
    mutate(trip_id_orig = trip_id)
  
  # combine with early morning stop times
  stopTimes <- bind_rows(stopTimes, earlyMorningStopTimes)
  
  # if routing buses on road, then find routes between pairs of nodes along the routes
  # note - 'shape.links' will not be NA if onroadBus=T and there are gtfs shapes
  if (onroadBusRouting) {
    system.time(nodePairRoutes <- findNodePairRoutes(stopTimes, 
                                                     trips, 
                                                     routes, 
                                                     shape.links,
                                                     existingNodePairs = NA))
    
    system.time(unroutedStopOutputs <- removeUnroutedStops(stopTimes, 
                                                           trips,
                                                           routes,
                                                           shape.links,
                                                           nodePairRoutes))
    stopTimes <- unroutedStopOutputs[[1]]
    nodePairRoutes <- unroutedStopOutputs[[2]]
    
    # saveRDS(nodePairRoutes, file=paste0(outputLocation,"nodePairRoutes.rds"))
  }
  
  # the public transport network
  ptNetwork <- stopTimes %>%
    dplyr::select(trip_id,arrival_time,departure_time,from_id=stop_id,from_x=x,from_y=y, trip_id_orig, service_type) %>%
    group_by(trip_id) %>%
    mutate(arrivalOffset=arrival_time-min(arrival_time)) %>%
    mutate(departureOffset=departure_time-min(arrival_time)) %>%
    # stop requires a related link; stop can only be served by vehicles driving on that link
    mutate(to_id=lead(from_id),
           to_x=lead(from_x),
           to_y=lead(from_y)) %>%
    # for the last node in the trip, the to_id is the same as the from_id, so the related 'link' is a point
    mutate(to_id=ifelse(is.na(to_id),from_id,to_id)) %>%
    mutate(to_x=ifelse(is.na(to_x),from_x,to_x)) %>%
    mutate(to_y=ifelse(is.na(to_y),from_y,to_y)) %>%
    ungroup() %>%
    mutate(arrivalOffset=as.character(as_hms(arrivalOffset)),
           departureOffset=as.character(as_hms(departureOffset)),
           arrival_time=as.character(as_hms(arrival_time)),
           departure_time=as.character(as_hms(departure_time))) %>%
    # join trips, to obtain route_id
    left_join(., trips, by = c("trip_id_orig" = "trip_id")) %>%
    as.data.frame()
  
  vehicleTripMatching <- ptNetwork %>%
    distinct(route_id, service_id, trip_id, service_type)
  
  arrivalTimes <- ptNetwork %>%
    dplyr::select(arrival_time,trip_id) %>%
    group_by(trip_id) %>%
    summarise(arrival_time=min(arrival_time,na.rm=T)) %>%
    as.data.frame()
  
  # finding the distinct routes (same stops and arrival/departure offset times),
  # and determining their first departure time
  ptNetworkDepartures <- ptNetwork %>%
    dplyr::select(trip_id,from_id,from_x,from_y,arrivalOffset,departureOffset,to_id,to_x,to_y) %>%
    group_by(trip_id) %>%
    summarise(from_id=paste0(from_id,collapse = "_"),
              to_id=paste0(to_id,collapse = "_"),
              arrivalOffset=paste0(arrivalOffset,collapse = "_"),
              departureOffset=paste0(departureOffset,collapse = "_")) %>%
    inner_join(arrivalTimes,by='trip_id') %>%
    arrange(trip_id,arrival_time)%>%
    group_by(from_id,to_id,arrivalOffset,departureOffset) %>%
    mutate(route_id_new=formatC(cur_group_id(),digits=0,width=5,flag="0",format="d")) %>%
    mutate(departure_id=formatC(row_number(),digits=0,width=2,flag="0",format="d")) %>%
    ungroup() %>%
    dplyr::select(trip_id,route_id_new,departure_id,departure_time=arrival_time) %>%
    # arrange(route_id_new,departure_id) %>%
    arrange(route_id_new,departure_time) %>%
    as.data.frame()
  
  # network of stops
  ptNetwork_Stops <- ptNetwork %>%
    dplyr::select(from_id,to_id,from_x,from_y,to_x,to_y, service_type) %>%
    distinct() %>%
    filter(!is.na(to_id)) %>% 
    mutate(geom=paste0("LINESTRING(",from_x," ",from_y,",",to_x," ",to_y,")")) %>%
    st_as_sf(wkt = "geom", crs = outputCrs) %>%
    # stop_id naming convention: stop_id is in the form 'mode_x_y', where 'mode' is 
    # bus, train or tram, 'x' is the node of the stop, and 'y' is the node of the 
    # next stop (and for the last stop, x and y are the same)
    mutate(stop_id = paste0(service_type, "_", from_id, "_", to_id))

  # add edges to network
  # link_id naming convention: for pseudo links: link_id the service type (eg 'train_');
  # plus a row number; for on-road links, it's the first link in the chain of links between the stops
  if (onroadBusRouting) {
    ptNetwork_StopsAndEdges <- ptNetwork_Stops %>%
      # join the chains of links ('link_ids') between pairs of nodes
      left_join(nodePairRoutes, 
                by = c("from_id" = "stop_id", "to_id" = "next_stop_id")) %>%
      # remove any chains that aren't bus routes (eg when train and bus both run between same pair)
      mutate(link_ids = ifelse(service_type != "bus", NA, link_ids)) %>%
      # link id: service type plus an identifying number
      mutate(link_id = paste0(service_type, "_",
                              formatC(row_number(), digits=0, width=5, flag="0", format="d"))) %>%
      # update link id for buses: first link in the chain
      rowwise() %>%
      mutate(link_id = if_else(service_type == "bus" & from_id != to_id, 
                               unlist(str_split(link_ids, ", "))[1],
                               link_id)) %>%
      ungroup()
  } else {
    ptNetwork_StopsAndEdges <- ptNetwork_Stops %>%
      # link id - service type plus an identifying number
      mutate(link_id = paste0(service_type, "_",
                              formatC(row_number(), digits=0, width=5, flag="0", format="d")))
  }
  
  ptNetworkRoutes <- ptNetwork %>%
    inner_join(ptNetworkDepartures%>%group_by(route_id_new)%>%
                 slice(which.min(departure_id))%>%dplyr::select(trip_id,route_id_new),
               by="trip_id") %>%
    dplyr::select(route_id_new,from_id,to_id,arrivalOffset,departureOffset, service_type) %>%
    inner_join(ptNetwork_StopsAndEdges%>%st_drop_geometry()%>%
                 dplyr::select(from_id,to_id,stop_id,any_of("link_ids"),link_id,service_type),
               by=c("from_id","to_id", "service_type")) %>%
    dplyr::select(route_id_new,arrivalOffset,departureOffset,stop_id,link_id,any_of("link_ids"),service_type)
  
  # adding Stop numbers to railway station stop table 
  nodeStopIds <- ptNetwork_StopsAndEdges %>%
    dplyr::select(from_id, stop_id) %>%
    st_drop_geometry() %>%
    distinct() %>%
    group_by(from_id) %>%
    summarise(stop_ids_for_node = paste(stop_id, collapse = ", ")) %>%
    ungroup()
  stopTable <- stopTable %>%
    left_join(nodeStopIds, by = c("node_id" = "from_id"))
  
  write.csv(stopTable, file=paste0(outputLocation, "stopTable.csv"))
  
  
  # making tables for XML
  
  # ./data/transitVehicles.xml: vehicle
  # id is just the trip_id. This means we can potentially have a different vehicle 
  # for each trip. Have also set the vehicle type here.
  vehicles <- vehicleTripMatching %>%
    dplyr::select(id=trip_id,service_type) %>%
    arrange(id,service_type) %>%
    as.data.frame()
  
  # ./data/transitSchedule.xml: transitSchedule > transitStops
  transitStops <- ptNetwork_StopsAndEdges %>%
    st_drop_geometry() %>%
    dplyr::select(stop_id,linkRefId=link_id,x=from_x,y=from_y)
  
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > routeProfile
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > route
  # * trip_id is the transitRoute (i.e., each trip is its own route, with a single
  #   trip. This allows for longer offsets during peak traffic).
  # * route is the same as the refID column since we use a direct line between each stop.
  routeProfile <- ptNetworkRoutes %>%
    dplyr::select(transitRouteId=route_id_new, refId=stop_id,arrivalOffset,
                  departureOffset,linkRefId=link_id, any_of("link_ids"),service_type)
  
  # ./data/transitSchedule.xml: transitSchedule > transitRoute > departures
  # vehicleRefId is just the trip_id. This means we can potentially have a 
  # different vehicle for each trip. I have also set the vehicle type here.
  departures <- ptNetworkDepartures %>%
    # mutate(departure_time=as.character(as.hms(departure_time))) %>%
    left_join(vehicles, by=c("trip_id"="id")) %>%
    # arrange(route_id_new,departure_id,service_type) %>%
    arrange(route_id_new,departure_time,service_type) %>%
    group_by(service_type) %>%
    mutate(type=NA,
           type=ifelse(service_type=="train",1,type),
           type=ifelse(service_type=="bus",2,type),
           type=ifelse(service_type=="tram",3,type)) %>%
    mutate(vehicleRefId=paste0(service_type,"_",formatC(row_number(),digits=0,width=5,flag="0",format="d"))) %>%
    ungroup() %>%
    dplyr::select(transitRouteId=route_id_new,departureId=departure_id,
                  departureTime=departure_time,vehicleRefId,type,serviceType=service_type) %>%
    as.data.frame()
  
  # Types of vehicles to place in the network - TO BE REVIEWED
  vehicleTypes <- tribble(
    ~id, ~service_type, ~seats, ~standingRoom, ~length, ~accessTime, ~egressTime, ~passengerCarEquivalents,
    1  , "train"      , 114   , 206          , 150    , "0.0"      , "0.0"      , 0.25                    ,
    2  , "bus"        , 25    , 13           , 15     , "0.0"      , "0.0"      , 0.25                    ,
    3  , "tram"       , 16    , 50           , 30     , "0.0"      , "0.0"      , 0.25
  )
  
  echo("writing transitVehicles.xml\n")
  outxml<-paste0(outputLocation,"transitVehicles.xml")
  # transitVehicles
  cat(
    "<?xml version=\"1.0\" ?>
<vehicleDefinitions xmlns=\"http://www.matsim.org/files/dtd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.matsim.org/files/dtd http://www.matsim.org/files/dtd/vehicleDefinitions_v1.0.xsd\">\n",
    file=outxml,append=FALSE)
  str<-""
  writeInterval<-500
  processed<-0
  for (i in 1:nrow(vehicleTypes)) {
    str<-paste0(str,"  <vehicleType id=\"",vehicleTypes[i,]$id,"\">\n")
    str<-paste0(str,"    <description>",vehicleTypes[i,]$service_type,"</description>\n")
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
  for (i in 1:nrow(departures)) {
    str<-paste0(str,"  <vehicle id=\"",departures[i,]$vehicleRefId,"\" type=\"",departures[i,]$type,"\"/>\n")
    if (i%%writeInterval==0 || i==nrow(departures)) {
      cat(str,file=outxml,append=TRUE)
      str<-"" # clear the buffer after writing it out
    }
    # report progress
    if (i%%50==0 || i==nrow(departures)) printProgress(i,nrow(departures),' Vehicles')
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
  cat(paste0("  <transitLine id=\"",city,"\">\n"),file=outxml,append=TRUE)
  
  echo("writing vehicleTripMatching\n")
  str<-""
  writeInterval<-100
  
  transitRoutes<-routeProfile$transitRouteId%>%unique()%>%sort()
  
  for (i in 1:length(transitRoutes)) {
    # for (i in 1:100) {
    routeProfileCurrent <- routeProfile[routeProfile$transitRouteId==transitRoutes[i],]
    departuresCurrent <- departures[departures$transitRouteId==transitRoutes[i],]
    if(nrow(routeProfileCurrent)>1){ # I added this to drop those empty route profiles
      str<-paste0(str,"    <transitRoute id=\"",transitRoutes[i],"\">\n")
      str<-paste0(str,"      <description>",departuresCurrent[1,]$type,"</description>\n")
      str<-paste0(str,"      <transportMode>",departuresCurrent[1,]$serviceType,"</transportMode>\n")  ### HERE
      str<-paste0(str,"      <routeProfile>\n")
      
      for (j in 1:nrow(routeProfileCurrent)) { 
        # first row: no arrival offset
        # <stop awaitDeparture="true" departureOffset="departureOffset" refId="refId">
        if (j == 1) str<-paste0(str,"        <stop awaitDeparture=\"true\" departureOffset=\"",
                                routeProfileCurrent[j,]$departureOffset,
                                "\" refId=\"",
                                routeProfileCurrent[j,]$refId,
                                "\"/>\n")
        # rows except first and last
        # <stop arrivalOffset="arrivalOffset" awaitDeparture="true" departureOffset="departureOffset" refId="refId">
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
        # <stop arrivalOffset="arrivalOffset" refId="refId">
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
        if (onroadBusRouting) {
          if (!is.na(routeProfileCurrent[j,]$link_ids)) {
            link_ids <- unlist(str_split(routeProfileCurrent[j,]$link_ids, ", "))
            for (k in 1:length(link_ids)) {
              str<-paste0(str,"        <link refId=\"",
                          link_ids[k],
                          "\"/>\n")
            }
          } else {
            str<-paste0(str,"        <link refId=\"",
                        routeProfileCurrent[j,]$linkRefId,
                        "\"/>\n")
          }
        } else {
          str<-paste0(str,"        <link refId=\"",
                      routeProfileCurrent[j,]$linkRefId,
                      "\"/>\n")
        }
      }
      str<-paste0(str,"      </route>\n")
      
      str<-paste0(str,"      <departures>\n")
      for (k in 1:nrow(departuresCurrent)) {
        str<-paste0(str,"        <departure departureTime=\"",
                    departuresCurrent[k,]$departureTime,
                    "\" id=\"",
                    departuresCurrent[k,]$departureId,
                    "\" vehicleRefId=\"",
                    departuresCurrent[k,]$vehicleRefId,
                    "\"/>\n")
      }   
      str<-paste0(str,"      </departures>\n")
      str<-paste0(str,"    </transitRoute>\n")
    }
    
    if (i%%writeInterval==0 || i==length(transitRoutes)) {
      cat(str,file=outxml,append=TRUE)
      str<-"" # clear the buffer after writing it out
    }
    # report progress
    if (i%%50==0 || i==length(transitRoutes)) printProgress(i,length(transitRoutes),' vehicleTripMatching')
  }
  cat(paste0("  </transitLine>\n"),file=outxml,append=TRUE)
  cat(paste0("</transitSchedule>\n"),file=outxml,append=TRUE)
  
  # edges to be added to road network 
  if (onroadBusRouting) {
    ptBaseEdges <- ptNetwork_StopsAndEdges %>%
      # remove bus segments where onroad links are used (that is, where link_ids are present)
      filter(is.na(link_ids))
  } else {
    ptBaseEdges <- ptNetwork_StopsAndEdges
  }
  
  ptNetworkMATSim <- ptBaseEdges %>% 
    mutate(length=round(as.numeric(st_length(.)),3)) %>%
    mutate(length=ifelse(length<1,1,length)) %>%
    mutate(highway=service_type) %>% 
    mutate(freespeed=11.1) %>% 
    mutate(permlanes=1) %>% 
    mutate(capacity=600) %>% 
    mutate(is_oneway=1) %>% 
    mutate(cycleway=NA) %>% 
    mutate(surface=NA) %>%
    mutate(slope_pct=NA) %>%
    mutate(is_cycle=0) %>% 
    mutate(is_walk=0) %>% 
    mutate(is_car=0) %>% 
    mutate(modes=service_type) %>%
    mutate(id = link_id) %>%  # the link_id created above
    mutate(link_id = max(links$link_id) + row_number()) %>%  # row-number link_id, consistent with road links
    dplyr::select(from_id, to_id, fromx=from_x, fromy=from_y, tox=to_x, toy=to_y,
                  length, freespeed, permlanes, capacity, highway, cycleway, 
                  surface, is_cycle, is_walk, is_car, modes, slope_pct, link_id, id)
  
  edgesCombined <- bind_rows(links,ptNetworkMATSim) %>%
    st_sf()
  
  # add 'bus' to modes for onroad buses
  if (onroadBusRouting) {
    buslinks <- c()
    for (i in 1:nrow(nodePairRoutes)) {
      row.links <- nodePairRoutes$link_ids[i] %>%
        str_split(., ", ") %>%
        unlist() %>%
        as.numeric()
      buslinks <- c(buslinks, row.links)
    }
    buslinks <- unique(buslinks)
    edgesCombined <- edgesCombined %>%
      mutate(modes = ifelse(link_id %in% buslinks, paste0(modes, ",bus"), modes))
  }
  
  return(edgesCombined)
}


# function to make subnetwork of nodes and links falling within a 50m buffer of
# 'shapes' in the GTFS feed used on valid routes - only used where onroadBus = T
makeShapeSubnetwork <- function(gtfs, 
                                nodes, 
                                links, 
                                validRoutes,
                                studyRegion = NA,
                                outputCrs) {

  # convert shapes to sf, and filter to bus
  shapes <- gtfs_as_sf(gtfs) %>%
    .$shapes %>% 
    st_transform(outputCrs) %>%
    st_snap_to_grid(1)
  
  # limit to study area (note - this crops shapes at edge of study area)
  if (!is.na(studyRegion)[1]) {
    shapes <- shapes %>%
      st_intersection(., studyRegion %>% st_geometry())
  }
  
  # filter to bus shapes only
  bus.shapes <- shapes %>%
    # bus only
    left_join(gtfs$trips %>% dplyr::select(shape_id, route_id) %>% distinct(),
              by = "shape_id") %>%
    left_join(validRoutes, by = "route_id") %>%  
    filter(service_type == "bus")
  
  # buffer shapes to 100m  - routes and stops must be within this buffer
  buffered.shapes <- bus.shapes %>%
    st_buffer(., 100) %>%
    summarise()
  
  # select links and nodes within the buffered shapes, and that meet the following tests:
  # - for links - must be driveable, so buses can traverse them
  # - for nodes - must be accessible by car, bike and walking (but this filter will be done when snapping stops)
  intersecting.nodes <- nodes  %>%  # all nodes
    dplyr::select(id) %>% 
    st_filter(buffered.shapes, .predicate = st_intersects)
  
  shape.links <- links %>% 
    filter(from_id %in% intersecting.nodes$id & to_id %in% intersecting.nodes$id) %>%
    # must be driveable
    filter(is_car == 1) %>% 
    # must not be dead end
    filter(from_id %in% .$to_id & to_id %in% .$from_id)
  
  # make a  graph of the shape links, and remove any small sections (< 10 nodes)
  g <- graph_from_data_frame(shape.links %>%
                               dplyr::select(from_id, to_id),
                             directed = T)
  components <- components(g)
  component_sizes <- components$csize
  small_components <- which(component_sizes < 10)
  # nodes in the small components
  small_component_nodes <- c()
  for (i in 1:length(small_components)) {
    small_component_nodes <- c(small_component_nodes,
                               V(g)[which(components$membership == small_components[i])] %>%
                                 as_ids())
  }
  # remove links with a from_id or to_id in the small components
  shape.links <- shape.links %>%
    filter(!from_id %in% small_component_nodes | !to_id %in% small_component_nodes)

  shape.nodes <- nodes %>% 
    filter(id %in% shape.links$from_id | id %in% shape.links$to_id)
  
  return(list(shape.nodes, shape.links))
  
}


# function to find route between pairs of nodes representing adjacent stops on 
# a bus route - only used where onroadBus = T
findNodePairRoutes <- function(stopTimes, 
                               trips, 
                               routes, 
                               shape.links,
                               existingNodePairs = NA) {
  
  nodePairs <- stopTimes %>%
    # filter to bus
    filter(trip_id %in% (trips %>% 
                           left_join(routes, by = "route_id") %>%
                           filter(service_type == "bus") %>%
                           .$trip_id)) %>%
    # add next stop ID if it's the same trip (note - these 'stop_ids' are node id's not gtfs id's)
    mutate(next_stop_id = ifelse(trip_id == lead(trip_id),
                                 lead(stop_id), NA)) %>%
    distinct(stop_id, next_stop_id) %>%
    filter(!is.na(next_stop_id)) 
  
  # remove any that have already been found
  if (!is.na(existingNodePairs)[1]) {
    nodePairs <- nodePairs %>%
      filter(!(paste0(stop_id, "_", next_stop_id) %in%
                 (existingNodePairs %>%
                 mutate(node_pair = paste0(stop_id, "_", next_stop_id)) %>%
                 .$node_pair)))
    
  }
  
  if (nrow(nodePairs) > 0) {
    # directed graph of shape nodes and links
    shape.g <- 
      graph_from_data_frame(shape.links %>%
                              dplyr::select(from_id, to_id, weight = length, link_id),
                            # dplyr::select(from_id, to_id, weight = length/freespeed, link_id), # time rather than length
                            directed = T)
    
    # setup for parallel processing and progress reporting
    cores <- detectCores()
    cluster <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cluster)
    pb <- txtProgressBar(max = max(nrow(nodePairs), 2), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    # report
    echo(paste("Finding routes for", nrow(nodePairs), "pairs of bus stop nodes;",
               "parallel processing with", cores, "cores\n"))
    
    # loop to find list of boundary points
    nodePairRoutes <-
      foreach(i = 1:nrow(nodePairs),
              # foreach(i = 21280:21300,
              .combine = rbind,
              .packages = c("dplyr", "sf", "igraph"),
              .options.snow = opts) %dopar% {
                
                row <- nodePairs[i, ]
                
                shortest <- shortest_paths(shape.g,
                                           from = as.character(row$stop_id),
                                           to = as.character(row$next_stop_id),
                                           mode = "out", 
                                           output = "epath")
                
                if (length(shortest$epath[[1]]) > 0) {
                  shortest_link_ids <- edge_attr(shape.g, "link_id", shortest$epath[[1]]) %>%
                    toString()
                } else {
                  shortest_link_ids <- NA
                }
                
                output.row <- cbind(row, link_ids = shortest_link_ids) %>%
                  as.data.frame()
                
                return(output.row)
              }
    
    # close the progress bar and cluster
    close(pb)
    stopCluster(cluster)
    
  } else {
    nodePairRoutes <- c()
  }
  
  return(nodePairRoutes)
  
}

# function to remove unrouted bus stops from stopTimes - only used where onroadBus = T
removeUnroutedStops <- function(stopTimes,
                                trips,
                                routes,
                                shape.links,
                                nodePairRoutes) {
  
  # find missing pair routes
  missingPairRoutes <- nodePairRoutes %>% filter(is.na(link_ids))
  
  # loop to remove stops where unrouted
  counter = 0
  while (nrow(missingPairRoutes) > 0) {
    
    echo(paste("Removing stops for", nrow(missingPairRoutes), "pairs of stops for which routes can't be found\n"))
    
    stopTimes <- stopTimes %>%
      mutate(missing = 0)
    
    # identify stops to which a route can't be found - first time, end of the 
    # missing link; second time, start of the missing link, and so on 
    for (i in 1:nrow(missingPairRoutes)) {
      if (counter %% 2 == 0) {
        stopTimes <- stopTimes %>%
          mutate(missing = ifelse((stop_id == missingPairRoutes$next_stop_id[i] &
                                     lag(stop_id) == missingPairRoutes$stop_id[i] &
                                     trip_id == lag(trip_id)), 1, missing))
      } else {
        stopTimes <- stopTimes %>%
          mutate(missing = ifelse((stop_id == missingPairRoutes$stop_id[i] &
                                     lead(stop_id) == missingPairRoutes$next_stop_id[i] &
                                     trip_id == lead(trip_id)), 1, missing))
      }
    }
    
    # remove where missing and re-do stop sequences
    stopTimes <- stopTimes %>%
      filter(missing != 1) %>%
      group_by(trip_id) %>%
      # remove any with fewer than 2 stops for the trip
      filter(n() >= 2) %>%
      # stop sequence
      mutate(stop_sequence = row_number()) %>%
      ungroup()
    
    # remove the missing pairs from nodePairRoutes
    nodePairRoutes <- nodePairRoutes %>%
      filter(!is.na(link_ids))
    
    # find routes for new node pairs
    newNodePairs <- findNodePairRoutes(stopTimes, 
                                       trips,
                                       routes,
                                       shape.links,
                                       existingNodePairs = nodePairRoutes)
    
    # add new pairs to output, and recalculate missing pair routes
    nodePairRoutes <- rbind(nodePairRoutes, newNodePairs)
    missingPairRoutes <- nodePairRoutes %>% filter(is.na(link_ids))
    
    # increment counter
    counter = counter + 1
  }
  
  if ("missing" %in% names(stopTimes)) {
    stopTimes <- stopTimes %>% dplyr::select(-missing)
  }
  
  return(list(stopTimes, nodePairRoutes))
}
