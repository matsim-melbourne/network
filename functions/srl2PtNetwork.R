processSrl <- function(stations, lines, validRoadNodes,
                       HOURS, INTERVAL, SPEED,
                       ROUTEIDs, SERVICEID, SERVICETYPE) {
  
  # process lines: lengths, endpoints and travel time
  # ---------------------------------------------------------------------------
  # add line lengths
  lines <- lines %>%
    mutate(length = st_length(lines))
  
  # add from/to fields for sequence no. of stations at each end
  startpoints <- st_nearest_feature(st_startpoint(lines), stations)  # index of nearest stations to line startpoints
  endpoints <- st_nearest_feature(st_endpoint(lines), stations)   # index of nearest stations to line endpoints
  # 'from' and 'to': sequence numbers for stations at ends of lines, with 'from' being the lower number
  for (i in 1:nrow(lines)) {
    lines[i, "from"] <- min(stations$sequence[startpoints[i]], stations$sequence[endpoints[i]])
    lines[i, "to"] <- max(stations$sequence[startpoints[i]], stations$sequence[endpoints[i]])
  }
  
  #  calculate time to travel each line at parameter speed, rounded to nearest minute, in seconds
  lines <- lines %>%
    mutate(time.sec = as.numeric(round((length/SPEED) * (60/1000), 0)) * 60) 
  
  
  # process stations: find nearest nodes, and make stops table
  # ---------------------------------------------------------------------------
  # nearest nodes to stations
  nearestNodes <- st_nearest_feature(stations, validRoadNodes)
  stations <- stations %>%
    mutate(node.id = validRoadNodes$id[nearestNodes],
           node.x = validRoadNodes$x[nearestNodes],
           node.y = validRoadNodes$y[nearestNodes])
  
  stations <- st_drop_geometry(stations)  # later steps assume 'stations' is referenced as df, not list
  
  # make srlStops table
  srlStops <- stations %>%
    dplyr::select(stop_id=node.id, x=node.x, y=node.y) %>%
    left_join(validRoadNodes, by = c("x", "y")) %>%
    dplyr::select(stop_id, x, y, GEOMETRY)
  
  
  # make vector of departure times, at which trains will depart from each end of route
  # ---------------------------------------------------------------------------
  departures <- c()
  
  for (i in 1:(length(HOURS)-1)) {
    deptime <- as_hms(HOURS[i])
    departures <- append(departures, deptime)
    while (deptime < as_hms(HOURS[i+1])) {
      deptime <- as_hms(deptime + INTERVAL[i])
      if (deptime < as_hms(HOURS[i+1]))  departures <- append(departures, deptime) 
    }
  }
  
  
  # set up srlStopTimes, srlTrips and srlRoutes tables
  # ---------------------------------------------------------------------------
  srlStopTimes <- tibble(trip_id = factor(),
                         # station = character(),  # for testing
                         stop_sequence = integer(),
                         arrival_time = numeric(),
                         departure_time = numeric(),
                         # tmp_time = character(),  # for testing
                         stop_id = numeric(),
                         x = numeric(),
                         y = numeric())
  
  srlTrips <- tibble(route_id = factor(),
                     service_id = character(),
                     trip_id = character())
  
  srlRoutes <- tibble(route_id = factor(),
                      service_type = factor())
  
  
  # make timetable for outbound direction
  # ---------------------------------------------------------------------------
  tripIDcounter <- 1  # all odd for output direction
  
  stations <- stations %>% 
    arrange(sequence) 
  
  # calculate cumulative offset for each station in outbound direction
  for (i in 1:nrow(stations)) {
    lines2i <- lines %>% 
      filter(from < stations[i, "sequence"])
    stations[i, "outoffset"] <- sum(lines2i$time.sec)
  }
  
  # build srlTrips and srlStopTimes
  for (i in 1:length(departures)) {
    tripID <- sprintf("SRL_%04d", tripIDcounter)  # fixed width 4, with leading zeros
    
    srlTrips <- srlTrips %>%
      add_row(route_id = as.factor(ROUTEIDs[1]),
              service_id = SERVICEID,
              trip_id = as.factor(tripID))
    
    for (j in 1:nrow(stations)) {
      srlStopTimes <- srlStopTimes %>%
        add_row(trip_id = as.factor(tripID), 
                # station = stations[j, "stations"],  # for testing
                stop_sequence = j,
                arrival_time = as.numeric(departures[i] + stations[j, "outoffset"]),
                departure_time = arrival_time,
                # tmp_time = as_hms(arrival_time),  # for testing
                stop_id = stations[j, "node.id"],
                x = stations[j, "node.x"],
                y = stations[j, "node.y"])
      
    } 
    tripIDcounter <- tripIDcounter + 2 
  }
  
  # build srlRoutes
  srlRoutes <- srlRoutes %>%
    add_row(route_id = as.factor(ROUTEIDs[1]),
            service_type = as.factor(SERVICETYPE))
  
  
  # make timetable for return direction
  # ---------------------------------------------------------------------------
  tripIDcounter <- 2  # all even for output direction
  
  stations <- stations %>% 
    arrange(desc(sequence))  # reverse of outbound
  
  # calculate cumulative offset for each station in outbound direction
  for (i in 1:nrow(stations)) {
    lines2i <- lines %>% 
      filter(to > stations[i, "sequence"])  # cf outbound, 'from <'
    stations[i, "backoffset"] <- sum(lines2i$time.sec)  # cf outbound, "outoffset"
  }
  
  # build srlTrips and srlStopTimes
  for (i in 1:length(departures)) {
    tripID <- sprintf("SRL_%04d", tripIDcounter)  # fixed width 4, with leading zeros
    
    srlTrips <- srlTrips %>%
      add_row(route_id = as.factor(ROUTEIDs[2]),  # cf outbound, '1'
              service_id = SERVICEID,
              trip_id = as.factor(tripID))
    
    for (j in 1:nrow(stations)) {
      srlStopTimes <- srlStopTimes %>%
        add_row(trip_id = as.factor(tripID), 
                # station = stations[j, "stations"],  # for testing
                stop_sequence = j,
                arrival_time = as.numeric(departures[i] + stations[j, "backoffset"]),  ## cf outbound, "backoffset"
                departure_time = arrival_time,
                # tmp_time = as_hms(arrival_time),  # for testing
                stop_id = stations[j, "node.id"],
                x = stations[j, "node.x"],
                y = stations[j, "node.y"])
      
    } 
    tripIDcounter <- tripIDcounter + 2 

  }
  
  # build srlRoutes
  srlRoutes <- srlRoutes %>%
    add_row(route_id = as.factor(ROUTEIDs[2]),  # cf outbound, '1'
            service_type = as.factor(SERVICETYPE))
  

  # remove stop times after midnight (because times > 24:00:00 are not valid)
  # ---------------------------------------------------------------------------
  srlStopTimes <- srlStopTimes %>%
    filter(arrival_time <= 86400)  # 86400 is 24:00:00
  
  
  # arrange srlTrips
  # ---------------------------------------------------------------------------
  srlTrips <- srlTrips %>%
    arrange(trip_id)
  
  
  # write exports to file
  # ---------------------------------------------------------------------------
  st_write(srlStops, paste0(outputLocation,"srlStops.sqlite"), delete_layer=TRUE)
  saveRDS(srlStopTimes, file = paste0(outputLocation, "srlStopTimes.rds"))
  saveRDS(srlTrips, file = paste0(outputLocation, "srlTrips.rds"))
  saveRDS(srlRoutes, file = paste0(outputLocation, "srlRoutes.rds"))
  
}
