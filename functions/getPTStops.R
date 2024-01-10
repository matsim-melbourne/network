# read in PT stops from GTFS

# requires tidytransit (loaded in NetworkGenerator.R)

getPTStops <- function(city, gtfs_feed, outputCrs, edges_current) {
  # city = "Melbourne"
  # gtfs_feed = "./data/gtfs.zip"
  # outputCrs = 28355
  # edges_current = networkDensified[[2]]
  
  # read in GTFS feed
  gtfs <- read_gtfs(gtfs_feed) %>%
    gtfs_as_sf(., crs = 4326)
  
  # extract stops with their locations
  stops <- gtfs$stops %>%
    st_transform(outputCrs)
  
  # limit to stops within the study area (convex hull of edges)
  stops <- stops %>%
    st_filter(., st_convex_hull(st_union(edges_current)), 
              predicate = st_intersects)
  
  # table of stops and route types
  stops.routetypes <- gtfs$stop_times %>%
    left_join(gtfs$trips, by = "trip_id") %>%
    left_join(gtfs$routes, by = "route_id") %>%
    
    # keep only distinct stop_id and route_type combinations
    dplyr::select(stop_id, route_type) %>%
    distinct()
  
  # apply route types
  route_types = stops.routetypes$route_type %>% unique() %>% sort()
  
  if (city == "ProvisionForMunich") { # test should be city is Munich AND stops are in the expected list
    
  
  } else if (!all(route_types %in% c("0", "1", "2", "3", "4", "5", "6", "7", "11", "12"))) {
    message("GTFS Feed contains the following route type codes: ", paste(route_types, collapse = ", "), ". Unable to process these using 
the standard route codes from https://developers.google.com/transit/gtfs/reference, which are:
   0-tram, 1-metro, 2-train, 3-bus, 4-ferry, 5-cable tram, 6-cable car, 7-funicular, 11-trolleybus, 12 monorail. 
Edit getPTStops.R to specify the meanings of the codes used in the GTFS Feed.
PT stops will not be included in destinations.")
    stops.found = FALSE

  } else {
    message("GTFS Feed contains the following route type codes: ", paste(route_types, collapse = ", "), ".
Using standard route_type codes from https://developers.google.com/transit/gtfs/reference:
   0-tram, 1-metro, 2-train, 3-bus, 4-ferry, 5-cable tram, 6-cable car, 7-funicular, 11-trolleybus, 12 monorail. 
Check that these match the codes used in your GTFS feed.")
    stops.routetypes.coded <- stops.routetypes %>%
      mutate(pt_stop_type = case_when(
        route_type == 0  ~ "tram",
        route_type == 1  ~ "metro",
        route_type == 2  ~ "train",
        route_type == 3  ~ "bus",
        route_type == 4  ~ "ferry",
        route_type == 5  ~ "cable tram",
        route_type == 6  ~ "cable car",
        route_type == 7  ~ "funicular",
        route_type == 11 ~ "trolleybus",
        route_type == 12 ~ "monorail"
      )) %>%
      dplyr::select(stop_id, pt_stop_type)
    
    stops.found = TRUE
    
  }
  
  if(stops.found) {
    stops.with.types <- stops %>%
      left_join(stops.routetypes.coded, by = "stop_id")
    
    return(stops.with.types)
    
  } else {
    return(c())  # empty vector if no stops can be returned
  }
  

  
}