# networkAttributed=networkDirect
restructureData <- function(networkList, highway_lookup, 
                            defaults_df, adjustCapacity=F){
  
  nodes <- networkList[[1]]
  links <- networkList[[2]]
  
  links <- links %>% 
    mutate(uid=row_number()) 
  # finding merged bikepath ids
  bikepath_uids <- links %>% 
    st_drop_geometry() %>% 
    filter((cycleway == "4" | cycleway == "5") & highway_order < 16) %>% 
    dplyr::select(uid) %>% unlist() %>%  as.double()
  # changing merged bikepaths to regular bikepaths
  bikepaths <- links %>% 
    filter(uid %in% bikepath_uids) %>% 
    mutate(highway_order = 16) %>% 
    mutate(freespeed=defaults_df$freespeed[15]) %>% 
    mutate(laneCapacity=defaults_df$laneCapacity[15]) %>% 
    mutate(is_car=0) %>% 
    mutate(permlanes=1) %>% # bikepaths are assumed sinlge lane
    mutate(is_oneway=0) %>% # bikepaths are assumed bi-directional 
    dplyr::select(-uid)
  # merging changed bikepaths back with rest of the links
  links <- links %>% 
    mutate(cycleway=ifelse(uid %in% bikepath_uids,0,cycleway)) %>% # removing bikepaths from those that had it merged 
    mutate(is_cycle = ifelse(uid %in% bikepath_uids & highway_order %in% c(1, 8), 0, is_cycle)) %>% # removing bikepaths from those that had it merged 
    dplyr::select(-uid) %>% 
    rbind(bikepaths)

  nodes <- nodes %>% # Changing to MATSim expected format
    mutate(x = as.numeric(sf::st_coordinates(.)[,1]),
           y = as.numeric(sf::st_coordinates(.)[,2])) %>% 
    mutate(type=if_else(as.logical(is_roundabout), 
                        true = if_else(as.logical(is_signal), 
                                       true = "signalised_roundabout",
                                       false = "simple_roundabout"), 
                        false = if_else(as.logical(is_signal), 
                                        true = "signalised_intersection",
                                        false = "simple_intersection"))) %>% 
    dplyr::select(id, x, y, type, geom) %>% 
    distinct(id, .keep_all=T)
  
  # Bike hierarchy:
  # bikepath           = 4
  # separated_lane     = 3
  # lane               = 2
  # shared_lane        = 1
  # no_lane/no_cycling = 0
  
  links <- links %>%  
    # st_drop_geometry() %>%
    left_join(highway_lookup, by="highway_order") %>%  # Adding back the highway tags 
    mutate(capacity=laneCapacity*permlanes) %>% # capacity for all lanes
    mutate(modes=ifelse(                is_car==1,                          "car",    NA)) %>%
    mutate(modes=ifelse(!is.na(modes)&is_cycle==1,    paste(modes,"bike",sep=","), modes)) %>%
    mutate(modes=ifelse( is.na(modes)&is_cycle==1,                         "bike", modes)) %>%
    mutate(modes=ifelse( !is.na(modes)&is_walk==1,    paste(modes,"walk",sep=","), modes)) %>%
    mutate(modes=ifelse(  is.na(modes)&is_walk==1,                         "walk", modes)) %>%
    # convert cycleway from numbers to text
    mutate(cycleway=ifelse(cycleway==5, "bikepath"      , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==4, "shared_path"   , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==3, "separated_lane", cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==2, "simple_lane"   , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==1, "shared_street" , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==0, NA              , cycleway)) %>%
    dplyr::select(any_of(c("osm_id","from_id", "to_id", "fromX", "fromY", "toX", "toY",  
                           "length", "freespeed", "permlanes", "capacity",  
                           "highway", "is_oneway", "cycleway", "surface",
                           "is_cycle", "is_walk", "is_car", "modes")), 
                  contains("ndvi")) %>%
    mutate(id=NA) %>%
    relocate(id)

  return(list(nodes,links))
}
