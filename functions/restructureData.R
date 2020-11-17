# networkAttributed=networkDirect
restructureData <- function(networkDirect, networkDirect_bikepath){
  nodes <- networkDirect[[1]]
  links <- networkDirect[[2]]
  nodes_bp <- networkDirect_bikepath[[1]]
  links_bp <- networkDirect_bikepath[[2]]
  
  # Merging bikepaths and total networks
  nodes <- bind_rows(nodes, nodes_bp) %>% 
    distinct(.keep_all = T)
  links <- links %>% 
    bind_rows(links_bp)
  
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
  # seperated_lane     = 3
  # lane               = 2
  # shared_lane        = 1
  # no_lane/no_cycling = 0

  links <- links %>%  
    st_drop_geometry() %>%
    # sf::st_coordinates() %>%
    # as.data.frame() %>%
    # cbind(name=c("from","to")) %>%
    # tidyr::pivot_wider(names_from = name, values_from = c(X,Y)) %>% 
    # cbind(st_drop_geometry(links)) %>%
    # add in mode
    mutate(modes=ifelse(                is_car==1,                          "car",    NA)) %>%
    mutate(modes=ifelse(!is.na(modes)&is_cycle==1, paste(modes,"bicycle",sep=","), modes)) %>%
    mutate(modes=ifelse( is.na(modes)&is_cycle==1,                      "bicycle", modes)) %>%
    mutate(modes=ifelse( !is.na(modes)&is_walk==1,    paste(modes,"walk",sep=","), modes)) %>%
    mutate(modes=ifelse(  is.na(modes)&is_walk==1,                         "walk", modes)) %>%
    # convert cycleway from numbers to text
    mutate(cycleway=ifelse(cycleway==4, "bikepath"      , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==3, "seperated_lane", cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==2, "lane"          , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==1, "shared_lane"   , cycleway)) %>%
    mutate(cycleway=ifelse(cycleway==0, NA              , cycleway)) %>%
    dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
                  permlanes, capacity, highway, is_oneway, cycleway, is_cycle, is_walk,
                  is_car, modes)
  
  return(list(nodes,links))
}
