osmMetaCorrection <- function(osmAttributes){
  
  # Add to the list below to add simple bike lane
  setBikeLane <- c(23024198, 26901968, 30031517, 30031522, 65086227,
                   65086229, 83885918, 106599114, 172410574, 209335987,
                   209335988, 215504485, 217499573, 294269421, 747471017,
                   787193574, 787193575, 794789649, 794789650) # Chapel St
  
  # Add to the list below to add separated bike lane
  setSepBikeLane <- c()
  
  removeCycleway <- c(45020428, 45020444,  45020461, 45020467, 45020504, 45020505,  45020469,  45020487,  106603846,  106603850, #alma park 
                      62353346,  62353439,  62353440,  62353441,
                      18523511)
  
  # These should be two lane
  setOneLane <- c()
  setTwoLane <- c()
  setThreeLane <- c(13301454)
  setFourLane <- c(10592122)
  
  # OSM ids that modes must be removed from them
  removeCar <- c(35600976,35600978, 58199845)
  removeBike <- c(58199845,
                  45020428, 45020444,  45020461, 45020467, 45020504, 45020505,  45020469,  45020487,  106603846,  106603850, #alma park 
                  62353346,  62353439,  62353440,  62353441,
                  18523511) 
  removeWalk <- c()
  addCar <- c()
  addBike <- c()
  addWalk <- c()
  
  # Add to the mutate/replace code below if there are other fields to correct
  osmAttributesCorrected <- osmAttributes %>% 
    # modifying bicycle infrastructure
    mutate(cycleway  = ifelse(osm_id%in%setBikeLane   , 2 , cycleway)) %>% 
    mutate(cycleway  = ifelse(osm_id%in%setSepBikeLane, 3 , cycleway)) %>%
    mutate(cycleway  = ifelse(osm_id%in%removeCycleway, 0 , cycleway)) %>% 
    mutate(highway = ifelse(osm_id%in%removeCycleway, "footway", highway)) %>%
    mutate(highway_order = ifelse(osm_id%in%removeCycleway, 19, highway_order)) %>%
    # modifying number of lanes
    mutate(permlanes = ifelse(osm_id%in%setOneLane    , 1 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setTwoLane    , 2 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setThreeLane  , 3 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setFourLane   , 4 , permlanes)) %>%
    # modifying permitted modes
    mutate(is_car    = ifelse(osm_id%in%removeCar     , 0 , is_car)) %>%  
    mutate(is_cycle  = ifelse(osm_id%in%removeBike    , 0 , is_cycle)) %>%  
    mutate(is_walk   = ifelse(osm_id%in%removeWalk    , 0 , is_walk)) %>%  
    mutate(is_car    = ifelse(osm_id%in%addCar        , 1 , is_car)) %>%  
    mutate(is_cycle  = ifelse(osm_id%in%addBike       , 1 , is_cycle)) %>%  
    mutate(is_walk   = ifelse(osm_id%in%addWalk       , 1 , is_walk)) 
  
  return(osmAttributesCorrected)
}
  
osmNetworkCorrection <- function(networkInput){
  nodes <- networkInput[[1]] 
  edges <- networkInput[[2]] 
  
  nodesCoordinated <- nodes %>% 
    st_drop_geometry() %>% 
    cbind(st_coordinates(nodes)) %>% 
    dplyr::select(id,X,Y)
  
  # Add the osm_id, from and to ids for new links to be added
  addLinks <- tibble(osm_id=8066126 , from_id=as.integer(151368), to_id=as.integer(140998))
  
  addLinksCoordinated <- addLinks %>% 
    left_join(nodesCoordinated,by=c("from_id"="id")) %>%
    rename(fromX=X,fromY=Y) %>%
    left_join(nodesCoordinated,by=c("to_id"="id")) %>%
    rename(toX=X,toY=Y) %>%
    mutate(geom=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
    st_as_sf(wkt = "geom", crs = 28355) %>% 
    mutate(length=as.numeric(st_length(geom))) %>% 
    dplyr::select(osm_id, length, from_id, to_id, geom)
  
  # add the osm IDs of the links to be removed here.
  removeLinks <- c(252833218,252833216,252833215)
  
  edgesCorrected <- edges %>% 
    rbind(addLinksCoordinated) %>% 
    filter(!osm_id%in%removeLinks)
  
  return(edgesCorrected)
}
