osmMetaCorrection <- function(osmAttributes){
  
  # Add to the list below to add simple bike lane
  setBikeLane <- c(23024198, 26901968, 30031517, 30031522, 65086227,
                   65086229, 83885918, 106599114, 172410574, 209335987,
                   209335988, 215504485, 217499573, 294269421, 747471017,
                   787193574, 787193575, 794789649, 794789650) # Chapel St
  
  # Add to the list below to add separated bike lane
  setSepBikeLane <- c()
  
  # These should be two lane
  setOneLane <- c()
  setTwoLane <- c()
  setThreeLane <- c(13301454)
  setFourLane <- c(10592122)
  
  # OSM ids that modes must be removed from them
  removeCar <- c(35600976,35600978)
  removeBike <- c()
  removeWalk <- c()
  addCar <- c()
  addBike <- c()
  addWalk <- c()
  
  # Add to the mutate/replace code below if there are other fields to correct
  osmAttributesCorrected <- osmAttributes %>% 
    mutate(cycleway  = ifelse(osm_id%in%setBikeLane   , 2 , cycleway)) %>% 
    mutate(cycleway  = ifelse(osm_id%in%setSepBikeLane, 3 , cycleway)) %>% 
    mutate(permlanes = ifelse(osm_id%in%setOneLane    , 1 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setTwoLane    , 2 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setThreeLane  , 3 , permlanes)) %>%  
    mutate(permlanes = ifelse(osm_id%in%setFourLane   , 4 , permlanes)) %>%  
    mutate(is_car    = ifelse(osm_id%in%removeCar     , 0 , is_car)) %>%  
    mutate(is_cycle  = ifelse(osm_id%in%removeBike    , 0 , is_cycle)) %>%  
    mutate(is_walk   = ifelse(osm_id%in%removeWalk    , 0 , is_walk)) %>%  
    mutate(is_car    = ifelse(osm_id%in%addCar        , 1 , is_car)) %>%  
    mutate(is_cycle  = ifelse(osm_id%in%addBike       , 1 , is_cycle)) %>%  
    mutate(is_walk   = ifelse(osm_id%in%addWalk       , 1 , is_walk)) 
  
  return(osmAttributesCorrected)
}
  