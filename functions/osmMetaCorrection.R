osmMetaCorrection <- function(osmAttributes){
  
  # Add to the list below to add simple bike lane
  bikeLaneMissing <- c(23024198, 26901968, 30031517, 30031522, 65086227,
                       65086229, 83885918, 106599114, 172410574, 209335987,
                       209335988, 215504485, 217499573, 294269421, 747471017,
                       787193574, 787193575, 794789649, 794789650) # Chapel St
  
  # Add to the list below to add separated bike lane
  sepBikeLaneMissing <- c()
  
  # Add to the mutate/replace code below if there are other fields to correct
  osmAttributesCorrected <- osmAttributes %>% 
    mutate(cycleway=ifelse(osm_id%in%bikeLaneMissing, yes = 2,cycleway)) %>% 
    mutate(cycleway=ifelse(osm_id%in%sepBikeLaneMissing, yes = 3,cycleway))
  
  return(osmAttributesCorrected)
}
  