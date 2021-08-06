addElevation2Nodes <- function(nodes, rasterFile, multiplier=1){
  elevation <- raster(rasterFile) 
  nodes$z <- round(raster::extract(elevation ,as(nodes, "Spatial"),method='bilinear'))/multiplier
  return(nodes)
}

addElevation2Links <- function(network){
  network <- networkRestructured
  nodes <- network[[1]]
  links <- network[[2]]
  
  if("z"%in%colnames(nodes)){
    # Changing all is_oneway=0 to 2x is_oneway=1 with reverse directions
    # Calculating forward slope pct
    links <- links %>% 
      left_join(st_drop_geometry(nodes[,c("id","z")]), by=c("to_id"="id")) %>% 
      rename("to_z"="z") %>% 
      left_join(st_drop_geometry(nodes[,c("id","z")]), by=c("from_id"="id")) %>% 
      rename("from_z"="z") %>%
      mutate(fwd_slope_pct=100*(to_z-from_z)/length) %>% 
      mutate(rvs_slope_pct=ifelse(is_oneway==0, yes = -fwd_slope_pct , no = NA)) %>% 
      dplyr::select(-to_z, -from_z) 
  # Calculating reverse slope pct
    
  }else warning("z var was not found in nodes df, skipping adding slope to links")
  
  return(links)
}