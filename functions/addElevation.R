addElevation2Nodes <- function(nodes, rasterFile, elevationMultiplier=10){
  elevation <- raster(rasterFile) 
  nodes$z <- round(raster::extract(elevation ,as(nodes, "Spatial"),method='bilinear'))/elevationMultiplier
  
  return(nodes)
}
