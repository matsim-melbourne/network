# function to add NDVI to links, where NDVI is the average of the NDVI
# values within a 30m buffer of the links

addNDVI2Links <- function(links, ndviFile, ndviBuffDist) {
  
  # links = networkRestructured[[2]]
  # ndviFile = "./data/NDVI_1600mBuffer_Melbourne_reprojected.tif"
  # ndviBuffDist = 30
  
  echo("Reading in the NDVI file\n")
  
  # read in NDVI file
  ndvi <- rast(ndviFile)
  
  # buffer each link
  links.buffered <- st_buffer(links, 30)
  
  echo(paste0("Finding NDVI values within ", ndviBuffDist, "m of each link\n"))
  
  # extract the NDVI values for the buffered links - produces a table with 
  # 2 columns, ID (which is the row number from links.buffered) and NDVI
  ndvi_values <- terra::extract(ndvi, links.buffered)
  
  echo(paste("Finding mean pf NDVI values for each link\n"))
  
  # find the mean of the values for each link
  ndvi_values_mean <- ndvi_values %>%
    group_by(ID) %>%
    summarise(ndvi = mean(NDVI, na.rm = TRUE))
  
  # join to the links, using the row number and ID
  links.with.ndvi <- links %>%
    mutate(row_no = row_number()) %>%
    left_join(., ndvi_values_mean, by = c("row_no" = "ID")) %>%
    dplyr::select(-row_no)

  return(links.with.ndvi)
  
}