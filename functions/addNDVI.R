# function to add NDVI to links, where NDVI is the average of the NDVI
# values within a 30m buffer of the links

addNDVI2Links <- function(links, ndviFile, ndviBuffDist, outputCrs) {
  
  # links = networkDensified[[2]]
  # ndviFile = "./data/NDVI_Bendigo_2023.tif"
  # ndviBuffDist = 30
  
  echo("Reading in the NDVI file and reprojecting if necessary\n")
  
  # read in NDVI file, and convert to outputCrs if necessary
  ndvi <- rast(ndviFile)
  outputCrsEPSG <- paste0("EPSG:", outputCrs)
  if (!same.crs(ndvi, outputCrsEPSG)) ndvi <- project(ndvi, outputCrsEPSG)
 
  # buffer each link
  links.buffered <- st_buffer(links, 30)
  
  echo(paste0("Finding NDVI values within ", ndviBuffDist, "m of each link\n"))
  
  # extract the NDVI values for the buffered links - produces a table with 
  # 2 columns, ID (which is the row number from links.buffered) and NDVI
  ndvi_values <- terra::extract(ndvi, links.buffered)
  
  echo(paste("Finding mean of NDVI values for each link\n"))
  
  # find the mean of the values for each link
  ndvi_values_mean <- ndvi_values %>%
    group_by(ID) %>%
    summarise(ndvi = mean(NDVI, na.rm = TRUE))
  
  # find the mean AND OTHER VALUES of the values for each link
  ndvi_values_mean <- ndvi_values %>%
    group_by(ID) %>%
    summarise(ndvi = mean(NDVI, na.rm = TRUE),
              ndvi_md = median(NDVI, na.rm = TRUE),
              ndvi_75 = quantile(NDVI, na.rm = TRUE, probs = 0.75),
              ndvi_90 = quantile(NDVI, na.rm = TRUE, probs = 0.9))
  
  # join to the links, using the row number and ID
  links.with.ndvi <- links %>%
    mutate(row_no = row_number()) %>%
    left_join(., ndvi_values_mean, by = c("row_no" = "ID")) %>%
    dplyr::select(-row_no)

  # st_write(links.with.ndvi, "./SP_working/links_with_NDVI.sqlite", delete_layer = TRUE)

  return(links.with.ndvi)
  
}