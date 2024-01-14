# function to retrieve OSM extract for given region

getOsmExtract <- function(region, 
                          outputCrs, 
                          regionBufferDist = 10000, 
                          osmGpkg) {
  
  # region = "../data/processed/greater_bendigo.sqlite"
  # outputCrs = 7899
  # regionBufferDist = 10000  # 10km
  # osmGpkg = "../data/processed/bendigo_osm.gpkg"

  # load region and buffer by selected distance (eg 10km)
  region <- st_read(region)
  region.buffer <- st_buffer(region, regionBufferDist)
 
  # increase timeout to allow time for large Australia extract to download
  default.timeout <- getOption("timeout")
  options(timeout = 1200)
  
  # download the full extract (whole of Australia; quite slow)
  download.url <- oe_match(region, crs = outputCrs)$url
  echo(paste("Downloading OSM extract from", download.url, "\n"))
  full.extract <- oe_download(download.url, download_directory = ".")
  
  # convert to gpkg, including all layers
  echo(paste("Converting downloaded OSM extract to .gpkg for selected region\n"))
  region.gpkg <- 
    oe_vectortranslate(full.extract, 
                       layer = st_layers(full.extract)$name,
                       vectortranslate_options = c("-t_srs",
                                                   paste0("EPSG:", outputCrs)),
                       boundary = region.buffer)

  # save to permanent location
  for (i in 1:length(st_layers(region.gpkg)$name)) {
    current.layer.name <- st_layers(region.gpkg)$name[i]
    st_write(st_read(region.gpkg, layer = current.layer.name),
             osmGpkg, 
             layer = current.layer.name,
             delete_layer = TRUE)
  }
  
  # delete full extract and temporary location of region extract, restore timeout to default
  if (!retainDownload) {
    unlink(full.extract)
  }
  unlink(region.gpkg)
  options(timeout = default.timeout)

}
  

