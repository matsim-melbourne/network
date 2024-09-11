# function to retrieve OSM extract for given region

getOsmExtract <- function(region, 
                          outputCrs, 
                          regionBufferDist = 10000, 
                          osmGpkg,
                          retainDownload,
                          useFullExtractHeld = F,
                          fullExtractLocation = NA) {
  
  # region = "./data/greater_bendigo.sqlite"
  # outputCrs = 7899
  # regionBufferDist = 10000  # 10km
  # osmGpkg = "./output/temp_bendigo_osm.gpkg"
  # useFullExtractHeld = F
  # fullExtractLocation = "./data/geofabrik_australia-latest.osm"
  
  # load region and buffer by selected distance (eg 10km)
  region.poly <- st_read(region)
  if (st_crs(region.poly)$epsg != outputCrs) {
    region.poly <- st_transform(region.poly, outputCrs)
  }
  region.buffer <- st_buffer(region.poly, regionBufferDist) %>%
    st_snap_to_grid(1)
 
  # increase timeout to allow time for large Australia extract to download
  default.timeout <- getOption("timeout")
  options(timeout = 7200)
  
  # load full extract, if already held (after checking that a location is specified);
  # or else download full extract (can be quite slow for whole of Australia)
  if (useFullExtractHeld) {
    if (is.na(fullExtractLocation)) {
      echo(paste("No location for existing OSM extract has been specified; downloading extract"))
      useFullExtractHeld <- F
    } else if (!file.exists(fullExtractLocation)) {
      echo(paste0("OSM extract not found at the specified location '", fullExtractLocation, 
                  "'; downloading extract instead\n"))
      useFullExtractHeld <- F
    } else if (!str_ends(fullExtractLocation, ".osm.pbf")) {
      echo(paste0("File at the specified location '", fullExtractLocation, 
                  "' is not in expected format; downloading extract instead\n"))
      useFullExtractHeld <- F
    }
  }
  if (useFullExtractHeld) {
    full.extract <- fullExtractLocation
  } else {
    download.url <- oe_match(region.buffer, crs = outputCrs)$url
    echo(paste("Downloading OSM extract from", download.url, "\n"))
    full.extract <- oe_download(download.url, download_directory = "./data")
  }
  
  # convert to gpkg, including all layers ('boundary' will clip to bounding box)
  echo(paste("Converting downloaded OSM extract to .gpkg for selected region\n"))
  layers <- st_layers(full.extract)$name
  for(i in 1:length(layers)) {
    layer <- layers[i]
    region.gpkg <- 
      oe_vectortranslate(full.extract, 
                         layer = layer,
                         vectortranslate_options = c("-t_srs",
                                                     paste0("EPSG:", outputCrs)),
                         boundary = region.buffer,
                         boundary_type = "spat")  # 'spat' should intersect rather than clip, but still seems to clip
  }
  
  # intersect with region buffer, eliminating errors and save to permanent location
  for (i in 1:length(st_layers(region.gpkg)$name)) {
    current.layer.name <- st_layers(region.gpkg)$name[i]
    current.layer <- st_read(region.gpkg, layer = current.layer.name) %>%
      filter(st_is_valid(geometry))

    # some features have validity problems that cause intersection issues
    # (and may not be detected by st_is_valid) - find and remove them
    problem.features <- c()
    
    # create current.layer.intersected if possible, or else identify problem features
    tryCatch({
      current.layer.intersected <- current.layer %>%
        st_filter(region.buffer, .predicate = st_intersects)
    }, error = function(e) {
      # if error, loop through the layer by feature and identify problem
      message(paste("Problem features detected in OSM layer", current.layer.name,
                 "; removing problems (may take a while)"))
      for (j in 1:nrow(current.layer)) {
        if (j %% 500 == 0) print(paste("Checked", j, "of", nrow(current.layer), "features"))
        tryCatch({
          current.feature.intersected <- current.layer[j,] %>%
            st_filter(region.buffer, .predicate = st_intersects)
        }, error = function(e) {
          problem.features <<- c(problem.features, j)  # <<- modifies variable in parent environment
        })
      }
    })
    
    # if problem features found, remove from current layer and create current.layer.intersected
    if (length(problem.features) > 0) {
      current.layer <- current.layer[-problem.features,]
      current.layer.intersected <- current.layer %>%
        st_filter(region.buffer, .predicate = st_intersects)
    }
    
    st_write(current.layer.intersected,
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
  

