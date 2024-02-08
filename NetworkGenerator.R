makeNetwork<-function(city, outputSubdirectory = "generated_network"){
  # city = "Bendigo"
  # city = "Melbourne"
  
  # outputSubdirectory = "generated_network"
  
  # Parameters --------------------------------------------------------------
  
  # CITY PARAMETERS
  # City parameters to be set
  # •	region: if 'downloadOsm=T', file delineating the boundary of the area for 
  #   which Osm extract is to be downloaded (assumed to be in sqlite format 
  #   with a single layer)
  # •	outputCrs: desired coordinate system for network
  # •	osmGpkg: location where downloaded OSM extract for region is to be stored
  #   (if 'downloadOsm=T') and/or read from (if 'processOsm=T')
  # •	unconfiguredSqlite: location where processed OSM file is to be stored
  #   (if 'networkFromOsm=T') or read from (if 'networkFromOsm=F')
  # •	cropAreaPoly: if 'crop2TestArea=T' cropArea location from 
  #   https://github.com/JamesChevalier/cities/tree/master/australia/victoria 
  #   (only supported for Victoria at this stage)
  # •	demFile: if 'addElevation=T', digital elevation model raster file (must be 
  #   in same coordinate system as network)
  # •	ndviFile: if 'addNDVI=T', raster file with NDVI values (must be in same
  #   coordinate system as network)
  # •	gtfs_feed: if 'addGtfs=T' or 'addDestinationLayer=T, zip file containing 
  #   GTFS data (and, if 'addGtfs=T', also set start and end dates in GTFS section)

  if (city == "Bendigo") {
    region = "./data/greater_bendigo.sqlite"
    outputCrs = 7899
    osmGpkg = "./output/bendigo_osm.gpkg"
    unconfiguredSqlite = "./output/bendigo_network_unconfigured.sqlite"
    cropAreaPoly = ""  # must set 'crop2Area=F'
    demFile = "./data/dem_bendigo.tif" 
    # ndviFile = ""  # must set 'addNDVI=F'
    gtfs_feed = "./data/gtfs.zip"
    
  } else if (city == "Melbourne") {
    region = "./data/greater_melbourne.sqlite"
    outputCrs = 7899
    osmGpkg = "./output/melbourne_osm.gpkg"
    unconfiguredSqlite = "./output/melbourne_network_unconfigured.sqlite"
    cropAreaPoly = "city-of-melbourne_victoria"
    demFile = "./data/dem_melbourne.tif"
    # ndviFile = "./data/NDVI_1600mBuffer_Melbourne_reprojected.tif"
    gtfs_feed = "./data/gtfs.zip"

  } else {
    echo(paste("City parameters for", city, "have not been set; unable to proceed\n"))
    return()
  }

  # DOWNLOAD OSM EXTRACT
  # A flag for whether to download osm extract for the region (if not, and if
  # network needs to be processed, then must already have osmGpkg file)
  downloadOsm=T
  regionBufferDist=10000  # Distance to buffer region when getting osm extract
  retainDownload=F  # Whether to retain downloaded file after region extracted
  
  # NETWORK FROM OSM 
  # A flag for whether to build unconfigured network from osm extract (if not,
  # must already have unconfigured sqlite)
  networkFromOsm=T
  saveUnconfigured=T

  # SIMPLIFICATION
  shortLinkLength=20
  minDangleLinkLengh=500
  crop2Area=F

  # DENSIFICATION
  densificationMaxLength=500
  densifyBikeways=T

  # CAPACITY ADJUSTMENT
  # A flag for whether to multiply capacity of links shorter than 100m by 2 or not
  # In some cases such as when building network for simulation of small samples (e.g. <1%) it might be desired
  adjustCapacity=F

  # ELEVATION
  # A flag for whether to add elevation or not
  addElevation=T
  ElevationMultiplier=1
  
  # DESTINATIONS
  # A flag for whether to add a destinations layer (drawn from OSM, and GTFS for PT) or not
  addDestinationLayer=T

  # NDVI
  # A flag for whether to add NDVI or not
  addNDVI=F
  # Buffer distance for finding average NDVI for links
  ndviBuffDist=30

  # GTFS
  addGtfs=F
  analysis_start = as.Date("2019-10-11","%Y-%m-%d") # Transit Feed start date
  analysis_end = as.Date("2019-10-17","%Y-%m-%d") # Transit Feed end date

  # Outputs
  # outputSubdirectory=format(Sys.time(),"%d%b%y_%H%M") # date_hour, eg. "17Aug21_1308"
  if(exists("outputSubdirectory")){
    outputSubdirectory=outputSubdirectory
  } else {outputSubdirectory="generated_network"}
  writeXml=F
  writeShp=F
  writeSqlite=T

  # Packages ----------------------------------------------------------------

  library(sf)
  library(fs)
  library(tidyverse)
  library(data.table)
  library(igraph)
  library(raster)
  library(terra)
  library(lwgeom)
  library(tidytransit)
  library(hablar)
  library(hms)
  library(osmextract)
  library(doSNOW)
  library(parallel)
  library(foreach)
  library(nngeo)
  

  # Building the output folder structure ------------------------------------
  outputDir <- paste0("output/",outputSubdirectory)
  if(outputSubdirectory != "" & dir.exists(outputDir)) dir_delete(outputDir)
  dir_create(paste0('./',outputDir))
  sink(paste0('./',outputDir,'/makeMatsimNetwork.log'), append=FALSE, split=TRUE)
  if (addGtfs) dir_create(paste0(outputDir,"/gtfs"))

  #  Functions --------------------------------------------------------------

  dir_walk(path="./functions/",source, recurse=T, type = "file")

  # Network processing-------------------------------------------------------
  echo("========================================================\n")
  echo("                **Network Generation Setting**          \n")
  echo("--------------------------------------------------------\n")
  echo(paste0("- Downloading OSM extract:                        ", downloadOsm,"\n"))
  echo(paste0("- Processing the OSM extract:                     ", networkFromOsm,"\n"))
  echo(paste0("- Cropping to a test area:                        ", crop2Area,"\n"))
  echo(paste0("- Shortest link length in network simplification: ", shortLinkLength,"\n"))
  echo(paste0("- Adding elevation:                               ", addElevation,"\n"))
  echo(paste0("- Adding destination layer:                       ", addDestinationLayer,"\n"))
  echo(paste0("- Adding NDVI:                                    ", addNDVI,"\n"))
  echo(paste0("- Adding PT from GTFS:                            ", addGtfs,"\n"))
  echo(paste0("- Writing outputs in SQLite format:               ", writeSqlite,"\n"))
  echo(paste0("- Writing outputs in ShapeFile format:            ", writeShp,"\n"))
  echo(paste0("- Writing outputs in MATSim XML format:           ", writeXml,"\n"))
  echo("========================================================\n")
  echo("                **Launching Network Generation**        \n")
  echo("--------------------------------------------------------\n")
  
  # Downloading OSM
  if (downloadOsm) {
    echo(paste0("Downloading OSM extract for ", city, "\n"))
    getOsmExtract(region, outputCrs, regionBufferDist, osmGpkg, retainDownload)
  }
  
  # Processing OSM, or loading existing layers if not required
  if(networkFromOsm) {
    echo(paste0("Starting to process osm extract file, ", osmGpkg, "\n"))
    networkUnconfiguredOutputs <- processOsm(osmGpkg, outputCrs)
    
    if (saveUnconfigured) {
      if (file_exists(unconfiguredSqlite)) st_delete(unconfiguredSqlite)
      st_write(networkUnconfiguredOutputs[[1]], unconfiguredSqlite, layer = "nodes")
      st_write(networkUnconfiguredOutputs[[2]], unconfiguredSqlite, layer = "edges")
      st_write(networkUnconfiguredOutputs[[3]], unconfiguredSqlite, layer = "osm_metadata")
    }
    
    networkUnconfigured <- list(networkUnconfiguredOutputs[[1]],
                                networkUnconfiguredOutputs[[2]])
    osm_metadata <- networkUnconfiguredOutputs[[3]]

  } else {
    
    if (file_exists(unconfiguredSqlite)) {
      echo(paste("Reading in existing unconfigured network,", unconfiguredSqlite, "\n"))
      networkUnconfigured <- 
        list(st_read(unconfiguredSqlite, layer = "nodes") %>% st_set_geometry("geom"),
             st_read(unconfiguredSqlite, layer = "edges") %>% st_set_geometry("geom"))
      osm_metadata <- st_read(unconfiguredSqlite, layer = "osm_metadata") %>%
        filter(osm_id %in% networkUnconfigured[[2]]$osm_id)
      
    } else {
      echo(paste("Unconfigured network file", unconfiguredSqlite, "not found; unable to proceed\n"))
      return()
    }
  }
  
  # crop to test area if required
  if(crop2Area)system.time(networkUnconfigured <- crop2Poly(networkUnconfigured,
                                                            cropAreaPoly,
                                                            outputCrs))
  # process OSM metadata
  echo("processing OSM meta data\n")
  echo("Building default OSM attribute tables\n")
  defaults_df <- buildDefaultsDF()
  highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
  echo("Processing OSM tags and joining with defaults\n")
  system.time( osmAttributes <- processOsmTags(osm_metadata,defaults_df))
  
  edgesAttributed <- networkUnconfigured[[2]] %>%
    inner_join(osmAttributes, by="osm_id") %>%
    dplyr::select(-highway, highway_order)
  
  # keep only the largest connected component
  largestComponent <- largestConnectedComponent(networkUnconfigured[[1]], edgesAttributed)
  
  # simplify intersections while preserving attributes and original geometry.
  system.time(intersectionsSimplified <- simplifyIntersections(largestComponent[[1]],
                                                               largestComponent[[2]],
                                                               shortLinkLength,
                                                               outputCrs))
  
  # Merge edges going between the same two nodes, picking the shortest geometry.
  # * One-way edges going in the same direction will be merged
  # * Pairs of one-way edges in opposite directions will be merged into a two-way edge.
  # * Two-way edges will be merged regardless of direction.
  # * One-way edges will NOT be merged with two-way edges.
  # * Non-car edges do NOT count towards the merged lane count (permlanes)
  system.time(edgesCombined <- combineRedundantEdges(intersectionsSimplified[[1]],
                                                     intersectionsSimplified[[2]],
                                                     outputCrs))
  
  # Merge one-way and two-way edges going between the same two nodes. In these 
  # cases, the merged attributes will be two-way.
  # This guarantees that there will only be a single edge between any two nodes.
  system.time(combinedUndirectedAndDirected <- 
                combineUndirectedAndDirectedEdges(edgesCombined[[1]],
                                                  edgesCombined[[2]],
                                                  outputCrs))
  
  # If there is a chain of edges between intersections, merge them together
  system.time(edgesSimplified <- simplifyLines(combinedUndirectedAndDirected[[1]],
                                               combinedUndirectedAndDirected[[2]]))
  
  # Remove dangles
  system.time(noDangles <- removeDangles(edgesSimplified[[1]],edgesSimplified[[2]],
                                         minDangleLinkLengh))
  
  # Do a second round of simplification.
  system.time(edgesCombined2 <- combineRedundantEdges(noDangles[[1]],
                                                      noDangles[[2]],
                                                      outputCrs))
  system.time(combinedUndirectedAndDirected2 <- 
                combineUndirectedAndDirectedEdges(edgesCombined2[[1]],
                                                  edgesCombined2[[2]],
                                                  outputCrs))
  
  system.time(edgesSimplified2 <- simplifyLines(combinedUndirectedAndDirected2[[1]],
                                                combinedUndirectedAndDirected2[[2]]))
  system.time(edgesCombined3 <- combineRedundantEdges(edgesSimplified2[[1]],
                                                      edgesSimplified2[[2]],
                                                      outputCrs))
  
  networkMode <- addMode(edgesCombined3)
  
  # ensure transport is a directed routeable graph for each mode (i.e., connected
  # subgraph). The first function ensures a connected directed subgraph and the
  # second function ensures a connected subgraph but doesn't consider directionality.
  # We car and bike modes are directed, but walk is undirected.
  networkNonDisconnected <- largestDirectedNetworkSubgraph(networkMode,'car,bike')
  networkConnected <- largestNetworkSubgraph(networkNonDisconnected,'walk')
  
  # densify the network so that no residential streets are longer than 500m
  if (addElevation==T & densifyBikeways==F) message("Consider changing densifyBikeways to true when addElevation is true to get a more accurate slope esimation for bikeways")
  networkDensified <- densifyNetwork(networkConnected,densificationMaxLength,
                                     densifyBikeways)
  
  # Adding NDVI to links
  if(addNDVI) {
    system.time(networkDensified[[2]] <- addNDVI2Links(networkDensified[[2]],
                                                       ndviFile,
                                                       ndviBuffDist))
  }
  
  # adding destinations layer
  if (addDestinationLayer) {
    destinations <- addDestinations(networkDensified[[1]],
                                    networkDensified[[2]],
                                    osmGpkg,
                                    city,
                                    gtfs_feed,
                                    outputCrs,
                                    region,
                                    regionBufferDist)
  }

  # simplify geometry so all edges are straight lines
  system.time(networkDirect <-
                makeEdgesDirect(networkDensified[[1]],
                                networkDensified[[2]],
                                outputCrs))
  
  # add mode to edges, add type to nodes, change cycleway from numbers to text
  networkRestructured <- restructureData(networkDirect, highway_lookup,
                                         defaults_df)
  
  # Doubling capacity for small road segments to avoid bottlenecks
  # Set adjustCapacity to True if this adjustment is desired
  if(adjustCapacity) {
    networkRestructured[[2]] <- networkRestructured[[2]] %>% 
      mutate(capacity = ifelse(length<100 , capacity*2, capacity))
  }
  
  # Adding elevation to nodes and gradient to links
  if(addElevation){ 
    networkRestructured[[1]] <- addElevation2Nodes(networkRestructured[[1]], 
                                                   demFile,
                                                   ElevationMultiplier)
    networkRestructured[[2]] <- addElevation2Links(networkRestructured)
  }
  
  # Adding PT pseudo-network based on GTFS
  # Adjust your analysis start date, end data and gtfs feed name above
  if(addGtfs) {
    # Adjust these parameters based on your GTFS file
    if(file.exists("data/studyRegion.sqlite")){
      # read in the study region boundary 
      echo("Using Study Region file for GTFS processing")
      studyRegion <- st_read("data/studyRegion.sqlite",quiet=T) %>%
        st_buffer(10000) %>%
        st_snap_to_grid(1)
    }else{
      echo("Study Region file was not found, skipping")
      studyRegion = NA
    }
    system.time(
      networkRestructured[[2]] <- addGtfsLinks(outputLocation=paste0(outputDir,"/gtfs/"),
                                               nodes=networkRestructured[[1]], 
                                               links=networkRestructured[[2]],
                                               gtfs_feed=gtfs_feed,
                                               analysis_start= analysis_start,
                                               analysis_end=analysis_end,
                                               studyRegion=studyRegion,
                                               outputCrs=outputCrs)) 
  }
  
  # Make network oneway (required because cycling impedances such as level of 
  # traffic stress and slope may be different in each direction)
  echo("Making all links one way\n")
  networkOneway <- makeEdgesOneway(networkRestructured[[1]], 
                                   networkRestructured[[2]])
  
  networkFinal <- networkOneway
  
  if (addDestinationLayer) {
    networkFinal[[3]] <- destinations
  }
  
  # writing outputs
  echo("========================================================\n")
  echo("|               **Launching Output Writing**           |\n")
  echo("--------------------------------------------------------\n")
  
  if(writeSqlite) system.time(exportSQlite(networkFinal, outputDir, outputCrs))
  if(writeShp) system.time(exportShp(networkFinal, outputDir, outputCrs))
  if(writeXml) system.time(exportXML(networkFinal, outputDir)) 
  
  # end logging
  sink()
}

## JUST FOR TESTING
makeNetwork(city = "Bendigo")
makeNetwork(city = "Melbourne")
