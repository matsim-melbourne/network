makeNetwork<-function(outputFileName="test"){
  # outputFileName="network"
  # Parameters --------------------------------------------------------------
  
  # INPUT NETWORK 
  # Set this to your desired cooridinate system for the network
  outputCrs=28355
  # A flag for whether process raw osm extract or not
  processOsm=F
  # If processOsm=T, Set this to your osm extract file name, e.g., melbourne.osm
  # Note that osm.pbf format is not yet supported
  osmExtract='./data/melbourne.osm'
  # If procesOsm=F, set the following to the network sqlite file
  networkSqlite="data/network.sqlite"

  # SIMPLIFICATION
  shortLinkLength=20
  minDangleLinkLengh=500
  crop2Area=F
  # If crop2TestArea=T, find your area from https://github.com/JamesChevalier/cities/tree/master/australia/victoria and set the following to its poly name
  cropAreaPoly="city-of-melbourne_victoria"

  # DENSIFICATION
  desnificationMaxLengh=500
  densifyBikeways=F

  # CORRECTION
  # To add/remove specified links - see osmCorrection.R
  # Change to TRUE if running on Greater Melbourne OSM, Otherwise, keep FALSE
  # Also you can use the same function to correct networks for your region if needed
  correctNetwork=F
  # A flag for whether to multiply capacity of links shorter than 100m by 2 or not
  # In some cases such as when building network for simulation of small samples (e.g. <1%) it might be desired
  adjustCapacity=F

  # ELEVATION
  # A flag for whether to add elevation or not
  addElevation=F
  # Digital elevation model file - make sure it is in the same coordinate system as your network
  demFile= 'data/DEMx10EPSG28355.tif'
  # DEM's multiplier- set to 1 if DEM contains actual elevation
  ElevationMultiplier=10

  # GTFS
  addGtfs=F
  gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip" # link to the GTFS .zip file
  analysis_start = as.Date("2019-10-11","%Y-%m-%d") # Transit Feed start date
  analysis_end = as.Date("2019-10-17","%Y-%m-%d") # Transit Feed end date

  # Outputs
  # outputFileName=format(Sys.time(),"%d%b%y_%H%M") # date_hour, eg. "17Aug21_1308"
  if(exists("outputFileName")){
    outputFileName=outputFileName
  }else{outputFileName="test"}
  writeXml=F
  writeShp=F
  writeSqlite=T

  # Packages ----------------------------------------------------------------

 library(sf)
 library(fs)
 library(dplyr)
 library(data.table)
 library(stringr)
 library(igraph)
 library(raster)
 library(rgdal)
 library(purrr)
 library(lwgeom)
 library(tidytransit)
 library(hablar)
 library(hms)

  # Building the output folder structure ------------------------------------

  outputDir <- paste0("output/",outputFileName)
  if(dir.exists(outputDir)) dir_delete(outputDir)
  dir_create(paste0('./',outputDir))
  sink(paste0('./',outputDir,'/makeMatsimNetwork.log'), append=FALSE, split=TRUE)
  if (addGtfs) dir_create(paste0(outputDir,"/gtfs"))

  #  Functions --------------------------------------------------------------

  dir_walk(path="./functions/",source, recurse=T, type = "file")

  # Network processing-------------------------------------------------------
  echo("========================================================\n")
  echo("                **Network Generation Setting**          \n")
  echo("--------------------------------------------------------\n")
  echo(paste0("- Starting from OSM extract:                      ", processOsm,"\n"))
  echo(paste0("- Cropping to a test area:                        ", crop2Area,"\n"))
  echo(paste0("- Shortest link length in network simplification: ", shortLinkLength,"\n"))
  echo(paste0("- Adding elevation:                               ", addElevation,"\n"))
  echo(paste0("- Adding PT from GTFS:                            ", addGtfs,"\n"))
  echo(paste0("- Writing outputs in SQLite format:               ", writeSqlite,"\n"))
  echo(paste0("- Writing outputs in ShapeFile format:            ", writeShp,"\n"))
  echo(paste0("- Writing outputs in MATSim XML format:           ", writeXml,"\n"))
  echo("========================================================\n")
  echo("                **Launching Network Generation**        \n")
  echo("--------------------------------------------------------\n")
  
  # Processing OSM
  if(processOsm){
    echo(paste0("Starting to process osm extract file, ", osmExtract,"\n"))
    echo(paste0("This might take a while depending on your OSM extract size, ", osmExtract,"\n"))
    echo(paste0("Output coordinate system: ", outputCrs, "\n"))
    echo(paste0("Note that this step requires Postgres and GDAL/OGR to be installed, see readme for more info.\n"))
    networkSqlite="./data/network.sqlite"
    if(file_exists(osmExtract)){
    system(paste("./processOSM.sh ", osmExtract, outputCrs, networkSqlite))
    }else{
      warning("OSM extract not found, skipping this step")
    } 
  }
  
  # Note: writing logical fields to sqlite is a bad idea, so switching to integers
  networkInput <- list(st_read(networkSqlite,layer="nodes",quiet=T),
                       st_read(networkSqlite,layer="edges",quiet=T))
  
  # We run into trouble if the geometry column is 'geom' instead of 'GEOMETRY'
  if('GEOMETRY'%in%colnames(networkInput[[1]])) {
    networkInput[[1]]<-networkInput[[1]]%>%rename(geom=GEOMETRY)
  }
  if('GEOMETRY'%in%colnames(networkInput[[2]])) {
    networkInput[[2]]<-networkInput[[2]]%>%rename(geom=GEOMETRY)
  }
  
  cat(paste0("Network input, nodes:\n"))
  str(networkInput[[1]])
  # print.data.frame(head(networkInput[[1]]))
  cat(paste0("\nNetwork input, edges:\n"))
  str(networkInput[[2]])
  cat(paste0("\n"))
  
  if(crop2Area)system.time(networkInput <- crop2Poly(networkInput,
                                                     cropAreaPoly,
                                                     outputCrs))
  echo("processing OSM meta data\n")
  osm_metadata <- st_read(networkSqlite,layer="osm_metadata",quiet=T) %>%
    filter(osm_id%in%networkInput[[2]]$osm_id)
  echo("Building default OSM attribute tables\n")
  defaults_df <- buildDefaultsDF()
  highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
  echo("Processing OSM tags and joining with defaults\n")
  system.time( osmAttributes <- processOsmTags(osm_metadata,defaults_df))
  
  # There are some roads in OSM that are not correctly attributed
  # Use the function below to manually add their attributes based osm id
  osmAttributesCorrected <- osmMetaCorrection(osmAttributes)
  edgesOsm <- networkInput[[2]]
  # Some network link corrections (+/-) specifically for Greater Melbourne OSM
  if(correctNetwork) edgesOsm <- osmNetworkCorrection(networkInput)
  
  edgesAttributed <- edgesOsm %>%
    inner_join(osmAttributesCorrected, by="osm_id") %>%
    # dplyr::select(-osm_id,highway,highway_order)
    dplyr::select(-highway,highway_order)
  
  cat(paste0("edgesAttributed:\n"))
  str(edgesAttributed)
  cat(paste0("\n"))
  
  # keep only the largest connected component
  largestComponent <- largestConnectedComponent(networkInput[[1]],edgesAttributed)
  
  cat(paste0("largestComponent, nodes:\n"))
  str(largestComponent[[1]])
  cat(paste0("\nlargestComponent, edges:\n"))
  str(largestComponent[[2]])
  cat(paste0("\n"))
  
  # simplify intersections while preserving attributes and original geometry.
  system.time(intersectionsSimplified <- simplifyIntersections(largestComponent[[1]],
                                                               largestComponent[[2]],
                                                               shortLinkLength))
  
  # Merge edges going between the same two nodes, picking the shortest geometry.
  # * One-way edges going in the same direction will be merged
  # * Pairs of one-way edges in opposite directions will be merged into a two-way edge.
  # * Two-way edges will be merged regardless of direction.
  # * One-way edges will NOT be merged with two-way edges.
  # * Non-car edges do NOT count towards the merged lane count (permlanes)
  system.time(edgesCombined <- combineRedundantEdges(intersectionsSimplified[[1]],
                                                     intersectionsSimplified[[2]]))
  
  # Merge one-way and two-way edges going between the same two nodes. In these 
  # cases, the merged attributes will be two-way.
  # This guarantees that there will only be a single edge between any two nodes.
  system.time(combinedUndirectedAndDirected <- 
                combineUndirectedAndDirectedEdges(edgesCombined[[1]],
                                                  edgesCombined[[2]]))
  
  # If there is a chain of edges between intersections, merge them together
  system.time(edgesSimplified <- simplifyLines(combinedUndirectedAndDirected[[1]],
                                               combinedUndirectedAndDirected[[2]]))
  
  # Remove dangles
  system.time(noDangles <- removeDangles(edgesSimplified[[1]],edgesSimplified[[2]],
                                         minDangleLinkLengh))
  
  # Do a second round of simplification.
  system.time(edgesCombined2 <- combineRedundantEdges(noDangles[[1]],
                                                      noDangles[[2]]))
  system.time(combinedUndirectedAndDirected2 <- 
                combineUndirectedAndDirectedEdges(edgesCombined2[[1]],
                                                  edgesCombined2[[2]]))
  
  system.time(edgesSimplified2 <- simplifyLines(combinedUndirectedAndDirected2[[1]],
                                                combinedUndirectedAndDirected2[[2]]))
  system.time(edgesCombined3 <- combineRedundantEdges(edgesSimplified2[[1]],
                                                      edgesSimplified2[[2]]))
  
  networkMode <- addMode(edgesCombined3)
  
  # ensure transport is a directed routeable graph for each mode (i.e., connected
  # subgraph). The first function ensures a connected directed subgraph and the
  # second function ensures a connected subgraph but doesn't consider directionality.
  # We car and bike modes are directed, but walk is undirected.
  networkNonDisconnected <- largestDirectedNetworkSubgraph(networkMode,'car,bike')
  networkConnected <- largestNetworkSubgraph(networkNonDisconnected,'walk')
  
  # densify the network so that no residential streets are longer than 500m
  if (addElevation==T & densifyBikeways==F) message("Consider changing densifyBikeways to true when addElevation is true to ge a more accurate slope esimation for bikeways")
  networkDensified <- densifyNetwork(networkConnected,desnificationMaxLengh,
                                     densifyBikeways)
  
  # simplify geometry so all edges are straight lines
  system.time(networkDirect <-
                makeEdgesDirect(networkDensified[[1]],
                                networkDensified[[2]]))
  
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
  # Adjust your analysis start date, end data and gtfs feed name below
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
  
  networkFinal <- networkRestructured
  
  # writing outputs
  echo("========================================================\n")
  echo("|               **Launching Output Writing**           |\n")
  echo("--------------------------------------------------------\n")
  
  if(writeSqlite) system.time(exportSQlite(networkFinal, outputDir))
  if(writeShp) system.time(exportShp(networkFinal, outputDir))
  if(writeXml) system.time(exportXML(networkFinal, outputDir)) 
}

