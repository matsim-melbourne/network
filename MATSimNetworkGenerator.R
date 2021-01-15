makeMatsimNetwork<-function(crop2TestArea=F, shortLinkLength=20, addElevation=F, 
                            addGtfs=F, addIvabmPt=F, writeXml=F, writeSqlite=T,
                            networkSqlite="data/network.sqlite"){

    # crop2TestArea=F; shortLinkLength=20; addElevation=F; addGtfs=F
    # addIvabmPt=F; writeXml=F; writeSqlite=T; networkSqlite="data/network.sqlite"
    
    message("========================================================")
    message("                **Network Generation Setting**")
    message("--------------------------------------------------------")
    message(paste0("- Cropping to a test area:                        ",crop2TestArea))
   #message(paste0("- Detailed network only in the focus area:        ", focus_area_flag))
    message(paste0("- Shortest link length in network simplification: ", shortLinkLength))
    message(paste0("- Adding elevation:                               ", addElevation))
    message(paste0("- Adding PT from GTFS:                            ", addGtfs))
    message(paste0("- Adding PT from IV-ABM:                          ", addIvabmPt))
    message(paste0("- Writing outputs in MATSim XML format:           ", writeXml))
    message(paste0("- Writing outputs in SQLite format:               ", writeSqlite))
    message("========================================================")
  #libraries
  library(sf)
  library(lwgeom)
  library(dplyr)
  library(data.table)
  library(stringr)
  library(igraph)
  library(raster)
  library(XML)
  library(rgdal)
  library(purrr)
  # These are needed if addGtfs=T
  if(addGtfs){
    library(tidytransit)
    library(hablar)
    library(lwgeom)
    library(hms)
  }
  # This is needed if addIvabmPt=T
  if(addIvabmPt){
    library(nngeo)
  }
  
  #functions
  source('./functions/etc/logging.R')
  # source('./functions/simplifyNetwork.R')
  source('./functions/crop2TestArea.R')
  source('./functions/buildDefaultsDF.R')
  source('./functions/processOsmTags.R')
  source('./functions/largestConnectedComponent.R')
  source('./functions/simplifyIntersections.R')
  source('./functions/combineRedundantEdges.R')
  source('./functions/combineUndirectedAndDirectedEdges.R')
  source('./functions/simplifyLines.R')
  source('./functions/removeDangles.R')
  source('./functions/makeEdgesDirect.R')
  source('./functions/restructureData.R')
  source('./functions/addElevation2Nodes.R')
  source('./functions/gtfs2PtNetwork.R')
  source('./functions/etc/IVABMIntegrator.R')
  source('./functions/cleanNetwork.R')
  source('./functions/exportSQlite.R')
  source('./functions/exportXML.R')
    
  
  message("========================================================")
  message("                **Launching Network Generation**")
  message("--------------------------------------------------------")
  
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
  
  # select from https://github.com/JamesChevalier/cities/tree/master/australia/victoria
  if(crop2TestArea)system.time(networkInput <- crop2Poly(networkInput,
                                                         "city-of-melbourne_victoria"))
  # if(crop2TestArea) {
  #   nodes2 <- networkInput[[1]] %>%
  #     filter(lengths(st_intersects(., studyRegion,prepared=TRUE,sparse=TRUE)) > 0) 
  #   edges2 <- networkInput[[2]] %>%
  #     filter(from_id%in%nodes2$id & to_id%in%nodes2$id)
  #   nodes2 <- nodes2 %>%
  #     filter(id%in%edges2$from_id | id%in%edges2$to_id)
  #   networkInput[[1]] <- nodes2
  #   networkInput[[2]] <- edges2
  #   rm(nodes2,edges2)
  # }
  
  
  osm_metadata <- st_read(networkSqlite,layer="osm_metadata",quiet=T) %>%
    filter(osm_id%in%networkInput[[2]]$osm_id)
  defaults_df <- buildDefaultsDF()
  highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
  system.time( osmAttributes <- processOsmTags(osm_metadata,defaults_df))
  
  edgesAttributed <- networkInput[[2]] %>%
    inner_join(osmAttributes, by="osm_id") %>%
    dplyr::select(-osm_id,highway,highway_order)
  
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
                                                               20))
  
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
  system.time(noDangles <- removeDangles(edgesSimplified[[1]],edgesSimplified[[2]],500))
  
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
  # simplify geometry so all edges are straight lines
  system.time(networkDirect <- 
                makeEdgesDirect(edgesCombined3[[1]],
                                edgesCombined3[[2]]))
  
  # add mode to edges, add type to nodes, change cycleway from numbers to text
  networkRestructured <- restructureData(networkDirect, highway_lookup,defaults_df)

  # ensure transport is a directed routeable graph by first removing disconnected
  # directed links, and then ensuring the subgraph for each mode is connected
  networkNonDisconnected <- removeDisconnectedLinks(networkRestructured,'car,bike')
  networkConnected <- cleanNetworkSubgraph(networkNonDisconnected,'walk,car,bike')
  
  if(addElevation) system.time(networkConnected[[1]] <- addElevation2Nodes(networkConnected[[1]], 
                                                                           'data/DEMx10EPSG28355.tif'))
  if(addGtfs) {
    # read in the study region boundary
    greaterMelbourne <- st_read("data/studyRegion.sqlite",quiet=T) %>%
      st_buffer(10000) %>%
      st_snap_to_grid(1)
    system.time(networkConnected[[2]] <- addGtfsLinks(outputLocation="./gtfs/",
                                                      nodes=networkConnected[[1]], 
                                                      links=networkConnected[[2]],
                                                      studyRegion=greaterMelbourne)) 
  }
  if(addIvabmPt) system.time(networkConnected <- integrateIVABM(st_drop_geometry(networkConnected[[1]]), 
                                                                networkConnected[[2]]))
  
  # This step is no longer necessary
  # system.time(networkFinal <- cleanNetwork(networkRestructured, 
  #                                          network_modes="")) # leave the network_modes empty if not needed
  networkFinal <- networkConnected
  
  # writing outputs ---------------------------------------------------------
  message("========================================================")
  message("|               **Launching Output Writing**           |")
  message("--------------------------------------------------------")

  if(writeSqlite) system.time(exportSQlite(networkFinal, outputFileName = "MATSimMelbNetwork"))
  if(writeXml) system.time(exportXML(networkFinal, outputFileName = "MATSimMelbNetwork")) # uncomment if you want xml output
}

