# libraries and functions -------------------------------------------------
library(sf)
library(lwgeom)
library(dplyr)
library(data.table)
library(stringr)
library(igraph)
library(purrr)

source('./functions/etc/logging.R')
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
source('./functions/cleanNetwork.R')
source('./functions/exportSQlite.R')



# import road network -----------------------------------------------------
networkSqlite="data/network.sqlite"

networkInput <- list(st_read(networkSqlite,layer="nodes",quiet=T),
                     st_read(networkSqlite,layer="edges",quiet=T))



# processing road network -------------------------------------------------

osm_metadata <- st_read(networkSqlite,layer="osm_metadata",quiet=T)
defaults_df <- buildDefaultsDF()
highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
system.time( osmAttributes <- processOsmTags(osm_metadata,defaults_df))

edgesAttributed <- networkInput[[2]] %>%
  inner_join(osmAttributes, by="osm_id") %>%
  dplyr::select(-osm_id,highway,highway_order)

# keep only the largest connected component
largestComponent <- largestConnectedComponent(networkInput[[1]],edgesAttributed)

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



# writing sqlite outputs --------------------------------------------------
exportSQlite(networkRestructured, outputFileName = "roadNetwork")

