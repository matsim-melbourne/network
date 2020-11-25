# libraries and functions -------------------------------------------------
library(sf)
library(lwgeom)
library(dplyr)
library(data.table)
library(stringr)
library(igraph)
library(purrr)
library(tidytransit)
library(hablar)
library(hms)

source('./functions/etc/logging.R')
source('./functions/gtfs2PtNetwork.R')
source('./functions/exportSQlite.R')
source('./functions/exportXML.R')

# studyRegion <- st_read('/home/alan/Projects/virtual-population/data/absRegionsReprojected.sqlite',
#                        layer="gccsa_2016_aust",quiet=T)
# st_write(studyRegion,"data/studyRegion.sqlite",delete_layer=T)

# import processed road network ------------------------------------------
networkSqlite="generatedNetworks/roadNetwork.sqlite"

networkInput <- list(st_read(networkSqlite,layer="nodes",quiet=T),
                     st_read(networkSqlite,layer="links",quiet=T))

# read in the study region boundary
studyRegion <- st_read("data/studyRegion.sqlite",quiet=T) %>%
  st_buffer(10000) %>%
  st_snap_to_grid(1)

validRoadEdges <- networkInput[[2]] %>%
  st_drop_geometry() %>%
  filter(is_walk==1 & is_car==1 & is_cycle==1) %>%
  dplyr::select(from_id,to_id)
validRoadIds <- c(validRoadEdges$from_id,validRoadEdges$to_id) %>%
  unique()

# network nodes that can be reached via walking, cycling, and driving.  
validRoadNodes <- networkInput[[1]] %>%
  filter(id %in% validRoadIds) %>%
  st_set_crs(28355)
# st_write(validRoadNodes,"validRoadNodes.sqlite",delete_layer=TRUE)


# process GTFS feed -------------------------------------------------------

# process the GTFS feed and save the outputs to the gtfs folder
outputLocation <- "./gtfs/"
processGtfs(
  outputLocation, # I don't state outputLocation=outputLocation as it can cause issues
  networkNodes = validRoadNodes,
  gtfs_feed = "data/gtfs_au_vic_ptv_20191004.zip", 
  analysis_start = as.Date("2019-10-11","%Y-%m-%d"), 
  analysis_end = as.Date("2019-10-17","%Y-%m-%d"),
  studyRegion)
  
# read the outputs
stops <- st_read(paste0(outputLocation,"stops.sqlite"),quiet=T)
stopTimes <- readRDS(paste0(outputLocation,"stopTimes.rds"))
trips <- readRDS(paste0(outputLocation,"trips.rds"))
routes <- readRDS(paste0(outputLocation,"routes.rds"))

stopsAttributed <- stopTimes %>%
  dplyr::select(trip_id,stop_id) %>%
  inner_join(trips) %>%
  inner_join(routes) %>%
  dplyr::select(stop_id,service_type) %>%
  distinct() %>%
  inner_join(stops) %>%
  st_sf()
st_write(stopsAttributed,paste0(outputLocation,"stopsAttributed.sqlite"),delete_layer=TRUE)


# incorporating SRL -------------------------------------------------------


