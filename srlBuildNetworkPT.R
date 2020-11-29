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
library(XML)


source('./functions/etc/logging.R')
source('./functions/gtfs2PtNetwork.R')
source('./functions/exportSQlite.R')
source('./functions/exportXML.R')

# studyRegion <- st_read('/home/alan/Projects/virtual-population/data/absRegionsReprojected.sqlite',
#                        layer="gccsa_2016_aust",quiet=T)
# st_write(studyRegion,"data/studyRegion.sqlite",delete_layer=T)

# import processed road network ------------------------------------------
networkSqlite="generatedNetworks/roadNetwork.sqlite"

nodes <- st_read(networkSqlite,layer="nodes",quiet=T)
links <- st_read(networkSqlite,layer="links",quiet=T) %>%
  # saving to sqlite makes column names lowercase, reverting to uppercase
  rename(fromX=fromx, fromY=fromy, toX=tox, toY=toy)

# read in the study region boundary
studyRegion <- st_read("data/studyRegion.sqlite",quiet=T) %>%
  st_buffer(10000) %>%
  st_snap_to_grid(1)

validRoadEdges <- links %>%
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

# Steve to add in the SRL schedule here



# combining SRL and road network ------------------------------------------


# this returns the edges in the PT network as well as writing the
# transitVehicles.xml and transitSchedule.xml
ptEdges <- exportGtfsSchedule(
  stops,
  stopTimes,
  trips,
  routes
)

edgesCombined <- bind_rows(links,ptEdges) %>%
  st_sf()

exportSQlite(list(nodes,edgesCombined), outputFileName = "networkWithPT")

# this stage will take about an hour to complete. Just focus on the sqlite
# export for now.
exportXML(list(nodes,edgesCombined), outputFileName = "networkWithPT")


