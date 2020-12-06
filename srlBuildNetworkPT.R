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
source('./functions/srl2PtNetwork.R') 

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
greaterMelbourne <- st_read("data/studyRegion.sqlite",quiet=T) %>%
  st_buffer(10000) %>%
  st_snap_to_grid(1)

validRoadEdges <- links %>%
  st_drop_geometry() %>%
  filter(is_walk==1 & is_car==1 & is_cycle==1) %>%
  dplyr::select(from_id,to_id)
validRoadIds <- c(validRoadEdges$from_id,validRoadEdges$to_id) %>%
  unique()

# network nodes that can be reached via walking, cycling, and driving.  
validRoadNodes <- nodes %>%
  filter(id %in% validRoadIds) %>%
  st_set_crs(28355)
# st_write(validRoadNodes,"./data/validRoadNodes.sqlite",delete_layer=TRUE)


# process GTFS feed -------------------------------------------------------

# process the GTFS feed and save the outputs to the gtfs folder
outputLocation <- "./gtfs/"
processGtfs(outputLocation = outputLocation,
            networkNodes = validRoadNodes,
            studyRegion = greaterMelbourne)
  

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


# exporting base network --------------------------------------------------

# this returns the edges in the PT network as well as writing the
# transitVehicles.xml and transitSchedule.xml
ptEdges <- exportGtfsSchedule(
  outputLocation,
  stops,
  stopTimes,
  trips,
  routes
)

edgesCombined <- bind_rows(links,ptEdges) %>%
  st_sf() %>%
  mutate(cycleway=as.character(cycleway))

exportSQlite(list(nodes,edgesCombined), outputFileName = "networkBase")
# exportXML(list(nodes,edgesCombined), outputFileName = "networkWithSRL")



# incorporating SRL -------------------------------------------------------

# requires file 'srl_stg1.sqlite', in EPSG:28355 - GDA 94 / MGA zone 55, containing two layers:
#  - stations (point), with a 'sequence' field listing order of stations along line from one end to other
#  - lines (linestring), containing separate line features each joining two adjacent stations 

# read in SRL stations and lines
stations <- st_read("data/srl_stg1.sqlite",layer="stations")
lines <- st_read("data/srl_stg1.sqlite",layer="lines")

# validRoadNodes <- st_read("data/validRoadNodes.sqlite", layer="validroadnodes")

# set parameters for timetable 
HOURS <- c("05:00:00", "24:00:00")  # start and end of timetable period
INTERVAL <- 600  # 600 seconds, ie. 10 minutes  or, if using peak: c(600, 240, 600, 240, 600)
SPEED <- 60  # km/h
ROUTEIDs <- c("SRL1", "SRL2")  # identifiers for distinct service patterns (one in each direction)
SERVICEID <- "SRL0"  # identifier for distinct set of dates when services operate
SERVICETYPE <- "train"  # alternatively, "SRL" to distinguish from other trains, but would require changes in export functions

## alternative for HOURS and INTERVAL allowing different intervals in peak periods
## note must be one interval for each span of hours, so length of INTERVAL must be one less than length of HOURS 
# HOURS <- c("05:00:00", "07:00:00", "09:00:00", "16:00:00", "18:00:00", "24:00:00")
# INTERVAL <- c(600, 240, 600, 240, 600)  # 4 mins in peak periods; otherwise 10 mins


outputLocation <- "./srl/"
processSrl(
  outputLocation,
  stations = stations,
  lines = lines,
  validRoadNodes = validRoadNodes,
  HOURS = HOURS,
  INTERVAL = INTERVAL,
  SPEED = SPEED,
  ROUTEIDs = ROUTEIDs,
  SERVICEID = SERVICEID,
  SERVICETYPE = SERVICETYPE)


# read the outputs
srlStops <- st_read(paste0(outputLocation,"srlStops.sqlite"),quiet=T)
srlStopTimes <- readRDS(paste0(outputLocation,"srlStopTimes.rds"))
srlTrips <- readRDS(paste0(outputLocation,"srlTrips.rds"))
srlRoutes <- readRDS(paste0(outputLocation,"srlRoutes.rds"))


# combine with the GTFS outputs, ready for export to XML
stops <- bind_rows(stops, srlStops) %>% distinct()
stopTimes <- bind_rows(stopTimes, srlStopTimes)
trips <- bind_rows(trips, srlTrips)
routes <- bind_rows(routes, srlRoutes)



# combining SRL and road network ------------------------------------------


# this returns the edges in the PT network as well as writing the
# transitVehicles.xml and transitSchedule.xml
ptEdges <- exportGtfsSchedule(
  outputLocation,
  stops,
  stopTimes,
  trips,
  routes
)

edgesCombined <- bind_rows(links,ptEdges) %>%
  st_sf() %>%
  mutate(cycleway=as.character(cycleway))

exportSQlite(list(nodes,edgesCombined), outputFileName = "networkWithSRL")

# this stage will take about an hour to complete. Just focus on the sqlite
# export for now.
exportXML(list(nodes,edgesCombined), outputFileName = "networkWithSRL")


