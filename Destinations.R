# Make file of destinations required for accessibilty routing

library(dplyr)
library(sf)
library(osmextract)

# 1 Download OSM extract ----
# -----------------------------------------------------------------------------#
# Download from https://www.interline.io/osm/extracts/ 

## Downloaded for Melbourne - melbourne_australia.osm.pbf


# 2 Converting to .gpkg format ----
# -----------------------------------------------------------------------------#
# input file name and project CRS
INPUTFILE <- "./data/melbourne_australia.osm.pbf"

PROJECT.CRS = 28355

# check layers
st_layers(INPUTFILE)

# check keys
options(max.print = 2000)
polygon.tags <- oe_get_keys(INPUTFILE, layer = "multipolygons") %>% sort()
point.tags <- oe_get_keys(INPUTFILE, layer = "points") %>% sort()
line.tags <- oe_get_keys(INPUTFILE, layer = "lines") %>% sort()

# create gpkg file in same directory as INPUTFILE, using the 'extra_tags' 
# argument for specific extra tags required for various destination types
oe_vectortranslate(INPUTFILE, layer = "multipolygons", 
                   extra_tags = c("access", "building", "grades", "healthcare", 
                                  "healthcare:speciality","isced:level", 
                                  "network", "operator",
                                  "operator:type", "public_transport", "railway",
                                  "school", "social_facility", "sport", 
                                  "tourism", "train"))
oe_vectortranslate(INPUTFILE, layer = "points",
                   extra_tags = c("access", "amenity", "building", "grades", 
                                  "healthcare", "healthcare:speciality", 
                                  "isced:level", "landuse", "leisure", 
                                  "network", "operator",
                                  "operator:type", "public_transport", "railway",
                                  "school", "shop", "social_facility",  "sport",
                                  "tourism", "train"))
oe_vectortranslate(INPUTFILE, layer = "lines",
                   extra_tags = c("access", "amenity", "building", "grades", 
                                  "healthcare", "healthcare:speciality", 
                                  "isced:level", "landuse", "leisure", 
                                  "network", "operator",
                                  "operator:type", "public_transport", "railway",
                                  "school", "shop", "social_facility",  "sport",
                                  "tourism", "train",
                                  "smoothness", "surface"))
oe_vectortranslate(INPUTFILE, layer = "multilinestrings")
oe_vectortranslate(INPUTFILE, layer = "other_relations")


# 3 Read in the .gpkg file ----
# -----------------------------------------------------------------------------#
GPKG <- "./data/melbourne_australia.gpkg"

polygons <- st_read(GPKG, layer = "multipolygons") %>% st_transform(PROJECT.CRS)
points <- st_read(GPKG, layer = "points") %>% st_transform(PROJECT.CRS)
lines <- st_read(GPKG, layer = "lines") %>% st_transform(PROJECT.CRS)
multilines <- st_read(GPKG, layer = "multilinestrings") %>% st_transform(PROJECT.CRS)
other_relations <- st_read(GPKG, layer = "other_relations") %>% st_transform(PROJECT.CRS)


# 4 Extract required destinations ----
# -----------------------------------------------------------------------------#

## 4.1 Tag combinations for feature types  and network ----
## ----------------------------------------------------------------------------#
# load functions for locating specific feature types
source("./functions/getDestinationTypes.R")

# load network
NETWORK <- "./output/test/network.sqlite"  #<<< CHECK FINAL NAME
NODE.LAYER <- "nodes"
LINK.LAYER <- "links"

network.nodes <- st_read(NETWORK, layer = NODE.LAYER)  
network.links <- st_read(NETWORK, layer = LINK.LAYER)


## 4.2 Compile point and polygon destinations ----
## ----------------------------------------------------------------------------#
destination.layer <- function(layer) {
  return(
    bind_rows(
      getPlayground(layer) %>% mutate(dest_type = "playground"),
      getPark(layer) %>% mutate(dest_type = "park"),
      getSport(layer) %>% mutate(dest_type = "sport"),
      getKindergarten(layer) %>% mutate(dest_type = "kindergarten"),
      getCommunity(layer) %>% mutate(dest_type = "community_centre"),
      getLibrary(layer) %>% mutate(dest_type = "library"),
      getPrimary(layer) %>% mutate(dest_type = "primary_school"),
      getSecondary(layer) %>% mutate(dest_type = "secondary_school"),
      getClinic(layer) %>% mutate(dest_type = "health_clinic"),
      getDentist(layer) %>% mutate(dest_type = "dentist"),
      getPharmacy(layer) %>% mutate(dest_type = "pharmacy"),
      getConvenience(layer) %>% mutate(dest_type = "convenience_store"),
      getSupermarket(layer) %>% mutate(dest_type = "supermarket"),
      getShop(layer) %>% mutate(dest_type = "shop"),
      getPost(layer) %>% mutate(dest_type = "post_office"),
      getBank(layer) %>% mutate(dest_type = "bank"),
      getRestaurant(layer) %>% mutate(dest_type = "restaurant"),
      getCafe(layer) %>% mutate(dest_type = "cafe")
    ))
}

# create tables of destinations, and allocate unique id's (so features with
# multiple nodes can be grouped by the id where required)
destination.pt <- 
  bind_rows(destination.layer(points),
            getStation() %>% mutate(dest_type = "railway_station")) %>%
  mutate(dest_id = row_number())

destination.poly <- 
  destination.layer(polygons) %>%
  mutate(dest_id = max(destination.pt$dest_id) + row_number())


## 4.3 Find relevant nodes ----
## ----------------------------------------------------------------------------#
# TO CONFIRM:-
# For all destinations except parks and schools ('small features'), relevant 
# node is nearest node to point or to polygon centroid
# For parks and schools ('large features'):
# - points are buffered to 50m to create a polygon feature,
# - for buffered points and polygons, relevant nodes are all nodes within the 
#   feature and terminal nodes of links within 30m of boundary, or if none, 
#   then nearest node to boundary

# Maybe this should be all nodes within 30m of buffered feature, and if link is within
# 30m of boundary but doesn't have a node within the buffer, then also its closest terminal
# node ???

dest.small <- bind_rows(destination.pt,
                        destination.poly %>% st_centroid()) %>%
  filter(!(dest_type %in% c("park", "primary_school", "secondary_school")))
near_node <- network.nodes$id[st_nearest_feature(dest.small, network.nodes)]
dest.small.with.nodes <- cbind(dest.small %>% st_drop_geometry(), near_node) 


## NOTE - the code below is a simplified version which just finds nodes within
## feature or its 30m buffer, or nearest node if none - doesn't extend to terminal
## nodes of nearby features
dest.large <- bind_rows(destination.pt %>% st_buffer(50),
                        destination.poly) %>%
  filter(dest_type %in% c("park", "primary_school", "secondary_school"))

dest.large.found.nodes <- dest.large %>%
  st_buffer(30) %>%
  st_intersection(., network.nodes %>% dplyr::select(near_node = id))

dest.large.need.nodes <- dest.large %>%
  filter(!(dest_id %in% dest.large.found.nodes$dest_id))
near_node <- network.nodes$id[st_nearest_feature(dest.large.need.nodes, network.nodes)]

dest.large.with.nodes <- bind_rows(dest.large.found.nodes %>% st_drop_geometry(),
                                   cbind(dest.large.need.nodes %>% st_drop_geometry(),
                                         near_node))

dest.with.nodes <- bind_rows(dest.small.with.nodes,
                             dest.large.with.nodes)

