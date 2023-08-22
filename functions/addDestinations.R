# function to create a destination layer to add to output network

# assumes input file (OSMextract) is in .osm.pbf format, for example,
# as downloaded from  https://www.interline.io/osm/extracts/ 

# uses functions for various destination types with tag combinations set out
# in 'getDestinationTypes.R'

addDestinations <- function(nodes_current, 
                             edges_current, 
                             osmPbfExtract,
                             outputCRS) {
  
  # nodes_current = networkDensified[[1]]
  # edges_current = networkDensified[[2]]
  # osmPbfExtract = "./data/melbourne_australia.osm.pbf"
  # outputCrs = 28355
  
  # # check layers
  # st_layers(osmPbfExtract)
  # # only multipolygons, points and lines are required (not multilinestrings
  # # or other_relations)
  
  # # check keys
  # options(max.print = 2000)
  # polygon.tags <- oe_get_keys(osmPbfExtract, layer = "multipolygons") %>% sort()
  # point.tags <- oe_get_keys(osmPbfExtract, layer = "points") %>% sort()
  # line.tags <- oe_get_keys(osmPbfExtract, layer = "lines") %>% sort()
  
  # reading layers ----
  # ----------------------------------#
  echo("Reading in the .osm.pbf extract layers\n")

  # create gpkg file in same directory as osmPbfExtract, using the 'extra_tags'
  # Note:
  # - the gpkg does not need to be retained permanently, but its creation is part
  #   of the process of reading the layers; if already created, the reading 
  #   process will be quicker)
  # - for simplicity, the same extra tags are added for all layers, though
  #   some don't exist for some layer types
  extra.tags <- c("access", "amenity", "building", "grades", "healthcare", 
                  "healthcare:speciality", "isced:level", "landuse", "leisure", 
                  "network", "operator", "operator:type", "public_transport", 
                  "railway", "school", "shop", "social_facility",  "sport",
                  "tourism", "train")
  # oe_vectortranslate(osmPbfExtract, layer = "multipolygons", extra_tags = extra.tags)
  # oe_vectortranslate(osmPbfExtract, layer = "points", extra_tags = extra.tags)
  # oe_vectortranslate(osmPbfExtract, layer = "lines", extra_tags = extra.tags)
  # 
  # # read in the .gpkg file (same directory and name as .osm.pbf file, but .gpkg extension)
  # gpkg <- paste0(path_dir(osmPbfExtract), "/", 
  #                gsub(".osm.pbf", ".gpkg", path_file(osmPbfExtract)))
  # read in the layers
  polygons <- oe_read(osmPbfExtract, layer = "multipolygons", extra_tags = extra.tags) %>% 
    st_transform(outputCrs)
  points <- oe_read(osmPbfExtract, layer = "points", extra_tags = extra.tags) %>% 
    st_transform(outputCrs)
  lines <- oe_read(osmPbfExtract, layer = "lines", extra_tags = extra.tags) %>% 
    st_transform(outputCrs)


  # function to extract specific destination types from point or polygon layers ----
  # ----------------------------------#
  # all the tag combination functions in 'getDestinationTypes.R' apply to both
  # points and polygons, except 'railway station', which are a combination of
  # point, polygon and line features
  
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
  
  # create tables of point and polygon destinations ----
  # ----------------------------------#
  echo("Finding destinations and their nearby nodes\n")
  
  # create tables for points and polygons, and allocate unique id's (so features 
  # multiple multiple nodes can be grouped by the id where required)
  destination.pt <- 
    bind_rows(destination.layer(points),
              # add stations (from point, polygons and lines) to point table
              getStation() %>% mutate(dest_type = "railway_station")) %>%
    mutate(dest_id = row_number())
  
  destination.poly <- 
    destination.layer(polygons) %>%
    mutate(dest_id = max(destination.pt$dest_id) + row_number())
  
  # # check numbers of each destination type
  # chk <- full_join(destination.poly %>%
  #                    st_drop_geometry() %>%
  #                    group_by(dest_type) %>%
  #                    summarise(poly = n()), 
  #                  destination.pt %>%
  #                    st_drop_geometry() %>%
  #                    group_by(dest_type) %>%
  #                    summarise(pt = n()), 
  #                  by = "dest_type")
  
  
  # find relevant nodes ----
  # For all destinations except parks and schools ('small features'), relevant 
  # node is nearest node to point or to polygon centroid
  # For parks and schools ('large features'):
  # - points are buffered to 50m to create a polygon feature,
  # - for buffered points and polygons, relevant nodes are all nodes within the 
  #   feature and terminal nodes of links within 30m of boundary, or if none, 
  #   then nearest node to boundary
  # In each case, nodes/links must be cyclable
  
  cyclable.links <- edges_current %>%  
    filter(is_cycle == 1)
  cyclable.nodes <- nodes_current %>%
    filter(id %in% cyclable.links$from_id | id %in% cyclable.links$to_id)
  
  # 'small' destinations
  dest.small <- bind_rows(destination.pt,
                          destination.poly %>% st_centroid()) %>%
    filter(!(dest_type %in% c("park", "primary_school", "secondary_school")))
  near_node <- cyclable.nodes$id[st_nearest_feature(dest.small, cyclable.nodes)]
  dest.small.with.nodes <- cbind(dest.small %>% st_drop_geometry(), near_node) 
  
  
  # 'large' destinations
  dest.large <- bind_rows(destination.pt %>% st_buffer(50),
                          destination.poly) %>%
    filter(dest_type %in% c("park", "primary_school", "secondary_school"))
  
  # # - nodes within the feature
  # dest.large.nodes.within <- dest.large %>%
  #  st_intersection(., cyclable.nodes %>% dplyr::select(near_node = id)) %>%
  #   st_drop_geometry()
  
  # - terminal nodes of links within feature buffered to 30m (will include any
  #   nodes within feature itself, as their links will fall within the buffered 
  #   feature)
  dest.large.found.nodes <- dest.large %>%
    st_buffer(30) %>%
    st_intersection(., cyclable.links %>% dplyr::select(from_id, to_id)) %>%
    st_drop_geometry() %>%
    pivot_longer(cols = c("from_id", "to_id"), 
                 names_to = NULL,
                 values_to = "near_node") %>% 
    distinct()

  # - nearest node if none within and no links within 30m
  dest.large.other <- dest.large %>%
    filter(!(dest_id %in% dest.large.found.nodes$dest_id))
  near_node <- cyclable.nodes$id[st_nearest_feature(dest.large.other, cyclable.nodes)]
  dest.large.other.nodes <- cbind(dest.large.other %>% st_drop_geometry(), near_node) 
  
  # combine the large destinations
  dest.large.with.nodes <- bind_rows(dest.large.found.nodes,
                                     dest.large.other.nodes)
  
  
  # combine all destinations for output ----
  dest.with.nodes <- bind_rows(dest.small.with.nodes,
                               dest.large.with.nodes) %>%
    relocate(dest_id) %>%
    relocate(dest_type, .after = dest_id) %>%
    relocate(near_node, .after = dest_type) %>%
    relocate(other_tags, .after = last_col()) %>%
    
    # and join nodes for locations
    left_join(., nodes_current %>% dplyr::select(id), by = c("near_node" = "id"))
    
  return(dest.with.nodes)
  
}

