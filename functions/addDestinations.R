# function to create a destination layer to add to output network

# uses functions for various destination types with tag combinations set out
# in 'getDestinationTypes.R'

addDestinations <- function(nodes_current, 
                            edges_current, 
                            osmGpkg,
                            city, 
                            gtfs_feed,
                            outputCrs,
                            region,
                            regionBufferDist) {
   
  # nodes_current = networkDensified[[1]]
  # edges_current = networkDensified[[2]]
  # osmGpkg = "./data/melbourne_osm.gpkg"
  # city = "Melbourne"
  # gtfs_feed = "./data/gtfs.zip"
  # outputCrs = 28355
  
  # # check keys
  # options(max.print = 2000)
  # point.tags <- oe_get_keys(osmGpkg, layer = "points") %>% sort()
  # polygon.tags <- oe_get_keys(osmGpkg, layer = "multipolygons") %>% sort()

  # reading layers ----
  # ----------------------------------#
  echo("Reading in the OSM layers\n")

  extra.tag.string <- "SELECT *, 
                      hstore_get_value(other_tags, 'access') AS access,
                      hstore_get_value(other_tags, 'amenity') AS amenity,
                      hstore_get_value(other_tags, 'grades') AS grades,
                      hstore_get_value(other_tags, 'healthcare') AS healthcare,
                      hstore_get_value(other_tags, 'isced:level') AS isced_level,
                      hstore_get_value(other_tags, 'leisure') AS leisure,
                      hstore_get_value(other_tags, 'parking') AS parking,
                      hstore_get_value(other_tags, 'school') AS school,
                      hstore_get_value(other_tags, 'shop') AS shop,
                      hstore_get_value(other_tags, 'sport') AS sport"

  # read in the layers
  points <- oe_read(osmGpkg, query = paste(extra.tag.string, "FROM points"), quiet = TRUE)
  polygons <- oe_read(osmGpkg, query = paste(extra.tag.string, "FROM multipolygons"), quiet = TRUE)

  # function to extract specific destination types from point or polygon layers ----
  # ----------------------------------#
  # all the tag combination functions in 'getDestinationTypes.R' apply to both
  # points and polygons
  
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
        getCafe(layer) %>% mutate(dest_type = "cafe"),
        getParking(layer) %>% mutate(dest_type = "parking")
      ))
  }
  
  # create tables of point and polygon destinations ----
  # ----------------------------------#
  echo("Finding destinations and their nearby nodes\n")
  
  # create tables for points and polygons, allocate unique id's (so features 
  # multiple multiple nodes can be grouped by the id where required),
  # and store area and location details
  destination.pt <- 
    bind_rows(destination.layer(points),
              
              # add PT stops (from GTFS feed) to point table
              getPTStops(city, gtfs_feed, outputCrs, region, regionBufferDist) %>%
                mutate(dest_type = "pt_stop")) %>%
    
    mutate(dest_id = row_number(),
           area_m2 = 0,
           centroid_x = st_coordinates(.)[, 1],
           centroid_y = st_coordinates(.)[, 2])
  
  destination.poly <- 
    destination.layer(polygons) %>%
    filter(st_is_valid(geom)) %>%
    mutate(dest_id = max(destination.pt$dest_id) + row_number(),
           area_m2 = as.numeric(st_area(.)),
           centroid_x = st_coordinates(st_centroid(.))[, 1],
           centroid_y = st_coordinates(st_centroid(.))[, 2])
  

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

