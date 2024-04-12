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
  
  # read in and buffer the region
  region.poly <- st_read(region)
  if (st_crs(region.poly)$epsg != outputCrs) {
    region.poly <- st_transform(region.poly, outputCrs)
  }
  study.area <- st_buffer(region.poly, regionBufferDist)  %>%
    st_snap_to_grid(1)

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
  echo("Destination point features\n")
  destination.pt <- 
    bind_rows(destination.layer(points),
              
              # add PT stops (from GTFS feed) to point table
              getPTStops(city, gtfs_feed, outputCrs, study.area) %>%
                mutate(dest_type = "pt_stop")) %>%
    mutate(dest_id = row_number(),
           area_m2 = 0,
           centroid_x = st_coordinates(.)[, 1],
           centroid_y = st_coordinates(.)[, 2]) %>%
    st_filter(study.area, .predicate = st_intersects)
  
  echo("Destination polygon features\n")
  destination.poly <- 
    destination.layer(polygons) %>%
    filter(st_is_valid(geom)) %>%
    mutate(dest_id = max(destination.pt$dest_id) + row_number(),
           area_m2 = as.numeric(st_area(.)),
           centroid_x = st_coordinates(st_centroid(.))[, 1],
           centroid_y = st_coordinates(st_centroid(.))[, 2]) %>%
    st_filter(study.area, .predicate = st_intersects)
  

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
  
  # Find nodes available for each mode
  links.cycle <- edges_current %>% filter(is_cycle == 1)
  nodes.cycle <- nodes_current %>% filter(id %in% links.cycle$from_id | id %in% links.cycle$to_id)
  
  links.walk <- edges_current %>% filter(is_walk == 1)
  nodes.walk <- nodes_current %>% filter(id %in% links.walk$from_id | id %in% links.walk$to_id)
  
  links.car <- edges_current %>% filter(is_car == 1)
  nodes.car <- nodes_current %>% filter(id %in% links.car$from_id | id %in% links.car$to_id)
  
  # 'small' features
  dest.small <- bind_rows(destination.pt, destination.poly) %>%
    filter(!(dest_type %in% c("park", "primary_school", "secondary_school")))
  # nearest node of each mode
  node_cycle <- nodes.cycle$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.cycle)]
  node_walk <- nodes.walk$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.walk)]
  node_car <- nodes.car$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.car)]
  # join nearest nodes to features
  dest.small.with.nodes <- cbind(dest.small, node_cycle, node_walk, node_car) 
  
  # 'large' features - points
  dest.large.pt <- destination.pt %>%
    filter(dest_type %in% c("park", "primary_school", "secondary_school"))
  # nearest node of each mode
  node_cycle <- nodes.cycle$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.cycle)]
  node_walk <- nodes.walk$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.walk)]
  node_car <- nodes.car$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.car)]
  # join nearest nodes to features
  dest.large.pt.with.nodes <- cbind(dest.large.pt, node_cycle, node_walk, node_car) 

  # 'large' features - polygons
  dest.large.poly <- destination.poly %>%
    filter(dest_type %in% c("park", "primary_school", "secondary_school"))
  
  echo(paste("Finding entry nodes for", nrow(dest.large.poly), "parks and schools\n"))
  
  # internal nodes
  internal.nodes.cycle <- nodes.cycle %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  internal.nodes.walk <- nodes.walk %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  internal.nodes.car <- nodes.car %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  
  # pseudo entry points
  # buffered links
  links.cycle.buffered <- st_buffer(links.cycle, 30)
  links.walk.buffered <- st_buffer(links.walk, 30)
  links.car.buffered <- st_buffer(links.car, 30)
  
  # setup for parallel processing and progress reporting
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  pb <- txtProgressBar(max = nrow(dest.large.poly), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  echo(paste("Finding boundary points for", nrow(dest.large.poly), 
             "parks and schools; parallel processing with", cores, "cores\n"))
  
  # loop to find list of boundary points
  boundary.points <-
    foreach(i = 1:nrow(dest.large.poly),
            # foreach(i = 1:8,
            # .combine = rbind,
            .packages = c("dplyr", "sf"),
            .options.snow = opts) %dopar% {
              
              dest <- dest.large.poly[i,]
              
              dest.boundary.points <- dest %>% 
                # convert destination polygons boundaries to linestring
                st_cast(to = "MULTILINESTRING") %>%
                st_cast(to = "LINESTRING") %>%
                # locate points at 20m along boundaries
                st_line_sample(., density = units::set_units(20, m))
              
              return(dest.boundary.points)
            }
  
  # convert the list of boundary points into a dataframe
  # extract coordinates and ids (row numbers, corresponding to dest.large.poly rows)
  coordinates <- lapply(boundary.points, function(x) st_coordinates(x[[1]]))
  ids <- seq_along(boundary.points)
  # make dataframe of ids and x and y coordinates
  boundary.df <- data.frame(
    id = rep(ids, sapply(coordinates, nrow)),
    x = unlist(lapply(coordinates, function(x) x[,1])),
    y = unlist(lapply(coordinates, function(x) x[,2])))
  # convert to sf object
  boundary.sf <- st_as_sf(boundary.df, coords = c("x", "y")) %>%
    st_set_crs(outputCrs)
  
  # pseudo entry points with nodes
  pseudo.entry.nodes.cycle <- boundary.sf %>%
    # keep only those within 30m of cyclable links
    st_filter(links.cycle.buffered, .predicate = st_intersects) %>%
    # nearest nodes to the entry points
    mutate(node_cycle = nodes.cycle$id[st_nearest_feature(., nodes.cycle)]) %>%
    st_drop_geometry()
  
  pseudo.entry.nodes.walk <- boundary.sf %>%
    # keep only those within 30m of walkable links
    st_filter(links.walk.buffered, .predicate = st_intersects) %>%
    # nearest nodes to the entry points
    mutate(node_walk = nodes.walk$id[st_nearest_feature(., nodes.walk)]) %>%
    st_drop_geometry()
  
  pseudo.entry.nodes.car <- boundary.sf %>%
    # keep only those within 30m of driveable links
    st_filter(links.car.buffered, .predicate = st_intersects) %>%
    # nearest nodes to the entry points
    mutate(node_car = nodes.car$id[st_nearest_feature(., nodes.car)]) %>%
    st_drop_geometry()
  
  # loop through large destination polygons, find their internal and pseudo entry
  # nodes (and fallback nearest node), and combine
  echo(paste("Assembling entry points for", nrow(dest.large.poly), 
             "parks and schools; parallel processing with", cores, "cores\n"))
  
  # loop to assemble entry nodes
  dest.large.poly.with.nodes <-
    foreach(i = 1:nrow(dest.large.poly),
            # foreach(i = 1:8,
            .combine = rbind,
            .packages = c("dplyr"),
            .options.snow = opts) %dopar% {
              
              dest <- dest.large.poly[i,]
              
              # nodes within features (identifier is 'dest_id')
              dest.internal.nodes.cycle <- internal.nodes.cycle %>%
                filter(dest_id == dest$dest_id) %>%
                .$id
              dest.internal.nodes.walk <- internal.nodes.walk %>%
                filter(dest_id == dest$dest_id) %>%
                .$id
              dest.internal.nodes.car <- internal.nodes.car %>%
                filter(dest_id == dest$dest_id) %>%
                .$id
               
              # pseudo entry nodes (identifier is 'id', which is row number)
              dest.pseudo.entry.nodes.cycle <- pseudo.entry.nodes.cycle %>%
                filter(id == i) %>%
                .$node_cycle
              dest.pseudo.entry.nodes.walk <- pseudo.entry.nodes.walk %>%
                filter(id == i) %>%
                .$node_walk
              dest.pseudo.entry.nodes.car <- pseudo.entry.nodes.car %>%
                filter(id == i) %>%
                .$node_car
              
              # combined nodes
              entry.nodes.cycle <- unique(c(dest.internal.nodes.cycle, dest.pseudo.entry.nodes.cycle))
              entry.nodes.walk <- unique(c(dest.internal.nodes.walk, dest.pseudo.entry.nodes.walk))
              entry.nodes.car <- unique(c(dest.internal.nodes.car, dest.pseudo.entry.nodes.car))
              
              # fallback if no internal or pseudo entry nodes
              if (length(entry.nodes.cycle) == 0) {
                entry.nodes.cycle <- nodes.cycle$id[st_nearest_feature(dest, nodes.cycle)]
              }
              if (length(entry.nodes.walk) == 0) {
                entry.nodes.walk <- nodes.walk$id[st_nearest_feature(dest, nodes.walk)]
              }
              if (length(entry.nodes.car) == 0) {
                entry.nodes.car <- nodes.car$id[st_nearest_feature(dest, nodes.car)]
              }
              
              # convert the entry nodes to strings, and add to the dest row
              dest$node_cycle <- toString(entry.nodes.cycle)
              dest$node_walk <- toString(entry.nodes.walk)
              dest$node_car <- toString(entry.nodes.car)
              
              return(dest)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  
  # combine all destinations for output ----
  dest.with.nodes <- bind_rows(dest.small.with.nodes %>%
                                 mutate(node_cycle = as.character(node_cycle),
                                        node_walk = as.character(node_walk),
                                        node_car = as.character(node_car)),
                               dest.large.pt.with.nodes %>%
                                 mutate(node_cycle = as.character(node_cycle),
                                        node_walk = as.character(node_walk),
                                        node_car = as.character(node_car)),
                               dest.large.poly.with.nodes) %>%
    relocate(dest_id) %>%
    relocate(dest_type, .after = dest_id) %>%
    relocate(node_cycle, .after = dest_type) %>%
    relocate(node_walk, .after = node_cycle) %>%
    relocate(node_car, .after = node_walk) %>%
    relocate(other_tags, .after = last_col())
  
  return(dest.with.nodes)
  
}

