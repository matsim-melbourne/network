# function to convert OSM .gpkg file into network of nodes and edges

processOsm <- function(osmGpkg, outputCrs) {
  
  # osmGpkg = "./output/bendigo_osm.gpkg"
  # osmGpkg = "./output/melbourne_osm.gpkg"
  # outputCrs = 7899
  
  # read in OSM data
  # -----------------------------------#
  
  # read in osmGpkg lines and points
  osm.lines <- st_read(osmGpkg, layer = "lines") %>%
    st_set_geometry("geom")
  osm.points <- st_read(osmGpkg, layer = "points") %>%
    st_set_geometry("geom")
  
  
  # paths and intersections
  # -----------------------------------#

  # extract the roads and other paths
  echo("Extracting paths from OSM data\n")
  paths <- osm.lines %>%
    # filter to highways
    filter(!is.na(highway)) %>%
    # exclude specific highway types
    filter(!highway %in% c("bridleway", "bus_stop", "co", "platform",
                              "raceway", "services", "traffic_island")) %>%
    # exclude non-current highways
    filter(!str_detect(highway, "construction|proposed|disused|abandoned")) %>%
    # exclude other tags indicating not usable or current
    filter(is.na(other_tags) |
             !str_detect(other_tags, '"access"=>"private"|"abandoned"=>"yes"')) %>%
    # snap to grid and retain relevant fields
    st_snap_to_grid(1) %>%
    dplyr::select(osm_id, highway, other_tags) %>%
    # add bridge/tunnel column (0 for neither, 1 for bridge, 2 for tunnel)
    mutate(bridge_tunnel = case_when(str_detect(other_tags, "bridge") ~ 1,
                                      str_detect(other_tags, "tunnel") ~ 2,
                                      TRUE ~ 0))
  
  # temp dev notes, comparing to processOSM.sh (SP)
  # (1) I did not remove other_tags LIKE 'busbar'; these are electrical facilities, 
  #     but they are all highway=NA, so are removed anyway 
  # (2) I did not remove all other_tags LIKE 'abandoned'; this is too broad, and
  #     removes cycleways that are "railway"=>"abandoned" or similar; instead,
  #     narrowed to "abandoned"=>"yes"
  # (3) separate codes for bridges and tunnels, rather than 'TRUE' for both
  # (4) some paths have a 'level' or 'layer' tag that could be used to separate
  #     out different levels; however these are mostly within shopping centres
  #     or multi-storey carparks, and probably aren't of much interest to us
  
  
  # find intersections, but excluding any on different levels as
  # determined by bridge_tunnel
  echo("Finding path intersections\n")
  
  # intersect paths with a copy of itself - produces point for each pair of links
  # that intersect at an intersection (and also produces line intersections)
  intersection.base <- paths %>%
    st_intersection(., paths %>% 
                      dplyr::select(osm_id_a = osm_id, 
                                    bridge_tunnel_a = bridge_tunnel))
  
  # point intersections that are on the same level
  intersection.points <- intersection.base %>%
    # keep just the points where bridge_tunnel match (at grade, both bridge or both tunnel)
    st_collection_extract("POINT") %>%
    filter(bridge_tunnel == bridge_tunnel_a)
  
  # line intersections that are on the same level, and that are not the same osm_id
  # - that is, keep where there are two overlapping lines in osm with separate osm_id's
  intersection.lines <- intersection.base %>%
    st_collection_extract("LINESTRING") %>%
    filter(bridge_tunnel == bridge_tunnel_a &
             osm_id != osm_id_a)
  
  # combine the point intersections and the line start/end points, with the
  # osm_ids of their paths (used for splitting the lines)
  intersections <- bind_rows(
    # points
    intersection.points,
    # line start points (with the osm_id's of their paths)
    intersection.lines %>% 
      st_startpoint() %>% st_sf() %>% st_set_geometry("geom") %>%
      cbind(osm_id = intersection.lines$osm_id), 
    # line end points (with the osm_id's of their paths)
    intersection.lines %>% 
      st_endpoint() %>% st_sf() %>% st_set_geometry("geom") %>%
      cbind(osm_id = intersection.lines$osm_id)
  ) %>%
  # combine where same location with same osm_id
    group_by(osm_id, geom) %>%
    summarise() %>%
    ungroup()


  # temp dev notes (SP): 
  # (1) compared to network.sql, this only places intersections where both are 
  #     bridges, both are tunnels, or both are neither (whereas network.sql, 
  #     could make intersection where one is bridge and one is tunnel)
  # (2) this will not succeed in excluding intersections for multi-level bridges 
  #     over the top of each other (but in the absence of Z data, I don't have 
  #     any good ideas for doing that)
  
  
  # split paths at intersections
  # -----------------------------------#

  # split paths at intersections with matching osm_ids
  echo(paste("Splitting", nrow(paths), "paths at intersections\n"))
  split.path.list <- splitPathsAtPoints(paths, intersections, 0.001, "osm_id")
  
  # convert to dataframe, snap to grid, remove empty geometries, add unique id
  echo("Combining the split paths into a single dataframe\n")
  system.time(
  split.paths <- bind_rows(split.path.list) %>% 
    st_snap_to_grid(1) %>%
    # remove empty geometries
    filter(!st_is_empty(geom)) %>%
  # add unique id
    mutate(path_id = row_number())
  )

  # find endpoints of each split path
  echo("Finding endpoints of the split paths\n")
  endpoints <- rbind(lwgeom::st_startpoint(split.paths) %>% st_sf(),
                     lwgeom::st_endpoint(split.paths) %>% st_sf()) %>%
    # remove duplicates (produces multipoint)
    summarise() %>%
    # convert multipoint to point
    st_cast("POINT") %>%
    # add unique id and set geometry column name
    mutate(endpoint_id = row_number()) %>%
    st_set_geometry("geom")
  
  # find split.paths that have more than 2 endpoints within 0.1m, in order
  # to re-split at ajdacent endpoints
  echo("Finding paths that need to be re-split\n")
  paths.with.nearby.endpoints <- split.paths %>%
    # joint endpoints within 0.1m
    st_join(endpoints %>% st_buffer(0.1), join = st_intersects)
    
  multiple.endpoint.paths <- paths.with.nearby.endpoints %>%
    # count endpoints
    st_drop_geometry() %>%
    group_by(path_id) %>%
    summarise(n_endpoints = n()) %>%
    ungroup() %>%
    # filter to those with more than 2
    filter(n_endpoints > 2)
  
  # get the paths with more than 2 endpoints - these need to be resplit
  paths.to.resplit <- split.paths %>%
    filter(path_id %in% multiple.endpoint.paths$path_id)
  
  
  # do a second round of splitting, if needed: re-split the paths that have adjacent endpoints,
  # using 0.1 distance this time, but only where adjacent endpoint is an endpoint
  # for a path that has the same bridge_tunnel status as the path to be resplit
  if (nrow(paths.to.resplit) > 0) {
    echo(paste("Re-splitting", nrow(paths.to.resplit), "paths at adjacent endpoints\n"))
    
    endpoints.for.resplit <- paths.with.nearby.endpoints %>%
      # just keep paths that need to be resplit, with their bridge_tunnel status
      filter(path_id %in% paths.to.resplit$path_id) %>%
      st_drop_geometry() %>%
      rename(path_bridge_tunnel = bridge_tunnel) %>%
      # join the endpoint geometries
      left_join(endpoints, by = "endpoint_id") %>%
      st_sf()  %>%
      # join the bridge_tunnel status of each path that intersects the endpoint
      # (this is the endpoint's bridge_tunnel status, but it could have more than one,
      # say where a path enters a tunnel)
      st_join(paths %>% dplyr::select(endpoint_bridge_tunnel = bridge_tunnel), 
              join = st_intersects) %>%
      # only keep the endpoints if bridge_tunnel status for the endpoint
      # matches the bridge_tunnel status of the path to be resplit
      filter(path_bridge_tunnel == endpoint_bridge_tunnel) %>%
      distinct()
    
    resplit.path.list <- 
      splitPathsAtPoints(paths.to.resplit, endpoints.for.resplit, 0.1, "path_id")
    
    # convert to dataframe, snap to grid, remove empty geometries
    echo("Combining the resplit paths into a single dataframe\n")
    system.time(
      resplit.paths <- bind_rows(resplit.path.list) %>% 
        st_snap_to_grid(1) %>%
        filter(!st_is_empty(geom))
    )
    
    # remove paths that needed to be resplit, and replace with resplit paths
    combined.paths <- split.paths %>%
      filter(!path_id %in% paths.to.resplit$path_id) %>%
      rbind(resplit.paths) %>%
      # add a new id field, for joining to from and to id's
      mutate(combined_path_id = row_number())
    
  } else {
    combined.paths <- split.paths %>%
      # add a new id field, for joining to from and to id's
      mutate(combined_path_id = row_number())
  }
  
  # temp dev notes (SP): 
  # (1) compared to network.sql, the second round only resplits at adjacent 
  #     endpoints if those adjacent endpoints are on paths with the same 
  #     bridge_tunnel status as the path to be resplit
  # (2) however, resplitting at adjacent endpoints can result in multilevel
  #     intersections, for example in shopping centres or multi-storey carparks, 
  #     which could be avoided (to some extent) by also matching on the 'layer'
  #     or 'level' tag status

  
  # finalise paths with metadata
  # -----------------------------------#
  
  # find from and to id's from endpoints
  from_ids <- combined.paths %>%
    dplyr::select(combined_path_id) %>%
    # take startpoint jeometry, and do spatial join to endpoints
    st_set_geometry(lwgeom::st_startpoint(.)) %>%
    st_join(endpoints %>% rename(from_id = endpoint_id),
            join = st_intersects) %>%
    st_drop_geometry()
  
  to_ids <- combined.paths %>%
    dplyr::select(combined_path_id) %>%
    # take endpoint jeometry, and do spatial join to endpoints
    st_set_geometry(lwgeom::st_endpoint(.)) %>%
    st_join(endpoints %>% rename(to_id = endpoint_id),
            join = st_intersects) %>%
    st_drop_geometry()
  
  # assemble final paths with length, from_id and to_id
  final.paths.with.metadata <- combined.paths %>%
    # add length column
    mutate(length = as.integer(st_length(geom))) %>%
    
    # join from_id and to_id
    left_join(from_ids, by = "combined_path_id") %>%
    left_join(to_ids, by = "combined_path_id")
    
 
  # nodes
  # -----------------------------------#

  echo("Finding roundabout and traffic signal status of nodes\n")
  
  # extract from_id and to_id of edges that are roundabouts
  roundabout.edges <- final.paths.with.metadata %>%
    st_drop_geometry() %>%
    # left_join(osm.metadata, by = "osm_id") %>% # <<< don't now need this
    # is_roundabout: 1 if 'roundabout' in 'other_tags'; 0 if not, or if 'other_tags' is NA
    mutate(is_roundabout = case_when(
      is.na(other_tags)  ~ 0,
      str_detect(other_tags, "roundabout") ~ 1, 
      TRUE  ~ 0)) %>%
    dplyr::select(from_id, to_id, is_roundabout)
    
  # find node ids that connect to roundabouts
  roundabout.nodes <- roundabout.edges %>%
    filter(is_roundabout == 1) %>%
    # combine from_id and to_id columns into new 'id' column
    tidyr::gather(key = "key", value = "id", from_id, to_id) %>%
    # keep distinct ids
    dplyr::select(id) %>%
    distinct()
  
  # extract the traffic signals
  traffic.signals <- osm.points %>%
    # filter to traffic signals
    filter(str_detect(highway, "traffic_signals")) %>%
    # snap to grid and retain relevant fields
    st_snap_to_grid(1) %>%
    dplyr::select(osm_id, highway, other_tags)
  
  # find endpoints within 20m of traffic signals
  endpoints.near.signals <- endpoints %>%
    st_filter(st_buffer(traffic.signals, 20), .predicate = st_intersects) %>%
    .$endpoint_id
  
  # finalise nodes: attribute with roundabout and signal status
  final.nodes <- endpoints %>%
    mutate(is_roundabout = ifelse(endpoint_id %in% roundabout.nodes$id, 1, 0),
           is_signal = ifelse(endpoint_id %in% endpoints.near.signals, 1, 0)) %>%
    rename(id = endpoint_id)
  

  # separate final paths and osm metadata
  # -----------------------------------#
  
  # remove metadata from final paths
  final.paths <- final.paths.with.metadata %>%
    dplyr::select(osm_id, length, from_id, to_id)
  
  # extract the non-spatial data for the paths
  osm.metadata <- paths %>%
    st_drop_geometry() %>%
    dplyr::select(osm_id, highway, other_tags) %>%
    filter(osm_id %in% final.paths$osm_id)
  
  
  # return outputs
  # -----------------------------------#
  return(list(final.nodes, final.paths, osm.metadata))
  
}

