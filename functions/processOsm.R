# function to convert OSM .gpkg file into network of nodes and edges


library(sf)
library(fs)
library(dplyr)
library(lwgeom)
library(nngeo)
library(stringr)
library(doSNOW)
library(parallel)
library(foreach)
library(ggplot2)
source("./functions/etc/logging.R")
source("./functions/splitPathsAtPoints.R")
outputCrs = 7899

processOsm <- function(osmGpkg, outputCrs) {
  
  osmGpkg = "../data/processed/bendigo_osm.gpkg"
  # osmGpkg = "../data/processed/melbourne_osm.gpkg"
  
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
  
  # intersection points - intersect paths with a copy of itself, select points, 
  # keep where bridge/tunnel matches
  echo("Finding path intersections\n")
  system.time(
  intersections <- paths %>%
    # intersect with itself -  produces a separate point for each pair of links
    # that intersect at an intersection (and also produces line intersections)
    st_intersection(., paths %>% dplyr::select(bridge_tunnel_a = bridge_tunnel)) %>%
    # keep just the points
    st_collection_extract("POINT") %>%
    # only keep where bridge_tunnel match (at grade, both bridge or both tunnel)
    filter(bridge_tunnel == bridge_tunnel_a) %>%
    # combine where same location with same osm_id
    group_by(osm_id, geom) %>%
    summarise() %>%
    ungroup()
  )
  

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
  echo("Combining the split paths into a single dataframe")
  system.time(
  split.paths <- bind_rows(split.path.list) %>% 
    st_snap_to_grid(1) %>%
    # remove empty geometries
    filter(!st_is_empty(geom)) %>%
  # add unique id
    mutate(path_id = row_number())
  )

  # find endpoints of each split path
  echo("Finding endpoints of the split paths")
  endpoints <- rbind(lwgeom::st_startpoint(split.paths) %>% st_sf(),
                     lwgeom::st_endpoint(split.paths) %>% st_sf()) %>%
    # remove duplicates (produces multipoint)
    summarise() %>%
    # convert multipoint to point
    st_cast("POINT") %>%
    # add unique id
    mutate(endpoint_id = row_number())
  
  # find split.paths that have more than 2 endpoints within 0.1m, in order
  # to re-split at ajdacent endpoints
  echo("Finding paths that need to be re-split")
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
  
  
  # do a second round of splitting: re-split the paths that have adjacent endpoints,
  # using 0.1 distance this time, but only where adjacent endpoint is an endpoint
  # for a path that has the same bridge_tunnel status as the path to be resplit
  echo(paste("Re-splitting", nrow(paths.to.resplit), "paths at adjacent endpoints"))
  
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
  echo("Combining the resplit paths into a single dataframe")
  system.time(
    resplit.paths <- bind_rows(resplit.path.list) %>% 
      st_snap_to_grid(1) %>%
      filter(!st_is_empty(geom))
  )
  
  # remove paths that needed to be resplit, and replace with resplit paths
  combined.paths <- split.paths %>%
    filter(!path_id %in% paths.to.resplit$path_id) %>%
    rbind(resplit.paths) %>%
    dplyr::select(osm_id) %>%
    # add a new id field, for joining to from and to id's
    mutate(combined_path_id = row_number())

  # temp dev notes (SP): 
  # (1) compared to network.sql, the second round only resplits at adjacent 
  #     endpoints if those adjacent endpoints are on paths with the same 
  #     bridge_tunnel status as the path to be resplit
  # (2) however, resplitting at adjacent endpoints can result in multilevel
  #     intersections, for example in shopping centres or multi-storey carparks, 
  #     which could be avoided (to some extent) by also matching on the 'layer'
  #     or 'level' tag status

  
  # finalise paths
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
  final.paths <- combined.paths %>%
    # add length column
    mutate(length = as.integer(st_length(geom))) %>%
    
    # join from_id and to_id
    left_join(from_ids, by = "combined_path_id") %>%
    left_join(to_ids, by = "combined_path_id") %>%
    
    # select final fields
    dplyr::select(osm_id, length, from_id, to_id)
    
 
  
  
  
  # nodes.....
  # -----------------------------------#
  
  
  # extract the traffic signals  [MOVE THIS DOWN TO THE BIT DEALING WITH INTERSECTIONS]
  traffic.signals <- osm.points %>%
    # filter to traffic signals
    filter(str_detect(highway, "traffic_signals")) %>%
    # snap to grid and retain relevant fields
    st_snap_to_grid(1) %>%
    dplyr::select(osm_id, highway, other_tags)
  
  
  
  
  
  
  #------ working section
  
  # # read in temporary tables
  # sql.tables <- "./SP_working/temp_melb_sql_tables.sqlite"
  # roads <- st_read(sql.tables, layer = "roads")
  # 
  # write out temporary Bendigo outputs
  bend.out <- "./SP_working/temp_bendigo.sqlite"
  st_write(paths, bend.out, layer = "paths", delete_layer = T)
  st_write(intersections, bend.out, layer = "intersections", delete_layer = T)
  st_write(split.paths, bend.out, layer = "split_paths", delete_layer = T)
  st_write(endpoints, bend.out, layer = "endpoints", delete_layer = T)
  st_write(final.paths, bend.out, layer = "final_paths", delete_layer = T)

  st_write(split.paths, bend.out, layer = "split_paths_diff_loop", delete_layer = T)
  st_write(paths.to.resplit, bend.out, layer = "paths_to_resplit", delete_layer = T)

  st_delete(bend.out, layer = "split_paths_unbuffered_point_diff")

  paths <- st_read(bend.out, layer = "paths")
  intersections <- st_read(bend.out, layer = "intersections")
  split.paths <- st_read(bend.out, layer = "split_paths")
  endpoints <- st_read(bend.out, layer = "endpoints")
  final.paths <- st_read(bend.out, layer = "final_paths")
  
  # write out / read in temporary Melbourne outputs
  melb.out <- "./SP_working/temp_melbourne.sqlite"
  st_write(paths, melb.out, layer = "paths", delete_layer = T)
  st_write(intersections, melb.out, layer = "intersections", delete_layer = T)
  st_write(split.paths, melb.out, layer = "split_paths", delete_layer = T)
  st_write(endpoints, melb.out, layer = "endpoints", delete_layer = T)
  st_write(final.paths, melb.out, layer = "final_paths", delete_layer = T)
  
  st_write(paths.to.resplit, melb.out, layer = "paths_to_resplit", delete_layer = T)
  st_write(problem.paths, melb.out, layer = "problem_paths", delete_layer = T)
  
  st_delete(melb.out, layer = "problem_paths")
  
  paths <- st_read(melb.out, layer = "paths")
  intersections <- st_read(melb.out, layer = "intersections")
  split.paths <- st_read(melb.out, layer = "split_paths")
  endpoints <- st_read(melb.out, layer = "endpoints")
  final.paths <- st_read(melb.out, layer = "final_paths")
  
  #--------end working section
  
  # temporary return statement for testing
  return(list(paths, intersections, endpoints, final.paths))
  
  
  
}

