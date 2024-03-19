# function to split paths at points within a given distance of the path, 
# using matching field (such as osm_id) in both paths and points

splitPathsAtPoints <- function(paths, points, buff.dist, field) {
  
  # paths = paths
  # points = intersections
  # buff.dist = 0.001  # distance to which points are buffered to split
  # field = "osm_id"
  
  # paths = paths.to.resplit
  # points = endpoints.with.path.id
  # buff.dist = 0.1
  # field = "path_id"
  
  # run function once if no more than 100k paths, otherwise break into groups
  if (nrow(paths) <= 100000) {
    
    # buffer points to buffer distance
    echo("Buffering points to split paths\n")
    buffered.points <- st_buffer(points, buff.dist)
    
    # run the loop to split paths at points
    split.path.list <- splitPathsAtPointsParallelLoop(paths, 
                                                      buffered.points,
                                                      field)
    
  } else {
    
    # break larger path sets into groups for speed and memory
    
    group.nos <- seq(1:ceiling(nrow(paths) / 100000))  # groups of up to 100000
    groups <- c()
    
    for (i in group.nos) {
      
      echo(paste("Splitting group", i, "of", max(group.nos), "groups of paths\n"))
      
      # group of paths
      start.no <- ((group.nos[i] - 1) * 100000) + 1
      end.no <- min(group.nos[i] * 100000, nrow(paths))
      group.paths <- paths[start.no:end.no, ]
      
      # points used in that group
      group.points <- points %>% 
        filter(osm_id %in% group.paths$osm_id)
      
      # buffer points to buffer distance
      echo("Buffering points to split paths\n")
      group.buffered.points <- st_buffer(group.points, buff.dist)

      # for the group, run the loop to split paths at points
      group.split.path.list <- 
        splitPathsAtPointsParallelLoop(group.paths, 
                                       group.buffered.points,
                                       field)
      
      # temporarily save output
      group.output.name <- paste0("split_path_list_", i)
      saveRDS(group.split.path.list, paste0("./", group.output.name, ".rds"))
      groups <- c(groups, group.output.name)
      
    }
    
    # retrieve and assemble temporarily saved outputs
    split.path.list <- list()
    for (i in 1:length(groups)) {
      # read in the RDS and add it to split.path.list
      rds.path <- paste0("./", groups[i], ".rds")
      split.path.list <- c(split.path.list, readRDS(rds.path))
      # delete the RDS
      unlink(rds.path)
      
    }

  }

  return(split.path.list)
  
}


splitPathsAtPointsParallelLoop <- function(paths, 
                                           buffered.points,
                                           field) {
  
  # setup for parallel processing - detect no of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # set up progress reporting 
  pb <- txtProgressBar(max = nrow(paths), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report progress
  echo(paste("Splitting", nrow(paths), "paths; parallel processing with",
             cores, "cores\n"))
  
  # loop to split paths
  split.path.list <-
    foreach(i = 1:nrow(paths),
            # foreach(i = 1:10,
            .packages = c("dplyr", "sf"), 
            .options.snow = opts) %dopar% {
              
              path <- paths[i,]
              
              # intersections with the same variable field name (say, 'osm_id) as 
              # path: '.data[[field]]' is the variable field in the current data, 
              # which is 'buffered points'; 'path[[field]]' is the same field in 'path'
              
              path.intersections <- buffered.points %>%
                filter(.data[[field]] == path[[field]])
              
              
              # split paths at intersections
              if(nrow(path.intersections) > 0) {
                
                path.list <- path %>% st_geometry()
                
                path.intersections.list <- path.intersections %>%
                  # convert to multipoint
                  summarise() %>%
                  st_geometry()
                
                path.list.segmented <- st_difference(path.list, 
                                                     path.intersections.list)
                split.path <- path %>%
                  st_set_geometry(st_sfc(path.list.segmented)) %>%
                  st_sf() %>%
                  st_cast(to = "LINESTRING")
                
              } else {
                split.path <- path
              }
              
              return(split.path)
              
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  return(split.path.list)
  
}
