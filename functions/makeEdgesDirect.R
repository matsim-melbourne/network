# nodes_current <-combinedUndirectedAndDirected2[[1]]
# edges_current <-combinedUndirectedAndDirected2[[2]]
makeEdgesDirect <- function(nodes_current,edges_current,outputCrs){
  
  # nodes_coords <- nodes_current %>%
  #   st_drop_geometry() %>%
  #   cbind(st_coordinates(nodes_current))
  
  # simplify the lines so they go straight between their from and to nodes
  edges_current <- edges_current %>%
    filter(!is.na(from_id)) %>%
    st_drop_geometry() %>%
    left_join(st_drop_geometry(nodes_current),by=c("from_id"="id")) %>%
    rename(fromX=X,fromY=Y) %>%
    left_join(st_drop_geometry(nodes_current),by=c("to_id"="id")) %>%
    rename(toX=X,toY=Y) %>%
    mutate(geom=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
    st_as_sf(wkt = "geom", crs = outputCrs)
  return(list(nodes_current,edges_current))
}


