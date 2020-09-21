# nodes_current<-edgesCombined[[1]]
# edges_current<-edgesCombined[[2]]

combineUndirectedAndDirectedEdges <- function(nodes_current,edges_current){
  
  edges_current <- edges_current %>%
    mutate(uid=row_number()) %>%
    # Only road edges should count towards the number of lanes
    mutate(permlanes=ifelse(is_car==0,0,permlanes))
  
  # Group edges with the dame from and to ids, even if going in opposite
  # directions or a mixture of one-way and two-way edges
  edges_grouped <- edges_current %>%
    st_drop_geometry() %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    group_by(min_from_id,min_to_id) %>%
    mutate(current_group=cur_group_id()) %>%
    ungroup()
  
  # Find the shortest geometry of each group. Note: not using actual geometry
  # column as it would be too slow. Instead using uid to refer to it.
  edges_grouped_shortest_geom <- edges_grouped %>%
    group_by(current_group) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    dplyr::select(uid,from_id,to_id,current_group,length)
  
  # Merging one-way and two-way lanes
  # we take the min of is_oneway to ensure that merging one-way and two-way lanes
  # results in a two-way edge
  edges_grouped2 <- edges_grouped %>%
    dplyr::select(-uid,-length,-from_id,-to_id) %>%
    inner_join(edges_grouped_shortest_geom, by="current_group") %>%
    group_by(current_group) %>%
    summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
              from_id=min(from_id,na.rm=T),to_id=min(to_id,na.rm=T),
              freespeed=max(freespeed,na.rm=T),permlanes=sum(permlanes,na.rm=T),
              capacity=sum(capacity,na.rm=T),is_oneway=min(is_oneway,na.rm=T),
              bikeway=max(bikeway,na.rm=T),
              is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
              is_car=max(is_car,na.rm=T)) %>%
    dplyr::select(-current_group) %>%
    # Setting any edges with zero lanes to one.
    mutate(permlanes=ifelse(permlanes==0,1,permlanes))
  
  # geometry of shortest edges
  edges_geom <- edges_current %>%
    dplyr::select(uid) %>%
    filter(uid %in% edges_grouped2$uid)
  
  # adding geometry to groups
  edges_all_geom <- edges_geom %>%
    inner_join(edges_grouped2, by="uid") %>%
    dplyr::select(-uid) %>%
    st_sf() %>%
    st_set_crs(28355)
  return(list(nodes_current,edges_all_geom))
}


