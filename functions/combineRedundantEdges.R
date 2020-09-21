# nodes_current<-intersectionsSimplified[[1]]
# edges_current<-intersectionsSimplified[[2]]

combineRedundantEdges <- function(nodes_current,edges_current){
  # every edge needs a unique id, so the correct geometry can be selected
  edges_current <- edges_current %>%
    mutate(uid=row_number()) %>%
    # Only road edges should count towards the number of lanes
    mutate(permlanes=ifelse(is_car==0,0,permlanes))

  # one-way edges
  edges_directed <- edges_current %>%
    filter(is_oneway==1) %>%
    st_drop_geometry() %>%
    group_by(from_id,to_id) %>%
    mutate(current_group=cur_group_id()) %>%
    ungroup()
  
  # Find the shortest geometry of each directed edge with the same from and to
  # ids. This is the one that will be used when merging. Note: not using actual
  # geometry column as it would be too slow. Instead using uid to refer to it.
  edges_directed_shortest_geom <- edges_directed %>%
    group_by(current_group) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    dplyr::select(uid,current_group,length)
  
  # Now merging multiple one-way lanes going in the same direction, replacing
  # the length and geometry with the shortest one in the group.
  edges_directed2 <- edges_directed %>%
    dplyr::select(-uid,-length) %>%
    inner_join(edges_directed_shortest_geom, by="current_group") %>%
    group_by(current_group) %>%
    summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
              from_id=min(from_id,na.rm=T),to_id=min(to_id,na.rm=T),
              freespeed=max(freespeed,na.rm=T),permlanes=sum(permlanes,na.rm=T),
              capacity=sum(capacity,na.rm=T),is_oneway=max(is_oneway,na.rm=T),
              bikeway=max(bikeway,na.rm=T),
              is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
              is_car=max(is_car,na.rm=T)) %>%
    dplyr::select(-current_group)
  
  # Grouping pairs of one-way edges going in opposite directions. These will
  # be merged into two-way edges
  edges_directed3 <- edges_directed2 %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    group_by(min_from_id,min_to_id) %>%
    # group_count will be used to determine if it's a one or two-way road
    mutate(current_group=cur_group_id(),group_count=n()) %>%
    ungroup()
  
  # The shortest geometry of directed edge with the same from and to ids,
  # even if going in opposite directions
  edges_directed3_shortest_geom <- edges_directed3 %>%
    group_by(current_group) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    dplyr::select(uid,from_id,to_id,current_group,length)
  
  # Now merging multiple one-way lanes going in opposite directions, replacing
  # the length and geometry with the shortest one in the group and making them
  # two-way lanes.
  edges_directed4 <- edges_directed3 %>%
    dplyr::select(-uid,-length,-from_id,-to_id) %>%
    inner_join(edges_directed3_shortest_geom, by="current_group") %>%
    group_by(current_group) %>%
    summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
              from_id=min(from_id,na.rm=T),to_id=min(to_id,na.rm=T),
              freespeed=max(freespeed,na.rm=T),permlanes=sum(permlanes,na.rm=T),
              capacity=sum(capacity,na.rm=T),is_oneway=max(is_oneway,na.rm=T),
              bikeway=max(bikeway,na.rm=T),
              is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
              is_car=max(is_car,na.rm=T),group_count=max(group_count,na.rm=T)) %>%
    dplyr::select(-current_group) %>%
    # if the group_count is greater than one, then the road is two-way
    mutate(is_oneway=ifelse(group_count>1,0,1)) %>%
    dplyr::select(-group_count)
  
  # Adding the directed edges that have merged into undirected edges to the 
  # original undirected edges
  edges_undirected <- bind_rows(
    edges_directed4 %>%
      filter(is_oneway==0),
    edges_current %>% 
      st_drop_geometry() %>%
      filter(is_oneway==0) %>%
      dplyr::select(uid,length,from_id,to_id,freespeed,permlanes,capacity,
                    is_oneway,bikeway,is_cycle,is_walk,is_car)
  )
  
  
  # Grouping undirected edges even if going in opposite directions.
  edges_undirected2 <- edges_undirected %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    group_by(min_from_id,min_to_id) %>%
    mutate(current_group=cur_group_id()) %>%
    ungroup()
  
  # The shortest geometry of each group
  edges_undirected2_shortest_geom <- edges_undirected2 %>%
    group_by(current_group) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    dplyr::select(uid,from_id,to_id,current_group,length)
  
  # Merging undirected edges
  edges_undirected3 <- edges_undirected2 %>%
    dplyr::select(-uid,-length,-from_id,-to_id) %>%
    inner_join(edges_undirected2_shortest_geom, by="current_group") %>%
    group_by(current_group) %>%
    summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
              from_id=min(from_id,na.rm=T),to_id=min(to_id,na.rm=T),
              freespeed=max(freespeed,na.rm=T),permlanes=sum(permlanes,na.rm=T),
              capacity=sum(capacity,na.rm=T),is_oneway=max(is_oneway,na.rm=T),
              bikeway=max(bikeway,na.rm=T),
              is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
              is_car=max(is_car,na.rm=T)) %>%
    dplyr::select(-current_group)
  
  # Adding the undirected and directed edges, and setting any edges with zero
  # lanes to one.
  edges_all <-  bind_rows(
    edges_undirected3,
    edges_directed4%>%filter(is_oneway==1)
  ) %>%
    mutate(permlanes=ifelse(permlanes==0,1,permlanes))
  
  # Finding the geometry
  edges_geom <- edges_current %>%
    dplyr::select(uid) %>%
    filter(uid %in% edges_all$uid)
  
  # Attaching the geometry
  edges_all_geom <- edges_geom %>%
    inner_join(edges_all, by="uid") %>%
    dplyr::select(-uid) %>%
    st_sf() %>%
    st_set_crs(28355)
  return(list(nodes_current,edges_all_geom))
}


