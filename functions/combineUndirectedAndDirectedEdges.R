# nodes_current<-edgesCombined[[1]]
# edges_current<-edgesCombined[[2]]

combineUndirectedAndDirectedEdges <- function(nodes_current,edges_current,outputCrs){
  
  edges_current <- edges_current %>%
    mutate(uid=row_number()) %>%
    # Only road edges should count towards the number of lanes
    mutate(permlanes=ifelse(is_car==0,0,permlanes))

  # Group edges with the same from and to ids, even if going in opposite
  # directions or a mixture of one-way and two-way edges
  edges_grouped <- edges_current %>%
    st_drop_geometry() %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    # need to preserve from id, to id order for directed roads
    mutate(from_id_directed=ifelse(is_oneway==1,from_id,-1)) %>%
    mutate(to_id_directed=ifelse(is_oneway==1,to_id,-1)) %>%
    # if there is an undirected road, need to keep it two-way
    # note: using 0 to mean true as it will be filling in is_oneway
    mutate(undirected_road=ifelse(is_oneway==0 & is_car==1,0,1)) %>%
    group_by(min_from_id,min_to_id) %>%
    mutate(current_group=cur_group_id()) %>%
    ungroup()
  
  # Find the shortest geometry of each group of edges. This will be used when
  # merging. Note: not using actual geometry column as it would be too slow.
  # Instead using uid to refer to it.
  edges_grouped_shortest_geom <- edges_grouped %>%
    group_by(current_group) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    dplyr::select(uid,from_id,to_id,current_group,length)
  
  # Merging one-way and two-way lanes
  # we take the min of undirected_road to ensure that merging one-way and two-way lanes
  # results in a two-way edge if there is an undirected road present.
  edges_grouped2 <- edges_grouped %>%
    dplyr::select(-uid,-length,-from_id,-to_id) %>%
    inner_join(edges_grouped_shortest_geom, by="current_group") %>%
    group_by(current_group) %>%
    summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
              osm_id=paste(as.character(osm_id),collapse = "_"),
              from_id=min(min_from_id,na.rm=T),to_id=max(min_to_id,na.rm=T),
              freespeed_max=max(freespeed,na.rm = T),
              freespeed=weighted.mean(freespeed,w=(permlanes*laneCapacity),na.rm=T),
              laneCapacity_sum=sum(laneCapacity,na.rm=T),
              laneCapacity=weighted.mean(laneCapacity,w=permlanes,na.rm=T),
              permlanes=sum(permlanes,na.rm=T),
              is_oneway=min(undirected_road,na.rm=T),
              cycleway=max(cycleway,na.rm=T),
              highway_order=min(highway_order,na.rm=T), # selecting the highest rank
              # surface=surface[which.max(length[!is.na(surface)])], # Take the max length surface type
              surface=surface[which.max(length)], # Take the max length surface type
              is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
              is_car=max(is_car,na.rm=T),
              from_id_directed=max(from_id_directed,na.rm=T),
              to_id_directed=max(to_id_directed,na.rm=T)) %>%
    # If non-car links were merged, sum of weights will be 0, so using max speed
    mutate(freespeed=ifelse(freespeed=="NaN",freespeed_max,freespeed)) %>% 
    # If non-car links were merged, sum of weights will be 0, so using sum capacity
    mutate(laneCapacity=ifelse(laneCapacity=="NaN",laneCapacity_sum,laneCapacity)) %>% 
    # directed edges need the from id and to id in the correct order
    mutate(from_id=ifelse(from_id_directed==-1,from_id,from_id_directed)) %>%
    mutate(to_id=ifelse(to_id_directed==-1,to_id,to_id_directed)) %>%
    # Setting any edges with zero lanes to one.
    mutate(permlanes=ifelse(permlanes==0,1,permlanes)) %>%
    # Only road edges can be one way
    mutate(is_oneway=ifelse(is_car==0,0,is_oneway)) %>%
    dplyr::select(-current_group,-from_id_directed,-to_id_directed)
  
  # geometry of shortest edges
  edges_geom <- edges_current %>%
    dplyr::select(uid) %>%
    filter(uid %in% edges_grouped2$uid)
  
  # adding geometry to groups
  edges_all_geom <- edges_geom %>%
    inner_join(edges_grouped2, by="uid") %>%
    dplyr::select(-uid) %>%
    st_sf() %>%
    st_set_crs(outputCrs)

  return(list(nodes_current,edges_all_geom))
}


