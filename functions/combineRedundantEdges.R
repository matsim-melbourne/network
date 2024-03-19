# nodes_current<-intersectionsSimplified[[1]]
# edges_current<-intersectionsSimplified[[2]]

combineRedundantEdges <- function(nodes_current,edges_current,outputCrs){
  
  # assuming a dataframe with a 'current_group' column, merge edges together
  groupingFunction <- function(grouped_edges) {
    # Find the shortest geometry of each group of edges. This will be used when
    # merging. Note: not using actual geometry column as it would be too slow.
    # Instead using uid to refer to it.
    edges_shortest_geom <- grouped_edges %>%
      group_by(current_group) %>%
      slice(which.min(length)) %>%
      ungroup() %>%
      dplyr::select(uid,current_group,length)
    # Now merging based on the 'current_group' column, replacing the length and
    # geometry with the shortest one in the group.
    grouped_edges_merged <- grouped_edges %>% 
      dplyr::select(-uid,-length) %>%
      inner_join(edges_shortest_geom, by="current_group") %>%
      group_by(current_group) %>% 
      summarise(uid=min(uid,na.rm=T),length=min(length,na.rm=T),
                osm_id=paste(as.character(osm_id), collapse = "_"),
                from_id=min(from_id,na.rm=T),to_id=max(to_id,na.rm=T),
                freespeed_max=max(freespeed,na.rm = T),
                freespeed=weighted.mean(freespeed,w=(permlanes*laneCapacity),na.rm=T),
                laneCapacity_sum=sum(laneCapacity,na.rm=T),
                laneCapacity=round(weighted.mean(laneCapacity,w=permlanes,na.rm=T)),
                permlanes=sum(permlanes,na.rm=T),
                is_oneway=max(is_oneway,na.rm=T),cycleway=max(cycleway,na.rm=T),
                highway_order=min(highway_order,na.rm=T), # selecting the highest rank
                # surface=surface[which.max(length[!is.na(surface)])], # Take the max length surface type
                surface=surface[which.max(length)], # Take the max length surface type
                is_cycle=max(is_cycle,na.rm=T),is_walk=max(is_walk,na.rm=T),
                is_car=max(is_car,na.rm=T),group_count=max(group_count,na.rm=T),
                laneCapacity_average=round(mean(laneCapacity,na.rm=T))) %>%
      dplyr::select(-current_group) %>%
      # If non-car links were merged, sum of weights will be 0, so using max speed
      mutate(freespeed=ifelse(freespeed=="NaN",freespeed_max,freespeed)) %>% 
      # If non-car links were merged, sum of weights will be 0, so using sum capacity
      mutate(laneCapacity=ifelse(laneCapacity=="NaN",laneCapacity_sum,laneCapacity)) %>% 
      ungroup()
    return(grouped_edges_merged)
  }
  
  # every edge needs a unique id, so the correct geometry can be selected
  edges_current <- edges_current %>%
    mutate(uid=row_number()) %>%
    # Only road edges should count towards the number of lanes
    mutate(permlanes=ifelse(is_car==0,0,permlanes)) %>%
    # Only road edges can be one way
    mutate(is_oneway=ifelse(is_car==0,0,is_oneway))  

  # one-way edges
  edges_directed <- edges_current %>%
    filter(is_oneway==1) %>%
    st_drop_geometry() %>%
    group_by(from_id,to_id) %>%
    mutate(current_group=cur_group_id(),group_count=n()) %>%
    ungroup()
  # Now merging multiple one-way lanes going in the same direction.
  # Note, nothing appears to get merged here.
  edges_directed_merged <- groupingFunction(edges_directed) %>%
    #rename(laneCapacity=laneCapacity_sum) %>%
    dplyr::select(-group_count,-laneCapacity_average)
  
  
  # Grouping pairs of one-way edges going in opposite directions. These will
  # be merged into two-way edges
  edges_directed_opposite <- edges_directed_merged %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    group_by(min_from_id,min_to_id) %>%
    # group_count will be used to determine if it's a one or two-way road
    mutate(current_group=cur_group_id(),group_count=n()) %>%
    ungroup()
  
  # Now merging multiple one-way lanes going in opposite directions, replacing
  # the length and geometry with the shortest one in the group and making them
  # two-way lanes.
  edges_directed_opposite_merged <- groupingFunction(edges_directed_opposite) %>%
    # if the group_count is greater than one, then the road is two-way
    mutate(is_oneway=ifelse(group_count>1,0,1)) %>%
    #rename(laneCapacity=laneCapacity_sum) %>%
    #mutate(laneCapacity=ifelse(group_count>1,laneCapacity_average,laneCapacity)) %>%
    dplyr::select(-group_count,-laneCapacity_average)
  
  # Adding the directed edges that have merged into undirected edges to the 
  # original undirected edges
  edges_undirected <- bind_rows(
    edges_directed_opposite_merged %>%
      filter(is_oneway==0),
    edges_current %>% 
      st_drop_geometry() %>%
      filter(is_oneway==0) %>%
      mutate(osm_id=as.character(osm_id)) %>% 
      dplyr::select(uid,length,osm_id,from_id,to_id,freespeed,permlanes,laneCapacity,
                    is_oneway,cycleway,highway_order,is_cycle,is_walk,is_car)
  )
  
  # Grouping undirected edges even if going in opposite directions.
  edges_undirected_grouped <- edges_undirected %>%
    mutate(min_from_id=ifelse(from_id<to_id,from_id,to_id)) %>%
    mutate(min_to_id=ifelse(to_id>from_id,to_id,from_id)) %>%
    group_by(min_from_id,min_to_id) %>%
    mutate(current_group=cur_group_id(),group_count=n()) %>%
    ungroup()
  
  # Merging undirected edges
  edges_undirected_merged <- groupingFunction(edges_undirected_grouped) %>%
    #rename(laneCapacity=laneCapacity_sum) %>%
    dplyr::select(-group_count,-laneCapacity_average)
  
  
  # Adding the undirected and directed edges, and setting any edges with zero
  # lanes to one.
  edges_all <- bind_rows(
    edges_undirected_merged,
    edges_directed_opposite_merged%>%filter(is_oneway==1)
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
    st_set_crs(outputCrs)

  return(list(nodes_current,edges_all_geom))
}
  
  
