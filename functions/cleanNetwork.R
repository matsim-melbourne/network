cleanNetwork <- function(networkRestructured, network_modes = "car"){
  nodes_df <- networkRestructured[[1]]
  lines_df <- networkRestructured[[2]]
  get_biggest_component <- function(l_df,m){
    # Filtering links based on the mode
    l_df_mode <- l_df %>% filter(modes %like% m)
    # Making the graph for the intersections
    g <- graph_from_data_frame(dplyr::select(l_df_mode, from=from_id,to=to_id), directed = TRUE)
    #plot(g,  vertex.size=0.1, vertex.label=NA, vertex.color="red", edge.arrow.size=0, edge.curved = 0)
    
    # Getting components
    comp <- components(g)
    
    nodes_in_largest_component <- data.frame(segment_id=as.character(names(comp$membership)), cluster_id=comp$membership, row.names=NULL) %>%
      filter(cluster_id==which.max(comp$csize)) %>%
      pull(segment_id) %>%
      base::unique()
    
    #n_df_filtered <- n_df %>%
    #  filter(id%in%nodes_in_largest_component)
    # AB 2020-08-14: I think this should be: l_df_filtered <- lines_df %>%
    # This would include walk paths provided they don't have any walk exclusive
    # nodes.
    l_df_filtered <- l_df_mode %>%
      filter(from_id%in%nodes_in_largest_component & to_id%in%nodes_in_largest_component)
    
    return(l_df_filtered)
  }
  
  lines_df  <- lines_df %>%
    filter(from_id != to_id) %>%
    filter(capacity != "NA" & capacity != "0.0" & modes != "NA")
  
  
  if(network_modes!=""){
    lines_df_filtered <- lines_df[0,]
    for (mode in network_modes){
      warning(paste('Cleaning network for ', mode))
      temp_df <- get_biggest_component(lines_df,mode)
      lines_df_filtered<- rbind(lines_df_filtered, temp_df)
    }
  }else if(network_modes==""){
    warning('Empty list of network modes, skip cleaning')
    lines_df_filtered <- lines_df
  }else{
    warning('Proper mode for cleaning is not provided, skip cleaning')
    lines_df_filtered <- lines_df
  }
  
  nodes_df_cleaned <- nodes_df %>%
    filter(id %in% lines_df_filtered$from_id | id %in% lines_df_filtered$to_id)
  
  return(list(nodes_df_cleaned,lines_df_filtered))
}

removeBadModes <- function(df,mode,bad_nodes) {
  # df=links_df
  df <- df %>%
    # find links with current mode that use one of the bad nodes
    mutate(delete=ifelse(grepl(mode,modes) & (from_id%in%bad_nodes | to_id%in%bad_nodes),T,F))
  
  echo(paste0('Removing ',nrow(df%>%filter(delete==T)),' bad edges for ', mode,'\n'))
  
  df <- df %>%
    mutate(modes=as.character(modes)) %>%
    # delete mode from offending link. Example: gsub('walk,|walk|,walk', '', 'car,bike,walk')
    mutate(modes=ifelse(delete==T,gsub(paste0(mode,',|',mode,'|,',mode), '', modes),modes)) %>%
    # want to set is_walk/is_mode/is_car to the correct value too
    mutate(is_walk=ifelse(mode=='walk'&delete==T,0,is_walk)) %>%
    mutate(is_cycle=ifelse(mode=='bike'&delete==T,0,is_cycle)) %>%
    mutate(is_car=ifelse(mode=='car'&delete==T,0,is_car)) %>%
    dplyr::select(-delete) %>%
    # if we have removed all possible modes from a link, delete it.
    filter(modes!='')
  return(df)
}

# nodes that are only in from or in to (i.e., symmetric difference)
findDisconnected <- function(from,to) {
  all_nodes<-union(from,to)
  connected_nodes<-intersect(from,to)
  return(setdiff(all_nodes,connected_nodes))
}

# Make sure specified modes are part of a connected subgraph (i.e., there is a 
# directed routeable graph for each specified mode). This considers link
# direction, so is useful for the car and bike modes.
largestDirectedNetworkSubgraph <- function(networkList, network_modes = "car"){
  # networkList<-networkRestructured;network_modes = "walk,bike"
  nodes_df <- networkList[[1]]
  links_df <- networkList[[2]]
  if(network_modes==""){
    warning('Empty list of network modes, skip cleaning')
    nodes_df <- nodes_df %>%
      filter(id%in%c(links_df$from_id,links_df$to_id))
    return(list(nodes_df,links_df))
  }
  network_modes <- strsplit(network_modes,",")%>%unlist()
  
  
  for (mode in network_modes) {
    # mode='bike'
    echo(paste0('Cleaning network for ', mode,'\n'))
    # links with current mode
    traversable_links <- links_df%>%st_drop_geometry()%>%filter(grepl(mode,modes))
    links_directed <- rbind(
      traversable_links%>%dplyr::select(from=from_id,to=to_id),
      traversable_links%>%filter(is_oneway==0)%>%dplyr::select(from=to_id,to=from_id)
    )
    
    
    # FIRST PASS removes any obvious disconnected links (i.e., nodes that have
    # only one edge touching them).
    disconnected_nodes<-c()
    # keep iterating through the directed graph, finding nodes that are unreachable
    while(length(findDisconnected(links_directed$from,links_directed$to))>0) {
      disconnected_nodes<-c(disconnected_nodes,
                            findDisconnected(links_directed$from,links_directed$to))
      links_directed <- links_directed %>%
        filter(!from%in%disconnected_nodes & !to%in%disconnected_nodes)
    }
    # remove the mode from links with the disconnected_nodes
    links_df<-removeBadModes(links_df,mode,disconnected_nodes)
    
    
    # SECOND PASS removes any remaining disconnected links
    # edges that the current mode can use
    traversable_links <- links_df%>%st_drop_geometry()%>%filter(grepl(mode,modes))
    
    # split the undirected links into connected components. This will make the
    # distance calculations MUCH faster.
    undirected_links <- traversable_links%>%filter(is_oneway==0)%>%
      dplyr::select(from=to_id,to=from_id)
    g <- graph_from_data_frame(undirected_links, directed=FALSE) 
    # Getting components
    comp <- components(g)
    undirected_clusters <- data.frame(node_id=as.integer(names(comp$membership)),
                                      cluster_id=-comp$membership, row.names=NULL)
    # attaching oneway links to the undirected link clusters
    oneway_links <- traversable_links%>%filter(is_oneway==1) %>%
      dplyr::select(from_id,to_id) %>%
      left_join(undirected_clusters%>%rename(from_cluster=cluster_id),
                by=c("from_id"="node_id")) %>%
      left_join(undirected_clusters%>%rename(to_cluster=cluster_id),
                by=c("to_id"="node_id")) %>%
      # in case we have a long chain of edges between connected clusters,
      # nodes in the middle of the chain won't connect to any cluster so must be
      # assigned their original node id.
      mutate(from_cluster=ifelse(is.na(from_cluster),from_id,from_cluster)) %>%
      mutate(to_cluster=ifelse(is.na(to_cluster),to_id,to_cluster)) %>%
      # most directed edges start and end in the largest cluster, so can be ruled out
      filter(from_cluster!=-1 | to_cluster!=-1)
    
    # these are clusters that are completely unreachable from the main cluster
    unreachable_clusters<-setdiff(undirected_clusters$cluster_id,
                                  c(-1,oneway_links$from_cluster,oneway_links$to_from_cluster))
    
    # adding a line -1, -1 to ensure that the largest cluster will be present
    oneway_links_graph <- bind_rows(
      oneway_links%>%dplyr::select(from=from_cluster,to=to_cluster),
      data.frame(from=-1,to=-1)
    )
    
    # all of the nodes in the oneway edges (positive for actual node ids,
    # negative for undirected cluster ids)
    component_nodes=union(oneway_links_graph$from,oneway_links_graph$to)
    # Making a second graph out of directed edges and undirected clusters
    g2 <- graph_from_data_frame(oneway_links_graph,directed=TRUE)
    # calculating the distance from the largest cluster to all other nodes,
    # and from all other nodes to the largest cluster
    from_main_component <- distances(g2, v="-1", to=as.character(component_nodes),
                                     mode="out")
    to_main_component <- distances(g2, v="-1", to=as.character(component_nodes),
                                   mode="in")
    distancesDF <- data.frame(
      component_id=component_nodes,
      from_distance=from_main_component[1,],
      to_distance=to_main_component[1,]
    )
    # bad components are those with an infinite distance either from or to the
    # largest undirected cluster
    bad_components <- distancesDF %>%
      filter(is.infinite(from_distance) | is.infinite(to_distance)) %>%
      pull(component_id)
    
    bad_nodes <- union(
      # find all nodes that make up a bad cluster
      bad_components[bad_components>0],
      # some nodes are directly referenced
      undirected_clusters%>%filter(cluster_id%in%c(unreachable_clusters,bad_components))%>%pull(node_id)
      # bad_components %>%inner_join(undirected_clusters,by=c("component_id"="cluster_id"))%>%pull(node_id)
    )
    
    # remove the mode from links with the disconnected_nodes
    links_df<-removeBadModes(links_df,mode,bad_nodes)
  }
  
  # in case any links have been removed
  nodes_df <- nodes_df %>%
    filter(id%in%c(links_df$from_id,links_df$to_id))
  return(list(nodes_df,links_df))
}

# Make sure specified modes are part of a connected subgraph (i.e., there is a 
# routeable graph for each specified mode). This doesn't consider link
# direction, so is useful for the walk mode.
largestNetworkSubgraph <- function(networkList, network_modes = "walk"){
  # networkList=networkNonDisconnected;network_modes="walk,bike"
  nodes_df <- networkList[[1]]
  links_df <- networkList[[2]]
  
  if(network_modes==""){
    warning('Empty list of network modes, skip cleaning')
    nodes_df <- nodes_df %>%
      filter(id%in%c(links_df$from_id,links_df$to_id))
    return(list(nodes_df,links_df))
  }
  
  network_modes <- strsplit(network_modes,",")%>%unlist()
  
  for (mode in network_modes) {
    # mode='bike'
    echo(paste0('Cleaning network for ', mode,'\n'))
    # links with current mode
    links_undirected <- links_df%>%st_drop_geometry()%>%
      filter(grepl(mode,modes))%>%dplyr::select(from=from_id,to=to_id)
    g <- graph_from_data_frame(links_undirected, directed = FALSE) 
    
    # Getting components
    comp <- components(g)
    nodes_largest_comp <- data.frame(node_id=as.integer(names(comp$membership)),
                                     cluster_id=comp$membership, row.names=NULL) %>%
      filter(cluster_id==which.max(comp$csize)) %>%
      pull(node_id) %>% base::unique()
    
    # nodes not in the largest component
    disconnected_nodes<-setdiff(union(links_undirected$from,links_undirected$to),
                                nodes_largest_comp)
    
    # remove the mode from links with the disconnected_nodes
    links_df<-removeBadModes(links_df,mode,disconnected_nodes)
  }
  # in case any links have been removed
  nodes_df <- nodes_df %>%
    filter(id%in%c(links_df$from_id,links_df$to_id))
  return(list(nodes_df,links_df))
}