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
  df <- df %>%
    # find links with current mode that use one of the bad nodes
    mutate(delete=ifelse(grepl(mode,modes) & (from_id%in%bad_nodes | to_id%in%bad_nodes),T,F)) %>%
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

findDisconnected <- function(from,to) {
  all_nodes<-union(from,to)
  connected_nodes<-intersect(from,to)
  return(setdiff(all_nodes,connected_nodes))
}

# make sure specified modes have no disconnected directed links
removeDisconnectedLinks <- function(networkList, network_modes = "car"){
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
    warning(paste0('Cleaning network for ', mode,'\n'))
    # links with current mode
    traversable_links <- links_df%>%st_drop_geometry()%>%filter(grepl(mode,modes))
    links_directed <- rbind(
      traversable_links%>%dplyr::select(from=from_id,to=to_id),
      traversable_links%>%filter(is_oneway==0)%>%dplyr::select(from=to_id,to=from_id)
    )
    
    
    # First pass removes any obvious disconnected links
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
    
    
    # Second pass removes any remaining disconnected links
    traversable_links <- links_df%>%st_drop_geometry()%>%filter(grepl(mode,modes))
    links_directed <- rbind(
      traversable_links%>%dplyr::select(from=from_id,to=to_id),
      traversable_links%>%filter(is_oneway==0)%>%dplyr::select(from=to_id,to=from_id)
    )
    oneway_links <- traversable_links%>%filter(is_oneway==1) %>%
      dplyr::select(from=from_id,to=to_id)%>%mutate(connected=NA)
    # Making the graph for the intersections
    g <- graph_from_data_frame(links_directed, directed = TRUE)
    # Iterate through each oneway link to see if it's possible to get back
    for (i in 1:nrow(oneway_links)) {
      path <- shortest_paths(g, from=as.character(oneway_links$to[i]),
                             to=as.character(oneway_links$from[i]), mode="out",
                             output="vpath")$vpath[[1]]
      oneway_links$connected[i] <- length(path)
    }
    disconnected_links <- oneway_links%>%filter(connected==0)
    disconnected_nodes <- union(disconnected_links$from,disconnected_links$to)
    # remove the mode from links with the disconnected_nodes
    links_df<-removeBadModes(links_df,mode,disconnected_nodes)
  }
  
  # in case any links have been removed
  nodes_df <- nodes_df %>%
    filter(id%in%c(links_df$from_id,links_df$to_id))
  return(list(nodes_df,links_df))
}


# make sure specified modes are part of a connected subgraph
# this doesn't consider directionality
cleanNetworkSubgraph <- function(networkList, network_modes = "car"){
  # networkList<-networkRestructured
  # network_modes = "walk,bike"
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
    warning(paste0('Cleaning network for ', mode,'\n'))
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