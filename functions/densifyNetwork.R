#~highway          , ~highway_order
# "motorway"       ,  1            
# "motorway_link"  ,  8            
# "trunk"          ,  2            
# "trunk_link"     ,  9            
# "primary"        ,  3            
# "primary_link"   ,  10           
# "secondary"      ,  4            
# "secondary_link" ,  11           
# "tertiary"       ,  5            
# "tertiary_link"  ,  12           
# "residential"    ,  6            
# "road"           ,  7            
# "unclassified"   ,  13           
# "living_street"  ,  14           
# "service"        ,  15           
# "cycleway"       ,  16           
# "track"          ,  17           
# "pedestrian"     ,  18           
# "footway"        ,  19           
# "path"           ,  20           
# "corridor"       ,  21           
# "steps"          ,  22

densifyNetwork <- function(networkList, minimum_length=400, densifyBikeways=F){
  # networkList<-networkConnected;minimum_length = 400
  nodes_df <- networkList[[1]]
  links_df <- networkList[[2]] %>%
    mutate(tmp_id=row_number())
  
  if (densifyBikeways) {
    links_to_segmentize <- links_df %>%
      filter(length>minimum_length & is_cycle==1)
  }else{
  # Densifying all except for "cycleway", "footway","motorway","motorway_link","path","pedestrian","corridor","steps","track")
    links_to_segmentize <- links_df %>%
      filter(length>minimum_length & !highway_order%in%c(1,8,16,17,18,19,20,21,22))
  }

  links_unsegmented <- links_df %>%
    filter(!tmp_id%in%links_to_segmentize$tmp_id)
  
  # segment_nodes <- links_to_segmentize %>%
  #   st_line_sample(density = units::set_units(minimum_length, m))

  segment_nodes <- links_to_segmentize %>%
    st_line_sample(density = units::set_units(minimum_length, m)) %>%
    st_sf()

  # st_write(segment_nodes,"segment_nodes.sqlite",delete_dsn=T)
  
  links_list <- links_to_segmentize %>% st_geometry()
  nodes_list <- segment_nodes %>%
    st_buffer(0.001, endCapStyle="SQUARE") %>%
    st_geometry()
  links_list_segmented <- list()
  
  for (i in 1:length(links_list)) {
    links_list_segmented[i] <- st_difference(links_list[i],nodes_list[i])
    if((i%%500)/500==0) cat(paste0(i,"/",length(links_list)," rows segmented\n"))
    if(i==length(links_list)) cat(paste0(i,"/",length(links_list)," rows segmented\n"))
  }
  
  # add flag for direction in which geometry of links_to_segmentize is recorded
  links_to_segmentize <- links_to_segmentize %>%
    # join X & Y coordinates for from_id
    left_join(nodes_df %>%
                st_drop_geometry() %>%
                dplyr::select(id, fromx = X, fromy = Y), 
              by = c("from_id" = "id")) %>%
    # check whether startpoint of geometry matches from_id ("forward" if yes, "reverse" if no)
    mutate(startpoint = st_coordinates(st_startpoint(geom))) %>%
    rowwise() %>%
    mutate(direction = 
             ifelse(startpoint[[1]] == fromx & startpoint[[2]] == fromy, 
                    "forward", 
                    "reverse")) %>%
    ungroup()

  links_segmented <- links_to_segmentize %>%
    st_set_geometry(st_sfc(links_list_segmented)) %>%
    mutate(group_id=row_number()) %>%
    st_snap_to_grid(1) %>%
    st_sf() %>%
    st_cast(to="LINESTRING") %>%
    mutate(new_node_id=row_number()+max(nodes_df$id,na.rm=T)) %>%
    group_by(group_id) %>%
    # mutate(from_id=ifelse(row_number()!=1,new_node_id-1,from_id)) %>%
    mutate(from_id = case_when(
      direction == "forward" & row_number() == 1 ~ from_id,
      direction == "forward" & row_number() != 1 ~ new_node_id-1,
      direction == "reverse" & row_number() != max(row_number()) ~ new_node_id,
      direction == "reverse" & row_number() == max(row_number()) ~ from_id
    )) %>%
    # mutate(to_id=ifelse(row_number()!=max(row_number()),new_node_id,to_id)) %>%
    mutate(to_id = case_when(
      direction == "forward" & row_number() != max(row_number()) ~ new_node_id,
      direction == "forward" & row_number() == max(row_number()) ~ to_id,
      direction == "reverse" & row_number() == 1 ~ to_id,
      direction == "reverse" & row_number() != 1 ~ new_node_id-1,
    )) %>%
    mutate(length=round(as.numeric(st_length(geom)),3)) %>%
    dplyr::select(-group_id, -new_node_id, -fromx, -fromy, -startpoint)
  
  nodes_segmented_forward <- links_segmented %>%
    filter(direction == "forward") %>%
    dplyr::select(id=to_id) %>%
    filter(id>max(nodes_df$id,na.rm=T)) %>%
    st_set_geometry(st_endpoint(.))
  
  nodes_segmented_reverse <- links_segmented %>%
    filter(direction == "reverse") %>%
    dplyr::select(id=to_id) %>%
    filter(id>max(nodes_df$id,na.rm=T)) %>%
    st_set_geometry(st_startpoint(.))
  
  nodes_segmented <- rbind(nodes_segmented_forward,
                           nodes_segmented_reverse) %>%
    mutate(is_roundabout=0,is_signal=0)
    
  nodes_segmented <- bind_cols(nodes_segmented,
                               data.frame(st_coordinates(nodes_segmented))) %>%
    dplyr::select(id,is_roundabout,is_signal,X,Y)
  
  nodes_combined <- bind_rows(
    nodes_df,
    nodes_segmented %>% st_set_crs(st_crs(nodes_df))
  ) %>%
    st_sf()
  
  # some coordinates of new links may not exactly match node coordinates due to snapping to grid
  # join the node x and y details to the links
  links_segmented <- links_segmented %>%
    left_join(nodes_combined %>%
                st_drop_geometry() %>%
                dplyr::select(id, fromx = X, fromy = Y), 
              by = c("from_id" = "id")) %>%
    left_join(nodes_combined %>%
                st_drop_geometry() %>%
                dplyr::select(id, tox = X, toy = Y),
              by = c("to_id" = "id")) %>%
    mutate(startx = ifelse(direction == "forward", fromx, tox),
           starty = ifelse(direction == "forward", fromy, toy),
           endx = ifelse(direction == "forward", tox, fromx),
           endy = ifelse(direction == "forward", toy, fromy))
  
  # correct coordinates at start and end of links to exactly match node coordinates
  links_segmented <- correct_coords(links_segmented)
  
  links_segmented <- links_segmented %>%
    dplyr::select(names(links_unsegmented))
  
  links_combined <- bind_rows(
    links_unsegmented,
    links_segmented %>% st_set_crs(st_crs(links_unsegmented))
  ) %>%
    dplyr::select(-tmp_id) %>%
    st_sf()
  
  # st_write(nodes_combined,"networkDensified.sqlite",layer="nodes",delete_dsn=TRUE)
  # st_write(links_combined,"networkDensified.sqlite",layer="edges",delete_dsn=FALSE)
  
  return(list(nodes_combined,links_combined))
}

addMode <- function(networkList) {
  nodes <- networkList[[1]]
  links <- networkList[[2]]
  
  links <- links %>%
    mutate(modes=ifelse(                is_car==1,                          "car",    NA)) %>%
    mutate(modes=ifelse(!is.na(modes)&is_cycle==1,    paste(modes,"bike",sep=","), modes)) %>%
    mutate(modes=ifelse( is.na(modes)&is_cycle==1,                         "bike", modes)) %>%
    mutate(modes=ifelse( !is.na(modes)&is_walk==1,    paste(modes,"walk",sep=","), modes)) %>%
    mutate(modes=ifelse(  is.na(modes)&is_walk==1,                         "walk", modes))
  return(list(nodes,links))
}

# function to ensure link start and endpoints match the coordinates of their from 
# and to nodes - note each link must contain startx/y and endx/y coordinates from 
# its start and end nodes, where 'start' and 'end' match the direction of digitisation of the link
correct_coords <- function(links) {
  # extract coordinates for all simplified geometries
  coords <- st_coordinates(links)
  
  # find the indices for the coordinates that are start and end points
  line_ids <- coords[, "L1"]
  first_indices <- !duplicated(line_ids)
  last_indices <- !duplicated(line_ids, fromLast = TRUE)
  
  # extract just the coordinates for the first and last points
  first_coords <- coords[first_indices, ]
  last_coords <- coords[last_indices, ] 
  
  # replace first and last coordinates with original start/endpoint geometry
  first_coords[, c("X", "Y")] <- cbind(links$startx, links$starty)
  last_coords[, c("X", "Y")] <- cbind(links$endx, links$endy)
  
  # combine modified first and last coordinates with the rest of the coordinates
  modified_coords <- coords
  modified_coords[first_indices, c("X", "Y")] <- first_coords[, c("X", "Y")]
  modified_coords[last_indices, c("X", "Y")] <- last_coords[, c("X", "Y")]
  
  # create new geometries with the modified coordinates using split
  split_coords <- split(modified_coords[, c("X", "Y")], line_ids)
  new_geometries <- lapply(split_coords, function(x) {
    # convert coordinates to matrix
    mat <- matrix(x, ncol = 2)
    # create linestring from matrix
    st_linestring(mat)
  })
  
  # update the geometries in the simplified_edges object
  st_geometry(links) <- st_sfc(new_geometries, crs = st_crs(links))
  
  return(links)
}

