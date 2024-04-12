
# SQlite ------------------------------------------------------------------
exportSQlite <- function(networkFinal, outputDir, outputCrs){
  
  cat('\n')
  echo(paste0('Writing the sqlite output: ', nrow(networkFinal[[2]]), 
              ' links and ', nrow(networkFinal[[1]]),' nodes\n'))
  
  # dir.create('./generatedNetworks/', showWarnings = FALSE)
  
  if(class(networkFinal[[1]])[1]!="sf"){
    networkFinal[[1]] <- networkFinal[[1]] %>% 
      mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  if(class(networkFinal[[2]])[1]!="sf"){
    networkFinal[[2]] <- networkFinal[[2]] %>% 
      mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  
  # writing sqlite outputs
  st_write(networkFinal[[2]], paste0(outputDir,'/network.sqlite'), 
           layer = 'links', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  st_write(networkFinal[[1]], paste0(outputDir,'/network.sqlite'),
           layer = 'nodes', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  if (length(networkFinal) > 2) {
    st_write(networkFinal[[3]], paste0(outputDir,'/network.sqlite'),
             layer = 'destinations', driver = 'SQLite', 
             layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  }
  
  echo(paste0('Finished generating the sqlite output\n'))
}

# ShapeFile ---------------------------------------------------------------
exportShp <- function(networkFinal, outputDir, outputCrs){
  
  cat('\n')
  echo(paste0('Writing the ShapeFile output: ', nrow(networkFinal[[2]]), 
              ' links and ', nrow(networkFinal[[1]]),' nodes\n'))
  
  if(class(networkFinal[[1]])[1]!="sf"){
    networkFinal[[1]] <- networkFinal[[1]] %>% 
      mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  if(class(networkFinal[[2]])[1]!="sf"){
    networkFinal[[2]] <- networkFinal[[2]] %>% 
      mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  shpDir <- paste0(outputDir,"/shapefiles")
  dir.create(shpDir, showWarnings = FALSE)
  # writing ShapeFile outputs
  st_write(networkFinal[[2]], paste0(shpDir,'/links.shp'), 
           driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
           delete_layer = T)
  st_write(networkFinal[[1]], paste0(shpDir,'/nodes.shp'), 
           driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
           delete_layer = T)
  if (length(networkFinal) > 2) {
    message("When writing destinations to shapefile, long 'other_tags' may be truncated; consider using sqlite instead.")
    dest.pt <- st_collection_extract(networkFinal[[3]], type = "POINT")
    dest.poly <- st_collection_extract(networkFinal[[3]], type = "POLYGON")
    st_write(dest.pt, paste0(shpDir,'/destinations_point.shp'),
             driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
             delete_layer = T)
    st_write(dest.poly, paste0(shpDir,'/destinations_polygon.shp'),
             driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
             delete_layer = T)
  }
  
  echo(paste0('Finished generating the ShapeFile output\n'))
  
}

# XML ---------------------------------------------------------------------
exportXML <- function(networkFinal, outputDir){
  # Functions ---------------------------------------------------------------
  add3DNode <- function(x){
    this_node <- nodes[x,]
    str_node <- paste0("    <node id=\"",
                       this_node$id , "\" x=\"",
                       this_node$x  , "\" y=\"", 
                       this_node$y  , "\" z=\"", 
                       this_node$z  , "\" />"  ,
                       "\n")
    
    cat(str_node, file=xml_file,append=TRUE)
    if(x %%50 == 0) printProgress(x, nrow(nodes), 'node')
  }
  add2DNode <- function(x){
    this_node <- nodes[x,]
    str_node <- paste0("    <node id=\""       ,
                       this_node$id , "\" x=\"",
                       this_node$x  , "\" y=\"",
                       this_node$y  , "\" />"  ,
                       "\n")
    cat(str_node, file=xml_file,append=TRUE)
    if(x %%50 == 0) printProgress(x, nrow(nodes), 'node')
  }
  
  addLink <- function(x){
    this_link <- links[x,]
    if(x %%50 == 0) printProgress(x, nrow(links), 'link')
    
    links_str <- paste0("    <link id=\"", as.character(this_link$id),
                        "\" from=\"", as.character(this_link$from_id),
                        "\" to=\"", as.character(this_link$to_id),
                        "\" length=\"", this_link$length,
                        "\" capacity=\"", this_link$capacity,
                        "\" freespeed=\"", this_link$freespeed,
                        "\" permlanes=\"", this_link$permlanes,
                        "\" oneway=\"", "1",
                        "\" modes=\"", as.character(this_link$modes),
                        "\">\n",
                        "    <attributes>\n",
                        "        <attribute name=\"type\" class=\"java.lang.String\">", 
                        this_link$type, "</attribute>\n",
                        "        <attribute name=\"cycleway\" class=\"java.lang.String\">", 
                        this_link$cycleway, "</attribute>\n",
                        "        <attribute name=\"surface\" class=\"java.lang.String\">", 
                        this_link$surface, "</attribute>\n",
                        "        <attribute name=\"slopePct\" class=\"java.lang.String\">", 
                        this_link$slope, "</attribute>\n",
                        "        <attribute name=\"bicycleInfrastructureSpeedFactor\" class=\"java.lang.String\">", 
                        this_link$bicycleInfrastructureSpeedFactor, "</attribute>\n",
                        "    </attributes>\n",
                        "    </link>\n")
    
    cat(links_str, file=xml_file,append=TRUE)
  }
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]
    if(length(add)!=0) data[add] <- NA
    data
  }
  
  # Prefix ------------------------------------------------------------------
  cat('\n')
  nodes <- networkFinal[[1]]
  links <- networkFinal[[2]]
  
  echo(paste0('Writing the XML output: ', nrow(links), ' links and ', nrow(nodes),' nodes\n'))
  
  echo('Starting to write the XML\n')
  # Set the output file
  # dir.create('./generatedNetworks/', showWarnings = FALSE)
  xml_file <- paste0(outputDir,'/network.xml')
  
  # Adding the prefix
  open(file(xml_file), "wt")
  str_header <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
                       "<!DOCTYPE network SYSTEM \"http://www.matsim.org/files/dtd/network_v2.dtd\">\n",
                       "<network>\n",
                       "<nodes>\n") 
  cat(str_header,file=xml_file,append=FALSE)
  
  
  # Nodes -------------------------------------------------------------------
  if(class(nodes)[1]=="sf") nodes <- st_drop_geometry(nodes)
  echo('Starting to add nodes to XML\n')
  
  if ("z" %in% colnames(nodes)){
    nodes <- nodes %>% 
      mutate(z = if_else(is.na(z), true = 10, z))
    purrr::map_dfr(1:nrow(nodes),add3DNode)
  }else{
    purrr::map_dfr(1:nrow(nodes),add2DNode)
  }
  cat("</nodes>\n",file=xml_file,append=TRUE)
  
  # Links -------------------------------------------------------------------
  if(class(links)[1]=="sf") links <- st_drop_geometry(links)
  # Adding empty id column if doesn't exist
  if(!("id" %in% colnames(links))) links <- links %>% mutate(id=NA)
  if(!("fwd_slope_pct" %in% colnames(links))) links <- links %>% mutate(fwd_slope_pct=NA, rvs_slope_pct=NA)
  
  # # Adding a reverse links for bi-directionals - not required as makeEdgesOneway has made them all oneway
  # bi_links <- links %>% 
  #   filter(is_oneway==0) %>% 
  #   rename(from_id=to_id, to_id=from_id, toX=fromX, toY=fromY, fromX=toX, 
  #          fromY=toY, slope=rvs_slope_pct) %>% 
  #   mutate(id=NA) %>%
  #   dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
  #                 permlanes, capacity, is_oneway, cycleway, highway, surface, 
  #                 slope, is_cycle, is_walk, is_car, modes, id)
  # 
  # links <- rbind( 
  #   {links %>% dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
  #                            permlanes, capacity, is_oneway, cycleway, highway, surface, 
  #                            slope=fwd_slope_pct, is_cycle, is_walk, is_car, modes, id)},
  #   bi_links) 
  
  # Adding bicycle and extra information
  links <-  fncols(links, c("id","osm_id", "highway", "cycleway","slope", 
                            "bicycleInfrastructureSpeedFactor")) 
  links <- links %>%
    # mutate(id = ifelse(is.na(id),row_number(),id)) %>%
    mutate(id = case_when(
      is.na(id) & "link_id" %in% names(links) ~ as.character(link_id),
      is.na(id) ~ as.character(row_number()),
      TRUE ~ as.character(id))
    ) %>%
    mutate(type = replace(highway, is.na(highway), "NotSpecified")) %>% 
    mutate(surface = ifelse(is.na(surface),"asphalt",surface)) %>% 
    mutate(cycleway = replace(cycleway, is.na(cycleway),"No")) %>% 
    mutate(slope = replace(slope, is.na(slope),"NA")) %>% 
    mutate(bicycleInfrastructureSpeedFactor = 1) 
  # Adding links
  echo('\n')
  echo('Starting to add links to XML\n')
  cat("<links>\n",file=xml_file,append=TRUE)
  purrr::map_dfr(1:nrow(links),addLink)
  
  cat(paste0("</links>\n", "</network>\n"),
      file=xml_file,append=TRUE)
  echo(paste0('Finished generating the xml output\n'))
  close(file(xml_file))
}
