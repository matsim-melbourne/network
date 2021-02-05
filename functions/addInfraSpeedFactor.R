# TODO These values I used are for Infrastructure factor, not for the speed factor. 
# Speed factor is the multplier that impacts the speed only - which is something 
# I need to come up with myself.

# 23 km/h is considered as the bicycle speed in both simulation and here
addInfraSpeedFactor <- function(nodes,links){
  #links <- networkRestructured[[2]]
  #nodes <- networkRestructured[[1]]
   
  # Making all links single way:
  bi_links <- links %>% 
    filter(is_oneway==0) %>% 
    mutate(is_oneway=1) %>% 
    rename(from_id=to_id, to_id=from_id, toX=fromX, toY=fromY, fromX=toX, fromY=toY) %>% 
    dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, permlanes,
                  capacity, is_oneway, cycleway, highway, is_cycle, is_walk, is_car, modes,bicycleSpeedFactor)
  links <- links %>%
    mutate(is_oneway=ifelse(is_oneway==0, yes = 1, no=1)) %>% 
    rbind(bi_links)
    
  # Adding end node to the links
  links_joined <- links %>% 
    left_join(st_drop_geometry(nodes[,c("id","max_highway","type")]), 
              by=c("to_id"="id")) %>% 
    rename("to_type"="type") %>%  
    left_join(st_drop_geometry(nodes[,c("id","type")]),
              by=c("from_id"="id")) %>% 
    rename("from_type"="type") %>%  
    # Finding which links are middle intersections
    mutate(intersection_factor = 1) %>% # intersection multiplyer 
    mutate(intersection_factor = ifelse(to_type == "simple_intersection" & length<30, yes = 1.3, no = intersection_factor)) %>% 
    mutate(intersection_factor = ifelse(to_type == "simple_intersection" & length<30 & highway==max_highway, yes = 1, no = intersection_factor)) %>% 
    mutate(intersection_factor = ifelse(to_type == "signalised_intersection" & length<30, yes = 3.6, no = intersection_factor)) %>% 
    mutate(intersection_factor = ifelse(to_type == "simple_roundabout" & length<30, yes = 1.2, no = intersection_factor)) %>% 
    mutate(intersection_factor = ifelse(to_type == "signalised_roundabout" & length<30, yes = 3.6, no = intersection_factor)) %>%
    # based 20 km/h, 5.5 m/s, assuming 10 meter for all intersections
    mutate(intersection_factor = ifelse(to_type == "simple_intersection" & length>30, 
                       yes = 1+((10/5.5)*1.3/(length/5.5)), no = intersection_factor)) %>% # T'/T = (T+t)/T = 1+t/T 
    mutate(intersection_factor = ifelse(to_type == "signalised_intersection" & length>30, 
                       yes = 1+((15/5.5)*3.6/(length/5.5)), no = intersection_factor)) %>%
    mutate(intersection_factor = ifelse(to_type == "simple_roundabout" & length>30, 
                       yes = 1+((15/5.5)*1.2/(length/5.5)), no = intersection_factor)) %>%
    mutate(intersection_factor = ifelse(to_type == "signalised_roundabout" & length>30, 
                       yes = 1+((20/5.5)*3.6/(length/5.5)), no = intersection_factor)) #%>% 
    #mutate(intersection_factor = 1) # intersection multiplyer 
    
  
  
  #mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
  #  st_as_sf(wkt = "GEOMETRY", crs = 28355) %>% 
  #  as.data.frame() %>%
  #  st_sf() %>% 
  #st_write("links_estimtated3.sqlite", delete_layer=T)
  
  
  # Adding V1
  links_estimtated <- links_joined %>% 
    mutate(cycleway=ifelse(is.na(cycleway),"no",cycleway)) %>% 
    mutate(v=5.5) %>% 
    mutate(v=ifelse(cycleway=="bikepath",v*1.106,v)) %>% 
    mutate(v=ifelse(cycleway=="separed_lane",v*1.106,v)) %>% 
    mutate(v=ifelse(cycleway=="lane",v*1.082,v)) %>% 
    mutate(v=ifelse(cycleway=="share_lane",v*1.061,v)) %>% 
    mutate(t=length/v) %>% 
    mutate(new_v=length/(t*intersection_factor)) %>% 
    mutate(bicycleSpeedFactor=new_v/5.5) %>% 
    mutate(expectedSpeed=bicycleSpeedFactor*23) #%>% 
    #dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
    #              permlanes, capacity, highway, is_oneway, cycleway, is_cycle, is_walk,
    #              is_car, modes, bicycleSpeedFactor,expectedSpeed)
 
    
    if("z"%in%colnames(nodes)){
      # Changing all is_oneway=0 to 2x is_oneway=1 with reverse directions
      # This is to estimate slope
      # Effect of slope on speed is considered to be XXXXX % for each XXXX m 
      print("Adding elevation impact")
      links_elevated <- links_estimtated %>% 
        left_join(st_drop_geometry(nodes[,c("id","z")]), by=c("to_id"="id")) %>% 
        rename("to_z"="z") %>% 
        left_join(st_drop_geometry(nodes[,c("id","z")]), by=c("from_id"="id")) %>% 
        rename("from_z"="z") %>%
        mutate(slope=100*(to_z-from_z)/length)
      
      links_estimtated <- links_elevated %>% 
        mutate(gradient_factor= case_when(slope < -9 ~  4.91,
                                          slope < -7 ~  10.81,
                                          slope < -6 ~  13.57,
                                          slope < -5 ~  17.95,
                                          slope < -4 ~  18.02,
                                          slope < -3 ~  14.94,
                                          slope < -2 ~  11.24,
                                          slope < -1 ~  5.89,
                                          slope < -0 ~  4.12,
                                          slope >  9 ~ -42.67,
                                          slope >  7 ~ -39.49,
                                          slope >  6 ~ -38.54,
                                          slope >  5 ~ -30.34,
                                          slope >  4 ~ -26.69,
                                          slope >  3 ~ -19.51,
                                          slope >  2 ~ -12.99,
                                          slope >  1 ~ -9.73,
                                          slope >=  0 ~  1)) %>% 
        mutate(bicycleSpeedFactor=bicycleSpeedFactor*(1+gradient_factor/100)) %>% 
        mutate(expectedSpeed=bicycleSpeedFactor*23) # %>% 
        #dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
        #              permlanes, capacity, highway, is_oneway, cycleway, is_cycle, 
        #              is_walk, is_car, modes, bicycleSpeedFactor, expectedSpeed)
    }
    
     #links_estimtated %>% 
    # Writing links for visualisation
    #links_estimtated %>% 
    #  mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
    #  st_as_sf(wkt = "GEOMETRY", crs = 28355) %>% 
    #  as.data.frame() %>%
    #  st_sf() %>% 
    #  st_write("infrastructure.sqlite",delete_layer=T)
    
    #nodes %>% 
    #  st_write("nodes3D.sqlite")
  return(list(nodes,links_estimtated))
}