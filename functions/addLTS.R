# function to add level of traffic stress to network, based on highway class, 
# traffic (ADT) and speed, and related impedance

# traffic volumes (eg ADT 10000) are for two-way traffic, so are halved
# (eg 10000 / 2) in order to apply to the one-way links in edges_current



addLTS <- function(nodes_current, edges_current) {
 
  # testing
  # nodes_current <- networkTraffic[[1]]
  # edges_current <- networkTraffic[[2]]
  
  # assign LTS to edges 
  # '1' to '4' are categories of increasing stress, as per table below]
  
  # road groups
  local <- c("residential", "road", "unclassified", "living_street", "service")
  tertiary <- c("tertiary", "tertiary_link")
  secondary <- c("secondary", "secondary_link")
  primary <- c("primary", "primary_link")
  

  edges_current <- edges_current %>%
    # make speed field (rounded, to avoid floating point issues)
    mutate(speed = round(freespeed * 3.6)) %>%
    # add LTS 
    mutate(lvl_traf_stress = case_when(
      
      # LTS 1 - off-road paths
      cycleway %in% c("bikepath", "shared_path")                   ~ 1,
      highway %in% c("cycleway", "track", "pedestrian", 
                     "footway", "path", "corridor", "steps")       ~ 1,
      
      # LTS 1 - separated cycle lanes
      cycleway == "separated_lane" & speed <= 50                   ~ 1,
      
      # LTS 1 - on-road cycle lanes
      cycleway == "simple_lane" & 
        highway %in% c(local, tertiary, secondary) &
        ADT <= 10000 /2 & speed <= 30                              ~ 1,
      
      # LTS 1 - mixed traffic
      highway %in% local & ADT <= 2000 / 2 & speed <= 30           ~ 1,
      
      # LTS 2 - separated cycle lanes
      cycleway == "separated_lane" & speed <= 60                   ~ 2,
      
      # LTS 2 - on-road cycle lanes
      cycleway == "simple_lane" &
        highway %in% c(local, tertiary, secondary) &
        ADT <= 10000 / 2 & speed <= 50                             ~ 2,
      cycleway == "simple_lane" &
        (highway %in% primary | 
           (highway %in% c(local, tertiary, secondary) & 
              ADT > 10000 / 2)) &
        speed <= 40                                                ~ 2,
      
      # LTS 2 - mixed traffic
      highway %in% local & ADT <= 750 / 2 & speed <= 50            ~ 2,
      highway %in% local & ADT <= 2000 / 2 & speed <= 40           ~ 2,
      highway %in% c(local, tertiary) & ADT <= 3000 / 2 & speed <= 30  ~ 2,
      
      # LTS 3 - on-road cycle lanes
      cycleway == "simple_lane" & speed <= 60                      ~ 3,
      
      # LTS 3 - mixed traffic
      highway %in% local & ADT <= 750 / 2 & speed <= 60                ~ 3,
      highway %in% c(local, tertiary) & ADT <= 3000 / 2 & speed <= 50  ~ 3,
      (highway %in% c(secondary, primary) |
         (highway %in% c(local, tertiary) & ADT > 3000 / 2)) &
        speed <= 40                                                ~ 3,
        
      # LTS 4 - everything not covered above
      TRUE                                                         ~ 4
    ))
  
  # check to test how many in each category
  # LTS_table <- edges_current %>%
  #   st_drop_geometry() %>%
  #   group_by(highway, lvl_traf_stress) %>%
  #   summarise(n = n())

  # assign LTS to nodes, based on highest 
  # begin with all nodes (from and to) and the LTS level of the associated link
  node_max_lookup <- rbind(edges_current %>%
                             st_drop_geometry() %>%
                             dplyr::select(id = from_id, LTS = lvl_traf_stress),
                           edges_current %>%
                             st_drop_geometry() %>%
                             dplyr::select(id = to_id, LTS = lvl_traf_stress)) %>%
    group_by(id) %>%
    # find highest level of LTS for links connecting with the node
    summarise(max_LTS = max(LTS)) %>%
    ungroup()
  
  # Calculate impedance  for intersection, and total impedance
  
  # Impedance for intersection applies to the to-node (the intersection
  # that the link arrives at), and only if it's unsignalised
  # penalty is calculated as: 
  #     penaltya = (Buffb – Buffa) * (IFb – 1)for
  # where 
  #   a is the link for which the penalty (penaltya) is being calculated
  #   b is the highest-ranked other link at the relevant intersection
  #   Buffa and Buffb are the buffer distances for a and b, where the buffer 
  #     distance is 0, 5, 10 or 25m for a link of LTS 1, 2, 3 or 4 respectively
  #   IFb is the impedance factor for b, where the impedance factor is 1.00, 
  #     1.05, 1.10 or 1.15 for a link of LTS 1, 2, 3 or 4 respectively
  
  # LTS impedance, which is to be added to the length of the link and any other 
  # impedances (outside this function) to create the weight for the link,
  # is the length-based impedance for the link plus its intersection impedance.
  # - Length-based impedance is the link multiplied by its impedance factor
  #   minus 1 (that is, subtracting 1 so it os only the additional impedance,
  #   not the length itself:
  #     total_imped = length * (IFa - 1)
  #   where IFa is the impedance factor for a, where the impedance factor is 1.00, 
  #   1.05, 1.10 or 1.15 for a link of LTS 1, 2, 3 or 4 respectively
  # - Intersection impedance is calcualted as above
  
  buff_imped_df <- data.frame(cbind(LTS = c(1, 2, 3, 4),
                              buffer = c(0, 5, 10, 25),
                              imped = c(1, 1.05, 1.10, 1.15)))
  
  edges_current <- edges_current %>%
    # join node intersection details for the to-node
    left_join(., nodes_current %>%
                st_drop_geometry() %>%
                dplyr::select(id, type),
              by = c("to_id" = "id")) %>%
    # join the node max LTS buffer & impedance details for the to-node 
    left_join(., node_max_lookup, by = c("to_id" = "id")) %>%
    left_join(., buff_imped_df, by = c("lvl_traf_stress" = "LTS")) %>%
    # and the buff_imped_df details for the max LTS
    left_join(., buff_imped_df, by = c("max_LTS" = "LTS"), suffix = c(".a", ".b")) %>%
    
    # calculate intersection impedance, using formula above (unsignalised only)
    mutate(intersec_imped = ifelse(type %in% c("simple_intersection", 
                                                "simple_roundabout"),
                                    (buffer.b - buffer.a) * (imped.b - 1),
                                    0)) %>%
    # calculate total LTS impedance (to be added to length along with other impedances)
    mutate(LTS_imped = (length * (imped.a - 1)) + intersec_imped) %>%
    
    # remove unwanted fields
    dplyr::select(-speed, -type, -max_LTS, -buffer.a, -buffer.b, 
                  -imped.a, -imped.b, -intersec_imped)

  return(list(nodes_current, edges_current))
}
