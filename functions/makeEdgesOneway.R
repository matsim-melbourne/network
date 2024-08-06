# function to convert two-way edges to one-way

makeEdgesOneway <- function(nodes_current, edges_current, defaults_df) {
  
  # testing
  # nodes_current <- input.nodes
  # edges_current <- input.links
  
  # ensure fromx, fromy, tox and toy column names are lower case (eg not 'fromX')
  names.to.change <- c("fromX", "fromY", "toX", "toY")
  edges_current <- rename_with(edges_current, tolower, any_of(names.to.change))
  
  # for two-way, divide permlanes and capacity by 2, rounded up (as they will be split into 2 * one-way)
  edges_current <- edges_current %>%
    mutate(permlanes = ifelse(is_oneway == 0, ceiling(permlanes / 2), permlanes),
           capacity = ifelse(is_oneway == 0, ceiling(capacity / 2), capacity))
  
  # select only two-way edges
  edges_twoway <- edges_current %>%
    filter(is_oneway == 0)
  
  # add the one-way edges with contra-flow bike lanes, and set their attributes to bike only
  edges_contrabike <- edges_current %>%
    filter(is_oneway == 1 & contrabike == 1) %>%
    mutate(freespeed = ifelse(freespeed < defaults_df$freespeed[16],
                              freespeed, defaults_df$freespeed[16]),
           permlanes = defaults_df$permlanes[16],
           capacity = ifelse(capacity < defaults_df$laneCapacity[16],
                             capacity,
                             defaults_df$laneCapacity[16]),
           is_cycle = 1, is_car = 0,  # no change to is_walk
           modes = if_else(is_walk == 0, "bike", "bike,walk"))
  edges_twoway <- bind_rows(edges_twoway, edges_contrabike)
  
  # swap from/to details
  edges_twoway_reversed <- edges_twoway %>%
    # store original from/to details
    mutate(orig_from_id = from_id,
           orig_to_id = to_id,
           orig_fromx = fromx,
           orig_fromy = fromy,
           orig_tox = tox,
           orig_toy = toy) %>%
    # swap from/to
    mutate(from_id = orig_to_id,
           to_id = orig_from_id,
           fromx = orig_tox,
           fromy = orig_toy,
           tox = orig_fromx,
           toy = orig_fromy)
  
  # if elevation is present, use the reverse slope
  if("rvs_slope_pct" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(slope_pct = rvs_slope_pct)
  }
  
  # select required fields (excluding 'is_oneway') [note that "id" is not 
  # retained here - it is replaced by link_id]
  required_fields <- c("from_id", "to_id", "fromx", "fromy", "tox", "toy",
                       "length", "freespeed", "permlanes", "capacity", "highway",
                       "cycleway", "surface", "is_cycle", "is_walk", "is_car", 
                       "modes")
  if ("slope_pct" %in% colnames(edges_twoway_reversed)) {
    required_fields <- c(required_fields, "slope_pct")
  }
  if ("osm_id" %in% colnames(edges_twoway_reversed)) {
    required_fields <- c(required_fields, "osm_id")
  }
  ndvi_columns <- colnames(edges_twoway_reversed)[grep("ndvi", colnames(edges_twoway_reversed))]
  if (length(ndvi_columns) > 0) {
    required_fields <- c(required_fields, ndvi_columns)
  }
  edges_twoway_reversed <- edges_twoway_reversed %>%
    dplyr::select(all_of(required_fields))
  
  # modify original edges to rename fwd_slope_pct if present
  if ("fwd_slope_pct" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(slope_pct = fwd_slope_pct)
  }
  
  # exclude 'is_oneway' from original edges, and bind with reversed two-way edges
  edges_current <- edges_current %>%
    dplyr::select(all_of(required_fields)) %>%
    rbind(., edges_twoway_reversed)
  
  # add link_id, based on rownumber (at the end, not beginning, because igraph 
  # requires from_id and to_id to be the first two columns)
  edges_current <- edges_current %>%
    mutate(link_id = row_number())

  
  return(list(nodes_current, edges_current))
}
