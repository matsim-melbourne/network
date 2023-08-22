# function to convert two-way edges to one-way

makeEdgesOneway <- function(nodes_current, edges_current) {
  
  # testing
  # nodes_current <- input.nodes
  # edges_current <- input.links
  
  # ensure fromx, fromy, tox and toy column names are lower case (eg not 'fromX')
  names.to.change <- c("fromX", "fromY", "toX", "toY")
  edges_current <- rename_with(edges_current, tolower, any_of(names.to.change))
  
  # select only two-way edges
  edges_twoway <- edges_current %>%
    filter(is_oneway == 0)
  
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
