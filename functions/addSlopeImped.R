# function to add impedance to network links, based on slope

# impedance is 50m of length per 1m of climb (Ziemke, D, Metzler, S & Nagel, K 2019, 
# 'Bicycle traffic and its interaction with motorized traffic in an agent-based 
# transport simulation framework', Future Generation Computer Systems, vol. 97, pp. 30-40.)

addSlopeImped <- function(nodes_current, edges_current) {
  
  # testing
  # nodes_current <- networkLTS[[1]]
  # edges_current <- networkLTS[[2]]
  
  edges_current <- edges_current %>%
    
    # some coastal links are missing elevation; make slope 0
    mutate(slope_pct = ifelse(is.na(slope_pct), 0, slope_pct)) %>%
  
  # 50m of length per 1m of climb
  # climb = run * slope_pct / 100
  mutate(slope_imped = case_when(
    slope_pct <= 0  ~ 0,
    slope_pct > 0   ~ (length * slope_pct / 100) * 50
  ))

  return(list(nodes_current, edges_current))
}
