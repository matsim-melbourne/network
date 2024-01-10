# function to add impedance to network links, based on surface
addSurfImped <- function(nodes_current, edges_current) {
  
  edges_current <- edges_current %>%
    
    # add surface level, based on OSM surface categories
    # [CONSIDER REVISING THIS LIST WHEN USING IN OTHER CITIES - SEE 
    #  'checkSurfTypes()' BELOW FOR A FUNCTION TO HELP INVESTIGATE]
    mutate(surf_lvl = case_when(
      str_detect(tolower(surface), 
                 "bluestone|boardwalk|brick|cobblestone|composite|fibre|metal|paving|stone|plastic|sett|stone|tile|timber|wood")             
      ~ "rough",
      str_detect(tolower(surface), 
                 "clay|compacted|dirt|earth|grass|gravel|mud|rock|sand|turf|unpacved|unpaved|unsealed")
      ~ "unpaved",
      TRUE ~ "smooth"
    )) %>%
    
    # add impedance from surface level [ILLUSTRATIVE IMPEDANCES ONLY, TO BE 
    # REVISED BASED ON FURTHER RESEARCH OR SURVEY RESULTS]
    mutate(surf_imped = case_when(
      surf_lvl == "smooth"  ~ 0,
      surf_lvl == "unpaved" ~ length * 0.1,
      surf_lvl == "rough"   ~ length * 0.2
    ))
  
  return(list(nodes_current, edges_current))
}


# investigation function to check number and type of surfaces
checkSurfTypes <- function(osmPbfExtract) {
  
  # osmPbfExtract = "./data/melbourne_australia.osm.pbf"
 
  echo("Reading in the .osm.pbf extract line layer\n")
  lines <- oe_read(osmPbfExtract, layer = "lines", extra_tags = "surface")
  
  highways <- lines %>%
    filter (highway %in% 
              c("motorway", "motorway_link", "trunk", "trunk_link",
                "primary", "primary_link", "secondary", "secondary_link",
                "tertiary", "tertiary_link", "residential", "road", "unclassified",
                "living_street", "cycleway", "track", "service", 
                "pedestrian", "footway", "path", "corridor", "steps"))
  
  surface.totals <- highways %>%
    st_drop_geometry() %>%
    group_by(surface) %>%
    summarise(n = n()) %>%
    ungroup()
  
  # # print outputs as comma-separated string
  # surface.totals.output <- ""
  # for (i in 1:nrow(surface.totals)) {
  #   line <- paste0(surface.totals$surface[i], " (", surface.totals$n[i], ")")
  #   surface.totals.output <- paste0(surface.totals.output, line)
  #   if (i < nrow(surface.totals)) {
  #     surface.totals.output <- paste0(surface.totals.output, ", ")
  #   }
  # }
  # print(surface.totals.output)
  
  return(surface.totals)

}

# # using checkSurfTypes to compile table of surface types, no. and surf_lvl
# check <- checkSurfTypes("./data/melbourne_australia.osm.pbf")
# check.totals <- addSurfImped(NA, check %>% mutate(length = 0))[[2]] %>%
#   dplyr::select(surface, n, surf_lvl)
