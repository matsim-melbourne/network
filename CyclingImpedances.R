# Using a network created by 'NetworkGenerator.R', add impedances for 
# calculating cycling accessibility

# Assumes input network is a one-way network that includes elevation, 
# and that one-way daily traffic volumes are available

addImpedances <- function() {
    
  # Parameters -----------------------------------------------------------------
  # Input network, to which cycling impedances are to be added, with layer names
  # input.network <- "./output/test/network.sqlite"
  input.network <- "./output/test/melbourne_network.sqlite"  #<<< OLD VERSION FOR TESTING
  input.node.layer <- "nodes"
  input.link.layer <- "links"
  
  # Traffic file, with links layer name - file must match input.network, 
  # and with a 'total_vol' column containing 1-way daily traffic
  # traffic.file <- "./output/test/network_traffic.sqlite"
  traffic.file <- "./output/test/links_with_traffic.sqlite" #<<< OLD VERSION FOR TESTING
  traffic.link.layer <- "cars_aht" 
  
  # Traffic multiplier - where volumes are for a sample of traffic only (eg
  # multiplier of 20 if the volumes are a 5% sample; use 1 if full volumes)
  traffic.multiplier <- 10

  # Output location - same directory as input.network
  output.location <- paste0(path_dir(input.network), "/networkWeighted.sqlite")
  
  
  # Packages and functions -----------------------------------------------------
  library(dplyr)
  library(sf)
  library(fs)
  library(osmextract)
  library(stringr)
  
  dir_walk(path="./functions/",source, recurse=T, type = "file")
  
  
  # Load input network and traffic file ----------------------------------------
  input.nodes <- st_read(input.network, layer = input.node.layer)  
  input.links <- st_read(input.network, layer = input.link.layer)
  traffic.links <- st_read(traffic.file, layer = traffic.link.layer)
  
  
  # Add LTS and its impedance --------------------------------------------------
  echo("Adding daily traffic volumes\n")
  networkTraffic <- addTraffic(input.nodes,
                               input.links, 
                               traffic.links, 
                               traffic.multiplier)
  ## TO DO - maybe traffic can just be joined on link_id?  See whether traffic
  ## file neatly uses the link_id's from the one-way input (also in cycling-adoption)
  
  echo("Adding LTS and its impedance\n")
  networkLTS <- addLTS(networkTraffic[[1]], networkTraffic[[2]])
  
  
  # Add slope impedance --------------------------------------------------------
  echo("Adding slope impedance")
  networkSlope <- addSlopeImped(networkLTS[[1]], networkLTS[[2]])
  
  
  # Add surface impedance --------------------------------------------------------
  echo("Adding surface impedance")
  networkSurf <- addSurfImped(networkSlope[[1]], networkSlope[[2]])
  
  
  # Calculate total weight -----------------------------------------------------
  echo("Calculating cycling weight")
  networkWeighted <- 
    list(networkSurf[[1]],
         networkSurf[[2]] %>%
           mutate(cycle.weight = length + LTS_imped + slope_imped + surf_imped))
  
  
  # write output ---------------------------------------------------------------
  st_write(networkWeighted[[1]], "./output/test/networkWeighted.sqlite", layer = "nodes")
  st_write(networkWeighted[[2]], "./output/test/networkWeighted.sqlite", layer = "links")
  
}