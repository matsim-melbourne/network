#########################################################
# OSM TAG         | Network Name       | Bike hierarchy |
#-----------------|--------------------|----------------|
# cycleway        | bikepath           | 5              |
# cycleway + walk | shared_path        | 4              | 
# track           | separated_lane     | 3              |
# lane            | simple_lane        | 2              |
# shared_lane     | shared_street      | 1              |
# -               | no lane/track/path | 0              |
#########################################################

processOsmTags <- function(osm_df,this_defaults_df){
  # osm_df <- osm_metadata
  # this_defaults_df <- defaults_df
  
  osmWithDefaults <- inner_join(osm_df,this_defaults_df,by="highway")
  # pre splitting the tags to save time
  tagList <- strsplit(gsub('=>',',', gsub('"', '', osmWithDefaults$other_tags)),',')
  
  osmWithDefaults <- osmWithDefaults %>%
    mutate(cycleway=ifelse(highway=="cycleway",4,0)) %>%
    dplyr::select(osm_id,highway,highway_order,freespeed,permlanes,laneCapacity,is_oneway,cycleway,is_cycle,is_walk,is_car)

  getMetadataInfo <- function(i) {
    df <- osmWithDefaults[i,]
    tags=tagList[[i]]

    if (length(tags)>1) {
      
      cycleway_tags <- tags[which(tags %like% "cycleway")+1]
      if(any(is.na(cycleway_tags))) cycleway_tags <- c()
      bicycle_tags <- tags[which(tags=="bicycle")+1]
      if(any(is.na(bicycle_tags))) bicycle_tags <- c()
      car_tags <- tags[which(tags %in% c("car","motor_vehicle"))+1]
      if(any(is.na(car_tags))) car_tags <- c()
      foot_tags <- tags[which(tags %like% "foot")+1]
      if(any(is.na(foot_tags))) foot_tags <- c()
      surface_tags <- tags[which(tags=="surface")+1]
      if(any(is.na(surface_tags))) surface_tags <- c()
      oneway_tags <-  as.character(tags[which(tags=="oneway")+1])
      if(length(oneway_tags)==0) oneway_tags <- c()
      
      if("maxspeed" %in% tags) {
        maxSpeed=as.integer(tags[which(tags=="maxspeed")+1])
        # added this as some links had weird "masxspeed" values such as 500km/h!
        # 150km/h limit might cause issues for autobahns in Germany, AJ Jan 2021.
        if(!(is.na(maxSpeed)) & 140 < maxSpeed){
          message("Skiping speeds higher than 140km/h from OSM, consider editing processOSMTags.R if higher speeds are desired - AJ Jan 2021.")
          freeSpeed <- NA
        }else{
          freeSpeed=maxSpeed/3.6
        }
        # added is.na since one of the maxspeed has a value of "50; 40"
        if(!is.na(freeSpeed)) {
          df$freespeed[1]=freeSpeed
        }
      }
      if("lanes" %in% tags) {
        newLanes=as.integer(tags[which(tags=="lanes")+1])
        # some osm tags set the number of lanes to zero
        # added is.na since one of the lanes has a value of "2; 3"
        if(!is.na(newLanes) & newLanes > 0) {
          # Lane capacity is per lane and should not be adjusted based on number of lanes
          # df$laneCapacity[1]= df$laneCapacity[1] * (newLanes/df$permlanes[1])
          df$permlanes[1]=newLanes
        }
      }
      
      df$surface[1]=surface_tags
      if(any(oneway_tags=="yes")) df$is_oneway[1]=1
      #if(any(bicycle_tags %in% c("yes","designated"))) df$cycleway[1]="unmarked"
      if(any(cycleway_tags=="shared_lane")) df$cycleway[1]=1
      if(any(cycleway_tags=="lane") & df$highway[1]!="cycleway") df$cycleway[1]=2
      if(any(cycleway_tags=="track")& df$highway[1]!="cycleway") df$cycleway[1]=3
      if(any(foot_tags=="no")& df$highway[1]=="cycleway") df$cycleway[1]=5
      if(any(car_tags=="no")) df$is_car[1]=0
      if(any(foot_tags=="no")) df$is_walk[1]=0
      if(any(foot_tags %in% c("yes","designated"))) df$is_walk[1]=1
      if(df$cycleway[1]>0 | any(bicycle_tags %in% c("yes","designated"))) df$is_cycle[1]=1
      if(any(bicycle_tags %in% "no")) df$is_cycle[1]=0
    }
    return(df)
  }

  osmAttributed <- lapply(1:nrow(osmWithDefaults),getMetadataInfo) %>%
    bind_rows() %>%
    # looks like the ones with no modes are mostly closed walking or cycling tracks
    filter(is_cycle+is_walk+is_car>0)
    
  return(osmAttributed)
}
