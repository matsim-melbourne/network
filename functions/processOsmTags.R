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
  
  # pre splitting the tags to save time: replace tag separator "," with unique
  # symbol ',,'; replace key:value separator "=>" with same unique symbol ',,'; 
  # remove " at start and end of tag string; then split at the unique symbol ',,'
  tagList <-strsplit(gsub('"', '', gsub('"=>"', ',,', gsub('","', '",,"', osmWithDefaults$other_tags))), ',,')
  
  osmWithDefaults <- osmWithDefaults %>%
    mutate(cycleway=ifelse(highway=="cycleway",4,0)) %>%
    dplyr::select(osm_id,highway,highway_order,freespeed,permlanes,laneCapacity,is_oneway,cycleway,is_cycle,is_walk,is_car)

  getMetadataInfo <- function(i) {
    df <- osmWithDefaults[i,]
    tags=tagList[[i]]

    if (length(tags)>1) {
      
      # keys and values are odd and even-numbered tags respectively
      keys <- tags[seq(1, length(tags), by = 2)]
      values <- tags[seq(2, length(tags), by = 2)]
      
      cycleway_tags <- values[which(keys %like% "cycleway")]
      if(any(is.na(cycleway_tags))) cycleway_tags <- c()
      bicycle_tags <- values[which(keys=="bicycle")]
      if(any(is.na(bicycle_tags))) bicycle_tags <- c()
      car_tags <- values[which(keys %in% c("car","motor_vehicle"))]
      if(any(is.na(car_tags))) car_tags <- c()
      foot_tags <- values[which(keys %like% "foot")]
      if(any(is.na(foot_tags))) foot_tags <- c()
      surface_tags <- values[which(keys=="surface")]
      if(any(is.na(surface_tags))) surface_tags <- c()
      oneway_tags <-  as.character(values[which(keys=="oneway")])
      if(length(oneway_tags)==0) oneway_tags <- c()
      
      if("maxspeed" %in% keys) {
        maxSpeed=as.integer(values[which(keys=="maxspeed")])
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
      
      df$surface[1]=surface_tags
      if(any(oneway_tags=="yes")) df$is_oneway[1]=1
      #if(any(bicycle_tags %in% c("yes","designated"))) df$cycleway[1]="unmarked"
      if(any(cycleway_tags=="shared_lane")) df$cycleway[1]=1
      if(any(cycleway_tags=="lane") & df$highway[1]!="cycleway") df$cycleway[1]=2
      if(any(cycleway_tags=="track")& df$highway[1]!="cycleway") df$cycleway[1]=3
      if(any(foot_tags=="no")& df$highway[1]=="cycleway") df$cycleway[1]=5
      if(any(car_tags=="no")) df$is_car[1]=0
      if(df$is_car[1]==0 & any(bicycle_tags %in% c("yes", "designated")) & df$cycleway[1]<5) df$cycleway[1]=4
      if(any(foot_tags=="no")) df$is_walk[1]=0
      if(any(foot_tags %in% c("yes","designated"))) df$is_walk[1]=1
      if(df$cycleway[1]>0 | any(bicycle_tags %in% c("yes","designated"))) df$is_cycle[1]=1
      if(any(bicycle_tags %in% "no")) df$is_cycle[1]=0
      
      if ("lanes" %in% keys) {
        taggedLanes = as.integer(values[which(keys == "lanes")])
        # lanes is number of tagged lanes if one-way, or divide by 2 (rounded up) if two-way
        newLanes = ifelse(df$is_oneway[1] == 1, taggedLanes, ceiling(taggedLanes / 2))
        if (!is.na(newLanes) & newLanes > 0) {
          df$permlanes[1] = newLanes
        }
      }
    }
    return(df)
  }

  osmAttributed <- lapply(1:nrow(osmWithDefaults),getMetadataInfo) %>%
    bind_rows() %>%
    # looks like the ones with no modes are mostly closed walking or cycling tracks
    filter(is_cycle+is_walk+is_car>0)
    
  return(osmAttributed)
}
