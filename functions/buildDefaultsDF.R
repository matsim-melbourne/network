buildDefaultsDF <- function(){
  
  defaults_df <- tribble(
    ~highway          , ~permlanes, ~freespeed, ~is_oneway, ~laneCapacity, ~is_cycle, ~is_walk, ~is_car, ~highway_order,
     "motorway"       ,  4        ,  (110/3.6),  0        ,  2000        ,  0       ,  0      ,  1     ,  1            ,
     "motorway_link"  ,  2        ,  (80/3.6) ,  0        ,  1500        ,  0       ,  0      ,  1     ,  8            ,
     "trunk"          ,  3        ,  (100/3.6),  0        ,  2000        ,  1       ,  1      ,  1     ,  2            ,
     "trunk_link"     ,  2        ,  (80/3.6) ,  0        ,  1500        ,  1       ,  1      ,  1     ,  9            ,

     "primary"        ,  2        ,  (80/3.6) ,  0        ,  1500        ,  1       ,  1      ,  1     ,  3            ,
     "primary_link"   ,  1        ,  (60/3.6) ,  0        ,  1500        ,  1       ,  1      ,  1     , 10            ,
     "secondary"      ,  2        ,  (60/3.6) ,  0        ,  1000        ,  1       ,  1      ,  1     ,  4            ,
     "secondary_link" ,  1        ,  (60/3.6) ,  0        ,  1000        ,  1       ,  1      ,  1     , 11            ,

     "tertiary"       ,  1        ,  (50/3.6) ,  0        ,   600        ,  1       ,  1      ,  1     ,  5            ,
     "tertiary_link"  ,  1        ,  (50/3.6) ,  0        ,   600        ,  1       ,  1      ,  1     , 12            ,
     "residential"    ,  1        ,  (50/3.6) ,  0        ,   600        ,  1       ,  1      ,  1     ,  6            ,
     "road"           ,  1        ,  (50/3.6) ,  0        ,   600        ,  1       ,  1      ,  1     ,  7            ,
     "unclassified"   ,  1        ,  (50/3.6) ,  0        ,   600        ,  1       ,  1      ,  1     , 13            ,

     "living_street"  ,  1        ,  (40/3.6) ,  0        ,   300        ,  1       ,  1      ,  1     , 14            ,
     "service"        ,  1        ,  (40/3.6) ,  0        ,   300        ,  1       ,  1      ,  1     , 15            ,
     "cycleway"       ,  1        ,  (30/3.6) ,  0        ,   300        ,  1       ,  0      ,  0     , 16            ,
     "track"          ,  1        ,  (30/3.6) ,  0        ,   300        ,  1       ,  1      ,  0     , 17            ,

     "pedestrian"     ,  1        ,  (30/3.6) ,  0        ,   120        ,  0       ,  1      ,  0     , 18            ,
     "footway"        ,  1        ,  (10/3.6) ,  0        ,   120        ,  0       ,  1      ,  0     , 19            ,
     "path"           ,  1        ,  (10/3.6) ,  0        ,   120        ,  0       ,  1      ,  0     , 20            ,
     "corridor"       ,  1        ,  (10/3.6) ,  0        ,    50        ,  0       ,  1      ,  0     , 21            ,
     "steps"          ,  1        ,  (10/3.6) ,  0        ,    10        ,  0       ,  1      ,  0     , 22
  )
  message("Using default road attribute values in Australia, source https://github.com/agentsoz/bdi-abm-integration/blob/master/util/src/main/java/io/github/agentsoz/util/NetworkGenerator.java#L90-L163. Edit buildDefaults.R function if other values are desired for other countries.")
  
  return(defaults_df)
}