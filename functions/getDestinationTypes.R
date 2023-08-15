# functions to locate specific types of destinations

## All tag combinations below can be applied to both points and polygons, except
## railway stations which are a combination of points, polygons and lines, and
## require aggregation within a boundary distance

# 1 open space ----
getPlayground <- function(layer) {
  return(layer %>% filter(leisure == "playground"))
}

getPark <- function(layer) {
  return(layer %>% filter(leisure == "park"))
}


# 2 sport ----
getSport <- function(layer) {
  return(layer %>% filter(!is.na(sport)))
}


# 3 lifelong learning ----
getKindergarten <- function(layer) {
  return(layer %>% filter(amenity == "kindergarten" | school == "kindergarten"))
}

getCommunity <- function(layer) {
  return(layer %>% filter(amenity == "community_centre"))
}

getLibrary <- function(layer) {
  return(layer %>% filter(amenity == "library"))
}


# 4 schools ----
getPrimary <- function(layer) {
  return(
    layer %>%
      rowwise() %>%
      mutate(lowest_grade = unlist(strsplit(grades, "-"))[1]) %>%
      ungroup() %>%
      filter(amenity == "school" & 
               (as.numeric(lowest_grade) < 7 | 
                  lowest_grade %in% c("P", "K") |
                  school %in% c("primary", "primary;secondary") |
                  isced_level %in% c("0", "0-1", "0-2", "0-3", "0;1", "0;2", "0;3",
                                     "1", "1-2", "1-3", "1;2", "1;3") | 
                  grepl("Primary", name)) &
               # omit other types such as special_education_needs, prison
               (school %in% c("primary", "primary;secondary") | is.na(school)))
  )
}

getSecondary <- function(layer) {
  return(
    layer %>%
      rowwise() %>%
      # some are eg "5-8; 10-12' or '0-4;9' - first, find the grade after the 
      # last hyphen, then find the grade after the last semi=colon
      mutate(highest_grade = dplyr::last(unlist(strsplit(grades, "-")))) %>%  
      mutate(highest_grade = dplyr::last(unlist(strsplit(highest_grade, ";")))) %>%
      ungroup() %>%
      filter(amenity == "school" & 
               (as.numeric(highest_grade) >= 7 | 
                  school %in% c("secondary", "primary;secondary") |
                  isced_level %in% c("0-2", "0-3", "0;2", "0;3", 
                                     "1-2", "1-3", "1;2", "1;3",
                                     "2", "3", "2-3", "2;3") | 
                  (grepl("Secondary", name) | grepl("High ", name))) &  # space after "High" to avoid eg "Highview Primary"
               # omit other types such as special_education_needs, prison
               (school %in% c("secondary", "primary;secondary") | is.na(school)))
  )
}


# 5 health ----
getClinic <- function(layer) {
  return(layer %>% 
           filter(amenity %in% c("clinic", "doctor", "doctors") | 
                    healthcare %in% c("clinic", "doctor", "doctors")))
}

getDentist <- function(layer) {
  return(layer %>% filter(amenity == "dentist" | healthcare == "dentist"))
} 

getPharmacy <- function(layer) {
  return(layer %>%
           filter(amenity %in% c("chemist", "pharmacy") | 
                    healthcare %in% c("chemist", "pharmacy") |
                    shop %in% c("chemist", "pharmacy")))
}


# 6 shopping ----
getConvenience <- function(layer) {
  return(layer %>% filter(shop == "convenience"))
}

getSupermarket <- function(layer) {
  return(layer %>% filter(shop == "supermarket"))
}

getShop <- function(layer) {
  return(layer %>% filter(!(is.na(shop))))
}

getPost <- function(layer) {
  return(layer %>% filter(amenity == "post_office"))
}

getBank <- function(layer) {
  return(layer %>% filter(amenity == "bank"))
}


# 7 eating ----
getRestaurant <- function(layer) {
  return(layer %>% filter(amenity == "restaurant"))
}

getCafe <- function(layer) {
  return(layer %>% filter(amenity == "cafe"))
}

# 8 railway stations ----
# Returns list of  stations as points
# Note the buffer distance of 100m below; closest railway stations in Melbourne are
# Riversdale & Willison (about 420m)
getStation <- function() {
  # general filter to find station objects
  filterStation <- function(layer) {
    return(layer %>%
             filter((public_transport == "station" | public_transport == "stop_position") & 
                      (railway == "station" | railway == "stop" | train == "yes" | 
                         grepl("train", tolower(network)) | grepl("train", tolower(operator))) &
                      (is.na(tourism) |  tourism != "yes") &
                      (is.na(railway) | railway != "construction")))
  }
  
  # find each object, and buffer to 100m
  buff.dist <- 100
  station.pt <- filterStation(points) %>% st_buffer(buff.dist)
  station.poly <- filterStation(polygons) %>% st_buffer(buff.dist)
  station.line <- filterStation(lines) %>% st_buffer(buff.dist)
  
  # dissolve, then separate to individual polygons
  stations <- bind_rows(station.pt, station.poly, station.line) %>%
    st_union() %>%
    st_as_sf() %>%
    st_cast("POLYGON") %>%
    st_centroid() %>%
    # label geometry column
    rename(geometry = x)
  
}

