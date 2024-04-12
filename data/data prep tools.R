# Tools for creating input data files for use in NetworkGenerator
# Currently these tools are applicable for Victoria only

library(tidyverse)
library(sf)
library(terra)

# 1 Region boundaries from LGA or GCCSA files ----
# -----------------------------------------------------------------------------#
# function for extracting specific region from an administrative districts file
getRegion <- function(input.file, input.field, input.name,
                      output.filename, outputCrs) {
  
  input.transformed <- input.file %>%
    st_transform(outputCrs)
  
  output <- input.transformed %>%
    filter(.data[[input.field]] == input.name)
  
  st_write(output, paste0("./data/", output.filename, ".sqlite"))
}

# function to extract zipped shapefile
# Note: 'subpath' is the string between the top zipped file and the ultimate file, eg "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMTRANS"
# 'file' not needed for files that don't have layers (eg shapefiles) if there is only one in the directory
# use 'file' (rather than 'layer') for shapefiles and mapinfo files; use both for gpkg and sqlite
read_zipped_GIS <- function(zipfile, subpath = "", file = NULL, layer = NULL) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  if (is.null(layer)) {
    st_read(paste0(temp, subpath, file))
  } else {
    st_read(paste0(temp, subpath, file), layer)
  }
}

# Greater Bendigo - from LGAs
getRegion(input.file = read_zipped_GIS(zipfile = "./data/LGAs.zip",
                                            subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN"),
               input.field = "NAME", input.name = "GREATER BENDIGO",
               output.filename = "greater_bendigo", outputCrs = 7899)


# Greater Melbourne - from GCCSA
getRegion(input.file = read_zipped_GIS(zipfile = "./data/GCCSA_2021_AUST_SHP_GDA2020.zip"),
               input.field = "GCC_NAME21", input.name = "Greater Melbourne",
               output.filename = "greater_melbourne", outputCrs = 7899)



# 2 Elevation from whole of state file ----
# -----------------------------------------------------------------------------#
# Download 10m DEM for Victoria from https://discover.data.vic.gov.au/dataset/vicmap-elevation-dem-10m,
# and manually unzip it into the data folder - about 9.3GB (programmatic unzipping 
# may not work for such a large file)

# function for extracting region's elevation from whole of state file
getRegionDem <- function(dem.location, region.location, regionBufferDist,
                         output.filename, outputCrs) {
  
  # read in the raster, and convert to outputCrs if necessary
  dem <- rast(dem.location)
  if (!same.crs(dem, outputCrs)) dem <- project(dem, outputCrs)

  # region, buffered to selected distance (eg 10km)
  region <- st_read(region.location) %>% st_buffer(regionBufferDist)
  
  # crop the DEM to the region
  dem.cropped <- terra::crop(x = dem, y = region %>% st_buffer(1))
  
  # write output
  writeRaster(dem.cropped, paste0("./data/", output.filename, ".tif"), 
              gdal = "COMPRESS = DEFLATE", overwrite = TRUE)
}

# Bendigo
getRegionDem(dem.location = "./data/vmelev_dem10m_ESRI_grid_GDA94_VicGrid/vmelev_dem10m_ESRI_grid_GDA94_Vicgrid/vmelev_dem10m/dem10m/hdr.adf",
             region.location = "./data/greater_bendigo.sqlite", regionBufferDist = 10000,
             output.filename = "dem_bendigo", outputCrs = "EPSG:7899")

# Melbourne
getRegionDem(dem.location = "./data/vmelev_dem10m_ESRI_grid_GDA94_VicGrid/vmelev_dem10m_ESRI_grid_GDA94_Vicgrid/vmelev_dem10m/dem10m/hdr.adf",
             region.location = "./data/greater_melbourne.sqlite", regionBufferDist = 10000,
             output.filename = "dem_melbourne", outputCrs = "EPSG:7899")

