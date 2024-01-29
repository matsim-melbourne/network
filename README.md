# MATSim network for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=dev)

This page explains the steps for building a road network model for active transport simulation models such as MATSim, including active transportation related infrastructure and attributes.    

## Publications
- Jafari, A., Both, A., Singh, D., Gunn, L., & Giles-Corti, B. (2022). [Building the road network for city-scale active transport simulation models](https://doi.org/10.1016/j.simpat.2021.102398). *Simulation Modelling Practice and Theory*, 114, 102398 ( [Pre-print version](https://arxiv.org/abs/2104.03063) )

## Prerequisites
* GDAL
* R 4.2+

## Building the network

Network generation code is written primarily in R programming language,therefore a working knowledge of R is expected.

All required R packages must be installed before running the algorithm. `renv` will take of that for you and you just need to run the following in R to install the packages:
```
install.packages("renv")
renv::restore()
```

Before running the algorithm, adjust the parameters and input/output file names for your scenario in `NetworkGenerator.R`.
Adjustable parameters are listed under the Parameters sub-heading.

Running the algorithm requires an input parameter 'city', and adjustable parameters must be completed for that city, specifying locations of relevant input files and the applicable CRS. If running for a location for which 'city' parameters have not already been defined, then these must be added, using existing city parameters as a template.

The city parameters are as follows.
* region - required if OSM extract is to be downloaded, or destinations (see below) are to be extracted. This must be the location of a file in sqlite format which defines the boundary of the area for which the OSM extract is required.
* outputCrs - specify the appropriate EPSG coordinate reference system number for the region.
* osmGpkg - the location to which an OSM extract in .gpkg format will be saved, or where an existing .gpkg file is stored if already held.
* unconfiguredSqlite - the location to which an unconfigured network in .sqlite format will be saved, or where an existing unconfigured network is stored if already held.
* cropAreaPoly - an optional parameter for cropping the OSM extract to a smaller test area. 
* demFile - required if 'addElevation' is set to 'T'.  This must be the location of a digital elevation model raster file in the same CRS as the the network.
* ndviFile - required if 'addNDVI' is set to 'T'.  This must be the location of a raster file with NDVI values in the same CRS as the network.
* gtfs_feed - required if 'addGtfs' or 'addDestinationLayer' is set to 'T'.  This must be the location of a zip file containing GTFS data.

The parameters assume that the relevant files are stored in the a 'data' subdirectory.

The algorithm will do the following:
* if 'downloadOsm' is set to 'T', download an OSM extract for the selected 'region' and save it as a .gpkg file.
* if 'networkFromOsm' is set to 'T', process the downloaded OSM extract to an unconfigured network in the form of an .sqlite file with layers of nodes, edges and osm tags ('osm_metadata').
* simplify the network, producing an output network in .sqlite format (with options to select .shp and .xml formats as well).
* if 'addDestination' is set to 'T', include a layer of destination points of interest for use in accessibility analysis, such as as supermarkets, doctors and schools, drawn from OSM and GTFS layers.

To run the network generation algorithm from the terminal, you need to run something like below, specifying your city and your desired output folder name as the arguments for `makeNetwork()`:
```
Rscript -e 'source("NetworkGenerator.R"); makeNetwork(, "Melbourne", "example")'

```

## Troubleshooting
### Installing sf
SF package in R requires a few dependencies, see https://r-spatial.github.io/sf/ for more details.

### iGraph install on macOS
If running R from a homebrew install, be sure to unlink suite-sparse before installing iGraph
```
brew unlink suite-sparse
```

### sf install on macOS
The sf library requires the following to run
```
brew install gdal
brew install udunits
```
