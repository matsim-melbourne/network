# MATSim network for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=dev)

This page explains the steps for building a road network model for active transport simulation models such as MATSim, including active transportation related infrastructure and attribute.    

## Prerequisites
* Postgres
* GDAL/OGR
* R
* Required R packages

## Building the network

Network generation code is written primarily in R programming language, with some accompanying SQL and Bash scripts, therefore a working knowledge of R is expected.

To get started, you must first prepare the required input files for generating the network.
There are multiple entry points to the algorithm. If you want to start from raw OSM extract, download the extract for your region. Please note that it should be in .osm format, .osm.pbf is not yet supported. You can use [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert) easily convert .osm.pbf to .osm.

All required R packages must be installed before running the algorithm.
`librarian` package in R automatically takes care of this for you when running the algorithm, however, you need internet connection in case you have a missing package.
Before running the algorithm, adjust the parameters and input/output file names for your scenario in `NetworkGenerator.R`.
Adjustable parameters are listed under the Parameters sub-heading.

To run the network generation algorithm from the terminal, you need to run something like below, specifying your desired output folder name as the argument for `makeNetwork()`:
```
Rscript -e 'source("NetworkGenerator.R"); makeNetwork("example")'

```

## Troubleshooting
### Postgres install on macOS 10.15+
```
brew install postgres
brew install postgis
brew services start postresql
/usr/local/opt/postgres/bin/createuser -s postgres
```
If you have osgeo-postgis you have may to unlink that and link the newly installed postgis as follows.
```
brew unlink osgeo-postgis
brew link postgis
```
Then restart your machine.

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

## SRL configuration
### Requirements and parameters
The `addGtfsLinks()` function in `gtfs2PtNetwork.R` includes the creation of an assumed timetable for the SRL, which forms part of the public transport timetable.

The function requires requires the file `srl_stg1.sqlite`, in EPSG:28355 - GDA 94 / MGA zone 55, containing two layers:
- stations (point), with a 'sequence' field listing order of stations along line from one end to other
- lines (linestring), containing separate line features each joining two adjacent stations

The parameters contained in the function (which are adjustable) provide for SRL services to run:
- between 5am and 12 midnight
- every 6 minutes from 7am to 9am and 4pm to 6pm, and every 10 minutes at other times.



## Publications
- Jafari, A., Both, A., Singh, D., Gunn, L., & Giles-Corti, B. (2021). [Building the road network for city-scale active transport simulation models](https://arxiv.org/abs/2104.03063). *arXiv preprint arXiv:2104.03063.*
