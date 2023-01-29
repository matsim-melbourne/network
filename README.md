# MATSim network for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=dev)

This page explains the steps for building a road network model for active transport simulation models such as MATSim, including active transportation related infrastructure and attribute.    

## Publications
- Jafari, A., Both, A., Singh, D., Gunn, L., & Giles-Corti, B. (2022). [Building the road network for city-scale active transport simulation models](https://doi.org/10.1016/j.simpat.2021.102398). *Simulation Modelling Practice and Theory*, 114, 102398 ( [Pre-print version](https://arxiv.org/abs/2104.03063) )

## Prerequisites
* Postgres and postgis
* GDAL
* R 4.2+

## Building the network

Network generation code is written primarily in R programming language, with some accompanying SQL and Bash scripts, therefore a working knowledge of R is expected.

To get started, you must first prepare the required input files for generating the network.
There are multiple entry points to the algorithm. If you want to start from raw OSM extract, download the extract for your region. Please note that it should be in .osm format, .osm.pbf is not yet supported. You can use [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert) easily convert .osm.pbf to .osm.

All required R packages must be installed before running the algorithm. `renv` will take of that for you and you just need to run the following in R to install the packages:
```
install.packages("renv")
renv::restore()
```

Before running the algorithm, adjust the parameters and input/output file names for your scenario in `NetworkGenerator.R`.
Adjustable parameters are listed under the Parameters sub-heading.

To run the network generation algorithm from the terminal, you need to run something like below, specifying your desired output folder name as the argument for `makeNetwork()`:
```
Rscript -e 'source("NetworkGenerator.R"); makeNetwork("example")'

```

## Troubleshooting
### Installing sf
SF package in R requires a few dependencies, see https://r-spatial.github.io/sf/ for more details.

### Password authentication error when using postgres
One solution is to temporarly change the METHOD in `pg_hba.conf` from md5 to trust. See [this post](https://hassanannajjar.medium.com/how-to-fix-error-password-authentication-failed-for-the-user-in-postgresql-896e1fd880dc) for more details.


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
