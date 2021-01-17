# MATSim network for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=dev)

This page explains the steps for building a MATSim network for Melbourne, including active transportation related infrastructure and attributes. To do so, you can start from a raw OSM extract (step 1) or from a set of nodes, edges and edge attributes in a format similar to OSM (step 2).    

## Prerequisites
* Postgres [Step 1]
* GDAL/OGR [Step 1]
* R [Step 2]
* Required R packages [Step 2]

## Building the network

### Step 0: Download the required inputs

To get started, you must first download the required input files for generating the network. The required input files depend on the selected entry point and what functions are going to be used during network generation. See `./data/README.md` for more details about the input files. A script is also provided that can be used to download relevant input files. For example, if the starting point is from raw OSM (entry point 1) and GTFS2PT is also going to be used to generate PT network, the following script will download the required inputs:
```
cd data && ./prepare.sh -osm19 -gtfs19
```

### Step 1: Raw OSM processing

This step processes the raw OSM data and generates two outcomes: `network.sqlite` and  `melbourne.sqlite`.

The only required inputs for this step is `melbourne.osm`.
```
cd data && ./prepare.sh -osm19
```
Once the required input is downloaded, run `./processOSM.sh` to process raw OSM data:
```
./processOSM.sh
```
**NOTE** To skip this step, you can download the previously generated outputs with the following script and go directly to step 2:
```
cd data && ./prepare.sh -melb -net
```

### Step 2: Generating MATSim network

Make sure to have the `newtork.sqlite` and `melbourne.sqlite` files from the previous step, in the data folder before starting this step.

MATSim network generation code is written in R programming language and a working knowledge of R is expected.
All required R packages must be installed before running the algorithm. `packrat` will take of that for you and you just need to run the following in R to install the packages:
```
install.packages("packrat")
packrat::restore()
```
Ensure that your setup is working by running:
```
testthat::test_dir("tests/testthat")
```
If all tests pass, you are all set to run the main algorithm.

MATSim network generation algorithm is a designed to be configurable depending on what features are desired. To tell the alogrithm what features needs to be included, you need to run `makeNetwork.sh` **with its predefined flags**:

| Argument | Description                                                                       |
|----------|-----------------------------------------------------------------------------------|
| -t       | Cropping to a small test area (Boundary can be adjusted by editing the code)      |
| -s       | simplifying the network, minimum link length=20m                                  |
| -z       | Adding elevation (requires the elevation data)                                    |
| -pt      | Adding pt from GTFS (requires the GTFS data)                                      |
| -xml     | Writing the output network in MATSim readable XML format                          |
| -shp     | Writing the output network in ESRI Shapefile format                               |
| -sqlite  | Writing the output network in SQLite format                                       |

**Note** Make sure to **at least specify one output format** for the `makeNetwork.sh`. For example, the minimum command for creating a MATSim readable output would be:

```
./makeNetwork.sh -xml
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
