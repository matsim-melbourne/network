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

## Publications
- Jafari, A., Both, A., Singh, D., Gunn, L., & Giles-Corti, B. (2021). [Building the road network for city-scale active transport simulation models](https://arxiv.org/abs/2104.03063). *arXiv preprint arXiv:2104.03063.*
