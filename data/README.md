# Network generation data

This directory contains input files required to generate MATSim networks for Bendigo and Melbourne, which can be found [here](https://osf.io/ajycn/). 

## Files to download

Download the following files for the relevant network location.

### Bendigo
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_bendigo.sqlite   | Boundary of the Greater Bendigo Local Government Area |
| dem_bendigo.tif          | Digital elevation model data for the Greater Bendigo area |
| [to come]                | [NDVI] data for the Greater Bendigo area              |
| gtfs.zip                 | GTFS feed for Victoria as at 20 October 2023      |


### Melbourne
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_melbourne.sqlite | Boundary of the Greater Melbourne Greater Capital City Statistical Area |
| dem_melbourne.tif        | Digital elevation model data for the Greater Melbourne area |
| [to come]                | [NDVI] data for the Greater Melbourne area        |
| gtfs.zip                 | GTFS feed for Victoria as at 20 October 2023      |


## Other files

The directory also contains the following other files, from which the region boundary files above were created.

| File                            | Description                                  |
|---------------------------------|----------------------------------------------|
| LGAs.zip                        | Local government areas of Victoria (Vicmap)  |
| GCCSA_2021_AUST_SHP_GDA2020.zip | Greater capital city statistical areas (ABS) |


The file `data/data prep tools.R` contains:
* the script used to extract the region boundary files from the LGA and GCCSA files above, and
* the script used to crop the digital elevation files from a DEM file for the whole of Victoria (available for download from https://discover.data.vic.gov.au/dataset/vicmap-elevation-dem-10m, 9.3 GB).  
Those scripts may also be useful to generate similar data input files for other locations if required.
