# Network generation data

This directory contains input files required to generate MATSim networks for Bendigo and Melbourne, which can be found [here](https://osf.io/ajycn/). 

## Files to download

Download the following files for the relevant network location.

### Bendigo
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_bendigo.sqlite   | Boundary of the Greater Bendigo Local Government Area |
| [to come]                | Digital elevation model data for the Greater Bendigo area |
| [to come]                | [NDVI] data for the Greater Bendigo area              |
| gtfs.zip                 | GTFS feed for Victoria as at 20 October 2023      |


### Melbourne
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_melbourne.sqlite | Boundary of the Greater Melbourne Greater Capital City Statistical Area |
| [to come]                | Digital elevation model data for the Greater Melbourne area |
| [to come]                | [NDVI] data for the Greater Melbourne area        |
| gtfs.zip                 | GTFS feed for Victoria as at 20 October 2023      |


## Other files

The directory also contains the following other files, from which the region boundary [and DEM] files above were created, using the code contained in `data/data prep tools.R`.  That code may also be useful to generate similar data input files for other locations if required.

| File                            | Description                                |
|---------------------------------|--------------------------------------------|
| LGAs.zip                        | Local government areas of Victoria (VICMAP)  |
| GCCSA_2021_AUST_SHP_GDA2020.zip | Greater capital city statistical areas (ABS) |
| [to come]                       | Digital elevation model data for Victoria  |

