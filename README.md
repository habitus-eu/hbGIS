[![R-CMD-check](https://github.com/habitus-eu/hbGIS/actions/workflows/r_new.yml/badge.svg)](https://github.com/habitus-eu/hbGIS/actions/workflows/r_new.yml)


# hbGIS

R package to analyse relationships between when, where and what behaviour takes place, by combining the output from package hbGPS (or old software library palms) and GIS files.
The code in this packages is an adaptation of the code in the R package palmsplusr.
In particular it has been modified to:
- Be build as a CRAN package
- Be used inside the Shiny app HabitusGUI
- Be used based on a configuration file
- Be used as a generic tool


## Installation

```
install.packages("remotes")
remotes::install_github("habitus-eu/hbGIS")
```

## Usage

The code below shows an example of how use hbGIS.


```
library(hbGIS)
hbGIS(gisdir = "D:/myproject/GIS", # path to GIS file
      palmsdir = "D:/myproject/hbGPSoutput", # path to palms or hbGPS output folder
      gislinkfile = "D:/myproject/Tables/participant_basis.csv", # same as palmsplusr, see documentation below
      outputdir = "D:/myproject/", # path to output folder
      dataset_name = "myproject", # dataset name 
      configfile = "D:/myproject/config_hbGIS.csv", # hbGIS config file (see note below)
      baselocation = "home", # base for individuals, see below (leave empty if not available)
      groupinglocation = "school",  # grouping for individuals, see below (leave empty if not available)
      write_shp = FALSE, # whether to store shape files as output
      split_GIS = TRUE, # whether to split GIS files in sublocations (only for public places)
      sublocationID = "ID_NR") # column name in GIS file to identify sublocation

```

Note:
- GIS filenames are used as location names and at the moment the code can only handle names that are shorter than 6 characters.
- Example config file can be found [here](https://github.com/habitus-eu/HabitusGUI/tree/main/inst/testfiles_hbGIS/config_hbGIS.csv)


### gislinkagefile

the `gislinkagefile` is used by hbGIS to match GIS shape files to individuals. Here a distinction is made between locations that are primarily link individuals and locations that primarily link groups, refer to as `baselocation` and `grouplocation`, respectively. The `gislinkagefile` was called `participant_basis` in palmsplusr, but as it was not documented and renamed here a description of what is expected. 

If no `gislinkagefile` is used then hbGIS simply analyses when any participant visits the geographical areas as defined by the GIS shape files. When using a `gislinkagefile` then hbGIS uses this to tailor the analyses. This comes with the following assumptions about the data format:

- gislinkagefile has a column named `identifier` referring to the participant identifier code that matches with the identifier extracted from the wearable sensor data.
- gislinkagefile has one or multiple `x_id` columns referring to the location id for location `x`. For example, `school_id` or `office_id`. The string `_id` needs to be part of the column name.
- GIS shape files corresponding to the `baselocation` to have a column named `identifier` which values match the values of the `identifer` column in the `gislinkagefile`.
- GIS shapefiles corresponding to the `groupinglocation` to have a column named `x_id` which name and values match the name and values of the `x_id` column in the `gislinkagefile`.

## Output

hbGIS will create four output files:

Name                          | Content
------------------------------|----------------
datasetname_whenwhatwhere.csv | Time series with information about when what where happened
datasetname_days.csv          | Day level summaries
datasetname_trajectories.csv  | Trajectory based summaries
datasetname_multimodal.csv    | Breakdown of trajectories by mode of transport


Abbreviations used:

Abbreviation | Meaning
-------------|-------------
mot          | mode of transport
iov          | indoor outdoor vehicle
nbh          | neighbourhood
dow          | day of week
