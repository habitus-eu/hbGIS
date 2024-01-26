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
      gislinkfile = "D:/myproject/Tables/participant_basis.csv", # same as palmsplusr
      outputdir = "D:/myproject/", # path to output folder
      dataset_name = "myproject", # dataset name 
      configfile = "D:/myproject/config_hbGIS.csv", # hbGIS config file (see note below)
      baselocation = "home", # base for individuals (leave empty if not available)
      groupinglocation = "school",  # grouping for individuals (leave empty if not available)
      write_shp = FALSE, # whether to store shape files as output
      split_GIS = TRUE, # whether to split GIS files in sublocations (only for public places)
      sublocationID = "ID_NR") # column name in GIS file to identify sublocation

```

Note:
- GIS filenames are used as location names and at the moment the code can only handle names that are shorter than 6 characters.
- Example config file can be found [here](https://github.com/habitus-eu/hbGIS/blob/main/inst/testfiles_hbGIS/config_hbGIS.csv)

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
