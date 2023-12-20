![GitHub Actions R-CMD-check](https://github.com/habitus-eu/hbGIS/workflows/R-CMD-check-full/badge.svg)


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
hbGIS(gisdir = "D:/myproject/GIS",
                 palmsdir = "D:/myproject/hbGPSoutput",
                 gislinkfile = "D:/myproject/Tables/participant_basis.csv",
                 outputdir = "D:/myproject/",
                 dataset_name = "myproject",
                 configfile = "D:/myproject/config_hbGIS.csv")

```

Note: GIS filenames are used as location names and at the moment the code can only handle names that are shorter than 6 characters.

## Output

hbGIS will create four output files:

Name                        | Content
----------------------------|----------------
$datasetname_whenwhatwhere.csv | Time series with information about when what where happened
$datasetname_days.csv           | Day level summaries
$datasetname_trajectories.csv   | Trajectory based summaries
$datasetname_multimodal.csv     | Breakdown of trajectories by mode of transport


