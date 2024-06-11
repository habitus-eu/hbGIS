# Description of R package hbGIS

Note: This is not a practical guide on how to use `hbGIS`, but a description of every step in the software intended to be used for a documentation page and/or to inform methods paragraphs in scientific articles.

**COMMENTS:**

- Package `hbGIS` is largely based on the code from R package `palmsplusr`, but revised to be able to work inside another package such as `HabitusGUI` and able to handle configuration file as main mode of operation. 
- The terminology used by `palmsplusr` and adopted by `hbGIS` does not align well with terminology used in `hbGPS`. This complicates communicating output and making the code more accessible. For example, it uses the term multimodel trip also for trips that are not multimodal and it is not clear how the concept of a 'trajectory' relates to 'trips' and 'multimodal' trips. See also: https://github.com/habitus-eu/hbGIS/issues/18.

_(in function hbGIS.R)_

Function `hbGIS` forms the interface to all `hbGIS` package functionality. A typical user is not expected to interact with the other functions in the package. 

## 1. Loading configuration file

_(in function hbGIS.R)_

First we load the configuration file content. Here, the following information is directly extracted:

-	Character `groupinglocation` to indicate the name of the location to identify groups of individuals, e.g. school. For example, if a dataset has a home, school and park then typically home is only linked to one or a few individuals, a park is not clearly linked to individuals, and school is the main location that connects groups of individuals in a study and by that the groupinglocation. If there is no grouping location then leave blank (NULL).
-	Character `baselocation` to indicate the name of the location to identify the base location of an individual, which defaults to “home” if not provided.
-	Boolean `write_shp` to indicate whether the shape files should be stored (TRUE) or not (FALSE). This then stores for each category of output as discussed in the last section of this document the tabular information as a shapefile. Note that hbGIS does not need these files itself, which is why it is turned off by default (FALSE).
-	Boolean indicator `split_GIS` to indicate whether to analyse the GIS files split by sublocation (TRUE) or not (FALSE). For example when working with green spaces you may want to leave this as FALSE as you probably do not want to know trajectories between all possible combinations of individual green spaces.
-	Character `sublocationID` to indicate the name of the column name in the GIS files that reflects the sublocation ID (default in config file: "OBJECTID").

## 2. Prepare loca(tion) objects

_(in function hbGIS.R)_

The aim of this section is to get a clear overview of all GIS information and their geographical information. If you want `hbGIS` to treat locations as similar put them in one shape file, e.g. school.shp holding multiple schools, while if you want your analysis to distinguish categories of this location then store them in separate files, e.g. swimmingpools.shp, parks.shp, and gyms.shp.
First step is to identify the names of the location categories. We do this based on the shapefile names. Here, `hbGIS` only looks at shape files in the directory as specified by function argument gisdir (when working with HabitusGUI this folder is specified via the GUI, so you do not need to worry about gisdir). hbGIS extracts the location name by removing the following character from the filenames: table, _, buffers, . , xml, shp, and loc. The resulting unique names are the main location categories, e.g.: home, school, and park.

From this hbGIS builds internally an object named `loca`, which holds everything we want to know about the location categories such that we only have to load the GIS file content once. Object `loca` is an R vector of length equal to the number of location categories where each element is a list for that location and contains the following items (example list item for home):

NR | Item name    | Item content
---|--------------|-----------------------------------
1	 | home	        | shape object for home
2	 | home_nbh     | neighbourhood or sometimes called buffer object for home
3	 | home_tablefile |	Path to file used for item 1
4	 | home_locbufferfile	| Path to file used for item 2

## 3. Prepare palms object from either hbGPS output or PALMS output

_(in function hbGIS.R)_

This step loads the hbGPS output file(s). If there is one file named combined.csv then it will use this and assume hbGPS combined all recordings in that file, otherwise it will append all files it finds.

_(in function check_and_clean_palms_data.R)_

This data is then cleaned:
-	All column names are turned to lower case to ease standardisation.
-	Duplicated rows are removed.

_(in function hbGIS.R)_

The resulting object is saved as csv file and read by function read_palms  from R package palmsplusr to ensure the resulting object is as `palmsplusr` (and by that `hbGIS`) would expect.

## 4. Expand configuration object with location-based fields

_(in function hbGIS.R)_

A substantial part of the hbGIS code is written around the assumption that the user provides field objects, which are collection of formula’s. Given that most of these are deterministic following known location names, hbGIS autogenerates the field objects where possible. The exact formulas generated by hbGIS used are always stored in the hbGIS output folder in file formula_log.csv. Below follows an overview of each field object, the first three are autogenerated by hbGIS:

-	**where_field** with definitions for where a person has been: in transport, each of the locations defined by GIS objects, or other. Examples:
    -	transport: !at_home & !at_school & (pedestrian | bicycle | vehicle)
    -	other: !at_home & !at_school & (!pedestrian & !bicycle & !vehicle) & !at_home_nbh & !at_school_nbh
    -	home: at_home
    -	school: at_school
    - green1_nbh: at_green1_nbh & (!vehicle)
- **whenwhat_field** with definitions to classify each time point (when) and the what of that time point, this includes: whether it is a weekday or weekend day, whether a person is indoor or outdoor, in a vehicle, a pedestrian, on a bicyle, not wearing the accelerometer, sedentary, moderate, vigorous, and MVPA active, and whether the person is at each of the specified main locations. Examples:
    - weekday: dow < 6
    - pedestrian: tripmot == 1
    -	Some formulas refer to internal functions are probably more difficult to read, for example the following checks whether a person is in their home. Remember that as a user you do not need to write these formulas:
   -	at_home: palms_in_polygon(datai, polygons = dplyr::filter(home, identifier == i), identifier)
-	**trajectory_locations** with trajectory definitions as expected by palmsplusr/hbGIS code.  Examples:
    -	home_home: at_home
    -	home_school: at_home
    -	school_home: at_school
    -	school_school: at_school

**COMMENT:**
Logic of the field `trajectory_locations` is not clear. Seeing that there is a pattern in the values the code auto-generates value to not bother the user with it in the config file.

Two other fields are directly extracted from the configuration file:

-	**trajectory_field** with definitions of the variables that need to be calculated per trajectory, such as mode of transport (mot), date, start time, end time, duration, nonwear, PA intensity levels, length and speed.
-	**multimodal_field** with definitions of the variables that can be calculated for multimodal trips. Multimodal trips are effectively a series of 1 or more trips , where each trip can be in a different mode of transport. The variables it calculates are for example: duration, nonwear, and PA intensity level, length and speed.


## 5. gislinkagefile

_(in function hbGIS.R)_

A gislinkagefile (previously referred to as participant_basis file) is a file to show the relationship between participant ID and the IDs for the locations they are associated with. The gislinkagefile is supplied as an argument or passed on via HabitusGUI, but by default is set to empty (NULL in R).

If the gislinkagefile is present we ignore all individuals for whom no matching ID could be found in any of the GIS files. This is done in the following function:

_(in function check_missing_id.R)_

The hard-coded assumption for this part of the code is currently that the base location (default home) has id column name “identified”, while other locations are identified with a column name constructed as location name followed by “_id”, e.g. school_id. 
At the end of this step we only have data for recordings with matching ID in all locations. For example, a recording for a study participant without an assigned school will not get through this stage.

_(in function hbGIS.R)_

If the gislinkagefile is not present then all individuals are kept.

6. Apply formulas from fields to calculate a range of variablespalmsplusr functions

We can now run the original palmsplusr functionality that has been integrated in hbGIS. The main difference with the original code is that all objects are explicitly passed on to functions.
Per functionality the code uses one or two of the fields as defined above as summarised in the table below:

function | where_field	| whenwhat_field | trajectory_locations |	trajectory_field | multimodal_field
---------|--------------|----------------|----------------------|------------------|--------------
build_whenwhatwhere | V |              |                      |                  |
build_days	        | V	| V			       |                      |                  |
build_trajectories	|   |              | V                    | V	               |	
build_multimodal		|   |              |                      |                  | V
 
(in function build_whenwhatwhere.R, named build_palms in palmsplusr)
Creates a timeseries object describing what happened when and where. More specifically it takes the hbGPS (or PALMS) output that already has the where and applies the formulas in the whenwhat_field to each timepoint.

_(in function build_days.R)_

Creates a summary per unique combination of day, individual, and where_field location, with the following statistics:
-	Time spent in the whenwhat_field categories (nonwear, wear, sedentary, …) for that location
-	Total time spent in each location (where).
-	Count of the number of segments in time per location (where), which could be interpreted as the number unique visits to that location.

_(in function build_trajectories.R)_

Creates a summary per trajectory (trip). To do tis it looks at the trajectory_fields and splits them into formulas to be applied to points (where column is after_conversion = FALSE) and formulas to be applied to linestrings (where column after_conversion  = TRUE). Further, trajectory_locations are used to deifne define locations corresponding to each trajectory. First the code takes all data points corresponding to detected trips and applies the formulas per point. Next, the output is converted to a linestring geometry per trip, and to these the formulas for linestrings are applied.

**COMMENT:**
Proposal is to rename `after_conversion` to `apply_to_linestring`, to make it more obvious what it does. However, this means that the configuration files need to be updated.

_(in function build_multimodal.R)_

Creates a summary per (multimodal) trip. First a multimodal trip is defined as clusters of trips that are separated in time and space by less than a user-specified thresholds. Trips that do not meet these criteria are kept and act as a one trip multimodal trip:  

-	Descriptives of the (multimodal) trips such as total time in sedentary behaviour or average speed per trip. These are derived with multimodal_fields formulas.
-	Start and end location of each trip, and basic descriptives such as the number of trips in a multi-modal trips, the subsequent modes of transport order.
