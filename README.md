# FishingEffort
Global and regional fishing effort data to integrate into models

This project aggregate gridded data to LME, FAO, and EEZ by country, gear, functional group, and year and explores trends in fishing effort. We also compare global fishing effort with regional fishing effort. There are numerous scripts that step through the process:

01_shapefiles_to_xyz.Rmd uses shapefiles for FAO regions to rasterise and convert into xyz dataframes. Preliminary conversion of alternate EEZ layers also in there.

02_merge_effort_spatial_data - merges effort data grouped by country with LME, FAO, and EEZ layers and aggregates by each.

03_NewVsOldData.R - plots the old and new effort data globally and by LME and checks for inconsistencies between versions.

04_ExploreData_RegionalvsGlobal.Rmd - plots global effort from newly aggregated data (i.e. inclusive of FAO area, EEZ, and LME, and gear) and compares trends in global effort for LME 42 with trends from effort data used to drive regional model for Southeast Australia


