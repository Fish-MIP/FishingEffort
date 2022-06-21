
# Aggregated LME/EEZ/FAO ----

rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library("raster")

# Steps: 
# Plot global effort and check old version (Yannick) and new version (aggregation by Cami/Rich) match
# do the same at LME level 
# Compare with original gridded files - are the global sum for 1 year the same? 
# Old Data on ISIMIP: DOI: https://data.isimip.org/datasets/98037e0f-04cd-41ea-a5e3-9e4b1e351418/
# talk to Matthias to replace old with new version. 

## Load data ----
yannick_dir_new <- "/rd/gem/private/users/yannickr"
# yannick_dir_old <- "/rd/gem/private/DataYannick"

# NEW ----
effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
head(effort_new)

# Adjust new effort and print final version for FishMIP after discussion wit Julia 
# 1. aggregate by industrial and Artisanal (UP + APW) 
# 2. consider only 1961 -2010 included. 
# 2.b cut unused columns 
# 3. print final -> effort_histsoc_1961_2010.csv
# 4. when ready adn after talking with Matthias, upload to DKRZ where he wants. 
# it should be somewhere around here isimip3a/input/socioeconomic/fishing

# other notes: 
# Emma/IMAS: the datafile here are differnt than the one on fishmip -> 
# they are aggregated as per Yannick info/papers (e.g. industrial, UP and APW) and they include gridded 
# Emma would like to have raster files instead of csv 
# but, given we typically use netcdf, we could directly provide this format for the gridded data instead of rasters or csv
# this needs to be further discussed (after Easter with Julia)

# # fist approach
# # spread(Sector, NomActive)
# effort_new_1<-effort_new  %>% 
#   group_by(eez_country_name, LME, SAUP, Gear, FGroup, Year, Sector) %>% 
#   summarise(NomActive = sum(NomActive, na.rm = TRUE),
#             # EffActive = sum(EffActive, na.rm = TRUE), # no need for EffActive
#             NV = sum(NV, na.rm = TRUE),
#             P = sum(P, na.rm = TRUE)) %>% 
#   ungroup()
# 
# head(effort_new_1)
# 
# effort_new_2<-effort_new_1 %>% 
#   spread(Sector, NomActive) # what about P and NV?? too slow and not worth - option below 
#   
# head(effort_new_2)
# 
# effort_new_3<-effort_new_2 %>% 
#   mutate(A=APW+UP) %>% 
#   select(-APW,-UP)
# 
# head(effort_new_3)
# 
# effort_new_3 # gather ....  
# 
# # explore LME 42 to check 
# 
# lem42<-effort_new_3 %>% 
#   filter(LME == 42)
# 
# head(lme42)
# 
# ggplot(lme42, aes(x = Year, Y = NomActive, group = Sector, Color = Sector))+
#   geaom_point()+
#   geom_line()


# second approach 
effort_new_Artisanal<-effort_new %>% 
  filter(Sector !="I") %>% 
  group_by(eez_country_name, LME, SAUP, Gear, FGroup, Year) %>% # delete fao_area
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            # EffActive = sum(EffActive, na.rm = TRUE), # no need for EffActive
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% # no need for GT
  # GT = sum(GT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Artisanal")

head(effort_new_Artisanal)

# effort_new_Industrial<-effort_new %>% 
#   filter(Sector =="I") %>% 
#   mutate(Sector = "Industrial")

effort_new_Industrial<-effort_new %>% 
  filter(Sector == "I") %>% 
  group_by(eez_country_name, LME, SAUP, Gear, FGroup, Year) %>% # delete fao_area
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            # EffActive = sum(EffActive, na.rm = TRUE), # no need for EffActive
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% # no need for GT
  # GT = sum(GT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Industrial")

head(effort_new_Industrial)

# put them back together
colnames(effort_new_Artisanal)
colnames(effort_new_Industrial)
effort_tot<-rbind(effort_new_Industrial, effort_new_Artisanal)
head(effort_tot)

nrow(effort_tot)
nrow(effort_new_Artisanal)+nrow(effort_new_Industrial)

# effort to calculate spin-up

effort_spinup<-effort_tot %>% 
  filter(Year >= 1950, Year <= 2017) # this should be the default years for effort 

write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017.csv") 

## NOTE: the below (besides checking) si not necessary anymore as the final effort data will include spin-up - all data on DKRZ need to be replaced 

trial<-effort_tot %>% 
  filter(Year >= 1961, Year <= 2010)

# explore lme42 and 38 for checking - OK plots match

unique(trial$LME)

lme42<-trial %>% 
  filter(LME == "38") %>% 
  group_by(Year, Sector) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

head(lme42)

ggplot(lme42, aes(x = Year, y = NomActive, group = Sector, color = Sector))+
  # geom_point()+
  geom_line()

write.csv(trial, "/rd/gem/private/users/yannickr/effort_histsoc_1961_2010.csv") # this is the effort data for model runs (to which spin-up needs to be added)

# load data from gem48 to isismip ----

# scp -r effort_histsoc_1961_2010.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/

# gridded -----

yannick_dir_new <- "/rd/gem/private/users/yannickr/otherdata"
effort<-read_csv(file.path(yannick_dir_new, "GriddedEffortby_FGroup_FishingCountry_Sector.csv"))
head(effort)
effort2<-effort[-1,]
head(effort2)

# provide gridded industrial ----

# effort3<-effort2 %>% 
#   group_by(Lat, Lon, Year, Sector) %>% 
#   summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
#   ungroup()
# 
# head(effort3)
# 
# effort4<-effort3 %>% 
#   spread(Sector, NomActive)
# 
# head(effort4)
# 
# # this is industrial 
# effort5<-effort4 %>% 
#   dplyr::select(-APW, -UP) %>% 
#   dplyr::rename(NomActive = I) %>% 
#   filter(!is.na(NomActive), Year >=1961, Year <=2010)
# 
# head(effort5)
# sort(unique(effort5$Year))

# OR... 

effort5<-effort2 %>% 
  filter(Sector == "I") %>% 
  group_by(Lat, Lon, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >=1961, Year <=2010)

sort(unique(effort5$Year))

IndustrialEffort<-effort5
# View(IndustrialEffort)

# check ----
# before you rearrange data 
trial_I<-effort2 %>% 
  filter(Year == 2010, Sector == "I") %>% 
  group_by(Lat, Lon) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

trial_I_2<-acast(trial_I, Lat ~ Lon)
plot(raster(log10(trial_I_2))) # something wrong with lat/lon

# try this
# plot(rasterFromXYZ(trial_I %>% rename(y = Lat, x = Lon, z = NomActive))) # still not working so check the lat/lon

# after you rearrange data 
trial_I_new<-effort5 %>% 
  filter(Year == 2010)

trial_I_2_new<-acast(trial_I_new, Lat ~ Lon)
plot(raster(log10(trial_I_2_new))) # OK same plot 

write.csv(IndustrialEffort, "/rd/gem/private/users/yannickr/gridded_industrial_effort_histsoc_1961_2010.csv")

# load data from gem48 to isismip ----

# scp -r gridded_industrial_effort_histsoc_1961_2010.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/

# provide effort gridded artisanal ----

# effort6<-effort4 %>%
#   dplyr::select(-I) %>%
#   mutate(NomActive = UP+APW) %>%
#   filter(!is.na(NomActive), Year >=1961, Year <=2010)
#  
# head(effort4)
# above is wrong 
effort6<-effort2 %>% 
  filter(Sector != "I") %>% 
  group_by(Lat, Lon, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >=1961, Year <=2010)

head(effort6)
sort(unique(effort6$Year))
ArtisanalEffort<-effort6

# check 
# before you rearrange data 
trial_I<-effort2 %>% 
  filter(Year == 2010, Sector != "I") %>% 
  group_by(Lat, Lon) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

trial_I_2<-acast(trial_I, Lat ~ Lon)
plot(raster(log10(trial_I_2))) # something wrong with lat/lon

# after you rearrange data 
trial_I_new<-effort6 %>% 
  filter(Year == 2010)

trial_I_2_new<-acast(trial_I_new, Lat ~ Lon)
plot(raster(log10(trial_I_2_new))) # OK 

write.csv(ArtisanalEffort, "/rd/gem/private/users/yannickr/gridded_artisanal_effort_histsoc_1961_2010.csv")

# load data from gem48 to isismip ----

# scp -r gridded_artisanal_effort_histsoc_1961_2010.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/

# catch ----
# dowload from DKRZ under 3b 
# scp -r b381217@levante.dkrz.de:/work/bb0820/ISIMIP/ISIMIP3b/InputData/socioeconomic/fishing/catch_histsoc_1950_2014.csv

# read in, rename, save  
yannick_dir_new<- "/rd/gem/private/users/yannickr"
catch<-read_csv(file.path(yannick_dir_new, "catch_histsoc_1950_2014.csv"))
head(catch)

catch2<-catch %>% 
  filter(Year >= 1961, Year<=2004)

write.csv(catch2, "/rd/gem/private/users/yannickr/calibration_catch_histsoc_1961_2004.csv")

# load data from gem48 to isismip ----

# scp -r calibration_catch_histsoc_1961_2004.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/







