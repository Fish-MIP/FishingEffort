

rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library("raster")

# Steps: 
# Adjust new effort and print final version for FishMIP after discussion wit Julia. 
# See 03 for comparison between old (Yannick) and new (Cami/Rich) effort
# 1. aggregate by industrial and Artisanal (UP + APW) 
# 2. consider only 1961-2010 included. 
# 2.b cut unused columns 
# 3. print final -> effort_histsoc_1961_2010.csv
# 4. when ready and after talking with Matthias upload to DKRZ.

# other notes: 
# Emma/IMAS: the datafile here are different than the one on fishmip -> 
# they are aggregated as per Yannick info/papers (e.g. industrial, UP and APW) and they include gridded 

# LME-level data ----
yannick_dir_new <- "/rd/gem/private/users/yannickr"

effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
head(effort_new)

# aggregate artisanal
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

# aggregate industrial 
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
  filter(Year >= 1950, Year <= 2017) 

write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017.csv") 

## NOTE: the below is not necessary anymore (besides checking) as the final effort data will include spin-up
# all data on DKRZ are replaced with spin-up version but better to keep this version as well.  

trial<-effort_tot %>% 
  filter(Year >= 1961, Year <= 2010)

# explore lme42 and 38 for checking - OK plots match
lme42<-trial %>% 
  filter(LME == "38") %>% 
  group_by(Year, Sector) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(lme42, aes(x = Year, y = NomActive, group = Sector, color = Sector))+
  geom_line()

write.csv(trial, "/rd/gem/private/users/yannickr/effort_histsoc_1961_2010.csv") 

# load data from gem48 to isismip ----

# scp -r effort_histsoc_1961_2010.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/

# gridded -----

yannick_dir_new <- "/rd/gem/private/users/yannickr/otherdata"
effort<-read_csv(file.path(yannick_dir_new, "GriddedEffortby_FGroup_FishingCountry_Sector.csv"))
head(effort)
effort2<-effort[-1,]
head(effort2)

# provide gridded industrial ----

effort5<-effort2 %>% 
  filter(Sector == "I") %>% 
  group_by(Lat, Lon, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >=1961, Year <=2010)

sort(unique(effort5$Year))

IndustrialEffort<-effort5

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
