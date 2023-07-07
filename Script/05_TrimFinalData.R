

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

# effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
# consider new aggregation using EEZ instead of administrative country 
effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated_EEZ.csv"))
head(effort_new)

# aggregate artisanal
effort_new_Artisanal<-effort_new %>% 
  filter(Sector !="I") %>% 
  group_by(fao_area, LME, eez_country_name, SAUP, Gear, FGroup, Year) %>% # add back fao_area - this aggregation is still needed as you are aggregating 2 artisanal efforts
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
  group_by(fao_area, LME, eez_country_name, SAUP, Gear, FGroup, Year) %>% # add back fao_area - this aggregation might not be needed anymore but do it anyway .... 
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


## CHECK that numbers are as original data - OK they are the same 
original<- effort_new %>% 
  group_by(Sector) %>% 
  summarise(e= sum(NomActive)) 

# 1 APW    329157324772.
# 2 I      492743814508.
# 3 UP      60126428597.

sum(original[1,2], original[3,2])
# [1] 389283753370

check<-effort_tot %>% 
  group_by(Sector) %>% 
  summarise(e= sum(NomActive)) 

# 1 Artisanal  389283753370.
# 2 Industrial 492743814508.

check<-effort_spinup %>% 
  group_by(Sector) %>% 
  summarise(e= sum(NomActive)) 

# 1 Artisanal  389283753370. 
# 2 Industrial 492743814508.

# write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017.csv") 
# consider new aggregation using EEZ instead of administrative country 
# write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_EEZ.csv") 
# consider new aggregation using EEZ instead of administrative country and adding FAO column for LME 0 calcaultions later on
# write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_EEZ_addFAO.csv") 

##### TESTS ----

# # CHECK that numbers are as original data after printing the data - i.e. decimal handling 
# check2<-read.csv("/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_EEZ_addFAO.csv")
# 
# check3<-check2 %>% 
#   group_by(Sector) %>% 
#   summarise(e= sum(NomActive)) 
# 
# # NOT THE SAME - doe to how wite.csv handles decimals I think 
# # 1 Artisanal  389283753417.
# # 2 Industrial 492743814471.
# 
# # when writing a new file instead of overwriting an old one? 
# # addFAO_2 (though deleted) - Same results as per the overwritten file 
# # 1 Artisanal  389283753417.
# # 2 Industrial 492743814471.
# 
# 
# # these summed values are also slightly across same 
# # files that have been printed at different times but using 
# # the same code and initial 
# # this I don't know why... 
# # e.g. previous version printed
# # 1 Artisanal  389283753417.
# # 2 Industrial 492743814481.
# # and 
# # 1 Artisanal  389283753417.
# # 2 Industrial 492743814468.

# # what happens with fwrite and read_csv? It does not happen... SAME VALUES as original data
library(data.table)
fwrite(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_EEZ_addFAO.csv")
# check2<-read_csv("/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_EEZ_addFAO.csv")
# 
# check3<-check2 %>%
#   group_by(Sector) %>%
#   summarise(e= sum(NomActive))
# 
# # 1 Artisanal  389283753370.
# # 2 Industrial 492743814508.
# 
# # is it the writing or the reading of the file? 
# # use write.csv and read_csv - if values are correct 
# # it is the reading function, otherwise it is the writing function or both
# 
# # it is the writing function mostly, and the reading function to a much smaller extent as 
# # these are not exactly the same of when using wite.csv adn read.csv. 
# # 1 Artisanal  389283753417.
# # 2 Industrial 492743814473.



#### NOTE RUN FOR NEW VERSIONS OF _addFAO ---------- 
## NOTE: the below is not necessary anymore (besides checking) as the final effort data will include spin-up
# all data on DKRZ are replaced with spin-up version but better to keep this version as well.  

trial<-effort_tot %>% 
  filter(Year >= 1961, Year <= 2010)

# explore lme42 and 38 for checking - OK plots match
lme42<-trial %>% 
  filter(LME == "42") %>% 
  group_by(Year, Sector) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(lme42, aes(x = Year, y = NomActive, group = Sector, color = Sector))+
  geom_line()

# consider new aggregation using EEZ instead of administrative country
write.csv(trial, "/rd/gem/private/users/yannickr/effort_histsoc_1961_2010_EEZ.csv") 

# load data from gem48 to isismip ----

# scp -r effort_histsoc_1961_2010.csv b381217@levante.dkrz.de:/home/b/b381217/temp_effort/

# NOTE no changes to the below as these are not aggregated at EEZ level and do not include spin-up.
# they are only provided for checks but not as model forcing - these are now in DKRZ 

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
