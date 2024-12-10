
rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library("raster")
library(terra)
library(data.table)
library(dtplyr)

# Steps: 
# Adjust new effort and print final version for FishMIP
# See 03 for comparison between old (Yannick) and new effort
# 1. aggregate by industrial and Artisanal (UP + APW) 
# 2. consider only 1961-2020 
# 2.b cut unused columns 
# 3. print final -> effort_histsoc_1961_2020.csv

# LME-level data ----
yannick_dir_old <- "/home/ubuntu/gem/private/users/yannickr"
dir_new <- "/home/ubuntu/gem/private/users/gclawson"


# use fread and data.table as in 02, much faster 

effort_new<-fread(file.path(dir_new, "all_effort_aggregated_EEZ.csv"))

class(effort_new) # [1] "data.table" "data.frame"
colnames(effort_new) # works

effort_new<-lazy_dt(effort_new) # to talk with dplyr
class(effort_new) # [1] "dtplyr_step_first" "dtplyr_step"

# aggregate artisanal
effort_new_Artisanal<-effort_new %>% 
  filter(Sector !="I") %>% 
  group_by(fao_area = fao_region_name, LME = lme_name, eez_country_name = eez_name, SAUP, Gear, FGroup, Year) %>% # add back fao_area - this aggregation is still needed as you are aggregating 2 artisanal efforts
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Artisanal")

# head(effort_new_Artisanal)

# aggregate industrial 
effort_new_Industrial<-effort_new %>% 
  filter(Sector == "I") %>% 
  group_by(fao_area = fao_region_name, LME = lme_name, eez_country_name = eez_name, SAUP, Gear, FGroup, Year) %>% # add back fao_area - this aggregation might not be needed anymore but do it anyway .... 
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Industrial")


# other option as the above is problematic/slow
effort_tot<-effort_new_Industrial %>%
  full_join(effort_new_Artisanal)

class(effort_tot)
nrow(as.data.frame(effort_tot)) # 26846169
nrow(as.data.frame(effort_new_Artisanal))+nrow(as.data.frame(effort_new_Industrial)) # 26846169

# effort to calculate spin-up
effort_spinup<-effort_tot %>% 
  filter(Year >= 1950, Year <= 2017) 

## CHECK that numbers are as original data - OK they are the same 
original<- effort_new %>% 
  group_by(Sector) %>% 
  summarise(e= sum(NomActive)) 

# Sector             e
# <chr>          <dbl>
#   1 APW    329157324772.
# 2 I      492743814507.
# 3 UP      60126428597.


check<-effort_tot %>% 
  group_by(Sector) %>% 
  summarise(e= sum(NomActive)) 

# Sector                 e
# <chr>              <dbl>
#   1 Artisanal  389283753370.
# 2 Industrial 492743814507.

## cool, these all match perfectly to what was done before. 

##### TESTS ----

# # CHECK that numbers are as original data after printing the data - i.e. decimal handling 
check2 <- read.csv(file.path(yannick_dir_old, "effort_histsoc_1950_2017_EEZ_addFAO.csv"))


(check3 <- check2 %>%
  group_by(Sector) %>%
  summarise(e= sum(NomActive)))

# # cool it matches
# # A tibble: 2 × 2
# Sector                 e
# <chr>              <dbl>
#   1 Artisanal  389283753370.
# 2 Industrial 492743814508.



effort_spinup <- as.data.table(effort_spinup)
fwrite(effort_spinup, file.path(dir_new, "effort_histsoc_1950_2017_EEZ_addFAO.csv"))

check2 <- fread(file.path(dir_new, "effort_histsoc_1950_2017_EEZ_addFAO.csv"))

(check3<-check2 %>%
  group_by(Sector) %>%
  summarise(e= sum(NomActive)))

# good it matches
# # 1 Artisanal  389283753370.
# # 2 Industrial 492743814508.


#### NOTE RUN FOR NEW VERSIONS OF _addFAO ---------- 
## NOTE: the below is not necessary anymore (besides checking) as the final effort data will include spin-up
# all data on DKRZ are replaced with spin-up version but better to keep this version as well.  

trial <- effort_tot %>% 
  filter(Year >= 1961, Year <= 2010)

# explore lme42 and 38 for checking - OK plots match
lme42 <- trial %>% 
  filter(LME == "Southeast Australian Shelf") %>% 
  group_by(Year, Sector) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.frame()

ggplot(lme42, aes(x = Year, y = NomActive, group = Sector, color = Sector))+
  geom_line()

# consider new aggregation using EEZ instead of administrative country
write.csv(trial, file.path(dir_new, "effort_histsoc_1961_2010_EEZ.csv"), row.names = FALSE)


# gridded -----

yannick_dir_new <- "/home/ubuntu/gem/private/users/yannickr/otherdata"
effort <- read_csv(file.path(yannick_dir_new, "GriddedEffortby_FGroup_FishingCountry_Sector.csv"))
colnames(effort)

effort2 <- effort %>% dplyr::select(-1)
colnames(effort2)
sort(unique(effort2$Year))

# provide gridded industrial ----

effort5 <- effort2 %>% 
  filter(Sector == "I") %>% 
  group_by(Lat, Lon, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >=1950, Year <=2017)

sort(unique(effort5$Year))

IndustrialEffort <- effort5

# check ----
# before you rearrange data 
trial_I<-effort2 %>% 
  filter(Year == 2010, Sector == "I") %>% 
  group_by(Lat, Lon) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

trial_I_2 <- acast(trial_I, Lat ~ Lon)
plot(raster(log10(trial_I_2))) # something wrong with lat/lon

# after you rearrange data 
trial_I_new <- effort5 %>% 
  filter(Year == 2010)

trial_I_2_new<-acast(trial_I_new, Lat ~ Lon)
plot(raster(log10(trial_I_2_new))) # OK same plot weird

write.csv(IndustrialEffort, file.path(dir_new, "gridded_industrial_effort_histsoc_1950_2017.csv"), row.names = FALSE)


# provide effort gridded artisanal ----

effort6 <- effort2 %>% 
  filter(Sector != "I") %>% 
  group_by(Lat, Lon, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >=1950, Year <=2017)

head(effort6)
sort(unique(effort6$Year))
ArtisanalEffort <- effort6

# check 
# before you rearrange data 
trial_I <- effort2 %>% 
  filter(Year == 2010, Sector != "I") %>% 
  group_by(Lat, Lon) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

trial_I_2<-acast(trial_I, Lat ~ Lon)
plot(raster(log10(trial_I_2))) # something wrong with lat/lon

# after you rearrange data 
trial_I_new <- effort6 %>% 
  filter(Year == 2010)

trial_I_2_new <- acast(trial_I_new, Lat ~ Lon)
plot(raster(log10(trial_I_2_new))) # OK 

write.csv(ArtisanalEffort, file.path(dir_new, "gridded_artisanal_effort_histsoc_1950_2017.csv"), row.names = FALSE)

# read in, rename, save  
catch <- read_csv(file.path(yannick_dir_old, "catch_histsoc_1869_2017.csv"))
head(catch)

catch2<-catch %>% 
  filter(Year >= 1950, Year<=2017)

write.csv(catch2, file.path(dir_new, "calibration_catch_histsoc_1950_2017.csv"), row.names = FALSE)

