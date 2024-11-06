

rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library(terra)
library(data.table)
library(raster)

# see 05 for comments - this is the regional version 

#### load file ----
# yannick_dir_new <- "/rd/gem/private/users/yannickr"
yannick_dir_new <- "/home/ubuntu/gem/private/users/yannickr" # this is the filepath Gage uses

effort_new <- fread(file.path(yannick_dir_new, "all_effort_aggregated_regional_models_new.csv")) # this is the new data gage made
head(effort_new)
# unique(effort_new$region)

#### aggregate artisanal power and unpower ----
# aggregate artisanal
effort_new_Artisanal <- effort_new %>% 
  filter(Sector !="I") %>% 
  group_by(region, SAUP, Gear, FGroup, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Artisanal")

head(effort_new_Artisanal)

# aggregate industrial 
effort_new_Industrial <- effort_new %>% 
  filter(Sector == "I") %>% 
  group_by(region, SAUP, Gear, FGroup, Year) %>% 
  summarise(NomActive = sum(NomActive, na.rm = TRUE),
            NV = sum(NV, na.rm = TRUE),
            P = sum(P, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sector = "Industrial")

head(effort_new_Industrial)

# put them back together
colnames(effort_new_Artisanal)
colnames(effort_new_Industrial)
effort_tot <- rbind(effort_new_Industrial, effort_new_Artisanal)
head(effort_tot)

nrow(effort_tot)
nrow(effort_new_Artisanal)+nrow(effort_new_Industrial)

### effort data to calculate spin-up in 05 ----
effort_spinup <- effort_tot %>% 
  filter(Year >= 1950, Year <= 2017) 

### checks ----
trial <- effort_spinup %>% 
  group_by(region, Year) %>% 
  summarise(NomActive = sum(NomActive))

plot<-ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()+
  facet_wrap(~region, scales = "free")

# compare with LME scale effort - Make sure that regions that overlap are included individually 
pdf("Output/new/regional_models_newMask2.pdf", width=10, height=10) # see option 2 meaning as per above trial2
plot
dev.off()

# compare LME and regional polygons files 
mask_df <- fread("/home/ubuntu/gem/private/fishmip_inputs/ISIMIP3b/fishmip_regions/Masks_netcdf_csv/fishMIP_regional_05deg_ISIMIP3b.csv")
unique(mask_df$region)
med <- mask_df %>% 
  filter(region == "Mediterranean Sea EwE") %>%
  mutate(region = 1) %>%
  rename(x = Lon, y = Lat)

med_raster <- rast(med, type = "xyz")

pdf("Output/new/med_poly.pdf") 
plot(med_raster)
dev.off() ## looks good

test_shp <- st_read(file.path("/home/ubuntu/gem/private/fishmip_inputs", "ISIMIP3b/fishmip_regions/FishMIP_regional_models"))
test_med_shp <- test_shp %>%
  filter(region == "Mediterranean Sea EwE") # looks good 


LMEs <- fread(file.path(yannick_dir_new, "LMESeq.csv"))
med_lme <- LMEs %>% 
  filter(LME == 26) %>% 
  dplyr::select(-Seq, -Area) %>% 
  as.data.frame()

med_raster_lme <- rast(med_lme, type = "xyz")

pdf("Output/new/med_lme.pdf") 
plot(med_raster_lme)
dev.off() # discrepancy due to different shape files, with the LME one overlapping with coast to capture all effort. 

# Compare to global data from LME file - same trend and magnitude. 
# Land is similar to sea possibly becasue most effort is in coastal and this land relates to coastal?? 
# this file 
trial <- effort_spinup %>% 
  group_by(Year) %>% 
  summarise(NomActive = sum(NomActive)) %>% 
  ungroup()

plot <- ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()

pdf("Output/new/global_effort_fromRegionaModelMerging.pdf") 
plot
dev.off()

# file created in 02 global version
effort_lme <- read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))

trial <- effort_lme %>% 
  group_by(Year) %>% 
  summarise(NomActive = sum(NomActive)) %>% 
  ungroup()

plot<-ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()

pdf("Output/new/global_effort_fromLMEmerging.pdf")
plot
dev.off()

##### print data ----
write.csv(effort_spinup, "/home/ubuntu/gem/private/users/yannickr/effort_histsoc_1950_2017_regional_model.csv")
