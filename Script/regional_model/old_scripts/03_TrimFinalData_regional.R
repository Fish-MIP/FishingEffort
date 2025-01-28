

rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library("raster")
library(data.table)

# see 05 for comments - this is the regional version 

#### load file ----
yannick_dir_new <- "/rd/gem/private/users/yannickr"

# effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated_regional_models_trial2.csv")) # trial2 is when you use Denisse's mask but rerun step 02 from your account
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

# compare with LME scale effort - Mediterranean sea (LME 26) and north sea (LME 22) are comparable in terms of shape file. 
# WARNING - when added all together some shape files overlap - check with Denisse 
# (e.g. SE_Australia and Bass strait) and the overlapping cells are allocated to one model only (i.e. bass strait)
# pdf("Output/regional_models_newMask2.pdf", width=10, height=10) # see option 2 meaning as per above trial2
# plot
# dev.off()

# mediterranean - show issues - thsy will decide 
# outliers - if > 2sd then delete as outlier and take the mean between the two adiacent points. 

# WARNING - LME 22 underestimated effort in this file (possibly due to land and low resolution of the mask?)
# though this is much better than before the pdated mask from Denisse 
# trends kind of OK
# WARNING - LME 26 similar to LME 22 

# compare LME and regional polygons files - to fimnd out why trends differ 
mask_df <- fread("/rd/gem/private/fishmip_inputs/ISIMIP3a/fishmip_regions/Masks_netcdf_csv/fishMIP_regional_05deg_ISIMIP3a.csv")
unique(mask_df$region)
med<-mask_df %>% 
  filter(region == "MediterraneanSea")

colnames(med)<-c("x", "y", "z")
med$z<-1

med_raster<-rasterFromXYZ(med)

pdf("Output/med_poly.pdf") 
plot(med_raster)
dev.off()

LMEs <- fread(file.path(yannick_dir_new, "LMESeq.csv"))
med_lme<-LMEs %>% 
  filter(LME == 26) %>% 
  dplyr::select(-Seq, -Area) %>% 
  as.data.frame()

colnames(med_lme)<-c("x", "y", "z")
med_lme$z<-1

med_raster_lme<-rasterFromXYZ(med_lme)

pdf("Output/new/med_lme.pdf") 
plot(med_raster_lme)
dev.off() # discrepancy due to different shape files, with the LME one overlapping with coast to capture all effort. 

# Compare to global data from LME file - same trend and magnitude. 
# Land is similar to sea possibly becasue most effort is in coastal and this land relates to coastal?? 
# this file 
trial<-effort_spinup %>% 
  group_by(Year) %>% 
  summarise(NomActive = sum(NomActive)) %>% 
  ungroup()

plot<-ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()

# pdf("Output/global_effort_fromRegionaModelMerging_newMask2.pdf") 
# WARNING - still different from global effort below and previous runs with old mask from Ryan - 
# higher effort from regional model merging than from LME merging - grid cell might be double counted if 1 grid cell belongs to 2 regional moldel polygons...  
pdf("Output/global_effort_fromRegionaModelMerging.pdf") 
plot
dev.off()

# file created in 02 global version
effort_lme<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))

trial<-effort_lme %>% 
  group_by(Year) %>% 
  summarise(NomActive = sum(NomActive)) %>% 
  ungroup()

plot<-ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()

pdf("Output/global_effort_fromLMEmerging.pdf")
plot
dev.off()

##### print data ----
write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_regional_model.csv")
