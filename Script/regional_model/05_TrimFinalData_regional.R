

rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")
library("reshape2")
library("raster")

# see 05 for comments - this is the regionla version 

yannick_dir_new <- "/rd/gem/private/users/yannickr"

effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated_regional_models.csv"))
head(effort_new)

# aggregate artisanal
effort_new_Artisanal<-effort_new %>% 
  filter(Sector !="I") %>% 
  group_by(model_area_name, SAUP, Gear, FGroup, Year) %>% 
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
  group_by(model_area_name, SAUP, Gear, FGroup, Year) %>% # delete fao_area
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

# check - plots 
head(effort_spinup)

trial<-effort_spinup %>% 
  group_by(model_area_name, Year) %>% 
  summarise(NomActive = sum(NomActive))

plot<-ggplot(trial, aes(y = NomActive, x = Year))+
  geom_point()+
  geom_line()+
  facet_wrap(~model_area_name, scales = "free")

# WARNING something wrong with the land... 

# compare with LME scale effort - Mediterranean sea (LME 26) and north sea (LME 22) are comparable in terms of shape file. 
# WARNING SE_Australia much bigger than what it should be for your model (it's the whole SESSF and possibly OK for Beth - LME 41, 42, 43)
# WARNING - when added all together some shape files overlap 
# (e.g. SE_Australia and Bass strait) and the overlapping cells are allocated to one model only (i.e. bass strait)
pdf("Output/regional_models.pdf")
plot
dev.off()

# WARNING - LME 22 underestimated effort in this file (possibly due to land and low resolution of the mask?) - of 1/2! - trends kind of OK
# WARNING - LME 26 similar to LME 22 

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

pdf("Output/global_effort_fromRegionaModelMerging.pdf")
plot
dev.off()

# file created in 02 
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

# print data 

write.csv(effort_spinup, "/rd/gem/private/users/yannickr/effort_histsoc_1950_2017_regional_model.csv") 
