
rm(list=ls())

# # load libraries 
# library("ggplot2")
library("tidyverse")
# library("patchwork")
# library("gridExtra")
# library("dplyr")
# library("networkD3")
# library("tidyr")

# why were these saved in 2 different directories? 
yannick_dir_new <- "/rd/gem/private/users/yannickr"
yannick_dir_old <- "/rd/gem/private/DataYannick"

# NEW

effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
head(effort_new)

# OLD 

effort_old<-read_csv(file.path(yannick_dir_old, "TotalAggregatedFGroupLME.csv"))
head(effort_old)
effort_old<-effort_old[2:nrow(effort_old), 2:ncol(effort_old)]
head(effort_old)

# explore by LME - some LMEs 
# keep <- seq(30,50)

sort(unique(effort_new$LME))
sort(unique(effort_new$fao_area))
sort(unique(effort_new$eez_country_name))

keep <- 20 # WARNING: SEA OK but some differences eg LME 20 - same trend but lower value in old effort - is this due to different LME, EEZ, FAO aggregation??!

head(effort_new)
effort_mapped_new<-effort_new %>% 
  filter(LME %in% keep) %>%
  group_by(Year, LME, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(effort_mapped_new, aes(x = Year, y = effort, group = Sector, color = Sector))+ # 3e+08 
  geom_line() +
  facet_wrap(~LME, scale = "free")

head(effort_old)
# cannot do comparison at FAO or EEZ becasue these are missing from old dataset - need to load another dataset 
effort_mapped_old<-effort_old %>% 
  filter(LME %in% keep) %>%
  group_by(Year, LME, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(effort_mapped_old, aes(x = Year, y = effort, group = Sector, color = Sector))+ # 2e+08
  geom_line()+
  facet_wrap(~LME, scale = "free")

# check the LME 20 
# WARNING - cannot see any difference - could these be due to NAs? 
head(effort_new)
unique(effort_new$Sector)
effort_mapped_new_FG<-effort_new %>% 
  filter(Sector == "I", LME %in% keep) %>%
  group_by(Year, LME, FGroup) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(filter(effort_mapped_new_FG, FGroup == "benthopelagic>=90cm"), aes(x = Year, y = effort, group = FGroup, color = FGroup))+ # 1.5e+08 pelagic 30-90, pelagic <30
  geom_line() +
  facet_wrap(~FGroup)

head(effort_old)
# cannot do comparison at FAO or EEZ becasue these are missing from old dataset - need to load another dataset 
effort_mapped_old_FG<-effort_old %>% 
  filter(Sector == "I",LME %in% keep) %>%
  group_by(Year, LME, FGroup) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()

ggplot(filter(effort_mapped_old_FG,FGroup =="benthopelagic>=90cm"), aes(x = Year, y = effort, group = FGroup, color = FGroup))+ # 1.5e+08 pelagic 30-90, pelagic <30
  geom_line()+
  facet_wrap(~FGroup)

# explore NA in Fgroup - none
filter(effort_old, is.na(FGroup))
filter(effort_new, is.na(FGroup))

# WARNING - can see differences again when I aggregate 
effort_mapped_old_FG %>% summarise(trial = sum(effort)) # lower values in old effort 
effort_mapped_new_FG %>% summarise(trial = sum(effort))

effort_mapped_old_FG %>% group_by(Year) %>% summarise(trial = sum(effort)) # lower value in old effort 
effort_mapped_new_FG %>% group_by(Year) %>% summarise(trial = sum(effort))

View(effort_mapped_old_FG %>% group_by(FGroup) %>% summarise(trial = sum(effort))) # differences at FGoup level. Less of bathypelagic30-90cm, benthopelagic30-90cm, and many other...   
View(effort_mapped_new_FG %>% group_by(FGroup) %>% summarise(trial = sum(effort)))

View(effort_mapped_old_FG %>% filter(FGroup == "benthopelagic>=90cm") %>% group_by(Year) %>% summarise(trial = sum(effort))) 
View(effort_mapped_new_FG %>% filter(FGroup == "benthopelagic>=90cm") %>% group_by(Year) %>% summarise(trial = sum(effort)))

# back to original data for more exploration... it is possibly the aggregation to SAUP, EEZ, LME and FAO but hard to dsay from these outpts
# need to explore the original file for mapped_1950_I_100.csv but all countries... Also it looks like it's happening in other sectors ... 
effort_old %>% filter(LME == 20, FGroup == "benthopelagic>=90cm", Year == 1950) 
effort_new %>% filter(LME == 20, FGroup == "benthopelagic>=90cm", Year == 1950) 

# ARRIVATA QUI












# load effort unmapped
# is teh pick for LME 42 present in the data before the mapping? kind of - but different trend with clear smoothing after the pick. ALos lots of smoothing pre mapping with all countries. 
effort_unmapped<-read.csv("/Users/camillan/Dropbox/DBPM_fishing_extension/Yannick_data/Final_DataStudyFAO_V5.csv")
head(effort_unmapped)

# explore by country - some countries
keep<-unique(effort_unmapped$Country)[1:15]

effort_unmappedA<-effort_unmapped %>% 
  filter(Country %in% keep) %>% 
  group_by(Year, Country) %>% 
  summarise(effort = sum(NV, na.rm = TRUE)) %>% # look at number of vessels (NV) first - can also be NomEffort
  ungroup()

ggplot(effort_unmappedA, aes(x = Year, y = effort))+
  geom_line()+
  facet_wrap(~Country, scale = "free")

# zoom in Au 
effort_unmapped2<-effort_unmapped %>% 
  filter(Country == "Australia") %>% 
  group_by(Year) %>% 
  summarise(effort = sum(NV, na.rm = TRUE)) %>% 
  ungroup()

ggplot(effort_unmapped2, aes(x = Year, y = effort))+
  geom_line()

# explore by coutry and by gear 
effort_unmapped2B<-effort_unmapped %>% 
  filter(Country == "Australia") %>% 
  group_by(Year, Gear) %>% 
  summarise(effort = sum(NV, na.rm = TRUE)) %>% 
  ungroup()

# Smoothing goes away as NV are allocated to the different gears based on a value that changes by year and given data on e.g. power 
ggplot(effort_unmapped2B, aes(x = Year, y = effort))+
  geom_line()+
  facet_wrap(~Gear, scale = "free")

# most influential gears in determining the smooth pattern: 
# "Gillnets","Lines_Longlines","Seine_Danish_and_Other","Lines_Handlines_and_poles", "Others_Support"                  

effort_unmapped3<-effort_unmapped %>% 
  filter(Country == "Australia", !Gear %in% c("Gillnets","Lines_Longlines","Seine_Danish_and_Other","Lines_Handlines_and_poles", "Others_Support")) %>% 
  group_by(Year) %>% 
  dplyr::summarise(effort = sum(NV, na.rm=TRUE)) %>% 
  ungroup()

ggplot(effort_unmapped3, aes(x = Year, y = effort))+
  geom_line()



