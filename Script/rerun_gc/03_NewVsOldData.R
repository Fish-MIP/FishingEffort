
rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")

# Steps: 
# Plot global effort and check that old version (Yannick) and new version (aggregation by Gage) match
# do the same at LME level 
# Compare with original gridded files - are the global sum for 1 year the same? 
# Old Data on ISIMIP: DOI: https://data.isimip.org/datasets/98037e0f-04cd-41ea-a5e3-9e4b1e351418/

## Load data ----
yannick_dir_new <- "/home/ubuntu/gem/private/users/gclawson"
yannick_dir_mid <- "/home/ubuntu/gem/private/users/yannickr"
yannick_dir_old <- "/home/ubuntu/gem/private/DataYannick"

# NEW ----
# effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
# CN using EEZ instrad of administrative country to aggregate data at EEZ level
effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated_EEZ.csv"))
head(effort_new)

# OLD ----
effort_old<-read_csv(file.path(yannick_dir_old, "TotalAggregatedFGroupLME.csv"))
effort_old<-effort_old[2:nrow(effort_old), 2:ncol(effort_old)]
head(effort_old)

# plot global effort ---- 
effort_mapped_new<-effort_new %>% 
  group_by(Year, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Version = "New")

effort_mapped_old<-effort_old %>% 
  group_by(Year, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()%>% 
  mutate(Version = "Old")

effort_mapped_all<-effort_mapped_old %>% 
  full_join(effort_mapped_new) 

(plot_effort_tot<-ggplot(effort_mapped_all, aes(x = Year, y = effort, color = Version, linetype = Sector))+
  geom_line()+
  labs(title = "Totla effort", x="Year",y="Nominal effort (NomActive)")+ 
  theme_bw()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        title=element_text(size=8))+
    facet_wrap(~Version))

pdf("Output/rerun/Summary_total_NewVsOldData_EEZ.pdf", width = 10, height=7)
plot_effort_tot
dev.off() # looks good

## compare with original gridded files ----
# back to original data (e.g. mapped_1950_I_100.csv) for more exploration
# check that the sum of effort for 1 year, 1 sector, across all countries (SAUP) 
# from the original csv files matches the value in the new effort dataset 

effort_file_list <- list.files(file.path(yannick_dir_mid, "effort_mapped_bycountry"), pattern = ".csv", full.names = TRUE)
industrial_effort_list <- effort_file_list[grep(pattern="1990", effort_file_list)]

sum_value<-vector()
for(i in 1:length(industrial_effort_list)){
  this_csv <- data.table::fread(industrial_effort_list[[i]])
  sum_value[[i]]<-sum(this_csv$NomActive, na.rm = TRUE)
}
effort_original<-sum(sum_value)

this_csv <- data.table::fread(industrial_effort_list[[1]])
head(this_csv)

# effort from old effort file
effort_tot_old<-effort_old %>% 
  filter(Year == 1990) 
effort_tot_old<-sum(effort_tot_old$NomActive, na.rm = TRUE)

# effort from new effort file 
effort_tot_new<-effort_new %>% 
  filter(Year == 1990) 
effort_tot_new<-sum(effort_tot_new$NomActive, na.rm = TRUE)

effort_original
effort_tot_old
effort_tot_new # all match 

# OR
filter(effort_mapped_all, Year == 1990) %>% group_by(Version) %>% summarise(sum(effort))

### plot by LME ----
sort(unique(effort_new$lme_name), na.last = TRUE)
sort(unique(effort_new$fao_region_name), na.last = TRUE)
sort(unique(effort_new$eez_name), na.last = TRUE)


effort_mapped_new<-effort_new %>% 
  group_by(Year, lme_name, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Version = "New")

lme_ids <- read.csv(file.path(yannick_dir_mid, "LME_FAO_coding_CN.csv")) %>%
  filter(LME_FAO_code == "LME") %>%
  distinct(LME = LME_FAO_number, lme_name = LME_FAO_name) # hmm looks like the LMEs are different? 

# cannot do comparison at FAO or EEZ because these are missing from the old dataset
effort_mapped_old<-effort_old %>% 
  left_join(lme_ids) %>% 
  group_by(Year, lme_name, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()%>% 
  mutate(Version = "Old")

effort_mapped_all<-effort_mapped_old %>% 
  full_join(effort_mapped_new) 

## ok so the LME data I am using for the new run is different. The regions are slightly different. 
# For the regions that do match, they look pretty much OK. They all do appear a little bit less in the new run than before. Did the old LME shapefile extend into high seas areas? The new one does not.  
# I'm not sure why the LME regions from the FAO report are different from the ones used previously though..? 

# plot data 
effort_list<-split(effort_mapped_all, effort_mapped_all$lme_name)

# for multiple plotting.... 
plot_effort<-list()

for(i in 1:length(effort_list)){
  
  plot_effort[[i]]<-ggplot(effort_list[[i]], aes(x = Year, y = effort, color = Version, linetype = Sector))+
    geom_line()+
    labs(title = paste("LME",unique(effort_list[[i]]$lme_name),sep=" "), x="Year",y="Nominal effort (NomActive)")+ 
    facet_wrap(~Version)+
    theme_bw()
    theme(axis.text=element_text(size=9),
          axis.title=element_text(size=9),
          title=element_text(size=8))
 
}

# # check manually as printing function is strange 
# plot_effort[[5]]

# 4 plot per page - all effort
# # one page per LME
pdf("Output/rerun/Summary_LME_NewVsOldData_corrected_EEZ.pdf", width = 10, height=5)
plot_effort
dev.off()
