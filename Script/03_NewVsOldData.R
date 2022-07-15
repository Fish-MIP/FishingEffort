
rm(list=ls())

# # load libraries 
library("tidyverse")
library("gridExtra")

# Steps: 
# Plot global effort and check that old version (Yannick) and new version (aggregation by Cami/Rich) match
# do the same at LME level 
# Compare with original gridded files - are the global sum for 1 year the same? 
# Old Data on ISIMIP: DOI: https://data.isimip.org/datasets/98037e0f-04cd-41ea-a5e3-9e4b1e351418/
# talk to Matthias to replace old with new version. 

## Load data ----
yannick_dir_new <- "/rd/gem/private/users/yannickr"
yannick_dir_old <- "/rd/gem/private/DataYannick"

# NEW ----
effort_new<-read_csv(file.path(yannick_dir_new, "all_effort_aggregated.csv"))
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

pdf("Output/Summary_total_NewVsOldData.pdf", width = 10, height=7)
plot_effort_tot
dev.off()

## WARNINIG - effort from original and old files match but not from new file! 
## CORRECTED now OK 

## compare with original gridded files ----
# back to original data (e.g. mapped_1950_I_100.csv) for more exploration
# check that the sum of effort for 1 year, 1 sector, across all countries (SAUP) 
# from the original csv files matches the value in the new effort dataset 

effort_file_list <- list.files(file.path(yannick_dir_new, "effort_mapped_bycountry"), pattern = ".csv", full.names = TRUE)
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
effort_tot_new

# OR
filter(effort_mapped_all, Year == 1990) %>% group_by(Version) %>% summarise(sum(effort))

### plot by LME ----
sort(unique(effort_new$LME))
sort(unique(effort_new$fao_area))
sort(unique(effort_new$eez_country_name))

effort_mapped_new<-effort_new %>% 
  group_by(Year, LME, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Version = "New")

# cannot do comparison at FAO or EEZ because these are missing from the old dataset
effort_mapped_old<-effort_old %>% 
  group_by(Year, LME, Sector) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup()%>% 
  mutate(Version = "Old")

effort_mapped_all<-effort_mapped_old %>% 
  full_join(effort_mapped_new) 

# plot data 
effort_list<-split(effort_mapped_all, effort_mapped_all$LME)

# for multiple plotting.... 
plot_effort<-list()

for(i in 1:length(effort_list)){
  
  plot_effort[[i]]<-ggplot(effort_list[[i]], aes(x = Year, y = effort, color = Version, linetype = Sector))+
    geom_line()+
    labs(title = paste("LME",unique(effort_list[[i]]$LME),sep=" "), x="Year",y="Nominal effort (NomActive)")+ 
    theme_bw()+
    theme(axis.text=element_text(size=9),
          axis.title=element_text(size=9),
          title=element_text(size=8)) #+
 
}

# # 4 plot per page - all effort
# ggsave("Output/Summary_LME_NewVsOldData_corrected.pdf", marrangeGrob(grobs = plot_effort, nrow=2, ncol=2), device = "pdf")
# # one page per LME
# pdf("Output/Summary_LME_NewVsOldData2_corrected_split.pdf", width = 10, height=5)
# plot_effort
# dev.off()

# WARNING 1: same LMEs show higher values in new effort
# WARNING 2: some LMEs show only one trend - is this becasue of perfect matching? YES
# WARNING 1 - pick an LME showing different trends and further check 
# CORRECTED all OK now 

keep <- 51 # worst are 47/51/2 

effort_mapped_new_FG<-effort_new %>% 
  filter(Sector == "I", LME %in% keep) %>%
  group_by(Year, LME, FGroup) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Version = "New")

effort_mapped_old_FG<-effort_old %>% 
  filter(Sector == "I",LME %in% keep) %>%
  group_by(Year, LME, FGroup) %>% 
  summarise(effort = sum(NomActive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Version = "Old")

effort_mapped_all_FG<- effort_mapped_old_FG %>% 
  full_join(effort_mapped_new_FG)

(Plot_LME<-ggplot(effort_mapped_all_FG, aes(x = Year, y = effort, color = Version))+ 
  geom_line()+
  labs(title = paste("LME",unique(effort_mapped_all_FG$LME),sep=" "), x="Year",y="Nominal effort (NomActive)")+ 
  theme_bw()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        title=element_text(size=8))+
  facet_wrap(~FGroup))

# pdf("Output/Summary_LME51_NewVsOldData.pdf", width = 10, height=7)
# Plot_LME
# dev.off()

# explore NA in Fgroup - none
filter(effort_old, is.na(FGroup))
filter(effort_new, is.na(FGroup))

effort_mapped_old_FG %>% summarise(trial = sum(effort)) # different values when I aggregate  
effort_mapped_new_FG %>% summarise(trial = sum(effort))

effort_mapped_old_FG %>% group_by(Year) %>% summarise(trial = sum(effort)) # different values by year  
effort_mapped_new_FG %>% group_by(Year) %>% summarise(trial = sum(effort))

### column names (see emails from Yannick and notes on desktop re links to draft paper)

# eez_country_name = The EEZ/high seas name in which the Effort/Catch
# is occurring (Rich's note = which EEZ was used for the new aggregation?)
# fao_area = A number code to the FAO area, 
# as per FAO notation (Rich's note = which FAO was used for the new aggregation?)
# LME = A number code to the LME, as per NOAA notation (Rich's note = which FAO was used for the new aggregation?)
# SAUP = ?? "Fcountry": Fishing country, in ISO3 notation. Ex supranational entities (USSR, Yugoslavia) 
# are disaggregated to their constituent countries. Serbian Fishing Effort included with Montenegro.
# Gear = not here before 
# FGroup = Functional Group of the target species
# Sector = Fishing sector (artisanal/industrial), defined by the law of the country 
# (varies by country) - needs to be updated to I, UP and APW
# NomActive = ?? "NomEffTotal": Total nominal fishing effort (artisanal and industrial, including reported and IUU), 
# by country and year. Unmapped, non gear specific. where Nominal (i.e. not including the technological creep). 
# where Active (i.e., active vessels - from draft paper: reconstruction considers "the ratio of vessels in activity to the total capacity".
# unit is Kw
# EffActive = effective effort - i.e. including technological creep. 
# From draft paper:Effective effort has been calculated using a 3.5% creep average per year. 
# NV = "NumberVessels": Number of fishing vessels by country and year.
# P = ??"EnginePowerkW": Total engine power equivalent of the fishing fleet, by country and year. 
# The non-motorised fleet was given an engine power equivalent to half the engine power of small 
# (powered) vessels of that country (approx 2kW/vessel).
# GT = ?? gross tonnage
# Year = Year (end of the year) when the Effort/Catch is occurring
