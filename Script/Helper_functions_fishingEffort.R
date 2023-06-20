
# helper function for FishingEffort repository 

join_effort_data <- function(this_file_name){
  
  # trial 
  # this_file_name = "mapped_1952_I_678.csv"
  
  this_source_path <- file.path("/rd/gem/private/users/yannickr/effort_mapped_bycountry", this_file_name)
  
  # this_destination_path <- paste0("/rd/gem/private/users/yannickr/effort_mapped_by_country_aggregated/", "aggregated_", this_file_name) 
  # CN this version consideres EEZ aggregation using EEZ FAOname instead of the administrative country name. 
  this_destination_path <- paste0("/rd/gem/private/users/yannickr/effort_mapped_by_country_aggregated_EEZ/", "aggregated_EEZ_", this_file_name)
  
  # if(file.exists(this_destination_path)){ # CN - why this? the file does not exist
  
  Year <- as.numeric(str_extract(this_file_name, pattern =  "([[:digit:]])+"))
  
  these_data <- fread(this_source_path)
  these_data <- lazy_dt(these_data)
  
  #data.table approach
  #tells data.table how to join
  # 
  # setkey(these_data, Lon, Lat)
  # setkey(LMEs, Lon, Lat)
  # setkey(EEZ_adj, Lon, Lat)
  # setkey(FAO_regions, Lon, Lat)
  # 
  # #make the joins of LMEs, FAO areas, and EEZs
  # merge_1 <- LMEs[these_data, on = c("Lon", "Lat")]
  # merge_2 <- FAO_regions[merge_1, on = c("Lon", "Lat")]
  # merge_3 <- EEZ_adj[merge_2, on = c("Lon", "Lat")] #merge 3 has retained the same number of rows
  # #group by all variables and sum the different effort forms, adding year at the end
  # 
  # this_aggregated_data <- 
  #   cbind(
  #     nom_active_gr <- merge_3[, .(NomActive= sum(NomActive, na.rm = TRUE)),
  #                              by = list(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector )][,"NomActive"],
  #     eff_active_gr <-  merge_3[, .(EffActive= sum(EffActive, na.rm = TRUE)), 
  #                               by = list(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector )][,"EffActive"],
  #     nv_gr <- merge_3[, .(NV= sum(NV, na.rm = TRUE)),
  #                      by = list(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector )][,"NV"],
  #     p_gr <- merge_3[, .(P= sum(P, na.rm = TRUE)), 
  #                     by = list(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector )][,"P"],
  #     gt_group <- merge_3[, .(GT= sum(GT, na.rm = TRUE)), 
  #                         by = list(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector )][,"GT"]
  #   )[, `:=`(Year = Year)]
  
  # dtplyr approach
  these_aggregated_data <-
    these_data %>%
    left_join(LMEs, by=c("Lat", "Lon")) %>%
    left_join(EEZ_adj, by=c("Lat", "Lon")) %>%
    left_join(FAO_regions, by=c("Lat", "Lon")) %>%
    # left_join(FAO_LME_new, by = c("Lat", "Lon")) %>% # CN added no need
    group_by(eez_country_name, fao_area, LME, SAUP, Gear, FGroup, Sector) %>%
    summarise(NomActive = sum(NomActive, na.rm = TRUE),
              EffActive = sum(EffActive, na.rm = TRUE),
              NV= sum(NV, na.rm = TRUE),
              P= sum(P, na.rm = TRUE),
              GT= sum(GT, na.rm = TRUE)) %>%
    mutate(Year = Year) %>% as.data.table()
  
  fwrite(x = these_aggregated_data, file = file.path(this_destination_path))
  # }
  
}

# extrapolate effort 1841 to 1950 

effort_extrapolate<-function(toKeep, kappa, war_param, war_decision, reference){
  
  # # # trial
  # # toKeep=66
  # toKeep = "FAO_for_LME0_88" # fix problem with effort not been available before 2004
  # toKeep = "LME_24" # fix problem with war years being removed when they should have not for bug in code ...
  # kappa=6
  # war_param = "include"
  # war_decision = "no-add"
  # reference = "1945-1955"
  
  effort_extr<-effort %>% 
    # filter(LME %in% toKeep) %>% 
    filter(LME_FAO %in% toKeep) %>% # Changed after new classification of LME 0 into FAO regions
    group_by(Year) %>% 
    summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
    ungroup()
  
  maxYearEffort<-max(effort_extr$Year) 
  
  catch_all <- catch %>% 
    # filter(LME %in% toKeep) %>% 
    filter(LME_FAO %in% toKeep) %>% 
    group_by(Year) %>% 
    summarise(catch_tot = sum(catch_tot, na.rm = TRUE))
  
  ## IF ZERO ----
  # if(dim(effort_extr)[1] !=0){ 
  
  ## IF HISTORICAL catch exist ----
  if(dim(filter(catch_all, Year <1950))[1] >=10) {  
    df = catch_all
    var = "catch_tot" 
    chechDataUsed = "catch"
  }else{
    df = effort_extr
    var = "NomActive"
    chechDataUsed = "effort"
  }
  
  # find the pick in data and filter out everything after the pick  
  maxVar<-max(df[[var]])  
  maxVar<-filter(df, eval(as.name(var)) == maxVar)
  toAddBack<-filter(df, Year > maxVar$Year)
  df<-filter(df, Year <= maxVar$Year)
  
  # filter out war years: 1914-18 and 39-45
  year<-sort(unique(df$Year))
  if (year[[1]]<=1918){
    war <- df %>% 
      filter(Year %in% c(1914:1918, 1939:1945)) 
    warToRemove<-war$Year
  }else if (year[[1]]<=1945){
    war<- df %>% 
      filter(Year %in% c(1939:1945)) 
    warToRemove<-war$Year
  }
  
  # # WARNING - this argument to if() was wrong before adding the () 
  # # because if year[[1]] = 1910 but war param = "include", 
  # # the argument returns TRUE hence war data is removed! 
  # # need to check whether this had any influence on the results. 
  # # by checking at plots (any data <1918 ?): LME 0, LME 12, LME 19, 2, 20, 21,22,23,24, etc... LOTS 
  # 
  # # this means I have excluded catches during war years only if catch data extended to before 1918!
  # and I did not re-added them back before calculating the rate of change in effort (code below) as  
  # war_param == "include" & war_decision == "no-add"
  # war_param = "include"
  # year = 1910
  # year = 1950
  # 
  # year<=1918 | year<=1945 & war_param == "exclude"
  # (year<=1918 | year<=1945) & war_param == "exclude"
  
  # re-run code corrected and check LMEs that have been influenced... 
  # yes some have: see eg. LME 9 as one of worst case 
  # FILE with: years, LME, % difference between approaches. (possibly not correct)
  # also other approach of excluding war years in all LMEs 
  
  
  
  
  
  
  #### why not just war_param == "exclude" ??? 
  if((year[[1]]<=1918 | year[[1]]<=1945) & war_param == "exclude"){
    df<-filter(df, !Year %in% warToRemove)
  }
  
  # fit the gam 
  gam_model<-gam(eval(as.name(var)) ~ s(Year, k=kappa), data=df, family=gaussian(link="log"))
  df$gam<-round(predict(gam_model,newdata = df,type='response'))
  
  # # check the gam
  # ylim = c(min(df$gam), max(df$gam))
  # plot(df$Year,df[[var]],  ylim=ylim)
  # par(new=T)
  # plot(df$Year,df$gam,col="red",type='l', ylim=ylim)
  
  # predict backwards
  newd <- data.frame(Year = seq(1861, maxVar$Year))
  newd$pred <- predict.gam(gam_model,newd,type='response') 
  newd$pred <- ifelse(newd$pred<=0, 0, newd$pred)
  
  # all data
  df<-df %>%
    full_join(newd, by = "Year") %>% 
    dplyr::select(-gam)
  
  if(var == "catch_tot"){
    
    # 1. calculate rate of change
    
    if(reference == "1950"){
      ref<-df %>% 
        filter(Year == 1950)
      ref<-as.numeric(ref$pred)
    }else{
      # 1B. calculate rate of change based on mean 1945-1955 years 
      ref<-df %>% 
        filter(Year >= 1945, Year <= 1955)
      ref<-as.numeric(mean(ref$pred, na.rm = TRUE))
    }
    
    # re-add war years for calculation of effort (if these were available). Add them to pred, because pred <1950 is what you are basing your calculation of effort on.
    if(war_param == "exclude" & war_decision == "re-add"){
      
      if (year[[1]]<=1914 | year[[1]]<=1939){
        df<-df %>% 
          full_join(war %>% rename(var_war = as.name(var)), by = "Year") %>% 
          mutate(pred = case_when(!is.na(var_war) ~ var_war,
                                  is.na(var_war) ~ pred)) %>% 
          select(-var_war)
      }
    }
    
    rate_of_change<-df %>% 
      arrange(Year) %>% 
      mutate(pred = as.numeric(pred)) %>% 
      mutate(rate = 100 * (pred - ref)/ref) %>% 
      mutate(Trial_back = ref + (rate/100)*ref) # only to check  
    
    # check 
    # filter(rate_of_change, Year >= 1950, Year <= 1960)
    
    # 2. extend effort back using rate of change in catch 
    
    if(reference == "1950"){
      ref<-effort_extr %>% 
        filter(Year == 1950)
      ref<-as.numeric(ref$NomActive)
    }else{
      # 2B. extend effort back using rate of change in catch using decadal mean 
      ref<-effort_extr %>% 
        filter(Year >= 1945, Year <=1955)
      ref<-as.numeric(mean(ref$NomActive, na.rm = TRUE))
    }
    
    effort_reconstruct<-effort_extr %>% 
      full_join(rate_of_change, by = "Year") %>% 
      mutate(NomActive_catch_based = case_when(Year >= 1950 ~ NomActive,
                                               Year < 1950 ~ ref + (rate/100)*ref))
  
    # # check
    # ggplot(data = effort_reconstruct, aes(x = Year, y = NomActive_catch_based)) +
    #   geom_point()
    # ggplot(data = effort_reconstruct, aes(x = Year, y = pred)) +
    #   geom_point()
    
    # build effort_all
    effort_all<-effort_reconstruct %>% 
      dplyr::select(Year, NomActive_catch_based) %>% 
      rename(NomActive = NomActive_catch_based) 
    
    # consider only recent years that are in effort and not in catch 
    effort_all<-effort_all %>% 
      filter(Year <= maxYearEffort)
    
  }else{ # END var = cacth_tot; beginning of var = NomActive 
    
    # build effort based on gam on effort. 
    # df<-df %>% 
    #   mutate(NomActive = ifelse(Year < 1950, pred, NomActive)) 
    
    # WARNING - this is to fix "FAO_for_LME0_88" where effort data is missing before 2004 
    # hence you have a gap 1950-2003 if you use the code above 
    df<-df %>% 
      mutate(NomActive = ifelse(is.na(NomActive), pred, NomActive)) 
    
  } # end difference between var 
  
  # FINAL STEPS ----
  
  # 1. add missing data 
  
  # add back war time in raw catches - only if you had excluded them above
  # WARNING - CHECK THIS but currently not used as war_param = "include"
  if(war_param == "exclude"){
    
    if (year[[1]]<=1918 | year[[1]]<=1945){
      df<-df %>% 
        full_join(war %>% rename(var_war = as.name(var)), by = "Year")
      
      if(var == "catch_tot"){
        df<-df %>% 
          mutate(catch_tot = case_when(!is.na(var_war) ~ var_war,
                                       is.na(var_war) ~ eval(as.name(var)))) %>% 
          select(-var_war)
      }
      # }else{ 
      #   # is this needed? was years are only excluded on catches, not effort. 
      #   df<-df %>% 
      #     mutate(NomActive = case_when(!is.na(var_war) ~ var_war,
      #                                  is.na(var_war) ~ eval(as.name(var)))) %>% 
      #     select(-var_war)
      #   
      # }
    }
  }
  
  # add back data after pick in both catches and effort
  if (dim(toAddBack)[1]!=0){
    df<-df %>% 
      full_join(toAddBack) 
  }
  
  # define df as either catch_all or effort_all
  if(var == "catch_tot"){
    catch_all2<-df # version 2 otherwise cannot # highlight data that was not used in GAM # as catch_all is overwritten
  }else{
    effort_all<-df
    catch_all2<-catch_all 
  }
  
  # 2. merge and plot 
  final_df<-effort_all %>% 
    full_join(catch_all2, by = "Year")
  
  # add 1841-1860 at 1861 fixed values
  trial<-final_df %>% 
    filter(Year == 1861) 
  
  add_df<-data.frame(Year = seq(1841, 1860), NomActive = rep(trial$NomActive, 20))
  
  final_df<-final_df %>% 
    full_join(add_df)
  
  # # relative to 1950
  # # WARNING CHECK THIS - not sure we need it 
  # ref<-final_df %>% 
  #   filter(Year == 1950)
  # 
  # final_relative<-final_df %>% 
  #   mutate(EffRelative = (NomActive-ref$NomActive)/ref$NomActive, 
  #          PredRelative = (pred-as.numeric(ref$pred))/as.numeric(ref$pred),
  #          CatchRelative = (catch_tot-ref$catch_tot)/ref$catch_tot) 
  # 
  # final_relative<-final_relative %>% 
  #   dplyr::select(Year, EffRelative:CatchRelative) %>% 
  #   gather("Type", "Value", -Year)
  
  # WARNING - check Warning with gather, use pivot_longer instead
  final_df<-final_df %>% 
    pivot_longer(cols = -Year, names_to = "Type", values_to = "Value") 
  
  # final_df<-final_df %>% 
  #   gather("Type", "Value", -Year) 
  
  # grouping for plot below - these are the titles of the two stacked plots 
  if(var == "catch_tot"){
    final_df<-final_df %>% 
      mutate(group = ifelse(Type == "NomActive", "Effort (nominal, DkW)", "Catch (tonnes)") )
    
  }else(
    final_df<-final_df %>% 
      mutate(group = ifelse(Type == "catch_tot", "Catch (tonnes)", "Effort (nominal, DkW)") )
  )
  
  # highlight data that was not used in GAM 
  if(dim(filter(catch_all, Year <1950))[1] >=10 & war_param == "exclude" & war_decision != "re-add") { 
    final_df <-final_df %>% 
      mutate(Considered = ifelse(Year %in% c(1914:1918, 1939:1945), "No", "Yes"))
  }else{ 
    final_df <-final_df %>% 
      mutate(Considered = "Yes")
  }
  
  if(dim(toAddBack)[1]!=0){
    final_df <-final_df %>% 
      mutate(Considered = ifelse(Year > maxVar$Year, "No", Considered))
  }
  
  final_df_plot<-final_df %>% 
    rename(Data = Type) %>% 
    mutate(Data = ifelse(Data == "pred", "GAM predictions", "Reconstructed values"),
           Considered = as.factor(Considered))
  
  # make "yes" always a dot and never a square 
  scale_fill_chris <- function(...){
    ggplot2:::manual_scale(
      'shape', 
      values = setNames(c(16,15), c("Yes", "No")), 
      ...
    )
  }
  
  data_for_plot_for_FAOreport = final_df_plot # Added to play with plots for FAO report
  
  # create data for annotation 
  yMax = max(filter(final_df, Type == "NomActive")$Value, na.rm = TRUE)
  # ann_text <- data.frame(Year = c(1841+10,1861+15,1961+15), Value = rep(yMax,3), lab = c("Spin-up","Transition","Experiment"), group = rep("Effort (nominal, DkW)", 3), Data = rep("Reconstructed values", 3), Considered = rep("Yes", 3))
  
  ann_text <- data.frame(Year = c(1841+10,1961+15), Value = rep(yMax,2), lab = c("Transition","Experiment"), group = rep("Effort (nominal, DkW)", 2), Data = rep("Reconstructed values", 2), Considered = rep("Yes", 2))
  
  plot<-ggplot(data = final_df_plot, aes(x = Year, y = Value, group = Data, color = Data, shape=Considered)) +
    # ggtitle(paste("LME", toKeep, sep = " "))+
    ggtitle(toKeep)+
    facet_wrap(~group, scales = "free", nrow = 2)+
    annotate("rect",xmin=1841, xmax=1960, ymin=0, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)+ # spin-up edf8fb
    # annotate("rect",xmin=1861, xmax=1960, ymin=0, ymax=Inf, fill = "#66c2a4", alpha = 0.4)+ # transition b2e2e2
    annotate("rect",xmin=1961, xmax=2010, ymin=0, ymax=Inf, fill = "#238b45", alpha = 0.4)+ # projection 66c2a4
    # annotate("rect",xmin=2010, xmax=2017, ymin=0, ymax=Inf, fill = "#238b45", alpha = 0.4)+ # validation 238b45
    geom_text(data = ann_text,label = ann_text$lab, color = "Black", size = 2)+
    geom_point(data = filter(final_df_plot, Data == "Reconstructed values"),size=1)+
    geom_line(data = filter(final_df_plot, Data == "GAM predictions")) +
    scale_color_manual(values = c("blue", "black"), drop = FALSE)+
    scale_fill_chris()+
    my_theme
  
  # # check low catches and whether they are 0 effort (FAO_21 as example): 
  # # YES problem in some cases as low catches results in reconstructed 0 effort. is this OK in terms of model spin-up? 
  # str(final_df_plot)
  # filter(final_df_plot, Year %in% c(1940, 1950, 1951, 1900))
  
  if(var == "catch_tot"){ # different reference year are only relevent when effort reconstruction is based on catches  
    if(reference == "1950"){
      plot<-plot+
        geom_vline(xintercept=1950, linetype="dashed", color = "red", size=0.5)
    }else{
      plot<-plot+
        geom_vline(xintercept=1945, linetype="dashed", color = "red", size=0.5)+
        geom_vline(xintercept=1955, linetype="dashed", color = "red", size=0.5)
    }}
  
  # save outputs 
  return(list(reconstructed_effort_LME = final_df, plot_reconstructed_effort_LME = plot, chechDataUsed = chechDataUsed, data_for_plot_for_FAOreport = data_for_plot_for_FAOreport)) 
  
  # } # END if data exist 
  
} 

# allocate reconstructed effort across groups function

effort_spread<-function(historical_effort, recent_effort, toKeep, reference){
  
  # # trial
  # # error i = 17 (FAO_for_LME0_88)
  # i = 17 # no effort reconstruction for this region! probably because only very recent data is available...
  # historical_effort = decision[[i]] # effort from 1841 to 2017 at LME level from analysis above - missing groups (eez, gear, fgroup, saup, sector, etc.)
  # recent_effort = recent_effort # effort from 1950 to 2017 with all groups
  # toKeep = toKeep[[i]]
  # reference = "1950-1960"
  
  # calculate effort distribution across group using 1950-1960, if you use one year only you might miss important groups not represented in 1950 
  if(reference == "1950"){
    recent_effortB<-recent_effort %>% 
      filter(Year == 1950)
  }else{
    recent_effortB<-recent_effort %>% 
      filter(Year >= 1950, Year <=1960)
  }
  
  # WARNING - problem with FAO_for_LME0_88 as there is no data until 1950 in original Yannick's dataset. 
  if(toKeep == "FAO_for_LME0_88"){
    recent_effortB<-recent_effort %>% 
      filter(Year >= 1974, Year <=1984)
  }
  
  
  
  
  
  
  
  
  
  
  # sort(unique(historical_effort$Year))
  # sort(unique(recent_effort$Year))
  
  # this is done for each LME (LME in grouping below is not necessary). 
  # recent_effort is effort from 1950-2017 and recent_effortB is effort from 1950-1960 (reference years) including all groups
  # NOTE from recent_effort you consider CONTRIBUTIONS by groups 
  # first you calculate the average effort by group 1950-1960 (reference effort)
  # then you calculate the effort contribution of each group. at the LME level this sums to 1 
  
  
  ####### WARNING this is where it get tricky with the new organisation of LME 0 if you are considering EEZ and FAO area only. 
  # What happens with area that is outside EEZ and LME0+FAO classification and withing an LME? 
  # should you check that the sum of effort when using different grouping is always the same??? 
  # of course need to group_by fao_area too below!
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  recent_effortB<-recent_effortB %>% 
    group_by(eez_country_name,SAUP,Gear,FGroup,Sector,LME,fao_area) %>% # added fao_area for LME0 grouping 
    summarise(NomActive = mean(NomActive, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(contribution = NomActive/sum(NomActive, na.rm = TRUE))
  
  # check
  sum(recent_effortB$contribution, na.rm = TRUE)
  
  # now you work on the historical data 1841-1950.
  # FIRST - create all combinations of groups in historical years - e.g. 1841 all eez, saup, grear, fgroup, sector present in the reference years (1950-1960) with contribution being constant across year (proportions don't change, effort does), 1842 same story ... 
  Year= seq(1841,1949)
  toExpend<-recent_effortB %>% 
    select(-NomActive) %>% 
    unique()
  
  toExpend<-rbindlist(lapply(1841:1949, function(x,d) data.frame(d, Year=x), d=toExpend))
  toExpend<-as_tibble(toExpend)
  
  ### WARNING adjust FAO_for_LME0_88
  if(toKeep == "FAO_for_LME0_88"){
    year = seq(1841, 1973) # better define last year given Year vector 
      toExpend<-recent_effortB %>% 
        select(-NomActive) %>% 
        unique()
    
    toExpend<-rbindlist(lapply(1841:1973, function(x,d) data.frame(d, Year=x), d=toExpend))
    toExpend<-as_tibble(toExpend) 
  }
  
  
  
  
  
  
  
  
  
  
  # SECOND - get effort value by year for 1841-1950
  # NOTE from historical_effort you consider EFFORT VALUES by year withing LME 
  # not much done here, just data wrangling to get year, effort dataset
  historical_effort<-historical_effort %>% 
    filter(Type == "NomActive") %>% # Here can keep catches too 
    select(-group, -Considered) %>% 
    spread(Type, Value) %>% 
    filter(!is.na(NomActive)) %>% # catch data goes to 2017 but effort to 2010 - source of NAs 
    rename(NomActive_pred = NomActive) 
  
  # THIRD - merge historical effort values with groups from recent_effortB (through toExpand)
  # and calculate NomActive for each group based on: historical effort values*effort contribution by each group in 1950-1960 (from recent_effortB) 
  final_df<-filter(historical_effort, Year <1950) %>% # this are the only year that we need to reconstruct and merge with recent_effort
    full_join(toExpend) %>% 
    mutate(NomActive = NomActive_pred*contribution) %>% 
    select(-NomActive_pred, -contribution)
  
  ### WARNING adjust FAO_for_LME0_88
  if(toKeep == "FAO_for_LME0_88"){
    final_df<-filter(historical_effort, Year <1974) %>% # this are the only year that we need to reconstruct and merge with recent_effort
      full_join(toExpend) %>% 
      mutate(NomActive = NomActive_pred*contribution) %>% 
      select(-NomActive_pred, -contribution)
  }
    
    
    
    
    
    
    
    
  
  
  
  # # check - ok - the sum of effort at the LME level matches .... 
  # final_df %>% filter(Year ==1841) %>% mutate(a = sum(NomActive))
  # decision[[1]] %>% filter(Year ==1841) # compare a with original data
  
  # FORTH - merge merge historical effort where contributions to the groups has been calculated above (1841-1950) with recent_effort (1950-2017)
  # sort(unique(final_df$Year))
  # sort(unique(recent_effort$Year))
  # 
  # tic()
  # final_df1<- lazy_dt(final_df) %>%
  #   full_join(lazy_dt(recent_effort)) %>%
  #   as.data.frame()
  # toc()
  
  # tic()
  final_df<- final_df %>% 
    full_join(recent_effort) %>% 
    as.data.frame()
  # toc()
  
  # # check they give the same ouputs -  
  # trial<-final_df1 %>% 
  #   group_by(Year) %>% 
  #   summarise(effort = sum(NomActive, na.rm = TRUE)) %>%
  #   ungroup()
  # 
  # pdf("Output/trial.pdf", height = 8, width = 6)
  # ggplot(trial, aes(x = Year, y = effort))+
  #   geom_point()
  # dev.off()
  
  # final df for plotting 
  data_plot<-final_df %>%
    group_by(Year, Gear) %>% 
    summarise(NomActive = sum(NomActive, na.rm = TRUE)) %>% 
    ungroup()
  
  # plot<-ggplot(data_plot, aes(y = NomActive, x = Year, group = Gear, color = Gear))+
  #   geom_line()+
  #   ggtitle(paste("LME", toKeep, sep = " ")) +# WARNING - check this is correct
  #   my_theme
  
  # create data for annotation 
  yMax = max(data_plot$NomActive, na.rm = TRUE)
  
  # ann_text <- data.frame(Year = c(1841+10,1861+15,1961+15), NomActive = rep(yMax,3), lab = c("Spin-up","Transition","Experiment"), Gear = rep(unique(data_plot$Gear)[1], 3))
  
  ann_text <- data.frame(Year = c(1841+10,1961+15), NomActive = rep(yMax,2), lab = c("Transition","Experiment"), Gear = rep(unique(data_plot$Gear)[1], 2))
  
  plot<-ggplot(data = data_plot, aes(x = Year, y = NomActive, group = Gear, color = Gear)) +
    ggtitle(paste("LME", toKeep, sep = " "))+ # WARNING - check this is correct
    annotate("rect",xmin=1841, xmax=1960, ymin=0, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)+ # spin-up edf8fb
    # annotate("rect",xmin=1861, xmax=1960, ymin=0, ymax=Inf, fill = "#66c2a4", alpha = 0.4)+ # transition b2e2e2
    annotate("rect",xmin=1961, xmax=2010, ymin=0, ymax=Inf, fill = "#238b45", alpha = 0.4)+ # projection 66c2a4
    # annotate("rect",xmin=1841, xmax=1950, ymin=0, ymax=Inf, fill = "#edf8fb", alpha = 0.4)+ # spin-up
    # annotate("rect",xmin=1950, xmax=1961, ymin=0, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)+ # transition
    # annotate("rect",xmin=1961, xmax=2010, ymin=0, ymax=Inf, fill = "#66c2a4", alpha = 0.4)+ # projection 
    # annotate("rect",xmin=2010, xmax=2017, ymin=0, ymax=Inf, fill = "#238b45", alpha = 0.4)+ # validation
    geom_text(data = ann_text,label = ann_text$lab, color = "Black", size = 2)+
    geom_line(size = 0.5)+
    geom_point(size = 0.5)+
    my_theme
  
  if(reference == "1950"){
    plot<-plot+
      geom_vline(xintercept=1950, linetype="dashed", color = "red", size=0.5)
  }else{
    plot<-plot+
      geom_vline(xintercept=1950, linetype="dashed", color = "red", size=0.5)+
      geom_vline(xintercept=1960, linetype="dashed", color = "red", size=0.5)
  }
  
  
  ### WARNING adjust FAO_for_LME0_88
  if(toKeep == "FAO_for_LME0_88"){
    plot<-plot+
      geom_vline(xintercept=1974, linetype="dashed", color = "red", size=0.5)+
      geom_vline(xintercept=1974, linetype="dashed", color = "red", size=0.5)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  # # check with gear and FGroup - OK
  # pdf("Output/trial1.pdf", height = 8, width = 6)
  # plot
  # dev.off()
  
  return(list(final_df = final_df, plot = plot))
  
}
