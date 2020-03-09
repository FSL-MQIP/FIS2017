library(dplyr)
d <- read.csv("Query_for_Stats.txt") %>%
  filter(Type=="BTM")

# add total hygiene scores observed, total teat end scores observed, whether
# a row is from an observation period or not, and calculate mean count.
d <- d %>%
  mutate(mean_count=(
    Rep1+Rep2+
      Rep3+Rep4+
      Rep5+Rep6+
      Rep7+Rep8+
      Rep9+Rep10)/10,
    observed=ifelse(!is.na(Hygiene2),"Observed","Not observed"),
    hyg_n=Hygiene1+Hygiene2+Hygiene3+Hygiene4,
    teat_n=TeatN+TeatS+TeatR+TeatVR
  )

# Calculate visit-level variables
visit_level <- d %>%
  group_by(Test,Visit,Farm) %>%
  summarize(
    hyg_n_agg=sum(hyg_n,na.rm=T),
    est_cows_seen=hyg_n_agg*5,
    hyg1_agg=sum(Hygiene1,na.rm=T),
    hyg2_agg=sum(Hygiene2,na.rm=T),
    hyg3_agg=sum(Hygiene3,na.rm=T),
    hyg4_agg=sum(Hygiene4,na.rm=T),
    teat_n_agg=sum(teat_n,na.rm=T),
    teat_none_agg=sum(TeatN,na.rm=T),
    teat_smooth_agg=sum(TeatS,na.rm=T),
    teat_rough_agg=sum(TeatR,na.rm=T),
    teat_veryrough_agg=sum(TeatVR,na.rm=T),
    kickoffs_agg=sum(KickOffs,na.rm=T)
  ) %>%
  ungroup() %>%
  select(-Test) %>%
  mutate(
    hyg1_agg_prop=hyg1_agg/hyg_n_agg,
    hyg2_agg_prop=hyg2_agg/hyg_n_agg,
    hyg3_agg_prop=hyg3_agg/hyg_n_agg,
    hyg4_agg_prop=hyg4_agg/hyg_n_agg,
    teat_none_agg_prop=teat_none_agg/teat_n_agg,
    teat_smooth_agg_prop=teat_smooth_agg/teat_n_agg,
    teat_rough_agg_prop=teat_rough_agg/teat_n_agg,
    teat_veryrough_agg_prop=teat_veryrough_agg/teat_n_agg
  ) %>%
  unique()


# THIS CURRENTLY IS NOT CORRECT BECAUSE THE DATABASE HAS FARM 5 VISIT 3
# CODED AS FARM 5 VISIT 2 AND HENCE THERE ARE COLLISIONS
# DATABASE UPDATED BY RACHEL, IS CORRECT NOW
d <- d %>%
  left_join(visit_level,by=c(Farm="Farm",Visit="Visit")) %>%
  mutate(kickoffs_per_hundred=100*kickoffs_agg/est_cows_seen) %>%
  mutate(intervention_recoded=ifelse(PostIntervention=="Yes",
                                     "After Intervention",
                                     "Before Intervention")) %>%
  mutate(log_mean_count=ifelse(mean_count>0,log10(mean_count),log10(0.05))) %>%
  filter(!is.na(log_mean_count))

d$intervention_recoded <- factor(d$intervention_recoded,levels=c("Before Intervention",
                                                                 "After Intervention"))
d$Visit <- as.factor(d$Visit)

rm(visit_level)

d$ShiftStartTime <- as.POSIXct(d$ShiftStartTime,format="%m/%d/%Y %H:%M")
d$TrainingDate <- as.POSIXct(d$TrainingDate,format="%m/%d/%Y %H:%M")
d$ShiftStartHour <- as.numeric(strftime(d$ShiftStartTime,format="%H"))

tsc <- d %>%
  filter(Test=="TSC")
msc <- d %>%
  filter(Test=="MSC")

###This is our attempt at getting means for subset farm subset before after intervention

##summarize mean and sd by FarmID and intervention_recoded
#msc
msc_tbl_byFarmbyInt <- msc %>%
  group_by(FarmID,intervention_recoded) %>%
  summarize(mean_log_mean_count_byFarmInt = mean(log_mean_count),
            sd_log_mean_count_byFarmInt = sd(log_mean_count))
#tsc
tsc_tbl_byFarmbyInt <- tsc %>%
  group_by(FarmID,intervention_recoded) %>%
  summarize(mean_log_mean_count_byFarmInt = mean(log_mean_count),
            sd_log_mean_count_byFarmInt = sd(log_mean_count))

##summarize mean and sd by visit and intervention_recoded
#msc
msc_tbl_byVisitbyInt <- msc %>%
  group_by(Visit,intervention_recoded) %>%
  summarize(mean_log_mean_count_byVisitInt = mean(log_mean_count),
            sd_log_mean_count_byVisitInt = sd(log_mean_count))
#tsc
tsc_tbl_byVisitbyInt <- tsc %>%
  group_by(Visit,intervention_recoded) %>%
  summarize(mean_log_mean_count_byVisitInt = mean(log_mean_count),
            sd_log_mean_count_byVisitInt = sd(log_mean_count))

#MSC with mean count
msc_tbl_byVisitbyInt_CFU <- msc %>%
  group_by(Visit,intervention_recoded) %>%
  summarize(mean_mean_count_byVisitInt = mean(mean_count),
            sd_mean_count_byVisitInt = sd(mean_count))

#TSC with mean count
tsc_tbl_byVisitbyInt_CFU <- tsc %>%
  group_by(Visit,intervention_recoded) %>%
  summarize(mean_mean_count_byVisitInt = mean(mean_count),
            sd_mean_count_byVisitInt = sd(mean_count))

##summarize mean and sd by sampling day and intervention_recoded
#msc
msc_tbl_byVisitbySamplingDaybyInt <- msc %>%
  group_by(Visit,SamplingDay,intervention_recoded) %>%
  summarize(mean_log_mean_count_byVisitbySamplingDaybyInt = mean(log_mean_count),
            sd_log_mean_count_byVisitbySamplingDaybyInt = sd(log_mean_count))
#tsc
tsc_tbl_byVisitbySamplingDaybyInt <- tsc %>%
  group_by(Visit,SamplingDay,intervention_recoded) %>%
  summarize(mean_log_mean_count_byVisitbySamplingDaybyInt = mean(log_mean_count),
            sd_log_mean_count_byVisitbySamplingDaybyInt = sd(log_mean_count))

