setwd("/Users/prossercathey/Documents/GitHub/Mask-Mandates-and-Consumer-Spending")

class_data <- read.csv("class_data.csv")
state_tax <- read.csv("StateTax.csv")
rural_urban <- read.csv("ruralurban.csv")
mask_policy <- read.csv("mask_policy.csv")
interventions <- read.csv("county_interventions.csv")
consumer_spending <- read.csv("consumer_spending.csv")
cases <- read.csv("cases.csv")


library(tidyverse)
library(lfe)
library(did)
library(stargazer)
library(ggplot2)
library(here)
library(foreign)
library(tidyverse)
library(dplyr)
library(did)
library(HonestDiD)
library(scales)
library(ggpubr)
library(lmtest)
library(sandwich)

#Clean & Format Data
# Drop columns I don't use
mask_policy <- mask_policy[,!(names(mask_policy) %in% c("a","b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))]
# Rename column
mask_policy$CountyFIPS <- mask_policy$CountFIPS
# Converting to days this year
mask_policy$EariliestPolicyDaysThisYear <- mask_policy$EarliestpolicyDay + (mask_policy$EarliestpolicyMonth==1) + (mask_policy$EarliestpolicyMonth==2)*31 + (mask_policy$EarliestpolicyMonth==3)*(31+29) + (mask_policy$EarliestpolicyMonth==4)*(2*31+29) + (mask_policy$EarliestpolicyMonth==5)*(2*31+30+29) + (mask_policy$EarliestpolicyMonth==6)*(3*31+30+29) + (mask_policy$EarliestpolicyMonth==7)*(3*31+2*30+29) + (mask_policy$EarliestpolicyMonth==8)*(4*31+2*30+29) + (mask_policy$EarliestpolicyMonth==9)*(5*31+2*30+29)
cases$DaysThisYear <- cases$day + (cases$month==1) + (cases$month==2)*31 + (cases$month==3)*(31+29) + (cases$month==4)*(2*31+29) + (cases$month==5)*(2*31+30+29) + (cases$month==6)*(3*31+30+29) + (cases$month==7)*(3*31+2*30+29) + (cases$month==8)*(4*31+2*30+29) + (cases$month==9)*(5*31+2*30+29)
consumer_spending$DaysThisYear <- consumer_spending$day + (consumer_spending$month==1)*0 + (consumer_spending$month==2)*31 + (consumer_spending$month==3)*(31+29) + (consumer_spending$month==4)*(2*31+29) + (consumer_spending$month==5)*(2*31+30+29) + (consumer_spending$month==6)*(3*31+30+29) + (consumer_spending$month==7)*(3*31+2*30+29) + (consumer_spending$month==8)*(4*31+2*30+29) + (consumer_spending$month==9)*(5*31+2*30+29)
# Tagging counties with mask mandates imposed by state
list <- unique(mask_policy$State_Code)
mask_policy$State_Imposed <- 0
for (x in list) {
  # Excluding states without state-level mandates
  if (sum(as.numeric(is.na(mask_policy$EariliestPolicyDaysThisYear[mask_policy$State_Code==x])))==0) {
    mask_policy$State_Imposed <- mask_policy$State_Imposed + as.numeric(mask_policy$State_Code==x & mask_policy$EariliestPolicyDaysThisYear==(max(mask_policy$EariliestPolicyDaysThisYear[mask_policy$State_Code==x])))
  }
}
# Renaming columns
cases$CountyFIPS <- cases$fips
consumer_spending$CountyFIPS <- consumer_spending$countyfips
# Converting spend_all to correct type
consumer_spending$spend_all <- as.numeric(as.character(consumer_spending$spend_all))
# Merging consumer spending and mask policy datasets
csxmp <- merge(consumer_spending,mask_policy,by="CountyFIPS")
# Creating Days Since Mandate Variable
csxmp$DaysSinceMandate <- csxmp$DaysThisYear - csxmp$EariliestPolicyDaysThisYear
# Creating Dummy Variable for Pre/Post Treatment
csxmp$PostDummy <- as.numeric(csxmp$DaysSinceMandate>0)
csxmp$PostDummy [csxmp$PostDummy == "true"] <- 1
csxmp$PostDummy [csxmp$PostDummy == "false"] <- 0
csxmp$PostDummy <- as.integer(csxmp$PostDummy)
csxmp$PostDummy[is.na(csxmp$PostDummy)==TRUE] <- 0
# Adding cases data
csxmpxc <- merge(csxmp, cases, by = c("CountyFIPS", "DaysThisYear" ), all = TRUE)
# Making cases NA into zeros
csxmpxc$cases[is.na(csxmpxc$cases)] <- 0
# Recoding reference frame of dates from days since 12/31/0000 to days since 12/31/2019
# 737426 is the number of days from 01/01/0001 until 01/01/2020
interventions[,c(4:16)] <- interventions[,c(4:16)] -737426
# Renaming column
interventions$CountyFIPS <- interventions$FIPS
# Adding in NPI dates
csxmpxc <- merge(csxmpxc, interventions, by = "CountyFIPS", all = TRUE)
csxmpxc$DiffSAH_ <- csxmpxc$EariliestPolicyDaysThisYear - csxmpxc$stay.at.home.rollback
csxmpxc$Diff500G_ <- csxmpxc$EariliestPolicyDaysThisYear - csxmpxc$X.500.gatherings.rollback
csxmpxc$Diff50G_ <- csxmpxc$EariliestPolicyDaysThisYear - csxmpxc$X.50.gatherings.rollback
csxmpxc$DiffRest_ <- csxmpxc$EariliestPolicyDaysThisYear - csxmpxc$restaurant.dine.in.rollback
csxmpxc$DiffGym_ <- csxmpxc$EariliestPolicyDaysThisYear - csxmpxc$entertainment.gym.rollback
# Including Dummys for NPIs
csxmpxc <- csxmpxc[is.na(csxmpxc$spend_all)==FALSE,]
csxmpxc$SAHDummy <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$stay.at.home & csxmpxc$DaysThisYear<=csxmpxc$stay.at.home.rollback) +
  as.numeric(csxmpxc$DaysThisYear>=csxmpxc$stay.at.home & is.na(csxmpxc$stay.at.home.rollback)==TRUE)
csxmpxc$SAHDummy[is.na(csxmpxc$SAHDummy)] <- 0
csxmpxc$RestDummy <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$restaurant.dine.in & csxmpxc$DaysThisYear<=csxmpxc$restaurant.dine.in.rollback) +
  as.numeric(csxmpxc$DaysThisYear>=csxmpxc$restaurant.dine.in & is.na(csxmpxc$restaurant.dine.in.rollback)==TRUE)
csxmpxc$RestDummy[is.na(csxmpxc$RestDummy)] <- 0
csxmpxc$GymDummy <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$entertainment.gym & csxmpxc$DaysThisYear<=csxmpxc$entertainment.gym.rollback) +
  as.numeric(csxmpxc$DaysThisYear>=csxmpxc$entertainment.gym & is.na(csxmpxc$entertainment.gym.rollback)==TRUE)
csxmpxc$GymDummy[is.na(csxmpxc$GymDummy)] <- 0
csxmpxc$Dummy50 <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$X.50.gatherings & csxmpxc$DaysThisYear<=csxmpxc$X.50.gatherings.rollback) +
  as.numeric(csxmpxc$DaysThisYear>=csxmpxc$X.50.gatherings & is.na(csxmpxc$X.50.gatherings.rollback)==TRUE)
csxmpxc$Dummy50[is.na(csxmpxc$Dummy50)] <- 0
csxmpxc$Dummy500 <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$X.500.gatherings & csxmpxc$DaysThisYear<=csxmpxc$X.500.gatherings.rollback) +
  as.numeric(csxmpxc$DaysThisYear>=csxmpxc$X.500.gatherings & is.na(csxmpxc$X.500.gatherings.rollback)==TRUE)
csxmpxc$Dummy500[is.na(csxmpxc$Dummy500)] <- 0
csxmpxc$DummyTravel <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$foreign.travel.ban)
csxmpxc$DummyTravel[is.na(csxmpxc$DummyTravel)] <- 0
csxmpxc$DummyFed <- as.numeric(csxmpxc$DaysThisYear>=csxmpxc$federal.guidelines)
csxmpxc$DummyFed[is.na(csxmpxc$DummyFed)] <- 0
csxmpxc$DummyAllNPIs <- as.numeric(csxmpxc$SAHDummy==1|csxmpxc$RestDummy==1|csxmpxc$GymDummy==1|csxmpxc$Dummy50==1|csxmpxc$Dummy500==1|csxmpxc$DummyTravel==1|csxmpxc$DummyFed==1)

# Replacing NAs with 0 for date of earliest mask mandate policy as necessary for did function
csxmpxc$EariliestPolicyDaysThisYear[is.na(csxmpxc$EariliestPolicyDaysThisYear)] <- 0
# Replacing NAs with 0 for covariates
csxmpxc$cases[is.na(csxmpxc$cases)] <- 0
csxmpxc$deaths[is.na(csxmpxc$deaths)] <- 0
csxmpxc$SAHDummy[is.na(csxmpxc$SAHDummy)] <- 0
csxmpxc$RestDummy[is.na(csxmpxc$RestDummy)] <- 0
csxmpxc$GymDummy[is.na(csxmpxc$GymDummy)] <- 0
csxmpxc$Dummy50[is.na(csxmpxc$Dummy50)] <- 0
csxmpxc$Dummy500[is.na(csxmpxc$Dummy500)] <- 0
csxmpxc$DummyFed[is.na(csxmpxc$DummyFed)] <- 0
csxmpxc$DummyTravel[is.na(csxmpxc$DummyTravel)] <- 0

# Creating composite NPI Score
csxmpxc$NPI <- (csxmpxc$SAHDummy + csxmpxc$RestDummy + csxmpxc$GymDumm + csxmpxc$Dummy50 + csxmpxc$Dummy500 + csxmpxc$DummyTravel)/6

# Removing 6 duplicate (i, t)s
csxmpxc <- csxmpxc %>%
  distinct(CountyFIPS, DaysThisYear, .keep_all = TRUE)

# Callaway & Sant'Anna DiD Regressions no Covariates
did <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = NULL,
  data = csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# Results
# did no covariates
did_simple <- aggte(
  did,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_dynamic <- aggte(
  did,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = FALSE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_group <- aggte(
  did,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = FALSE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_calendar <- aggte(
  did,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = FALSE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did)


# Callaway & Sant'Anna DiD Regressions with Covariates no fixed effects
did_covs <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + NPI,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# did with covariates results

did_covs_simple <- aggte(
  did_covs,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_covs_dynamic <- aggte(
  did_covs,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_covs_group <- aggte(
  did_covs,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_covs_calendar <- aggte(
  did_covs,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_covs)

# Robustness Checks
# Replacing composite NPI metric with Stay At Home Dummy Covariate
did_SAH <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + SAHDummy,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# Saving results
did_SAH_simple <- aggte(
  did_SAH,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_dynamic <- aggte(
  did_SAH,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_group <- aggte(
  did_SAH,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_calendar <- aggte(
  did_SAH,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_SAH)

# Only state level mask mandates 
did_state_level_mm1 <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + NPI + CountyFIPS,
  csxmpxc[csxmpxc$State_Imposed==1 | csxmpxc$EariliestPolicyDaysThisYear==0,],
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# Saving Results
did_state_level_mm1_simple <- aggte(
  did_state_level_mm1,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_state_level_mm1_dynamic <- aggte(
  did_state_level_mm1,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_state_level_mm1_group <- aggte(
  did_state_level_mm1,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_state_level_mm1_calendar <- aggte(
  did_state_level_mm1,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_state_level_mm1)

# Fixed effects & covariates
did_fe <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + NPI + CountyFIPS,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

did_fe_simple <- aggte(
  did_fe,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_dynamic <- aggte(
  did_fe,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_group <- aggte(
  did_fe,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_calendar <- aggte(
  did_fe,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_fe)

# Fixed effects only
did_fe_only <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ CountyFIPS,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

did_fe_only_simple <- aggte(
  did_fe_only,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_only_dynamic <- aggte(
  did_fe_only,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_only_group <- aggte(
  did_fe_only,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_only_calendar <- aggte(
  did_fe_only,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_fe_only)

# Fixed effects & cases
did_fe_cases <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ CountyFIPS + cases,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

did_fe_cases_simple <- aggte(
  did_fe_cases,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_cases_dynamic <- aggte(
  did_fe_cases,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_cases_group <- aggte(
  did_fe_cases,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_cases_calendar <- aggte(
  did_fe_cases,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_fe_cases)

# Robustness Check: Removing overlapped SAH orders & mask mandates 
# Tagging overlapped units
csxmpxc$SAHOverlap <- as.numeric(csxmpxc$PostDummy == 1 & csxmpxc$SAHDummy == 1)
list <- unique(csxmpxc$CountyFIPS)
csxmpxc$CountySAHOverlap <- 0
# Tagging any county with an overlapped unit
for (x in list) {
  csxmpxc[csxmpxc$CountyFIPS == x,]$CountySAHOverlap <- max(csxmpxc[csxmpxc$CountyFIPS == x,]$SAHOverlap)
  print(x)
}

did_SAH_no_overlap <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + NPI + CountyFIPS,
  csxmpxc[csxmpxc$CountySAHOverlap==0,],
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# Saving Results
did_SAH_no_overlap_simple <- aggte(
  did_SAH_no_overlap,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_no_overlap_dynamic <- aggte(
  did_SAH_no_overlap,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_no_overlap_group <- aggte(
  did_SAH_no_overlap,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_SAH_no_overlap_calendar <- aggte(
  did_SAH_no_overlap,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_SAH_no_overlap)

# Clustering standard errors on state level
did_fe_state_cluster <- att_gt(
  yname = "spend_all",
  tname = "DaysThisYear",
  idname = "CountyFIPS",
  gname = "EariliestPolicyDaysThisYear",
  xformla = ~ cases + NPI + CountyFIPS,
  csxmpxc,
  panel = TRUE,
  allow_unbalanced_panel = FALSE,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = NULL,
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = "State_Code",
  est_method = "dr",
  base_period = "varying",
  print_details = TRUE,
  pl = TRUE,
  cores = 8
)

# Saving results
did_fe_state_cluster_simple <- aggte(
  did_fe_state_cluster,
  type = "simple",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_state_cluster_dynamic <- aggte(
  did_fe_state_cluster,
  type = "dynamic",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_state_cluster_group <- aggte(
  did_fe_state_cluster,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

did_fe_state_cluster_calendar <- aggte(
  did_fe_state_cluster,
  type = "calendar",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = TRUE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL
)

rm(did_fe_state_cluster)

# Event Study Graph
# Covariates w/ fixed effects
did_fe_dynamic$nine_five_max <- did_fe_dynamic$att.egt + did_fe_dynamic$crit.val.egt*did_fe_dynamic$se.egt
did_fe_dynamic$nine_five_min <- did_fe_dynamic$att.egt - did_fe_dynamic$crit.val.egt*did_fe_dynamic$se.egt
did_fe_df <- as.data.frame(cbind(did_fe_dynamic$att.egt, did_fe_dynamic$se.egt, did_fe_dynamic$nine_five_min, did_fe_dynamic$nine_five_max))
colnames(did_fe_df) <- c("Estimate","SE","Min","Max")
did_fe_df$min_zero <- did_fe_df$Min>0
did_fe_df[did_fe_df$min_zero == TRUE,]$min_zero <- "Yes"
did_fe_df[did_fe_df$min_zero == FALSE,]$min_zero <- "No"
colnames(did_fe_df)[5] <- "95% Significance"
did_fe_df$day <- as.numeric(c(-147:142))
did_fe_df <- did_fe_df[did_fe_df$day<100,]
did_fe_df <- did_fe_df[did_fe_df$day>-100,]
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(did_fe_df, aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Min, ymax=Max), width=1, position=pd, colour = "blue") +
  geom_point(aes(colour = `95% Significance`)) +
  scale_color_manual(values = c("black", "red")) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x=-100,xend=-1,y=mean(did_fe_df[did_fe_df$day<0,]$Estimate),yend=mean(did_fe_df[did_fe_df$day<0,]$Estimate)), color = "red") +
  geom_segment(aes(x=-1,xend=100,y=mean(did_fe_df[did_fe_df$day>0,]$Estimate),yend=mean(did_fe_df[did_fe_df$day>0,]$Estimate)), color = "red") +
  xlab("Days Since Mandate") +
  ggtitle(c("Mask Mandate and Consumer Spending Estimates Relative to Treatment Timing")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

# Effects over time graph
# Covariates w/ fixed effects
did_fe_calendar$nine_five_max <- did_fe_calendar$att.egt + did_fe_calendar$crit.val.egt*did_fe_calendar$se.egt
did_fe_calendar$nine_five_min <- did_fe_calendar$att.egt - did_fe_calendar$crit.val.egt*did_fe_calendar$se.egt
did_fe_df <- as.data.frame(cbind(did_fe_calendar$att.egt, did_fe_calendar$se.egt, did_fe_calendar$nine_five_min, did_fe_calendar$nine_five_max))
colnames(did_fe_df) <- c("Estimate","SE","Min","Max")
did_fe_df$min_zero <- did_fe_df$Min>0
did_fe_df$max_zero <- did_fe_df$Max<0
did_fe_df[did_fe_df$min_zero == TRUE | did_fe_df$max_zero == TRUE,]$min_zero <- "Yes"
did_fe_df[did_fe_df$min_zero == FALSE,]$min_zero <- "No"
colnames(did_fe_df)[5] <- "95% Significance"
did_fe_df$day <- as.numeric(c(76:243))
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(did_fe_df, aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Min, ymax=Max), width=1, position=pd, colour = "blue") +
  geom_point(aes(colour = `95% Significance`)) +
  scale_color_manual(values = c("black", "red")) +
  geom_hline(yintercept = 0) +
  xlab("Days This Year") +
  ggtitle(c("Effect of Mask Mandate of Consumer Spending Over Time")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

# State Tax Implications
# for each state, hypothetical non-mm tax revenue = actual tax revenue/(1+.023%*(weighted average of counties that actually have mask mandates))
# Renaming column
rural_urban$CountyFIPS <- rural_urban$FIPS
# Merging population data
popxmp <- merge(mask_policy, rural_urban, by = "CountyFIPS")
# Making character data numeric
popxmp$Population_2010 <- as.numeric(gsub(",", "", popxmp$Population_2010))
# Making earliest mask mandate date NAs into 0s
popxmp$EariliestPolicyDaysThisYear[is.na(popxmp$EariliestPolicyDaysThisYear)] <- 0
# Calculating percent increase per state based on county pop & mandate status
list <- unique(popxmp$State_Code)
popxmp$percent_increase <- 0
for (x in list) {
    popxmp[popxmp$State_Code == x,]$percent_increase <- (1 + did_fe_simple$overall.att*
                                                           (sum(popxmp[popxmp$State_Code == x & popxmp$EariliestPolicyDaysThisYear >0,]$Population_2010))/sum(popxmp[popxmp$State_Code == x,]$Population_2010))
    print(x)
}
# Renaming column
state_tax$State <- state_tax$X
# Putting percent increase into state tax dataset
list <- unique(state_tax$StateFIPS)
state_tax$percent_increase <- 0
for (x in list) {
  state_tax[state_tax$StateFIPS == x,]$percent_increase <- median(popxmp[popxmp$State_Code == x,]$percent_increase)
  print(x)
}
# Calculating the expected sales tax in the absence of mask mandates
state_tax$hypo <- state_tax$Adjusted.Sales.Tax.Revenue/state_tax$percent_increase
# Calculating difference between hypothetical non-mask mandate tax revenue and observed revenue
state_tax$difference <- state_tax$Adjusted.Sales.Tax.Revenue - state_tax$hypo
# average increase
mean(state_tax$difference)
# Making character data numeric
state_tax$Total.Tax.Revenue <- as.numeric(gsub(",","",gsub("\\$", "", state_tax$Total.Tax.Revenue)))
# Fixing total revenue to proper scale
state_tax$Total.Tax.Revenue <- state_tax$Total.Tax.Revenue*1000
# Average % of revenue coming from sales tax
mean(state_tax$Adjusted.Sales.Tax.Revenue/state_tax$Total.Tax.Revenue)
# percent of total tax rev increase
state_tax$total_increase <- state_tax$difference/state_tax$Total.Tax.Revenue
# state with highest relative increase
state_tax[state_tax$total_increase == max(state_tax$total_increase),]$State
state_tax[state_tax$total_increase == max(state_tax$total_increase),]$total_increase
# Average relative increase
mean(state_tax$total_increase)
# Average absolute increase
mean(state_tax$difference)
# Nationwide absolute increase
sum(state_tax$difference)
# Loading scales
library(scales)
# Naming the columns
colnames(state_tax)[2] <- c("Sales Tax Revenue")
colnames(state_tax)[8] <- c("Est. Difference")
colnames(state_tax)[9] <- c("Est. % Increase")
# Dropping South Dakota because it had no mask mandates
state_tax <- state_tax[state_tax$State != "South Dakota",]
# Formatting as a percentage
state_tax$`Est. % Increase` <- label_percent(accuracy = .01)(state_tax$`Est. % Increase`)
# Formatting as $1,000,000s
state_tax$`Sales Tax Revenue` <- state_tax$`Sales Tax Revenue`
state_tax$`Est. Difference` <- state_tax$`Est. Difference`
state_tax$`Sales Tax Revenue` <- dollar_format()(state_tax$`Sales Tax Revenue`)
state_tax$`Est. Difference` <- dollar_format()(state_tax$`Est. Difference`)
# Cutting columns I don't want to export to Latex
state_tax <- state_tax[,c("State", "Sales Tax Revenue", "Est. Difference","Est. % Increase")]
# Creating Latex
stargazer(as.matrix(state_tax), rownames = FALSE)

# Event study of other NPIs
csxmpxc$DaysSinceMandateFactor <- as.factor(csxmpxc$DaysSinceMandate)
relevel
warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
csxmpxc$DaysSinceMandateFactor <- relevel(csxmpxc$DaysSinceMandateFactor, ref = "-1")
csxmpxc$CountyFactor <- as.factor(csxmpxc$CountyFIPS)
lout2 <- lm(DummyAllNPIs ~  CountyFIPS + cases + deaths + DaysSinceMandateFactor + CountyFactor, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(lout2)
lout2 <- as.data.frame(unclass(coeftest(lout2,vcov=vcovHC(lout2,type="HC0",cluster="CountyFIPS"))))
lout2 <- lout2[1:179,]
lout2$day <- as.numeric(c("NA", "NA", "NA", "NA", -50:-2, 0:125))
lout2$ci <- 1.96*lout2$`Std. Error`
pd <- position_dodge(0.1)
ggplot(lout2[is.na(lout2$day)==FALSE,], aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=1, position=pd, colour = "blue") +
  geom_point(size=1) + 
  geom_vline(xintercept = -1) +
  xlab("Days Since Mandate") +
  ggtitle(c("Non-Mask Mandate NPIs and Consumer Spending: Event Study")) +
  theme_classic()
# Checking the time distribution of mask mandates
subset <- csxmpxc[is.na(csxmpxc$EariliestPolicyDaysThisYear)==FALSE,]
subset <- subset[!duplicated(subset[,c("CountyFIPS")]),]
ggplot(subset[subset$EariliestPolicyDaysThisYear>0,], aes(x=EariliestPolicyDaysThisYear)) + 
  stat_ecdf(geom = "step") + 
  theme_classic() +
  xlab("Days This Year") +
  ylab("Density") +
  ggtitle("Cumulative Density Fuction of \nMask Mandate Implementation")
ggplot(subset[subset$EariliestPolicyDaysThisYear>0,], aes(x=EariliestPolicyDaysThisYear)) + 
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  xlab("Days This Year") +
  ylab("Density") +
  ggtitle("Time Distribution of \nMask Mandate Implementation")

# Reopenings Don't Matter Visualization
# Making Graphs
ggplot(interventions, aes(x=DiffSAH_)) + 
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  xlab("Difference Between Stay at Home Order End and \nMask Mandate Implementation (Days)") +
  ylab("Counties") +
  ggtitle("Stay at Home Order End and \nMask Mandate Implementation")
ggplot(interventions, aes(x=Diff500G_)) + 
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  ylab("Counties") +
  xlab("Difference Between 500 Person Gathering Ban \nEnd and Mask Mandate Implementation (Days)") +
  ggtitle("500 Person Gathering Ban End and \nMask Mandate Implementation")
ggplot(interventions, aes(x=Diff50G_)) +
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  ylab("Counties") +
  xlab("Difference Between 50 Person Gathering Ban \nEnd and Mask Mandate Implementation (Days)") +
  ggtitle("50 Person Gathering Ban End and \nMask Mandate Implementation")
ggplot(interventions, aes(x=DiffRest_)) + 
  geom_histogram(binwidth = 1) + 
  ylab("Counties") +
  theme_classic() +
  xlab("Difference Between Restaurant Reopening and \nMask Mandate Implementation (Days)") +
  ggtitle("Restaurant Reopening and \nMask Mandate Implementation")
ggplot(interventions, aes(x=DiffGym_)) + 
  ylab("Counties") +
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  xlab("Difference Between Gym & Enterntainment \nReopening and Mask Mandate Implementation (Days)") +
  ggtitle("Gym & Enterntainment Reopening \nand Mask Mandate Implementation")
