setwd("~/R Lesson")

mask_policy <- read.csv("mask_policy.csv")
class_data <- read.csv("class_data.csv")
consumer_spending <- read.csv("consumer_spending.csv")
rural_urban <- read.csv("ruralurban.csv")
presvotes <- read.csv("presvotes.csv")
cases <- read.csv("cases.csv")
state_tax <- read.csv("StateTax.csv")
interventions <- read.csv("county_interventions.csv")
cities <- read.csv("Cityspending.csv")
cityid <- read.csv("CityID.csv")

#Clean & Format Data
# Drop columns I don't use
mask_policy <- mask_policy[,!(names(mask_policy) %in% c("a","b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))]
# Rename column
mask_policy$CountyFIPS <- mask_policy$CountFIPS
# Converting to days this year
mask_policy$EariliestPolicyDaysThisYear <- mask_policy$EarliestpolicyDay + (mask_policy$EarliestpolicyMonth==1) + (mask_policy$EarliestpolicyMonth==2)*31 + (mask_policy$EarliestpolicyMonth==3)*(31+29) + (mask_policy$EarliestpolicyMonth==4)*(2*31+29) + (mask_policy$EarliestpolicyMonth==5)*(2*31+30+29) + (mask_policy$EarliestpolicyMonth==6)*(3*31+30+29) + (mask_policy$EarliestpolicyMonth==7)*(3*31+2*30+29) + (mask_policy$EarliestpolicyMonth==8)*(4*31+2*30+29) + (mask_policy$EarliestpolicyMonth==9)*(5*31+2*30+29)
cases$DaysThisYear <- cases$day + (cases$month==1) + (cases$month==2)*31 + (cases$month==3)*(31+29) + (cases$month==4)*(2*31+29) + (cases$month==5)*(2*31+30+29) + (cases$month==6)*(3*31+30+29) + (cases$month==7)*(3*31+2*30+29) + (cases$month==8)*(4*31+2*30+29) + (cases$month==9)*(5*31+2*30+29)
consumer_spending$DaysThisYear <- consumer_spending$day + (consumer_spending$month==1)*0 + (consumer_spending$month==2)*31 + (consumer_spending$month==3)*(31+29) + (consumer_spending$month==4)*(2*31+29) + (consumer_spending$month==5)*(2*31+30+29) + (consumer_spending$month==6)*(3*31+30+29) + (consumer_spending$month==7)*(3*31+2*30+29) + (consumer_spending$month==8)*(4*31+2*30+29) + (consumer_spending$month==9)*(5*31+2*30+29)
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
# Renaming columns
csxmp$statefips <- csxmp$State_Code
rural_urban$CountyFIPS <- rural_urban$FIPS
# Adding population data
csxmp <- merge(csxmp, rural_urban, by = "CountyFIPS")
# Renaming columns
csxmp$ruralurban <- csxmp$RUCC_2013
presvotes$CountyFIPS <- presvotes$combined_fips
# Adding pres. election data
csxmp <- merge(csxmp, presvotes, by = "CountyFIPS")
# Restricting to entries where I have consumer spending info (i.e. where spend_all is a number)
csxmp <- csxmp[csxmp$DaysThisYear>31,]
# Converting to days this year
class_data$ReopenDate <- class_data$begin_reopen_day + (class_data$begin_reopen_month==1)*31 + (class_data$begin_reopen_month==2)*(31+29) + (class_data$begin_reopen_month==3)*(2*31+29) + (class_data$begin_reopen_month==4)*(2*31+30+29) + (class_data$begin_reopen_month==5)*(3*31+30+29) + (class_data$begin_reopen_month==6)*(3*31+2*30+29) + (class_data$begin_reopen_month==7)*(4*31+2*30+29) + (class_data$begin_reopen_month==8)*(5*31+2*30+29) + (class_data$begin_reopen_month==9)*(3*30+5*31+29)
class_data$StateMaskMandateDate <- class_data$mandate_mask_day + (class_data$mandate_mask_month==1)*31 + (class_data$mandate_mask_month==2)*(31+29) + (class_data$mandate_mask_month==3)*(2*31+29) + (class_data$mandate_mask_month==4)*(2*31+30+29) + (class_data$mandate_mask_month==5)*(3*31+30+29) + (class_data$mandate_mask_month==6)*(3*31+2*30+29) + (class_data$mandate_mask_month==7)*(4*31+2*30+29) + (class_data$mandate_mask_month==8)*(5*31+2*30+29) + (class_data$mandate_mask_month==9)*(3*30+5*31+29)
# Adding cases data
csxmpxc <- merge(csxmp, cases, by = c("CountyFIPS", "DaysThisYear" ), all = TRUE)
# Making cases NA into zeros
csxmpxc$cases[is.na(csxmpxc$cases)] <- 0
# Calculating median consumer spending as a reference point (this number is used in the paper)
mean(aggregate(csxmp$spend_all, list(csxmp$month), median)$x)
# Recoding reference frame of dates from days since 12/31/0000 to days since 12/31/2019
# 737426 is the number of days from 01/01/0001 until 01/01/2020
interventions[,c(4:16)] <- interventions[,c(4:16)] -737426
# Renaming column
interventions$CountyFIPS <- interventions$FIPS
# Adding in NPI dates
csxmpxc <- merge(csxmpxc, interventions, by = "CountyFIPS", all = TRUE)
# Coding the difference in dates between each policy and mask mandates
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



# Fixed effects
library("lfe")
# long time frame
est <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(est)
# month time frame
estmonth <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<32 & csxmpxc$DaysSinceMandate>-32,])
summary(estmonth)
# week time frame
estweek <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<8 & csxmpxc$DaysSinceMandate>-8,])
summary(estweek)

# Event study
csxmpxc$DaysSinceMandateFactor <- as.factor(csxmpxc$DaysSinceMandate)
relevel
warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
csxmpxc$DaysSinceMandateFactor <- relevel(csxmpxc$DaysSinceMandateFactor, ref = "-1")
csxmpxc$CountyFactor <- as.factor(csxmpxc$CountyFIPS)
library("lmtest")
library("sandwich")
lout <- lm(spend_all ~  CountyFIPS + cases + SAHDummy + RestDummy + deaths + GymDummy + Dummy50 + Dummy500 + DummyTravel + DaysSinceMandateFactor + CountyFactor, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(lout)
lout <- as.data.frame(unclass(coeftest(lout,vcov=vcovHC(lout,type="HC0",cluster="CountyFIPS"))))
lout <- lout[1:185,]
lout$day <- as.numeric(c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", -50:-2, 0:125))
lout$ci <- 1.96*lout$`Std. Error`
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(lout[is.na(lout$day)==FALSE,], aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=1, position=pd, colour = "blue") +
  geom_point(size=1) + 
  geom_vline(xintercept = -1) +
  geom_segment(aes(x=-50,xend=-1,y=mean(lout[lout$day<0 & is.na(lout$day)==FALSE,]$Estimate),yend=mean(lout[lout$day<0 & is.na(lout$day)==FALSE,]$Estimate)), color = "red") +
  geom_segment(aes(x=-1,xend=125,y=mean(lout[lout$day>0 & is.na(lout$day)==FALSE,]$Estimate),yend=mean(lout[lout$day>0 & is.na(lout$day)==FALSE,]$Estimate)), color = "red") +
  xlab("Days Since Mandate") +
  ggtitle(c("Mask Mandate and Consumer Spending: Event Study")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

# Event study of other NPIs
lout2 <- lm(DummyAllNPIs ~  CountyFIPS + cases + deaths + DaysSinceMandateFactor + CountyFactor, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(lout2)
lout2 <- as.data.frame(unclass(coeftest(lout2,vcov=vcovHC(lout2,type="HC0",cluster="CountyFIPS"))))
lout2 <- lout2[1:179,]
lout2$day <- as.numeric(c("NA", "NA", "NA", "NA", -50:-2, 0:125))
lout2$ci <- 1.96*lout2$`Std. Error`
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(lout2[is.na(lout2$day)==FALSE,], aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=1, position=pd, colour = "blue") +
  geom_point(size=1) + 
  geom_vline(xintercept = -1) +
  xlab("Days Since Mandate") +
  ggtitle(c("Non-Mask Mandate NPIs and Consumer Spending: Event Study")) +
  theme_classic() +
  theme(plot.title = element_text(size = 13), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

# Event study of covid cases
lout3 <- lm(cases ~  CountyFIPS + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyTravel + DaysSinceMandateFactor + CountyFactor, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(lout3)
lout3 <- as.data.frame(unclass(coeftest(lout3,vcov=vcovHC(lout3,type="HC0",cluster="CountyFIPS"))))
lout3 <- lout3[1:179,]
lout3$day <- as.numeric(c("NA", "NA", "NA", "NA", -50:-2, 0:125))
lout3$ci <- 1.96*lout3$`Std. Error`
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(lout3[is.na(lout3$day)==FALSE,], aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=1, position=pd, colour = "blue") +
  geom_point(size=1) + 
  geom_vline(xintercept = -1) +
  geom_segment(aes(x=-50,xend=-1,y=mean(lout3[lout3$day<0 & is.na(lout3$day)==FALSE,]$Estimate),yend=mean(lout3[lout3$day<0 & is.na(lout3$day)==FALSE,]$Estimate)), color = "red") +
  geom_segment(aes(x=-1,xend=125,y=mean(lout3[lout3$day>0 & is.na(lout3$day)==FALSE,]$Estimate),yend=mean(lout3[lout3$day>0 & is.na(lout3$day)==FALSE,]$Estimate)), color = "red") +
  xlab("Days Since Mandate") +
  ggtitle(c("Mask Mandate and Consumer Spending: Event Study")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

# Robustness Check
felm <- felm(spend_all ~ PostDummy | 0 | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm2 <- felm(spend_all ~ PostDummy | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm3 <- felm(spend_all ~ PostDummy + cases + deaths| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm4 <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
stargazer(felm,felm2,felm3,felm4, float.env = "sidewaystable", omit=c("Constant"), column.labels = c("No Controls", "County Fixed Effects", "Cases and Deaths", "NPIs" ), model.numbers = FALSE)

# Checking the time distribution of mask mandates
library("ggplot2")
library("ggpubr")
library("dplyr")
subset <- csxmpxc[is.na(csxmpxc$EariliestPolicyDaysThisYear)==FALSE,]
subset <- subset[!duplicated(subset[,c("CountyFIPS")]),]
ggplot(subset, aes(x=EariliestPolicyDaysThisYear)) + 
  stat_ecdf(geom = "step") + 
  theme_classic() +
  xlab("Days This Year") +
  ylab("Density") +
  ggtitle("Cumulative Density Fuction of \nMask Mandate Implementation")
ggplot(subset, aes(x=EariliestPolicyDaysThisYear)) + 
  geom_histogram(binwidth = 1) + 
  theme_classic() +
  xlab("Days This Year") +
  ylab("Density") +
  ggtitle("Time Distribution of \nMask Mandate Implementation")


# Exploring Heterogeneity across groups
csxmpxc$ruralurbancentered <- scale(csxmpxc$ruralurban, center=TRUE, scale=FALSE)
csxmpxc$PostDummy_x_ruralurban <- csxmpxc$PostDummy*csxmpxc$ruralurban
fout <- felm(spend_all ~ PostDummy + PostDummy_x_ruralurban + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(fout)
csxmpxc$per_gop <- scale(csxmpxc$per_gop, center=TRUE, scale=FALSE)
csxmpxc$PostDummy_x_per_dem <- csxmpxc$PostDummy*csxmpxc$per_dem
fout2 <- felm(spend_all ~ PostDummy + PostDummy_x_per_dem + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(fout2)
csxmpxc$deaths <- scale(csxmpxc$deaths, center=TRUE, scale=FALSE)
csxmpxc$PostDummy_x_deaths <- csxmpxc$PostDummy*csxmpxc$deaths
fout3 <- felm(spend_all ~ PostDummy + PostDummy_x_deaths + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(fout3)
library("stargazer")
stargazer(fout,fout2,fout3)

# Exploring Heterogeneity across time
avg <- (range(csxmpxc[is.na(csxmpxc$DaysThisYear)==FALSE & is.na(csxmpxc$spend_all)==FALSE,]$DaysThisYear)[1]+range(csxmpxc[is.na(csxmpxc$DaysThisYear)==FALSE & is.na(csxmpxc$spend_all)==FALSE,]$DaysThisYear)[2])/2
fout3 <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS  | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51 & csxmpxc$DaysThisYear<avg,])
summary(fout)
fout4 <- felm(spend_all ~ PostDummy + cases + deaths + SAHDummy + RestDummy + GymDummy + Dummy50 + Dummy500 + DummyFed + DummyTravel| CountyFIPS  | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51 & csxmpxc$DaysThisYear>=avg,])
summary(fout2)
stargazer(fout3,fout4, dep.var.caption = "")

# Reopenings Don't Matter Visualization
# Counties
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

# State Tax Implications
# Cutting off emtpy datacells
state_tax <- state_tax[c(1:51),c(1:9)]
# Renaming column
state_tax$State <- state_tax$X
# Cutting states with no sales tax
state_tax <- state_tax[state_tax$State != "New Hampshire" & state_tax$State != "Oregon" &state_tax$State != "Delaware" &state_tax$State != "Alaska" &state_tax$State != "Montana",]
# Calculating the expected percent change in total state taxes
state_tax$Impact <- ((est$coefficients[1]+1)*(state_tax$Sales/100)*state_tax$Total.Taxes.1+(state_tax$Property/100)*state_tax$Total.Taxes.1+ (state_tax$Selective.Sales./100)*state_tax$Total.Taxes.1 + (state_tax$Individual.Income/100)*state_tax$Total.Taxes.1 + (state_tax$Corporate.Income/100)*state_tax$Total.Taxes.1 + (state_tax$Other/100)*state_tax$Total.Taxes.1)/state_tax$Total.Taxes.1-1
# Finding the absolute gain (adjusted down 8.8% to account for the pandemic)
state_tax$Absolute <- (1-.088)*state_tax$Total.Taxes.1*state_tax$Impact
# Finding the mean expected percent change
mean(state_tax$Impact)
# Finding the mean absolute gain
mean(state_tax$Absolute)
# Loading scales
library(scales)
# Formatting as a percentage
state_tax$Impact <- label_percent(accuracy = .01)(state_tax$Impact)
# Naming the column
colnames(state_tax)[11] <- c("Percentage Change")
# Cutting columns I don't want to export to Latex
state_tax <- state_tax[,c("State", "Percentage Change")]
# Creating Latex code
stargazer(as.matrix(state_tax), notes = c("We calculate the maximum expected percentage increase in tax revenue", "that can be attributed to implementing a mask mandate."), rownames = FALSE)
