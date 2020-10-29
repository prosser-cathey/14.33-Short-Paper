setwd("~/Desktop/R Lesson")

#Load Data
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
csxmp$PostDummy <- csxmp$DaysSinceMandate>0
csxmp$PostDummy [csxmp$PostDummy == "true"] <- 1
csxmp$PostDummy [csxmp$PostDummy == "false"] <- 0
csxmp$PostDummy <- as.integer(csxmp$PostDummy)
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
# Restricting to columns I care about
class_data <- class_data[,c("ReopenDate","statefips")]
# Getting rid of repeats
class_data <- unique(class_data)
# Adding reopening data 
csxmp <- merge(csxmp, class_data, by = "statefips")
# Adding cases data
csxmpxc <- merge(csxmp, cases, by = c("CountyFIPS", "DaysThisYear" ), all = TRUE)
# Calculating median consumer spending as a reference point (this number is used in the paper)
mean(aggregate(csxmp$spend_all, list(csxmp$month), median)$x)

# 2-way fixed effects
library("lfe")
# long time frame
est <- felm(spend_all ~ PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(est)
# short time frame
estmonth <- felm(spend_all ~ PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<32 & csxmpxc$DaysSinceMandate>-32,])
summary(estmonth)
# super short time frame
estweek <- felm(spend_all ~ PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<8 & csxmpxc$DaysSinceMandate>-8,])
summary(estweek)


# Event-study
csxmpxc$DaysSinceMandateFactor <- as.factor(csxmpxc$DaysSinceMandate)
relevel
warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
csxmpxc$DaysSinceMandateFactor <- relevel(csxmpxc$DaysSinceMandateFactor, ref = "-1")
library("lmtest")
library("sandwich")
lout <- lm(spend_all ~  CountyFIPS + cases + DaysSinceMandateFactor, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
summary(lout)
lout <- as.data.frame(unclass(coeftest(lout,vcov=vcovHC(lout,type="HC0",cluster="CountyFIPS"))))
lout$day <- as.numeric(c("NA", "NA", "NA", -50:-2, 0:125))
lout$ci <- 1.96*lout$`Std. Error`
library("ggplot2")
pd <- position_dodge(0.1)
ggplot(lout[is.na(lout$day)==FALSE,], aes(y=Estimate, x=day)) + 
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=.01, position=pd, colour = "blue") +
  geom_point(size=1) + 
  geom_vline(xintercept = -1) +
  geom_segment(aes(x=-50,xend=-1,y=median(lout[lout$day<0 & is.na(lout$day)==FALSE,]$Estimate),yend=median(lout[lout$day<0 & is.na(lout$day)==FALSE,]$Estimate)), color = "red") +
  geom_segment(aes(x=-1,xend=125,y=median(lout[lout$day>-1 & is.na(lout$day)==FALSE,]$Estimate),yend=median(lout[lout$day>-1 & is.na(lout$day)==FALSE,]$Estimate)), color = "red") +
  xlab("Days Since Mandate") +
  ggtitle(c("Mask Mandate and Consumer \nSpending: Event Study")) +
  theme_classic()

# Robustness Check
felm <- felm(spend_all ~ PostDummy | 0 | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm2 <- felm(spend_all ~ PostDummy | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm3 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
felm4 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code + ruralurban + per_dem| 0 | CountyFIPS, csxmpxc[csxmpxc$DaysSinceMandate<126 & csxmpxc$DaysSinceMandate>-51,])
stargazer(felm,felm2,felm3,felm4, float.env = "sidewaystable")

# Checking Homogeneous Time Effects Assumption
# 2-way fixed effects (same regression as before except restricted to a given month)
test3 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==3,])
test4 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==4,])
test5 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==5,])
test6 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==6,])
test7 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==7,])
test8 <- felm(spend_all ~  PostDummy + cases| CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$month.x==8,])
# Create matrix with results
testsummary <- matrix(c(test3$coefficients[1,1],
                        test3$cse[1], 
                        test3$cpval[1],
                        test4$coefficients[1,1], 
                        test4$cse[1], 
                        test4$cpval[1],
                        test5$coefficients[1,1], 
                        test5$cse[1], 
                        test5$cpval[1],
                        test6$coefficients[1,1], 
                        test6$cse[1], 
                        test6$cpval[1],
                        test7$coefficients[1,1], 
                        test7$cse[1], 
                        test7$cpval[1],
                        test8$coefficients[1,1], 
                        test8$cse[1], 
                        test8$cpval[1]), ncol = 3, byrow = TRUE)
# Export it to Latex code
library("stargazer")
x <- c( "March", "April", "May", "June", "July", "August")
rownames(testsummary) <- (x)
colnames(testsummary) <- c("Estimate", "Standard Error", "Signifigance")
stargazer(testsummary[,c(1:2)], notes = c("We repeat our analysis after subsetting the data by state.", "Some states were excluded due to scarcity of data."))

# Checking the time distribution of mask mandates
library("ggplot2")
library("ggpubr")
library("dplyr")
subset <- csxmpxc[,c("CountyFIPS","EariliestPolicyDaysThisYear")]
subset <- subset[is.na(subset$EariliestPolicyDaysThisYear)==FALSE,]
subset <- unique(subset)
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
  ggtitle("Cumulative Density Fuction of \nMask Mandate Implementation")


# Checking Homogenous Group Effects Assumption
# 2-way fixed effects (same regression as before except restricted to a given state)
# Don't have data for a couple of states - they are excluded
est1 <- felm(spend_all ~ PostDummy + cases | CountyFIPS  | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==1,])
est2 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==2,])
est4 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==4,])
est5 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==5,])
est6 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==6,])
est8 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==8,])
est9 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==9,])
est10 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==10,])
est11 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==11,])
est12 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==12,])
est13 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==13,])
est15 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==15,])
est16 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==16,])
est17 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==17,])
est18 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==18,])
est19 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==19,])
est20 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==20,])
est21 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==21,])
est22 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==22,])
est23 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==23,])
est24 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==24,])
est25 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==25,])
est26 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==26,])
est27 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==27,])
est28 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==28,])
est29 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==29,])
est30 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==30,])
est31 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==31,])
est32 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==32,])
est33 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==33,])
est34 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==34,])
est35 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==35,])
est36 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==36,])
est37 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==37,])
est38 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==38,])
est39 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==39,])
est40 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==40,])
est41 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==41,])
est42 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==42,])
est44 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==44,])
est45 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==45,])
est47 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==47,])
est48 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==48,])
est49 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==49,])
est50 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==50,])
est51 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==51,])
est53 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==53,])
est54 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==54,])
est55 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==55,])
est56 <- felm(spend_all ~ PostDummy + cases | CountyFIPS | 0 | CountyFIPS, csxmpxc[csxmpxc$State_Code==56,])
# Create matrix of results
estsummary <- matrix(c(est1$coefficients[1,1], 
                       est1$cse[1], 
                       est1$cpval[1],
                       est2$coefficients[1,1], 
                       est2$cse[1], 
                       est2$cpval[1],
                       est4$coefficients[1,1], 
                       est4$cse[1], 
                       est4$cpval[1],
                       est5$coefficients[1,1], 
                       est5$cse[1], 
                       est5$cpval[1],
                       est6$coefficients[1,1], 
                       est6$cse[1], 
                       est6$cpval[1],est8$coefficients[1,1], 
                       est8$cse[1], 
                       est8$cpval[1],
                       est9$coefficients[1,1], 
                       est9$cse[1], 
                       est9$cpval[1],
                       est10$coefficients[1,1], 
                       est10$cse[1], 
                       est10$cpval[1],
                       est12$coefficients[1,1], 
                       est12$cse[1], 
                       est12$cpval[1],
                       est13$coefficients[1,1], 
                       est13$cse[1], 
                       est13$cpval[1],
                       est15$coefficients[1,1], 
                       est15$cse[1], 
                       est15$cpval[1],
                       est16$coefficients[1,1], 
                       est16$cse[1], 
                       est16$cpval[1],
                       est17$coefficients[1,1], 
                       est17$cse[1], 
                       est17$cpval[1],
                       est18$coefficients[1,1], 
                       est18$cse[1], 
                       est18$cpval[1],
                       est19$coefficients[1,1], 
                       est19$cse[1], 
                       est19$cpval[1],
                       est20$coefficients[1,1], 
                       est20$cse[1], 
                       est20$cpval[1],
                       est21$coefficients[1,1], 
                       est21$cse[1], 
                       est21$cpval[1],
                       est22$coefficients[1,1], 
                       est22$cse[1], 
                       est22$cpval[1],
                       est23$coefficients[1,1], 
                       est23$cse[1], 
                       est23$cpval[1],
                       est24$coefficients[1,1], 
                       est24$cse[1], 
                       est24$cpval[1],
                       est25$coefficients[1,1], 
                       est25$cse[1], 
                       est25$cpval[1],
                       est26$coefficients[1,1], 
                       est26$cse[1], 
                       est26$cpval[1],
                       est27$coefficients[1,1], 
                       est27$cse[1], 
                       est27$cpval[1],
                       est28$coefficients[1,1], 
                       est28$cse[1], 
                       est28$cpval[1],
                       est29$coefficients[1,1], 
                       est29$cse[1], 
                       est29$cpval[1],
                       est30$coefficients[1,1], 
                       est30$cse[1], 
                       est30$cpval[1],
                       est32$coefficients[1,1], 
                       est32$cse[1], 
                       est32$cpval[1],
                       est34$coefficients[1,1], 
                       est34$cse[1], 
                       est34$cpval[1],
                       est35$coefficients[1,1], 
                       est35$cse[1], 
                       est35$cpval[1],
                       est36$coefficients[1,1], 
                       est36$cse[1], 
                       est36$cpval[1],
                       est37$coefficients[1,1], 
                       est37$cse[1], 
                       est37$cpval[1],
                       est39$coefficients[1,1], 
                       est39$cse[1], 
                       est39$cpval[1],
                       est40$coefficients[1,1], 
                       est40$cse[1], 
                       est40$cpval[1],
                       est41$coefficients[1,1], 
                       est41$cse[1], 
                       est41$cpval[1],
                       est42$coefficients[1,1], 
                       est42$cse[1], 
                       est42$cpval[1],
                       est44$coefficients[1,1], 
                       est44$cse[1], 
                       est44$cpval[1],
                       est45$coefficients[1,1], 
                       est45$cse[1], 
                       est45$cpval[1],
                       est47$coefficients[1,1], 
                       est47$cse[1], 
                       est47$cpval[1],
                       est48$coefficients[1,1], 
                       est48$cse[1], 
                       est48$cpval[1],
                       est49$coefficients[1,1], 
                       est49$cse[1], 
                       est49$cpval[1],
                       est50$coefficients[1,1], 
                       est50$cse[1], 
                       est50$cpval[1],
                       est51$coefficients[1,1], 
                       est51$cse[1], 
                       est51$cpval[1],
                       est53$coefficients[1,1], 
                       est53$cse[1], 
                       est53$cpval[1],
                       est54$coefficients[1,1], 
                       est54$cse[1], 
                       est54$cpval[1],
                       est55$coefficients[1,1], 
                       est55$cse[1], 
                       est55$cpval[1],
                       est56$coefficients[1,1], 
                       est56$cse[1], 
                       est56$cpval[1]), ncol = 3, byrow = TRUE)
# Export to Latex code
library("stargazer")
x <- as.data.frame(cbind(unique(as.data.frame(csxmp$State_Name)),unique(as.data.frame(csxmp$State_Code))))
x$StateName <- x$`csxmp$State_Name`
x$StateCode <- x$`csxmp$State_Code`
x <- x[x$StateCode!=35 & x$StateCode!=30 & x$StateCode!=28 & x$StateCode!=9 & x$StateCode!=46,]
rownames(estsummary) <- (x$StateName)
colnames(estsummary) <- c("Estimate", "Standard Error", "Signifigance")
stargazer(estsummary[,c(1:2)], notes = c("We repeat our analysis after subsetting the data by state.", "Some states were excluded due to scarcity of data."))

# 2-way fixed effects (same regression as before except restricted to a given population code)
estur9 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==9,])
estur8 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==8,])
estur7 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==7,])
estur6 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==6,])
estur5 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==5,])
estur4 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==4,])
estur3 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==3,])
estur2 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==2,])
estur1 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$ruralurban==1,])
# Matrix with results
estursummary <- matrix(c(estur1$coefficients[1,1], 
                         estur1$cse[1], 
                         estur1$cpval[1],
                         estur2$coefficients[1,1], 
                         estur2$cse[1], 
                         estur2$cpval[1],
                         estur3$coefficients[1,1], 
                         estur3$cse[1], 
                         estur3$cpval[1],
                         estur4$coefficients[1,1], 
                         estur4$cse[1], 
                         estur4$cpval[1],
                         estur5$coefficients[1,1], 
                         estur5$cse[1], 
                         estur5$cpval[1],
                         estur6$coefficients[1,1], 
                         estur6$cse[1], 
                         estur6$cpval[1],
                         estur7$coefficients[1,1], 
                         estur7$cse[1], 
                         estur7$cpval[1],
                         estur8$coefficients[1,1], 
                         estur8$cse[1], 
                         estur8$cpval[1],
                         estur9$coefficients[1,1], 
                         estur9$cse[1],
                         estur9$cpval[1]), ncol = 3, byrow = TRUE)
# Exporting to Latex code
rural_urban <- as.data.frame(rural_urban)
rural_urban <- rural_urban[order(rural_urban$RUCC_2013),]
x <- unique(rural_urban$Description)
estursummary <- as.data.frame(estursummary)
colnames(estursummary) <- c("Estimate", "Standard Errors", "Signifigance")
rownames(estursummary) <- as.vector(x)
stargazer(as.matrix(estursummary[,c(1:2)]), notes = c("We repeat our analysis after subsetting the data by state.", "Some states were excluded due to scarcity of data."))


# 2-way fixed effects (same regression as before except restricted to a given county's vote percentage)
estpol1 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$per_dem>=.5,])
estpol2 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$per_dem>=.75,])
estpol3 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$per_gop>=.5,])
estpol4 <- felm(spend_all ~ PostDummy + cases | CountyFIPS + State_Code | 0 | CountyFIPS, csxmpxc[csxmpxc$per_gop>=.75,])
# Matrix of results
estpolsummry <- matrix(c(estpol1$coefficients[1,1], 
                         estpol1$cse[1], 
                         estpol1$cpval[1],
                         estpol2$coefficients[1,1], 
                         estpol2$cse[1], 
                         estpol2$cpval[1],
                         estpol3$coefficients[1,1], 
                         estpol3$cse[1], 
                         estpol3$cpval[1],
                         estpol4$coefficients[1,1], 
                         estpol4$cse[1], 
                         estpol4$cpval[1]), ncol = 3, byrow = TRUE)
# Export to Latex code
x <- c("Majority Clinton", "More than 75% Clinton", "Majority Trump", "More than 75% Trump")
estpolsummry <- as.data.frame(estpolsummry)
colnames(estpolsummry) <- c("Estimate", "Standard Errors", "Signifigance")
rownames(estpolsummry) <- as.vector(x)
stargazer(as.matrix(estpolsummry[,c(1:2)]), notes = c("We repeat our analysis after subsetting the data by 2016 Presidential election votes"))

# Reopenings Don't Matter Visualization
# Counties
# Recoding reference frame of dates from days since 12/31/0000 to days since 12/31/2019
# 737426 is the number of days from 01/01/0001 until 01/01/2020
interventions[,c(4:16)] <- interventions[,c(4:16)] -737426
# Renaming column
interventions$CountyFIPS <- interventions$FIPS
# Adding in mask mandate dates
interventions <- merge(interventions, subset, by = "CountyFIPS")
# Coding the difference in dates between each policy and mask mandates
interventions$DiffSAH_ <- interventions$EariliestPolicyDaysThisYear - interventions$stay.at.home.rollback
interventions$Diff500G_ <- interventions$EariliestPolicyDaysThisYear - interventions$X.500.gatherings.rollback
interventions$Diff50G_ <- interventions$EariliestPolicyDaysThisYear - interventions$X.50.gatherings.rollback
interventions$DiffRest_ <- interventions$EariliestPolicyDaysThisYear - interventions$restaurant.dine.in.rollback
interventions$DiffGym_ <- interventions$EariliestPolicyDaysThisYear - interventions$entertainment.gym.rollback
# Adding an index so that ggplot works
interventions$index <- c(1:1245)
# Making Graphs
ggplot(interventions, aes(y=DiffSAH_,x=index)) + 
  geom_point() +
  theme_classic() +
  xlab("County") +
  ylab("Difference Between Stay at Home Order End and \nMask Mandate Implementation (Days)") +
  ggtitle("Stay at Home Order End and \nMask Mandate Implementation") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(interventions, aes(y=Diff500G_,x=index)) + 
  geom_point() +
  theme_classic() +
  xlab("County") +
  ylab("Difference Between 500 Person Gathering Ban \nEnd and Mask Mandate Implementation (Days)") +
  ggtitle("500 Person Gathering Ban End and \nMask Mandate Implementation") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(interventions, aes(y=Diff50G_,x=index)) + 
  geom_point() +
  theme_classic() +
  xlab("County") +
  ylab("Difference Between 50 Person Gathering Ban \nEnd and Mask Mandate Implementation (Days)") +
  ggtitle("50 Person Gathering Ban End and \nMask Mandate Implementation") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(interventions, aes(y=DiffRest_,x=index)) + 
  geom_point() +
  theme_classic() +
  xlab("County") +
  ylab("Difference Between Restaurant Reopening and \nMask Mandate Implementation (Days)") +
  ggtitle("Restaurant Reopening and \nMask Mandate Implementation") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplot(interventions, aes(y=DiffGym_,x=index)) + 
  geom_point() +
  theme_classic() +
  xlab("County") +
  ylab("Difference Between Gym & Enterntainment \nReopening and Mask Mandate Implementation (Days)") +
  ggtitle("Gym & Enterntainment Reopening \nand Mask Mandate Implementation") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# States (not using)
# Getting each states Reopen Date
class_data_medians <- aggregate(class_data$ReopenDate, list(class_data$state), median)
# Combing mask policy date and reopening date into one dataset
class_data_medians <- cbind(class_data_medians, aggregate(csxmp$EariliestPolicyDaysThisYear, list(csxmp$State), median))
# Loading ggplot
library(ggplot2)
# Naming columns
colnames(class_data_medians) <- c("State_Code", "ReopenDaysThisYear", "StateAB", "PolicyDaysThisYear")
# Creating Difference column
class_data_medians$Diff <- class_data_medians$PolicyDaysThisYear - class_data_medians$ReopenDaysThisYear
# Graphing the difference column - almost none are close to 0
ggplot(class_data_medians, aes(x=State_Code, y=Diff)) + geom_point() + xlab("State") + ylab("Difference") + ggtitle("Difference Between Re-opening Date \n and Mask Mandate Date by State")

# State Tax Implications
# Cutting off emtpy datacells
state_tax <- state_tax[c(1:51),c(1:9)]
# Renaming column
state_tax$State <- state_tax$X
# Cutting states with no sales tax
state_tax <- state_tax[state_tax$State != "New Hampshire" & state_tax$State != "Oregon" &state_tax$State != "Delaware" &state_tax$State != "Alaska" &state_tax$State != "Montana",]
# Calculating the expected percent change in total state taxes
state_tax$Impact <- ((est$coefficients[1]+1)*(state_tax$Sales/100)*state_tax$Total.Taxes.1+(state_tax$Property/100)*state_tax$Total.Taxes.1+ (state_tax$Selective.Sales./100)*state_tax$Total.Taxes.1 + (state_tax$Individual.Income/100)*state_tax$Total.Taxes.1 + (state_tax$Corporate.Income/100)*state_tax$Total.Taxes.1 + (state_tax$Other/100)*state_tax$Total.Taxes.1)/state_tax$Total.Taxes.1-1
# Finding the mean expected percent change
mean(state_tax$Impact)
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

# Other Stuff I tried but didn't include in the paper and don't want to delete in case it's useful later:
# Didn't think I needed to comment this because it's not part of the paper. 

# Subset by sector
# merge id & spending data
cities <- merge(cities,cityid, by ="cityid")
## Download the file
temp <- tempfile()
download.file("http://download.geonames.org/export/zip/US.zip",temp)
con <- unz(temp, "US.txt")
US <- read.delim(con, header=FALSE)
unlink(temp)
## Find state and county
colnames(US)[c(3,5,6)] <- c("city","state","county")
US$city <- tolower(US$city)
myCityNames <- tolower(cityid$cityname)
myCities <- US[US$city %in% myCityNames, ]
myCities <- myCities[c("city","state","county")]
myCities <- myCities[!duplicated(myCities),]
myCities <- myCities[order(myCities$city, myCities$state, decreasing = TRUE), ]
myPlaces <- data.frame(city = myCityNames, state = cityid$stateabbrev)
cities$city <- cities$cityname
library("stringr")
library("usmap")
thing <- merge(myCities, myPlaces, by = c("city", "state"))
thing$city <- str_to_title(thing$city)
city <- merge(cities,thing, by="city")
city$CountyFIPS <- fips(city$stateabbrev, county = city$county)

# Diff-in-diff did
library(did)
library(ggplot2)
csxmp$TreatedDummy <- as.integer(is.na(csxmp$EariliestPolicyDaysThisYear))
csxmp$TreatedDummy <- as.integer(csxmp$TreatedDummy==0)
csxmp$EariliestPolicyDaysThisYear[is.na(csxmp$EariliestPolicyDaysThisYear)==TRUE] <- 0
csxmp <- csxmp[,c("CountyFIPS", "EariliestPolicyDaysThisYear", "DaysThisYear", "State_Code", "TreatedDummy", "spend_all")]
csxmp <- csxmp[order(DaysThisYear),]
y <- data.frame(unclass(rle(as.integer(csxmp$CountyFIPS))))
max(y$lengths)
csxmp$TreatedDummy <- as.numeric(csxmp$TreatedDummy)
csxmp <- csxmp[is.na(csxmp$spend_all)==FALSE,]
out <- att_gt(yname="spend_all",
              tname="DaysThisYear",
              idname="CountyFIPS",
              first.treat.name="EariliestPolicyDaysThisYear",
              data=csxmp[csxmp$State_Code==1,],
              xformla=NULL,
              control.group = "notyettreated",
              printdetails=TRUE,
              clustervars = c("State_Code"))
summary(out$aggte, type="dynamic")
library(gridExtra)
library(ggplot2)
ggdid(out)
summary(out)

out1 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               first.treat.name="first.treat",
               xformla=~lpop,
               data=mpdta,
               printdetails=FALSE)
summary(out1)

# Diff-in-diff ES
library(eventStudy)
library(data.table)
library(ggplot2)
csxmp_1 <- csxmp_1[,c("CountyFIPS","spend_all","EariliestPolicyDaysThisYear","DaysThisYear")]
csxmp <- as.data.table(csxmp)
csxmpxc$EariliestPolicyDaysThisYear <- as.integer(csxmpxc$EariliestPolicyDaysThisYear)
csxmpxc$DaysThisYear <- as.integer(csxmpxc$DaysThisYear)

results <- ES(long_data=as.data.table(csxmp_1), outcomevar="spend_all", 
              unit_var="CountyFIPS", cal_time_var="DaysThisYear", 
              onset_time_var="EariliestPolicyDaysThisYear", cluster_vars="CountyFIPS", never_treat_action="none")
summary(results)
ES_plot_ATTs(results)


# Week aggregation
csxmp$week <- (csxmp$DaysThisYear>0 & csxmp$DaysThisYear<8)*1 +(csxmp$DaysThisYear>7 & csxmp$DaysThisYear<15)*2 +(csxmp$DaysThisYear>14 & csxmp$DaysThisYear<22)*3 +(csxmp$DaysThisYear>21 & csxmp$DaysThisYear<29)*4 +(csxmp$DaysThisYear>28 & csxmp$DaysThisYear<36)*5 +(csxmp$DaysThisYear>35 & csxmp$DaysThisYear<43)*6 +(csxmp$DaysThisYear>42 & csxmp$DaysThisYear<50)*7 +(csxmp$DaysThisYear>49 & csxmp$DaysThisYear<57)*8 +(csxmp$DaysThisYear>56 & csxmp$DaysThisYear<64)*9 +(csxmp$DaysThisYear>63 & csxmp$DaysThisYear<71)*10 +(csxmp$DaysThisYear>70 & csxmp$DaysThisYear<78)*11 +(csxmp$DaysThisYear>77 & csxmp$DaysThisYear<85)*12 +(csxmp$DaysThisYear>84 & csxmp$DaysThisYear<92)*13 +(csxmp$DaysThisYear>91 & csxmp$DaysThisYear<99)*14 +(csxmp$DaysThisYear>98 & csxmp$DaysThisYear<106)*15 +(csxmp$DaysThisYear>105 & csxmp$DaysThisYear<113)*16 +(csxmp$DaysThisYear>112 & csxmp$DaysThisYear<120)*17 +(csxmp$DaysThisYear>119 & csxmp$DaysThisYear<127)*18 +(csxmp$DaysThisYear>126 & csxmp$DaysThisYear<134)*19 +(csxmp$DaysThisYear>133 & csxmp$DaysThisYear<141)*20 +(csxmp$DaysThisYear>140 & csxmp$DaysThisYear<148)*21 +(csxmp$DaysThisYear>147 & csxmp$DaysThisYear<155)*22 +(csxmp$DaysThisYear>154 & csxmp$DaysThisYear<162)*23 +(csxmp$DaysThisYear>161 & csxmp$DaysThisYear<169)*24 +(csxmp$DaysThisYear>168 & csxmp$DaysThisYear<176)*25 +(csxmp$DaysThisYear>175 & csxmp$DaysThisYear<183)*26 +(csxmp$DaysThisYear>182 & csxmp$DaysThisYear<190)*27 +(csxmp$DaysThisYear>189 & csxmp$DaysThisYear<197)*28 +(csxmp$DaysThisYear>196 & csxmp$DaysThisYear<204)*29 +(csxmp$DaysThisYear>203 & csxmp$DaysThisYear<211)*30 +(csxmp$DaysThisYear>210 & csxmp$DaysThisYear<218)*31 +(csxmp$DaysThisYear>217 & csxmp$DaysThisYear<225)*32 +(csxmp$DaysThisYear>224 & csxmp$DaysThisYear<232)*33 +(csxmp$DaysThisYear>231 & csxmp$DaysThisYear<239)*34+ +(csxmp$DaysThisYear>238 & csxmp$DaysThisYear<246)*35 +(csxmp$DaysThisYear>245 & csxmp$DaysThisYear<253)*36 +(csxmp$DaysThisYear>252 & csxmp$DaysThisYear<260)*37 +(csxmp$DaysThisYear>259 & csxmp$DaysThisYear<267)*38 +(csxmp$DaysThisYear>266 & csxmp$DaysThisYear<274)*39 +(csxmp$DaysThisYear>273 & csxmp$DaysThisYear<281)*40
csxmp$MandateWeek <- (csxmp$EariliestPolicyDaysThisYear>0 & csxmp$EariliestPolicyDaysThisYear<8)*1 +(csxmp$EariliestPolicyDaysThisYear>7 & csxmp$EariliestPolicyDaysThisYear<15)*2 +(csxmp$EariliestPolicyDaysThisYear>14 & csxmp$EariliestPolicyDaysThisYear<22)*3 +(csxmp$EariliestPolicyDaysThisYear>21 & csxmp$EariliestPolicyDaysThisYear<29)*4 +(csxmp$EariliestPolicyDaysThisYear>28 & csxmp$EariliestPolicyDaysThisYear<36)*5 +(csxmp$EariliestPolicyDaysThisYear>35 & csxmp$EariliestPolicyDaysThisYear<43)*6 +(csxmp$EariliestPolicyDaysThisYear>42 & csxmp$EariliestPolicyDaysThisYear<50)*7 +(csxmp$EariliestPolicyDaysThisYear>49 & csxmp$EariliestPolicyDaysThisYear<57)*8 +(csxmp$EariliestPolicyDaysThisYear>56 & csxmp$EariliestPolicyDaysThisYear<64)*9 +(csxmp$EariliestPolicyDaysThisYear>63 & csxmp$EariliestPolicyDaysThisYear<71)*10 +(csxmp$EariliestPolicyDaysThisYear>70 & csxmp$EariliestPolicyDaysThisYear<78)*11 +(csxmp$EariliestPolicyDaysThisYear>77 & csxmp$EariliestPolicyDaysThisYear<85)*12 +(csxmp$EariliestPolicyDaysThisYear>84 & csxmp$EariliestPolicyDaysThisYear<92)*13 +(csxmp$EariliestPolicyDaysThisYear>91 & csxmp$EariliestPolicyDaysThisYear<99)*14 +(csxmp$EariliestPolicyDaysThisYear>98 & csxmp$EariliestPolicyDaysThisYear<106)*15 +(csxmp$EariliestPolicyDaysThisYear>105 & csxmp$EariliestPolicyDaysThisYear<113)*16 +(csxmp$EariliestPolicyDaysThisYear>112 & csxmp$EariliestPolicyDaysThisYear<120)*17 +(csxmp$EariliestPolicyDaysThisYear>119 & csxmp$EariliestPolicyDaysThisYear<127)*18 +(csxmp$EariliestPolicyDaysThisYear>126 & csxmp$EariliestPolicyDaysThisYear<134)*19 +(csxmp$EariliestPolicyDaysThisYear>133 & csxmp$EariliestPolicyDaysThisYear<141)*20 +(csxmp$EariliestPolicyDaysThisYear>140 & csxmp$EariliestPolicyDaysThisYear<148)*21 +(csxmp$EariliestPolicyDaysThisYear>147 & csxmp$EariliestPolicyDaysThisYear<155)*22 +(csxmp$EariliestPolicyDaysThisYear>154 & csxmp$EariliestPolicyDaysThisYear<162)*23 +(csxmp$EariliestPolicyDaysThisYear>161 & csxmp$EariliestPolicyDaysThisYear<169)*24 +(csxmp$EariliestPolicyDaysThisYear>168 & csxmp$EariliestPolicyDaysThisYear<176)*25 +(csxmp$EariliestPolicyDaysThisYear>175 & csxmp$EariliestPolicyDaysThisYear<183)*26 +(csxmp$EariliestPolicyDaysThisYear>182 & csxmp$EariliestPolicyDaysThisYear<190)*27 +(csxmp$EariliestPolicyDaysThisYear>189 & csxmp$EariliestPolicyDaysThisYear<197)*28 +(csxmp$EariliestPolicyDaysThisYear>196 & csxmp$EariliestPolicyDaysThisYear<204)*29 +(csxmp$EariliestPolicyDaysThisYear>203 & csxmp$EariliestPolicyDaysThisYear<211)*30 +(csxmp$EariliestPolicyDaysThisYear>210 & csxmp$EariliestPolicyDaysThisYear<218)*31 +(csxmp$EariliestPolicyDaysThisYear>217 & csxmp$EariliestPolicyDaysThisYear<225)*32 +(csxmp$EariliestPolicyDaysThisYear>224 & csxmp$EariliestPolicyDaysThisYear<232)*33 +(csxmp$EariliestPolicyDaysThisYear>231 & csxmp$EariliestPolicyDaysThisYear<239)*34+ +(csxmp$EariliestPolicyDaysThisYear>238 & csxmp$EariliestPolicyDaysThisYear<246)*35 +(csxmp$EariliestPolicyDaysThisYear>245 & csxmp$EariliestPolicyDaysThisYear<253)*36 +(csxmp$EariliestPolicyDaysThisYear>252 & csxmp$EariliestPolicyDaysThisYear<260)*37 +(csxmp$EariliestPolicyDaysThisYear>259 & csxmp$EariliestPolicyDaysThisYear<267)*38 +(csxmp$EariliestPolicyDaysThisYear>266 & csxmp$EariliestPolicyDaysThisYear<274)*39 +(csxmp$EariliestPolicyDaysThisYear>273 & csxmp$EariliestPolicyDaysThisYear<281)*40
csxmp_week <- csxmp[,c("spend_all", "CountyFIPS", "MandateWeek", "week")]
csxmp_week$Diff <- csxmp_week$week - csxmp_week$MandateWeek
csxmp_week <- subset(csxmp_week, Diff>-6)
csxmp_week <- as.data.table(csxmp_week)
csxmp_week$MandateWeek <- as.integer(csxmp_week$MandateWeek)
csxmp_week$week <- as.integer(csxmp_week$week)
results <- ES(long_data=csxmp_week, outcomevar="spend_all", 
              unit_var="CountyFIPS", cal_time_var="week", 
              onset_time_var="MandateWeek", cluster_vars="CountyFIPS", never_treat_action="none")
csxmp_week$DiffDummy <- as.integer(csxmp_week$Diff>=0)
lreg <- lm(spend_all ~ DiffDummy, data=csxmp_week[csxmp_week$Diff>-2&csxmp_week$Diff<3])
summary(lreg)

# Not sure what I'm doing here
class_data$ReopenDate <- class_data$begin_reopen_day + (class_data$begin_reopen_month==1)*31 + (class_data$begin_reopen_month==2)*(31+29) + (class_data$begin_reopen_month==3)*(2*31+29) + (class_data$begin_reopen_month==4)*(2*31+30+29) + (class_data$begin_reopen_month==5)*(3*31+30+29) + (class_data$begin_reopen_month==6)*(3*31+2*30+29) + (class_data$begin_reopen_month==7)*(4*31+2*30+29) + (class_data$begin_reopen_month==8)*(5*31+2*30+29) + (class_data$begin_reopen_month==9)*(3*30+5*31+29)
class_data$StateMaskMandateDate <- class_data$mandate_mask_day + (class_data$mandate_mask_month==1)*31 + (class_data$mandate_mask_month==2)*(31+29) + (class_data$mandate_mask_month==3)*(2*31+29) + (class_data$mandate_mask_month==4)*(2*31+30+29) + (class_data$mandate_mask_month==5)*(3*31+30+29) + (class_data$mandate_mask_month==6)*(3*31+2*30+29) + (class_data$mandate_mask_month==7)*(4*31+2*30+29) + (class_data$mandate_mask_month==8)*(5*31+2*30+29) + (class_data$mandate_mask_month==9)*(3*30+5*31+29)
class_data$DateDaysThisYear <- class_data$date_day + (class_data$date_month==1)*31 + (class_data$date_month==2)*(31+29) + (class_data$date_month==3)*(2*31+29) + (class_data$date_month==4)*(2*31+30+29) + (class_data$date_month==5)*(3*31+30+29) + (class_data$date_month==6)*(3*31+2*30+29) + (class_data$date_month==7)*(4*31+2*30+29) + (class_data$date_month==8)*(5*31+2*30+29) + (class_data$date_month==9)*(3*30+5*31+29)
class_data$DaysSinceMandate <- class_data$DateDaysThisYear - class_data$StateMaskMandateDate
class_data$DaysSinceMandateDummy <- class_data$DaysSinceMandate>0
class_data$DaysSinceMandateDummy [class_data$DaysSinceMandateDummy == "true"] <- 1
class_data$DaysSinceMandateDummy [class_data$DaysSinceMandateDummy == "false"] <- 0
class_data$DaysSinceMandateDummy <- as.integer(class_data$DaysSinceMandateDummy)
lreg2 <- lm(spend_all ~ DaysSinceMandateDummy, data=class_data[class_data$DaysSinceMandate<40 & class_data$DaysSinceMandate>-100,])
summary(lreg2)

# Doesn't work at state level visualization:
class_data_medians <- aggregate(class_data$spend_all, list(class_data$DaysSinceMandate), median)
class_data_medians$spend_all_median <- class_data_medians$x
class_data_medians$DaysSinceMandate <- class_data_medians$Group.1
ggplot(class_data_medians[class_data_medians$DaysSinceMandate<40 & class_data_medians$DaysSinceMandate>-40,], aes(x=DaysSinceMandate, y=spend_all_median)) + geom_point()

# County-level Data Analysis
csxmp_medians <- aggregate(csxmp$spend_all, list(csxmp$DaysSinceMandate), median)
csxmp_medians$spend_all_median <- csxmp_medians$x
csxmp_medians$DaysSinceMandate <- csxmp_medians$Group.1
ggplot(csxmp_medians[csxmp_medians$DaysSinceMandate<125 & csxmp_medians$DaysSinceMandate>-50,], aes(x=DaysSinceMandate, y=spend_all_median)) + geom_point()
lreg <- lm(spend_all ~ DaysSinceMandateDummy + DaysSinceMandate + DaysSinceMandate*DaysSinceMandateDummy, data=csxmp[csxmp$DaysSinceMandate<125 & csxmp$DaysSinceMandate>-50,])
summary(lreg)

# Medians Visualization
csxmp_medians <- aggregate(csxmp$spend_all, list(csxmp$DaysSinceMandate), median)
csxmp_medians$spend_all_median <- csxmp_medians$x
csxmp_medians$DaysSinceMandate <- csxmp_medians$Group.1
ggplot(csxmp_medians[csxmp_medians$DaysSinceMandate<125 & csxmp_medians$DaysSinceMandate>-50,], aes(x=DaysSinceMandate, y=spend_all_median)) + geom_point()

# Checking for reopening guidelines happening at same time as mandates - state level
mandate_medians <- aggregate(class_data$StateMaskMandateDate, list(class_data$statefips), median)
mandate_medians$EariliestPolicyDaysThisYear <- mandate_medians$x
mandate_medians$State_Code <- mandate_medians$Group.1
reopen_medians <- aggregate(class_data$ReopenDate, list(class_data$statefips), median)
reopen_medians$ReopenDate <- reopen_medians$x
reopen_medians$State_Code <- reopen_medians$Group.1
mandate_x_reopen <- merge(mandate_medians, reopen_medians, by="State_Code")
mandate_x_reopen$diff <- mandate_x_reopen$EariliestPolicyDaysThisYear - mandate_x_reopen$ReopenDate
ggplot(mandate_x_reopen, aes(x=mandate_x_reopen$diff, y=State_Code)) + geom_point()
mandate_x_reopen$diff
mandate_x_reopen[mandate_x_reopen$diff==0,]
