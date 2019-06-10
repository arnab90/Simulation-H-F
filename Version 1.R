library(ggplot2)
library(data.table)
library(arm)
library(ISLR)

#generating age period and cohort
n <- 10000

#instead of taking year as 1973 to 2018, coded as 1 to 46
year <- as.integer(runif(n,min = 1,max=46))

#age taken between 25 and 90, but to mimic the GSS age variable is systematically dependent on the year, i.e. mean age increases with year.
age <- as.integer(runif(n,min = 25,max=90))


#a -ve cohort means simply that the cohort year is before the survey year which is natural.
cohort <- year-age

year_effect <- 1.5
age_effect <-  -2

#the word primary signals coefficients that effect mediators.
secularization_primary <- 0.00003
culture_shock_primary <- 0.0001
autonomy_primary <- 0.00005

#Generating mediators according to DAG
m_secularization <- 10 + secularization_primary*cohort + rnorm(n,0,10)
m_culture_shock <- 80 + culture_shock_primary*cohort + rnorm(n,0,10)
m_autonomy <- 100 + autonomy_primary*cohort - rnorm(n,0,20)
#for autonomy the stochastic component is kept negative

#secondary coefficients that create the effect of mediators on outcome.
secularization_secondary <- 0.2
culture_shock_secondary <- 1
autonomy_secondary <- 0.5

#the relig variable is a continous outcome in the simulation and not a binary one
relig <- 10 + year_effect*year + age_effect*age + secularization_secondary*m_secularization + culture_shock_secondary*m_culture_shock + autonomy_secondary*m_autonomy + rnorm(n,0,5)

#creating data table with all necessary variables
sim1 <- data.table(cbind(relig,year,age,cohort,m_secularization,m_culture_shock,m_autonomy))


##generating plots by year, cohort and age

sim1[,"temp_mean" := mean(relig),by="year"]
ggplot(data =sim1, aes(x=year,y=temp_mean))+ xlab("Year")+ ylab("Religious None")+geom_point()+geom_smooth(method = "lm")

sim1[,"temp_mean" := mean(relig),by="cohort"]
ggplot(data =sim1, aes(x=cohort,y=temp_mean))+geom_point() +  xlab("Cohort")+ ylab("Religious None")+geom_smooth(method = "lm")

sim1[,"temp_mean" := mean(relig),by="age"]
ggplot(data =sim1, aes(x=age,y=temp_mean))+geom_point()+geom_smooth(method = "lm") +xlab("Age")+ ylab("Religious None")

##generating plots to show relationship between year and the other two variables

sim1[,"temp_mean" := mean(age),by="year"]
ggplot(data =sim1, aes(x=year,y=temp_mean))+geom_point()+geom_smooth(method = "lm") +xlab("Year")+ ylab("Age")

sim1[,"temp_mean" := mean(cohort),by="year"]
ggplot(data =sim1, aes(x=year,y=temp_mean))+geom_point()+geom_smooth(method = "lm") +xlab("Year")+ ylab("Cohort")

##generating mean plots for indices without loess.

sim1[,"secularization" := mean(m_secularization),by="cohort"]
sim1[,"culture_shock" := mean(m_culture_shock),by="cohort"]
sim1[,"autonomy" := mean(m_autonomy),by="cohort"]


plot(sim1$cohort,sim1$secularization, col="grey", xlim = c(-90,30), ylim = c(-30,120), xlab = "Cohort", ylab = "Cohort Indices - H&F Color Scheme") 
lines(sim1$cohort, sim1$culture_shock, col= "green",type = "p")
lines(sim1$cohort, sim1$autonomy, col= "red",type = "p")

##Phase II - Sheaf Experiments

#generating ysheaf

sheaf_making <- sim1[,mean(relig), by="year"]
sheaf_loess <- loess(V1~year, span=0.4, data = sheaf_making)

sheaf_making[, "ysheaf" := predict(sheaf_loess, newdata = sheaf_making)]
ggplot(data = sheaf_making, aes(year,ysheaf))+geom_point()+xlab("Year")+ ylab("Ysheaf")

sim1 <- merge(sim1, sheaf_making, by="year", all.x=TRUE)
#Linear Modelling

## Creating 5 Year Cohort Factor Variable ##
sim1[cohort%in% -88:-84, "cohort_fiveyear" :=1]
sim1[cohort%in% -83:-79, "cohort_fiveyear" :=2]
sim1[cohort%in% -78:-74, "cohort_fiveyear" :=3]
sim1[cohort%in% -73:-69, "cohort_fiveyear" :=4]
sim1[cohort%in% -68:-64, "cohort_fiveyear" :=5]
sim1[cohort%in% -63:-59, "cohort_fiveyear" :=6]
sim1[cohort%in% -58:-54, "cohort_fiveyear" :=7]
sim1[cohort%in% -53:-49, "cohort_fiveyear" :=8]
sim1[cohort%in% -48:-44, "cohort_fiveyear" :=9]
sim1[cohort%in% -43:-39, "cohort_fiveyear" :=10]
sim1[cohort%in% -38:-34, "cohort_fiveyear" :=11]
sim1[cohort%in% -33:-29, "cohort_fiveyear" :=12]
sim1[cohort%in% -28:-24, "cohort_fiveyear" :=13]
sim1[cohort%in% -23:-19, "cohort_fiveyear" :=14]
sim1[cohort%in% -18:-14, "cohort_fiveyear" :=15]
sim1[cohort%in% -13:-9, "cohort_fiveyear" :=16]
sim1[cohort%in% -8:-4, "cohort_fiveyear" :=17]
sim1[cohort%in% -3:1, "cohort_fiveyear" :=18]
sim1[cohort%in% 2:6, "cohort_fiveyear" :=19]
sim1[cohort%in% 7:11, "cohort_fiveyear" :=20]
sim1[cohort%in% 12:16, "cohort_fiveyear" :=21]
sim1[cohort%in% 17:20, "cohort_fiveyear" :=22]

sim1[, "cohort_fiveyear":=as.factor(cohort_fiveyear)]
relevel(sim1$cohort_fiveyear,ref = "11")

## Creating 5 year Age factor variable ##
sim1[age%in% 20:24, "age_fiveyear" :=1]
sim1[age%in% 25:29, "age_fiveyear" :=2]
sim1[age%in% 30:34, "age_fiveyear" :=3]
sim1[age%in% 35:39, "age_fiveyear" :=4]
sim1[age%in% 40:44, "age_fiveyear" :=5]
sim1[age%in% 45:49, "age_fiveyear" :=6]
sim1[age%in% 50:54, "age_fiveyear" :=7]
sim1[age%in% 55:59, "age_fiveyear" :=8]
sim1[age%in% 60:64, "age_fiveyear" :=9]
sim1[age%in% 65:69, "age_fiveyear" :=10]
sim1[age%in% 70:74, "age_fiveyear" :=11]
sim1[age%in% 75:79, "age_fiveyear" :=12]
sim1[age%in% 80:84, "age_fiveyear" :=13]
sim1[age%in% 85:90, "age_fiveyear" :=14]
sim1[, "age_fiveyear":=as.factor(age_fiveyear)]
relevel(sim1$age_fiveyear,ref = "9")

#null model with ysheaf
mod_null <-  lm(relig~ysheaf, data = sim1)
mod_null

#Baseline model with cohort
mod_baseline_cohort <- lm(relig~ysheaf+cohort_fiveyear, data = sim1)
mod_baseline_cohort

#Baseline model with age
mod_baseline_age <- lm(relig~ysheaf+age_fiveyear, data = sim1)
mod_baseline_age

#Baseline model with cohort age
mod_baseline_cohort_age <- lm(relig~ysheaf+cohort_fiveyear+age_fiveyear, data = sim1)
mod_baseline_cohort_age


##Multilevel Models - sheaf replaced by year random effects

#null model
mod_null_random <- lmer(relig~(1|year),data = sim1)
mod_null_random

#baseline model
mod_baseline_cohort_random <- lmer(relig~cohort_fiveyear+(1 | year),data = sim1)
mod_baseline_cohort_random

#Baseline model with age
mod_baseline_age_random <- lmer(relig~age_fiveyear+(1|year), data = sim1)
mod_baseline_age_random

#Baseline model with cohort age
mod_baseline_cohort_age_random <- lmer(relig~cohort_fiveyear+age_fiveyear+ (1|year), data = sim1)
mod_baseline_cohort_age_random





