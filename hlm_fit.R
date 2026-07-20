# Loading the required libraries 
library(arm)
library(lme4)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lmerTest)
library(cvms)
library(groupdata2)

# Load the florida datasets

florida_acc <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_acc.txt", quote="\"", comment.char="")
head(florida_acc)


# Formatting the data 

florida_acc = as.data.frame(florida_acc)

florida_acc_pv = pivot_longer(florida_acc,cols = !V1, names_to = "Zones", values_to = "Rate")

# Converting 1,2,3 into month names 

mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
#add abbreviated month name
florida_acc_pv$MonthAbb <- mymonths[florida_acc_pv$V1]

# add year 
year = c(rep(1960,4*51), rep(1961,12*51), rep(1962,12*51), rep(1963,12*51), rep(1964,12*51), rep(1965,12*51), rep(1966,12*51), rep(1967,12*51),
         rep(1968,12*51),rep(1969,12*51), rep(1970,12*51),rep(1971,12*51),rep(1972,12*51),rep(1973,12*51),rep(1974,12*51),rep(1975,12*51), 
         rep(1976,12*51), rep(1977,12*51), rep(1978,12*51), rep(1979,12*51),rep(1980,12*51),rep(1981,12*51), rep(1982,12*51), rep(1983,12*51),
         rep(1984,12*51), rep(1985,12*51),rep(1986,12*51),rep(1987,12*51),rep(1988,12*51),rep(1989,12*51), rep(1990,12*51), rep(1991,12*51),
         rep(1992,12*51), rep(1993,12*51), rep(1994,12*51),rep(1995,12*51), rep(1996,12*51), rep(1997,12*51), rep(1998,12*51),rep(1999,12*51),
         rep(2000,12*51),rep(2001,12*51),rep(2002,12*51),rep(2003,12*51), rep(2004,12*51),rep(2005,12*51),rep(2006,12*51),rep(2007,12*51),
         rep(2008,12*51),rep(2009,12*51),rep(2010,12*51),rep(2011,12*51),rep(2012,12*51),rep(2013,12*51),rep(2014,12*51),rep(2015,12*51),
         rep(2016,7*51))


# Merge the dataset 

florida_acc_pv = cbind(year,florida_acc_pv)

# removing the NA rows 

florida_acc_pv_nna = na.omit(florida_acc_pv)

# now removing the rows where rate is negative if there are any (there aren't)

florida_acc_pv_nna = florida_acc_pv_nna[florida_acc_pv_nna$Rate >= 0,]

nn = as.numeric(substr(florida_acc_pv_nna[,3], start = 2, stop = 4))

florida_acc_pv_nna = cbind(florida_acc_pv_nna, "zones"= nn-1)

# Transforming rate in order to treat is as a normal random variable
log_rate = log(florida_acc_pv_nna$Rate + 0.001)

florida_acc_pv_nna = cbind(florida_acc_pv_nna, log_rate)


# Create the response variable 

y = florida_acc_pv_nna$log_rate

# Create the explanatory variables matrix

X1 = florida_acc_pv_nna[,c("year", "zones", "MonthAbb")]

# Create dataframe 

florida_data = data.frame(y,X1)

florida_data$zones = cut(florida_data$zones, 50)


# removing zone 6 data 

florida_data = florida_data[florida_data$zones != 6,]


library(sjstats)

# Now fit the random intercept model with zones 
lme_1 = lmer(y ~ 1 + (1|zones), data=florida_data)

summary(lme_1)
performance::icc(lme_1)
ranova(lme_1)

# Now perform cross validation for the zones model 

florida_data_folded <- fold(florida_data, k = 10) %>%
  arrange(.folds)

model_just_zone <- c("y ~ 1 + (1|zones)")

cross_validate(florida_data_folded,
               models = model_just_zone,
               family='gaussian',
               REML = FALSE)

# MSE is  0.112896 not great 


# Now fit the random intercept model with month 
lme_month = lmer(y ~ 1 + (1|MonthAbb), data=florida_data)

summary(lme_month)
performance::icc(lme_month)
ranova(lme_month)

# Now perform cross validation for the month model 

florida_data_folded <- fold(florida_data, k = 10) %>%
  arrange(.folds)

model_just_month <- c("y ~ 1 + (1|MonthAbb)")

cross_validate(florida_data_folded,
               models = model_just_month,
               family='gaussian',
               REML = FALSE)

# The MSE is 0.138384 so not great 


# Now we can fit the fixed effects  model
florida_data_folded <- fold(florida_data, k = 10)

lme_2 <- lmer(y ~ year + MonthAbb + (1|zones), data=florida_data)

m2 = c("y ~ year + MonthAbb + (1|zones)")

c2 = cross_validate(florida_data_folded, models = m2, family = "gaussian")

c2$RMSE

# Mse is 0.10643

# Check the predictions 
View(as.data.frame(c2$Predictions))

display(lme_2)
summary(lme_2)
ranova(lme_2)

# Fit the same model as above but with the month effect being random 

lme_3 <- lmer(y ~ year + zones + (1|MonthAbb), data=florida_data)

m3 = c("y ~ year + zones + (1|MonthAbb)")

c3 = cross_validate(florida_data_folded, models = m3, family = "gaussian")

c3$RMSE

#MSE is 0.1064296 so not the same as the one of the previous model 

# Check that the predictions are different from the ones of the previous model 
View(as.data.frame(c3$Predictions))


display(lme_3)
summary(lme_3)
ranova(lme_3)
# getting the intraclass correlation coefficient 

performance::icc(lme_2)
performance::icc(lme_3)



