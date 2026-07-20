library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(lattice)
library(ggpubr)

# Load the florida datasets

florida_acc <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_acc.txt", quote="\"", comment.char="")
head(florida_acc)

nrow(florida_acc)


florida_coord <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_coord.txt", quote="\"", comment.char="")
head(florida_coord)


# Formatting the data into four columns of rate,month and zone

florida_acc = as.data.frame(florida_acc)

florida_acc_pv = pivot_longer(florida_acc,cols = !V1, names_to = "Zones", values_to = "Rate")

nrow(florida_acc_pv)

# Converting 1,2,3 into month names 

mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")


#add abbreviated month name
florida_acc_pv$MonthAbb <- mymonths[florida_acc_pv$V1]


# adding the year column
year = c(rep(1960,4*51), rep(1961,12*51), rep(1962,12*51), rep(1963,12*51), rep(1964,12*51), rep(1965,12*51), rep(1966,12*51), rep(1967,12*51),
         rep(1968,12*51),rep(1969,12*51), rep(1970,12*51),rep(1971,12*51),rep(1972,12*51),rep(1973,12*51),rep(1974,12*51),rep(1975,12*51), 
         rep(1976,12*51), rep(1977,12*51), rep(1978,12*51), rep(1979,12*51),rep(1980,12*51),rep(1981,12*51), rep(1982,12*51), rep(1983,12*51),
         rep(1984,12*51), rep(1985,12*51),rep(1986,12*51),rep(1987,12*51),rep(1988,12*51),rep(1989,12*51), rep(1990,12*51), rep(1991,12*51),
         rep(1992,12*51), rep(1993,12*51), rep(1994,12*51),rep(1995,12*51), rep(1996,12*51), rep(1997,12*51), rep(1998,12*51),rep(1999,12*51),
         rep(2000,12*51),rep(2001,12*51),rep(2002,12*51),rep(2003,12*51), rep(2004,12*51),rep(2005,12*51),rep(2006,12*51),rep(2007,12*51),
         rep(2008,12*51),rep(2009,12*51),rep(2010,12*51),rep(2011,12*51),rep(2012,12*51),rep(2013,12*51),rep(2014,12*51),rep(2015,12*51),
         rep(2016,7*51))

length(year)

# Merge the dataset/add year column 

florida_acc_pv = cbind(year,florida_acc_pv)


# removing the NA rows 

florida_acc_pv_nna = na.omit(florida_acc_pv)


# now removing the rows where rate is negative if there are any (there aren't)

florida_acc_pv_nna = florida_acc_pv_nna[florida_acc_pv_nna$Rate >= 0,]

# Turning zones into 1,2,3 instead of V2,V3,V4 
nn = as.numeric(substr(florida_acc_pv_nna[,3], start = 2, stop = 4))

florida_acc_pv_nna = cbind(florida_acc_pv_nna, "zones"= nn-1)
View(florida_acc_pv_nna)

# Transforming rate in order to treat is as a normal random variable in order to perform 
# linear regression 

log_rate = log(florida_acc_pv_nna$Rate + 0.001)
length(log_rate)

# Adding the log rate in the data frame 
florida_acc_pv_nna = cbind(florida_acc_pv_nna, log_rate)

head(florida_acc_pv_nna)

# Checking the distribution of lograte now

hist(florida_acc_pv_nna$log_rate, breaks = 100,main = "", probability = TRUE)


# check the response variable 

r = ggplot(florida_acc_pv_nna, aes(Rate)) + geom_density(fill="green", alpha = 0.5)

r_log = ggplot(florida_acc_pv_nna, aes(log_rate)) + geom_density(fill="green", alpha = 0.5, aes(color="Density")) +
  stat_function(fun = dnorm, args = list(mean=2.019902501, sd=0.433001342), lwd=1.2, aes(color="Proposed")) +
  scale_color_manual(name=NULL,
                     breaks=c("Density", "Proposed"),
                     values=c("Density"="green", "Proposed" = "red"))

# plot the distributions of rate before and after transformation side by side 

figure <- ggarrange(r,r_log,
                    labels = c("No transformation", "log(rate + 0.01) transformation"),
                    ncol = 2, nrow = 1)
figure



# Create the response variable y for the linear regression

y = florida_acc_pv_nna$log_rate

# Create the explanatory variables matrix for the linear regression

X1 = florida_acc_pv_nna[,c("year", "zones", "MonthAbb")]

# Create dataframe 

florida_data = data.frame(y,X1)

# EDA

#Find the mean rate per month and plot it 

mean_acc_per_month = aggregate(florida_acc_pv_nna$log_rate, list(florida_acc_pv_nna$MonthAbb), FUN=mean)

# Putting months in the right order 

m = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
r = c(2.012151, 1.947651, 1.926500, 1.891909, 1.917853, 2.018718, 2.056533, 2.096643,
      2.154360, 2.109572, 2.084957, 2.026623)

mean_acc_per_month_1 = data.frame(m,r)

plot(mean_acc_per_month_1[,2], type = "l", xlab = "Month", ylab = "Mean rate", main = "Month vs rate", 
     col = "red", lwd = 2, xaxt = "n")
axis(1, at=1:12, labels=mean_acc_per_month_1[,1])

# Find the average rate per year 

mean_acc_rate_per_year = florida_acc_pv_nna %>% group_by(year) %>% summarise(mean_rate = mean(log_rate))

mean_acc_rate_per_year = as.data.frame(mean_acc_rate_per_year)

# Creating the rate vs year plot 

plot(mean_acc_rate_per_year[,1], mean_acc_rate_per_year[,2], type = "l", xlab = "Year", ylab = "Mean rate", main = "Year vs rate", 
     col = "red", lwd = 2 )

# Find average rate per zone 

mean_acc_rate_per_zone = florida_acc_pv_nna %>% group_by(zones) %>% summarise(mean_rate = mean(log_rate))

mean_acc_rate_per_zone = as.data.frame(mean_acc_rate_per_zone)


plot(mean_acc_rate_per_zone[,1], mean_acc_rate_per_zone[,2], type = "l", xlab = "Zone", ylab = "Mean rate", main = "Zone vs rate", 
     col = "red", lwd = 2 )

# we observe that zone 6 has a log rate of -4.605. This happens because the rate was 0. Let's now
# investigate whether we have adequate data for zone 6. 

zone_6_data = florida_data[florida_data$zones == 6,]
dim(zone_6_data)

# We observe that we only have 14 rows of data, which are only from the year 2000 and just one entry 
# in 2021. As such we decide to remove this zone from our analysis

# look at the data for the other zones

table(florida_data$zones)

# removing zone 6 data 

florida_data = florida_data[florida_data$zones != 6,]

# Function for 10 fold cross validation for just the intercept model (caret package does not support the
#intercept model)

## Set the seed to make the analysis reproducible
set.seed(14)

## 10-fold cross validation
nfolds = 10
n = nrow(florida_data)
## Sample fold-assignment index
fold_index = sample(nfolds, n, replace=TRUE)

## Print first few
head(fold_index)

# creating a function to estimate the average MSE
reg_cv = function(X1, y, fold_ind) {
  Xy = data.frame(X1, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.")
  cv_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    tmp_fit = lm(y ~ ., data=Xy[fold_ind!=fold,])
    yhat = predict(tmp_fit, Xy[fold_ind==fold,])
    yobs = y[fold_ind==fold]
    cv_errors[fold] = mean((yobs - yhat)^2)
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold))
  test_error = weighted.mean(cv_errors, w=fold_sizes)
  return(test_error)
}



# MODEL WITHOUT PREDICTORS
# Fit the simplest linear model, with just the intercept and perform cross validation

reg_cv(1,florida_data[,1],fold_index)

# mse 0.155234 (not ideal)

lsq_fit_0 = lm(y~ 1, data = florida_data)

summary(lsq_fit_0)


# MODEL WITH JUST THE YEAR AS A PREDICTOR 

# K-fold cross-validation
# setting seed to generate a reproducible random sampling
set.seed(125)



# defining training control as cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv",number = 10)

model_year <- train(y ~ year, data = florida_data,
                    method = "lm",
                    trControl = train_control)

# see if the two methods give a very similar MSE
reg_cv(as.numeric(X1[,1]),y,fold_index)
# everything alright

# printing model performance metrics along with other details
print(model_year)

# MSE for the model is 0.11468

lsq_fit_year = lm(y~ year, data = florida_data)
summary(lsq_fit_year)



# MODEL WITH JUST THE ZONE AS A PREDICTOR 

train_control <- trainControl(method = "cv", number = 10)

model_zones <- train(y ~ factor(zones), data = florida_data,  method = "lm",
                     trControl = train_control)

# printing model performance metrics along with other details
print(model_zones)

# MSE for the model is 0.087665

lsq_fit_zones = lm(y~ factor(zones), data = florida_data)
summary(lsq_fit_zones)


# MODEL WITH JUST THE MONTH AS A PREDICTOR 

train_control <- trainControl(method = "cv", number = 10)

model_months <- train(y ~ MonthAbb, data = florida_data, method = "lm",
                      trControl = train_control)

# printing model performance metrics along with other details
print(model_months)

# MSE for the model is 0.114182

lsq_fit_months = lm(y~ MonthAbb, data = florida_data)
summary(lsq_fit_months)



# MODEL WITH JUST THE YEAR AND ZONE AS PREDICTORS 

train_control <- trainControl(method = "cv",number = 10)

model_year_zones <- train(y ~ year + factor(zones), data = florida_data, method = "lm",
                          trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones)

# MSE for the model is 0.0871

lsq_fit_year_zones = lm(y~ year + factor(zones), data = florida_data)
summary(lsq_fit_year_zones)


# MODEL WITH JUST THE YEAR AND MONTH AS PREDICTORS 

train_control <- trainControl(method = "cv",number = 10)

model_year_months <- train(y ~ year + MonthAbb, data = florida_data, method = "lm",
                           trControl = train_control)

# printing model performance metrics along with other details
print(model_year_months)

# MSE for the model is  0.10821

lsq_fit_year_months = lm(y~ year + MonthAbb, data = florida_data)
summary(lsq_fit_year_months)


# MODEL WITH JUST THE ZONE AND MONTH AS PREDICTORS 

train_control <- trainControl(method = "cv",number = 10)

model_months_zones <- train(y ~ factor(zones) + MonthAbb, data = florida_data,method = "lm",
                            trControl = train_control)

# printing model performance metrics along with other details
print(model_months_zones)

# MSE for the model is 0.081385

lsq_fit_months_zones = lm(y~ factor(zones) + MonthAbb, data = florida_data)
summary(lsq_fit_months_zones)



# MODEL WITH ALL THREE PREDICTORS 

model_year_zones_months <- train(y ~ year + factor(zones) + MonthAbb, data = florida_data,
                                 method = "lm", trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones_months)

# MSE for the model is 0.08004 and r squared is 0.55 (highest so far)

lsq_fit_year_zones_months = lm(y ~ year + factor(zones) + MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months)


# Comparing full model with just zone and month model 

anova(lsq_fit_months_zones,lsq_fit_year_zones_months)
anova(lsq_fit_year_months,lsq_fit_year_zones_months)
anova(lsq_fit_year_zones,lsq_fit_year_zones_months)


# form the anova tests it is clear that the more complex model is better 


# MODEL WITH ALL THREE PREDICTORS AND MONTH YEAR INTERACTION


train_control <- trainControl(method = "cv",number = 10)

model_year_zones_months_int1 <- train(y ~ year + factor(zones) + MonthAbb + year*MonthAbb , data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones_months_int1)

# MSE for the model is 0.07957

# We observe that the MSE is lower than the MSE in the full model 

lsq_fit_year_zones_months_int1 = lm(y ~ year + factor(zones) + MonthAbb + year*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int1)



# Compute the analysis of variance
anova(lsq_fit_year_zones_months,lsq_fit_year_zones_months_int1)
anova(lsq_fit_year_zones_months_int1)

# again it seems that the model with the interaction is better 

# Plot some simple diagnostic plots 

par(mfrow = c(1,2))

plot(fitted.values(lsq_fit_year_zones_months_int1),rstandard(lsq_fit_year_zones_months_int1),
     xlab = "Fitted values", ylab ="Standardised residuals",
     cex.lab = 1.4, pch = 21)
abline(h = 0, lty = 2)


qqnorm(rstandard(lsq_fit_year_zones_months_int1), ylab = "Studentised residuals",
       cex.lab = 1.4, pch = 21)
abline(0, 1, lty = 2)

# In general things look fine 

hist(resid(lsq_fit_year_zones_months), breaks = 100)



# MODEL WITH ALL THREE PREDICTORS AND MONTH ZONE INTERACTION

model_year_zones_months_int2 <- train(y ~ year + factor(zones) + MonthAbb + factor(zones)*MonthAbb , data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# In predict.lm(modelFit, newdata) :
# prediction from a rank-deficient fit may be misleading

# printing model performance metrics  along with other details
print(model_year_zones_months_int2)

# MSE for the model is 0.091727
# We observe that the MSE is higher than the MSE in the full model 

lsq_fit_year_zones_months_int2 = lm(y ~ year + factor(zones) + MonthAbb + factor(zones)*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int2)



# MODEL WITH ALL THREE PREDICTORS AND YEAR ZONE INTERACTION


model_year_zones_months_int3 <- train(y ~ year + factor(zones) + MonthAbb + factor(zones)*year , data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# printing model performance metrics
# along with other details
print(model_year_zones_months_int3)

# MSE for the model is 0.07339

# We observe that the MSE is lower than the MSE in the full model 

lsq_fit_year_zones_months_int3 = lm(y ~ year + factor(zones) + MonthAbb + factor(zones)*year, data = florida_data)
summary(lsq_fit_year_zones_months_int3)

# Compute the analysis of variance
anova(lsq_fit_year_zones_months_int3)

# MODEL WITH ZONE/YEAR AND ZONE/MONTH INTERACTIONS

model_year_zones_months_int4 <- train(y ~ year + factor(zones) + MonthAbb + factor(zones)*year + factor(zones)*MonthAbb, data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones_months_int4)

# MSE for the model is 0.089542

lsq_fit_year_zones_months_int4 = lm(y ~ year + factor(zones) + MonthAbb + factor(zones)*year + factor(zones)*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int4)

# MODEL WITH ALL DOUBLE INTERACTIONS 

model_year_zones_months_int5 <- train(y ~ year + factor(zones) + MonthAbb + factor(zones)*year + factor(zones)*MonthAbb + year*MonthAbb , data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones_months_int5)



# MSE for the model is 0.09039

lsq_fit_year_zones_months_int5 = lm(y ~ year + factor(zones) + MonthAbb + factor(zones)*year + factor(zones)*MonthAbb + year*MonthAbb , data = florida_data)
summary(lsq_fit_year_zones_months_int5)


# MODEL WITH ALL THREE PREDICTORS AND ALL INTERACTIONs

model_year_zones_months_int6 <- train(y ~ year + factor(zones) + MonthAbb + factor(zones)*year*MonthAbb , data = florida_data,
                                      method = "lm",
                                      trControl = train_control)

# printing model performance metrics along with other details
print(model_year_zones_months_int6)

# MSE for the model is 0.22305
# We observe that the MSE is higher than the MSE in the full model and in general it is 
# higher when compared to other simpler models

lsq_fit_year_zones_months_int6 = lm(y ~ year + factor(zones) + MonthAbb + factor(zones)*year*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int6)


