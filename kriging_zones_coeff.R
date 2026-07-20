# Load the required libraries 
library(rgdal)
library(sp)
library(sf)
library(spdep)
library(readr)
library(gstat)
library(raster)
library(dplyr)
library(readr)
library(tidyr)

# Load the florida datasets

florida_acc <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_acc.txt", quote="\"", comment.char="")


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

length(year)

# Merge the dataset 

florida_acc_pv = cbind(year,florida_acc_pv)

# removing the NA rows 

florida_acc_pv_nna = na.omit(florida_acc_pv)


nn = as.numeric(substr(florida_acc_pv_nna[,3], start = 2, stop = 4))

florida_acc_pv_nna = cbind(florida_acc_pv_nna, "zones"= nn-1)

# Transforming rate in order to treat is as a normal random variable in order to perform 
# liner regression 

log_rate = log(florida_acc_pv_nna$Rate + 0.001)
length(log_rate)


florida_acc_pv_nna = cbind(florida_acc_pv_nna, log_rate)

florida_acc_pv_nna_ = florida_acc_pv_nna[florida_acc_pv_nna$log_rate < 0,]


# Create the response variable 

y = florida_acc_pv_nna$log_rate

# Create the explanatory variables matrix

X1 = florida_acc_pv_nna[,c("year", "zones", "MonthAbb")]

# Create dataframe 

florida_data = data.frame(y,X1)


# removing zone 6 data 

florida_data = florida_data[florida_data$zones != 6,]


# First we need the coefficients of the zones for our selected model

# MODEL WITH ALL THREE PREDICTORS AND MONTH YEAR INTERACTION


lsq_fit_year_zones_months_int1 = lm(y ~ year + factor(zones) + MonthAbb + year*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int1)

coef = as.data.frame(lsq_fit_year_zones_months_int1$coefficients)

# now get the coefficients 

year_coef = coef[2]

intercept = coef[1]

months_coef = coef[52:62,]

interactions_coef = coef[63:73,]

zones_coef = coef[3:51,]
zones_coef = c(0,zones_coef)
zones_coef = as.numeric(zones_coef)
zones_coef



hist(zones_coef, breaks = 20)

# Checking if the variable is normally distributed 
shapiro.test(zones_coef)

zones = c(1:5,7:51)


zones_coefs = data.frame(Zones = zones, zones_coef)

# Now merge the zones with the coordinates points 

florida_coord <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_coord.txt", quote="\"", comment.char="")



Zones = c(1:51)

florida_coord = cbind(Zones,florida_coord)

# merge

florida_coord_zones_rate = merge(florida_coord, zones_coefs,by = "Zones")
View(florida_coord_zones_rate)

# drop the first column 
florida_coord_zones_rate = florida_coord_zones_rate[,2:4]


# Check for spatial autocorrelation by performing a Morans test 
library(ape)
# Moran test 
florida.dists <- as.matrix(dist(cbind(florida_coord_zones_rate$V1, florida_coord_zones_rate$V2)))

florida.dists.inv <- 1/florida.dists
diag(florida.dists.inv) <- 0
Moran.I(florida_coord_zones_rate$zones_coef, florida.dists.inv)

# No spatial correlation 

# Now plot the variogram for the data 

gg = gstat(formula = zones_coef ~ 1, # We don't want to include independent variables
           # We're just modelling how the effects change in space
           locations = ~V1+V2, # Include the longitude and latitude
           data = florida_coord_zones_rate # And where the data is
)

v_rm6 = variogram(gg) # Build the variogram from the gstat object
plot(v_rm6) # Plot it

head(v_rm6)

# there seems like there is no real pattern to indicate spatial autocorellation 


