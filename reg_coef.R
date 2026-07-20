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

# Merge the dataset 

florida_acc_pv = cbind(year,florida_acc_pv)

# removing the NA rows 

florida_acc_pv_nna = na.omit(florida_acc_pv)

nn = as.numeric(substr(florida_acc_pv_nna[,3], start = 2, stop = 4))

florida_acc_pv_nna = cbind(florida_acc_pv_nna, "zones"= nn-1)

# Transforming rate in order to treat is as a normal random variable in order to perform 
# liner regression 

log_rate = log(florida_acc_pv_nna$Rate + 0.001)


florida_acc_pv_nna = cbind(florida_acc_pv_nna, log_rate)

# Create the response variable 

y = florida_acc_pv_nna$log_rate

# Create the explanatory variables matrix

X1 = florida_acc_pv_nna[,c("year", "zones", "MonthAbb")]

# Create dataframe 

florida_data = data.frame(y,X1)

florida_data = florida_data[florida_data$zones != 6,]

lsq_fit_year_zones_months_int1 = lm(y ~ year + factor(zones) + MonthAbb + year*MonthAbb, data = florida_data)
summary(lsq_fit_year_zones_months_int1)


sum = summary(lsq_fit_year_zones_months_int1)
sum$coefficients

coef = as.data.frame(lsq_fit_year_zones_months_int1$coefficients)
p_val = as.data.frame(sum$coefficients[,4])
year_coef = coef[2,]

intercept = coef[1,]

months_coef = coef[52:62,]
months_coef = c(0,months_coef)
months_pval = p_val[52:62,]
months_pval = c(0,months_pval)

interactions_coef = coef[63:73,]
interactions_coef = c(0,interactions_coef)

zones_coef = coef[3:51,]
zones_coef = c(0,zones_coef)
zones_pval = p_val[3:51,]
zones_pval = c(0,zones_pval)

z = c(1:5,7:51)

mon = c("Apr", "Aug","Dec","Feb","Jan","Jul","Jun","Mar","May","Nov","Oct","Sep") 

mon_df = data.frame("Months" = mon, "Regression coefficients" = months_coef, "P value" = months_pval )
z_df = data.frame("Zones" = z, "Regression coefficients" = zones_coef, "P value" = zones_pval )

k = vector()

for (i in 1:nrow(z_df)){
  if (z_df$P.value[i] <= 0.1){
    k[i] = "Significant"
  }
  else{
    k[i] = "Not significant" 
  }
}

k

z_df$significance = k

zon_reg = ggplot(data=z_df, aes(x=Zones, y=Regression.coefficients, fill = significance)) +
  geom_bar(stat="identity")

zon_reg

d = vector()

for (i in 1:nrow(mon_df)){
  if (mon_df$P.value[i] <= 0.1){
    d[i] = "Significant"
  }
  else{
    d[i] = "Not significant" 
  }
}

d


mon_df$significance = d

mon_reg = ggplot(data=mon_df, aes(x=Months, y=Regression.coefficients, fill = significance)) +
  geom_bar(stat="identity")

mon_reg
