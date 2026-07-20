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

# load data 

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


head(florida_acc_pv_nna)

# Create the response variable 

y = florida_acc_pv_nna$Rate

# Create the explanatory variables matrix

X1 = florida_acc_pv_nna[,c("year", "zones", "MonthAbb")]

# Create dataframe 

florida_data = data.frame(y,X1)


# removing zone 6 data 

florida_data = florida_data[florida_data$zones != 6,]

# Now calculating the mean rate per zone 

mean_acc_rate_zone = florida_data %>% group_by(zones) %>% summarise(mean_rate = mean(y))

mean_acc_rate_zone = as.data.frame(mean_acc_rate_zone)

# Now merge the zones with the coordinates points 

florida_coord <- read.table("~/Documents/iro/uni/Dissertation/Data/florida_coord.txt", quote="\"", comment.char="")

zones = c(1:51)

florida_coord = cbind(zones,florida_coord)

# merge

florida_coord_zones_rates = merge(florida_coord, mean_acc_rate_zone,by = "zones")
#View(florida_coord_zones_rates)


# drop the first column 
florida_coord_zones_rates = florida_coord_zones_rates[,2:4]

florida_coord_zones_rates_df = florida_coord_zones_rates

# Check for normality 

hist(florida_coord_zones_rates$mean_rate, breaks = 20, main = "Histogram of mean rate before tranformation")
shapiro.test(florida_coord_zones_rates$mean_rate)

# Normality is not satisfied so the square root is taken

sq = sqrt(florida_coord_zones_rates$mean_rate)
hist(sq, breaks = 20, main = "Histogram of mean rate after square root transformation")
shapiro.test(sq)

# The assumption for normality is satisfied 

# Now check for spatial autocorrelation 

library(ape)
# Moran test 
florida.dists_zr <- as.matrix(dist(cbind(florida_coord_zones_rates$V1, florida_coord_zones_rates$V2)))

florida.dists.inv_zr <- 1/florida.dists_zr
diag(florida.dists.inv_zr) <- 0
Moran.I(florida_coord_zones_rates$mean_rate, florida.dists.inv_zr)

# The p-value now is 2.463692e-07 which means that there is spatial autocorrelation at the 0.5 level
# So the sptatial correlation assumption is also satisfied

# Now plotting the variogram
ggr = gstat(formula = sqrt(mean_rate) ~ 1, # We don't want to include independent variables
            # We're just modelling how the effects change in space
            locations = ~V1+V2, # Include the longitude and latitude
            data = florida_coord_zones_rates # And where the data is
)

v_r = variogram(ggr) # Build the variogram from the gstat object
plot(v_r) # Plot it



# transform to spatial object

coordinates(florida_coord_zones_rates) = ~ V1+V2

class(florida_coord_zones_rates)

st_crs(florida_coord_zones_rates)

# Set the right coordinate system 

crs.geo1 = CRS("+proj=longlat +datum=WGS84 +no_defs")

proj4string(florida_coord_zones_rates) <- crs.geo1

florida_coord_zones_rates = st_as_sf(florida_coord_zones_rates)


# Creating the grid to be used to make predictions in the unsampled locations

bbox_zr <- st_bbox(florida_coord_zones_rates)

florida_coord_zones_rate_grid_zr <- florida_coord_zones_rates %>% 
  st_bbox() %>%                    # determines bounding box coordinates from meuse
  st_as_sfc() %>%                  # creates sfc object from bounding box
  st_make_grid(                    # create grid 50 x 50 pixel size
    cellsize = c(0.05, 0.05), 
    what = "centers") %>%
  st_as_sf(crs=st_crs(florida_coord_zones_rates))

# Transforming to spatial pixels object

florida_coord_zones_rate_grid_sp_zr <- as(as(florida_coord_zones_rate_grid_zr, "Spatial"), "SpatialPixels")


plot(florida_coord_zones_rate_grid_zr)
plot(florida_coord_zones_rate_grid_sp_zr)


# turn back into sp object 

florida_coord_zones_rate_sp_zr <- as(florida_coord_zones_rates, "Spatial")


# Fit sample variogram along with the Exponential model selected

v = vgm(psill=0.07, "Exp", range=100, nugget=0.02)

lzn.fit_zr = fit.variogram(lzn.vgm_zr, v)


plot(lzn.vgm_zr,lzn.fit_zr)

# The model seems to fit well to the data

# Ordinary kriging 

lzn.kriged_zr <-krige(sqrt(mean_rate) ~ 1, florida_coord_zones_rate_sp_zr,
                      florida_coord_zones_rate_grid_sp_zr, model = lzn.fit_zr)

lzn.kriged_zr_df = as.data.frame(lzn.kriged_zr)

# Prediction plot 
plot(lzn.kriged_zr['var1.pred'], axes = TRUE, )
points(florida_coord_zones_rate_sp_zr, col="white", cex=0.5)

# Turning the predictions into a ggplot 
library(ggplot2)
library(scales)

krigpred = ggplot() + geom_tile(data = lzn.kriged_zr_df, aes(x = coords.x1, y=coords.x2, fill=var1.pred)) + coord_equal() + 
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", 
                    title = "Kriging predictions") + 
  geom_point(data = florida_coord_zones_rates_df, aes(x = V1, y = V2), shape = 1, size = 1)

krigpred

# Variance plot 
plot(lzn.kriged_zr['var1.var'], axes = TRUE)
points(florida_coord_zones_rate_sp_zr, col="white", cex=0.5)

krigvar = ggplot() + geom_tile(data = lzn.kriged_zr_df, aes(x = coords.x1, y=coords.x2, fill=var1.var)) + coord_equal() + 
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", 
                    title = "Kriging predictions variance") + 
  geom_point(data = florida_coord_zones_rates_df, aes(x = V1, y = V2), shape = 1, size = 1)

krigvar

# Performing cross validation for kriging 

cv_mean_rate <- krige.cv(sqrt(mean_rate) ~ 1, florida_coord_zones_rate_sp_zr, v, nmax = 40, nfold=10) 

bubble(cv_mean_rate, "residual", main = "mean rate: 10-fold CV residuals")

summary(cv_mean_rate)

# mean error, ideally 0:
mean(cv_mean_rate$residual)
# MSPE, ideally small
mean(cv_mean_rate$residual^2)
# Mean square normalized error, ideally close to 1
mean(cv_mean_rate$zscore^2)
# correlation observed and predicted, ideally 1
cor(cv_mean_rate$observed, cv_mean_rate$observed - cv_mean_rate$residual)
# correlation predicted and residual, ideally 0
cor(cv_mean_rate$observed - cv_mean_rate$residual, cv_mean_rate$residual)

plot(cv_mean_rate$observed, cv_mean_rate$observed - cv_mean_rate$residual)
