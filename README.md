# Dissertation
There 6 different R files that are required to run the whole project.Before running the files some specifc packages need to be loaded. These are: 
-dplyr
-tidyr
-readr
-tidyverse
-lattice
-ggplot2
-ggpubr
-caret
-gstat
-sp
-sf
-raster
-rgdal
-lmerTest
-cvms
-groupdata2
-lme4
-arm

Other than that, someone needs to have the following files in their working directory: 
-florida_acc.txt
-florida_coord.txt

In all of the files mentioned above someone needs to replace the path everytime these files are loaded. E.g in the following: 

read.table("~/Documents/iro/uni/Dissertation/Data/florida_acc.txt", quote="\"", comment.char="") 

replace: "~/Documents/iro/uni/Dissertation/Data/florida_acc.txt" with the right path. This needs to be done in all the files.  

The files are the followimg and each one of them relates to specific sections of the project:

1) eda_and_linear_reg file: This file inlcudes all the initial data manipulation that took place in order to transform the data into the format needed to perform some intitial analysis as well as the linear regression. The first 200 lines relate to Section 3.1 in the report where the data manipulation and EDA takes place.
In the next lines of the file someone can find all the models fitted for linear regression as well as the cross validation performed. These relate to Section 3.2.1 where the application of linear regression takes place, as well as to Section 4.1 where the results are presented and discussed. 

2) reg_coef file: This file includes the code that is needed to create Figure 3 in section 4.1. 

3) hlm_fit file: This file creates the code used to fit the four hierarchical linear models for the project. It also inlcudes the steps to perform the cross validation. This file relates to Section 3.2.2 and Section 4.2 where hlm are applied to the data and then the results are discussed. 

4) kriging_zones_coef file: This file inlcudes all the code which explores if the regression coefficients of the zones can be used for kriging. This file explores if the spatial autocorellation is present when using the zones coefficients as the predictor variable. This file relates mostly to Section 4.2 of the report where the results are presented. The code is used to produce the left plot in figure 4. 

5) kriging_mean_rate file: This file inlcudes all the code which explores if the mean rate of the zones can be used for kriging instead of the zones coefficients. Since it is found that by using the mean rate as a predictor there is spatial correlation, this file also inlcudes all the code relevant to fitting the variogram, performing kriging and then making the relevant visual plots. This file relates to Section 3.2.3 where the application of the methods is discussed and to Section 4.3 where the  results are presented. The code in this file was used to produce Figure 5 in the report. 

6) Diss-rep file: This file inlcudes all code used to produce the main report. Before running this file someone needs to load all the packages mentioned above as well as the kableExtra and rticles packages.

