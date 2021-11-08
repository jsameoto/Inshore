#Load libraries

library(tidyverse)
library(raster)
library(sf)
library(mgcv)
library(lme4)
library(mapview)
library(gridExtra)
library(gratia)
library(ggspatial)
library(lattice)

#Predictors:

# Predictors --------------------------------------------------------------


#-----------------------------------------#
# BAY OF FUNDY COVERAGE (50m resolution)
#-----------------------------------------#

# 1 - Bathy
# 2 - Mixed Layer Depth - BNAM Wang et al., 2018
# 3 -  BPI Broad - 1000m
# 4 - BPI Fine - 50m
# 5 - Backscatter - bulkshift method by Ben Misiuk
# 6 - Easterness - TASSE toolbox - Lecours et al., 2016
# 7 - Slope - Log slope
# 8 - Max velocity - Alleosfour et al., Max bottom velocity for period of July - Sept, 2017
# 9 - Max Salinity - Alleosfour et al., Maximumn bottom salintiy for period of July - Sept, 2017
# 10 - Max Temp - Alleosfour et al., Maximumn bottom Temperature for period of July - Sept, 2017
# 11 - Mean grain size - Ben Misiuk
# 12 - Mean velocity - Alleosfour et al. Mean bottom velocity for period of July - Sept, 2017
# 13 - Mean Salinity - Alleosfour et al.Mean bottom Salinity for period of July - Sept, 2017
# 14 - Mean Temperature - Alleosfour et al. Mean bottom Temperatuer for period of July - Sept, 2017
# 15 - Min Salinity - Alleosfour et al. Minimum bottom Salinity for period of July - Sept, 2017
# 16 - Min Temperature -Alleosfour et al. Minimum bottom Temperature for period of July - Sept, 2017
# 17 - northerness - TASSE toolbox - Lecours et al., 2016
# 18 - Relative deviation from mean value (pits and peaks)- TASSE toolbox - Lecours et al., 2016
# 19 - Sediment mobility frequency - Li et al., 2017
# 20 - Wave Shear Stress -  Li et al., 2017
# - Benthoscape (Shapefile) - Wilson et al., 2020

direct <- "C:/Users/WILSONB/Documents/1_Projects/Publications_working/BOF_SDMs/Projects/Scallop/Data/EnvData"
BoF_pred_list <- list.files(direct, pattern =".gri$",full.names = TRUE)  #Change path for data resolution
# Put the rasters into a RasterStack:
BoF_predictors <- raster::stack(BoF_pred_list)
mapview::mapview(BoF_predictors[[20]])

#----------------------------------#
# SFA29 COVERAGE - (50m resolution)
#----------------------------------#

# 1 - BNAM Btm Salinity yearly average
# 2 - BNAM Btm Temperature yearly average
# 3 - Aspect
# 4 - Bathymetry
# 5 - Broad BPI
# 6 - Backscatter
# 7 - Curvature
# 8 - Fine BPI
# 9 - Slope


#direct <- "C:/Users/WILSONB/Documents/1_Projects/GermanBank/Maxent_models/Data/MBES/50m_rasters/rasters_cropped/LL_reproj"
#SFA29_pred_list <- list.files(direct, pattern =".asc$",full.names = TRUE)  #Change path for data resolution
# Put the rasters into a RasterStack:
#SFA29_predictors <- raster::stack(SFA29_pred_list)
#mapview::mapview(SFA29_predictors[[1]])


#-------------------------------#
# BNAM coverage - all areas
#-------------------------------#

# 1 - Btm Salinity
# 2 - Btm Temperature
# 3 - Btm Stress
# 4 - Mixed Layer Depth
# 5 - Surface Salinity
# 6 - Surface Temperature

# Load environmental data - raster stack  ---------------------------------

#Read in Benthoscape shapefile
benthoscape <- st_read("Z:/Projects/BoF_Mapping_Project/Analysis/Benthoscape_mapping/Unsupervised_Classification/UnsupervisedClassification_shapefiles/BoF_Benthoscape_Unsupervised_objDissolve/BoF_Benthoscape_objDissolve.shp")
benthoscape <- st_transform(benthoscape, crs = st_crs(4326)) %>%
  st_transform(st_crs(32620))

# Load abundance data  ---------------------------------
hm.dat <- readRDS("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/Prorated/horsemussellive_prorated.rds") %>%
  mutate(mid_lon = MID_LONG) %>% #duplicate lat, longs to use after converting to sf
  mutate(mid_lat = MID_LAT) %>% 
  st_as_sf(coords = c("MID_LONG", "MID_LAT"), crs = 4326) %>% 
  st_transform(32620)

hm.dat <- hm.dat %>% 
  filter(!grepl("SFA29", CRUISE)) #Remove SFA29W - no enviro data
#check
table(hm.dat$CRUISE)

# Extract environmental data at tow midpoints: ---------------------------------

enviro.dat <- raster::extract(BoF_predictors, hm.dat, na.rm = FALSE) #from Rasters
benthoscape <- st_intersection(benthoscape, hm.dat)  #from shapefile

#Join raster data with horse mussel data
hm.enviro.dat <- cbind(hm.dat %>% dplyr::select(ID, CRUISE, TOW, ABUND.STD, STRATA_ID, START_LONG, START_LAT, mid_lon, mid_lat), enviro.dat)
#join benthoscape classes
hm.enviro.dat <- st_join(hm.enviro.dat,benthoscape  %>% dplyr::select(c(CLASS, ID)), by = "ID")


mapview::mapview(hm.enviro.dat)

# Tidy Data --------------------------------------------------------

hm.enviro.dat <- hm.enviro.dat %>% 
  rename(ID = ID.x) %>% 
  rename(MLD = BNAM_MLD_50m_UTMZ20) %>% 
  rename(BBPI = BPI.Broad.1000m_UTMZ20) %>% 
  rename(FBPI = BPI.Fine.50m_UTMZ20) %>% 
  rename(BS_Bulkshift = bulkshift_bs_50m_UTMZ20) %>% 
  rename(Easterness = easterness_50m_UTMZ20) %>% 
  rename(LogSlope = logslope_50m_UTMZ20) %>% 
  rename(MaxVel = max_vel_50m_UTMZ20) %>% 
  rename(MaxSal = maxSal_7.9_50m_UTMZ20) %>%
  rename(MaxTemp = maxTemp_7.9_50m_UTMZ20) %>%
  rename(MeanGrainsize = mean_gsize_50m_UTMZ20) %>% 
  rename(MeanVel = mean_vel_50m_UTMZ20) %>% 
  rename(MeanSal = meanSal_7.9_50m_UTMZ20) %>% 
  rename(MeanTemp = meanTemp_7.9_50m_UTMZ20) %>%
  rename(MinSal = minSal_7.9_50m_UTMZ20) %>%
  rename(MinTemp = minTemp_7.9_50m_UTMZ20) %>%
  rename(Northerness = northerness_50m_UTMZ20) %>% 
  rename(rdmv = rdmv_50m_UTMZ20) %>% 
  rename(SMF = SMF_krige50m_UTMZ20) %>%
  rename(WSV = WaveShear.Res50m_UTMZ20) %>%
  rename(Benthoscape = CLASS) %>% 
  dplyr::select(!ID.y)

# Separate by Strata ------------------------------------------------------

hm.enviro.dat <- hm.enviro.dat %>% 
  mutate(SPATIAL_AREA = case_when(STRATA_ID == 22 ~ "SMB", 
                                   STRATA_ID %in% c(23,56) ~ "BI",
                                   STRATA_ID %in% c(41:45) ~ "LURCHER",
                                   STRATA_ID %in% c(30:32) ~ "SPA6",
                                   STRATA_ID %in% c(1:21, 47, 48) ~ "SPA4",
                                   (STRATA_ID %in% c(35, 49, 50, 51, 52) & START_LONG >= -64.9254) ~ "UPPERBAY", 
                                   STRATA_ID %in% c(37:38, 53:55) ~ "INNERBAY",
                                   (STRATA_ID == 49 & START_LONG <= -64.9254) ~ "INNERBAY",
                                   STRATA_ID == 39 ~ "MIDBAYSOUTH")) %>% 
  mutate(SPATIAL_AREA = as.factor(SPATIAL_AREA))

summary(hm.enviro.dat)


hm.enviro.sf <- hm.enviro.dat %>% 
  filter(!is.na(Bathy)) %>% 
  filter(!is.na(MaxSal)) %>% 
  filter(Benthoscape != "Not_Classified") %>% 
  mutate(STRATA_ID = as.factor(STRATA_ID))

hm.enviro.dat <- hm.enviro.sf %>% st_set_geometry(NULL)

# Data Exploration --------------------------------------------------------


source("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/HighstatLibV7.R")

#Inspect the file
#What do we have?
names(hm.enviro.dat)

#[1] "ID"            "CRUISE"        "TOW"          
#[4] "ABUND.STD"     "STRATA_ID"     "START_LONG"   
#[7] "START_LAT"     "mid_lon"       "mid_lat"      
#[10] "Bathy"         "MLD"           "BBPI"         
#[13] "FBPI"          "BS_Bulkshift"  "Easterness"   
#[16] "LogSlope"      "MaxVel"        "MaxSal"       
#[19] "MaxTemp"       "MeanGrainsize" "MeanVel"      
#[22] "MeanSal"       "MeanTemp"      "MinSal"       
#[25] "MinTemp"       "Northerness"   "rdmv"         
#[28] "SMF"           "WSV"           "Benthoscape"  
#[31] "geometry"      "SPATIAL_AREA" 

#Are factor factors? - Yes (Benthoscape, SPATIAL_AREA)
str(hm.enviro.dat)

#Classes 'sf' and 'data.frame':	1177 obs. of  32 variables:
#  $ ID           : chr  "BF2018.1" "BF2018.2" "BF2018.3" "BF2018.4" ...
#$ CRUISE       : Factor w/ 12 levels "BF2018","BF2019",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ TOW          : int  1 2 3 4 5 6 7 8 9 10 ...
#$ ABUND.STD    : num  0 0 0 0 0 0 0 0 0 0 ...
#$ STRATA_ID    : Factor w/ 36 levels "1","2","3","4",..: 8 2 2 2 9 9 9 2 2 2 ...
#$ START_LONG   : num  -66 -66 -66 -66 -65.9 ...
#$ START_LAT    : num  44.6 44.6 44.7 44.7 44.7 ...
#$ mid_lon      : num  -66 -66 -66 -66 -65.9 ...
#$ mid_lat      : num  44.6 44.6 44.7 44.7 44.7 ...
#$ Bathy        : num  -99.6 -81 -89.2 -81.6 -93.4 ...
#$ MLD          : num  11.2 11.4 11.8 11.5 11.8 ...
#$ BBPI         : num  -3.41 4.61 2.71 7.69 -2.6 ...
#$ FBPI         : num  -0.281 1.085 -0.126 0.889 -0.178 ...
#$ BS_Bulkshift : num  -12.5 -13.3 -12.7 -11.7 -11.6 ...
#$ Easterness   : num  -0.108 -0.035 0.558 0.446 0.414 ...
#$ LogSlope     : num  -1.0321 0.4214 0.581 0.0111 1.0642 ...
#$ MaxVel       : num  0.688 0.69 0.661 0.668 0.661 ...
#$ MaxSal       : num  32.6 32.5 32.4 32.4 32.4 ...
#$ MaxTemp      : num  8.38 8.58 8.68 8.83 8.96 ...
#$ MeanGrainsize: num  -1.289 0.388 -1.059 -0.511 -0.805 ...
#$ MeanVel      : num  0.365 0.368 0.361 0.36 0.362 ...
#$ MeanSal      : num  32 32 32 32 32 ...
#$ MeanTemp     : num  7.41 7.58 7.64 7.75 7.85 ...
#$ MinSal       : num  32 32 32 31.9 32 ...
#$ MinTemp      : num  5.66 5.76 5.82 5.98 6.08 ...
#$ Northerness  : num  -0.907 0.981 -0.829 0.495 -0.91 ...
#$ rdmv         : num  -0.1488 0.1315 -0.0575 0.2351 -0.0922 ...
#$ SMF          : num  56.9 55.1 48.9 53.3 45.7 ...
#$ WSV          : num  0.033 0.033 0.033 0.033 0.032 ...
#$ Benthoscape  : Factor w/ 8 levels "Bedrock_and_Boulders",..: 3 3 3 3 3 3 3 3 3 3 ...
#$ geometry     :sfc_POINT of length 1177; first list element:  'XY' num  260340 4946429
#$ SPATIAL_AREA : Factor w/ 7 levels "BI","INNERBAY",..: 5 5 5 5 5 5 5 5 5 5 ...

##########################################################################################


# A Outliers in Y -------------------------------------------------
par(mfrow = c(1, 2))
boxplot(hm.enviro.dat$ABUND.STD, 
        main = "Abundance")
dotchart(hm.enviro.dat$ABUND.STD, 
         xlab = "Range of data", 
         ylab = "Values")



# A Outliers in X -------------------------------------------------
par(mfrow = c(2, 3), mar = c(4, 3, 3, 2))
dotchart(hm.enviro.dat$Bathy, main = "Depth")
dotchart(hm.enviro.dat$MLD, main = "Mixed Layer Depth")
dotchart(hm.enviro.dat$BBPI, main = "Broad BPI")
dotchart(hm.enviro.dat$FBPI, main = "Fine BPI")
dotchart(hm.enviro.dat$BS_Bulkshift, main = "Backscatter")
dotchart(hm.enviro.dat$Easterness, main = "Easterness")
dotchart(hm.enviro.dat$LogSlope, main = "Log Slope")
dotchart(hm.enviro.dat$MaxVel, main = "Maximum Velocity")
dotchart(hm.enviro.dat$MaxSal, main = "Maximum Salinity")
dotchart(hm.enviro.dat$MaxTemp, main = "Maximum Temp")
dotchart(hm.enviro.dat$MeanGrainsize, main = "Mean Grain size")
dotchart(hm.enviro.dat$MeanVel, main = "Mean Velocity")
dotchart(hm.enviro.dat$MeanSal, main = "Mean Salinity")
dotchart(hm.enviro.dat$MeanTemp, main = "Mean Temp")
dotchart(hm.enviro.dat$MinSal, main = "Minimum Salinity")
dotchart(hm.enviro.dat$MinTemp, main = "Minimum Temp")
dotchart(hm.enviro.dat$Northerness, main = "Norhterness (aspect)")
dotchart(hm.enviro.dat$rdmv, main = "Relative deviation from Mean")
dotchart(hm.enviro.dat$SMF, main = "Sediment Mobility Frequency")
dotchart(hm.enviro.dat$WSV, main = "Wave Shear Stress")
#dotchart(hm.enviro.dat$Benthoscape, main = "Benthoscape class")
#dotchart(hm.enviro.dat$SPATIAL_AREA, main = "Spatial area")


#OR#
#multi-panel dotplot
MyVar <- c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
  "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
  "SMF","WSV","Benthoscape","SPATIAL_AREA")

Mydotplot(as.matrix(hm.enviro.dat[,MyVar])) 


#Identify the outlier in area.
plot(x = hm.enviro.dat$SMF, 
     y = 1:nrow(hm.enviro.dat),
     xlab = "Value of variable",
     ylab = "Order of the data from text file")

identify(x = hm.enviro.dat$SMF, y = 1:nrow(hm.enviro.dat))


#Apply transformations

# B Collinearity X -------------------------------------------------

MyVar <- c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
           "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
           "SMF","WSV")
pairs(hm.enviro.dat[, MyVar])


pairs(hm.enviro.dat[,c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
               "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",
               "SMF","WSV")],
      lower.panel = panel.cor)

corvif(hm.enviro.dat[,c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
                "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",
                "SMF","WSV")])#,"Benthoscape","SPATIAL_AREA")])

#Variance inflation factors

#GVIF

#Bathy          7.816504  *
#MLD            4.287007  *
#MaxVel        17.379305  *
#MaxSal         6.289689  *
#MaxTemp       44.445146  *
#MeanGrainsize  3.285619  *
#MeanVel       23.460109  *
#MeanSal        3.305556  *
#MeanTemp      71.331070  *
#MinSal         4.008834  *
#MinTemp       35.930368  *
#SMF            3.207777  *
#WSV            5.286918  *

#BBPI           1.423243
#FBPI           1.563191
#BS_Bulkshift   1.738174
#Easterness     1.158229
#LogSlope       1.235219
#Northerness    1.136065
#rdmv           1.305054


#Correlation with Factors:

#Boxplots Loop through all variables
#Benthoscape - Derived from Bathy, Backscatter, Slope, Wave shear stress, Broad Batymetric Position Index
for(i in 10:29){
  factor.cor <- boxplot(hm.enviro.dat[,i] ~ factor(Benthoscape), 
                data = hm.enviro.dat,
                varwidth = TRUE,
                ylab = "Benthoscape",
                xlab = colnames(hm.enviro.dat[i]),
                main = "")
  factor.cor
  }

#Boxplots Loop through all variables
#SPATIAL AREA
for(i in 10:29){
  factor.cor <- boxplot(hm.enviro.dat[,i] ~ factor(SPATIAL_AREA), 
                        data = hm.enviro.dat,
                        varwidth = TRUE,
                        ylab = "Spatial Area",
                        xlab = colnames(hm.enviro.dat[i]),
                        main = "")
  factor.cor
}

# C Relationships Y vs X --------------------------------------------------

MyVar <- c("ABUND.STD", "Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
           "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
           "SMF","WSV","Benthoscape","SPATIAL_AREA")
pairs(hm.enviro.dat[, MyVar],
      lower.panel = panel.cor)


boxplot(ABUND.STD ~ factor(Benthoscape), 
        data = hm.enviro.dat,
        varwidth = TRUE,
        ylab = "Horse mussel abundance",
        xlab = "Benthoscape",
        main = "")

boxplot(ABUND.STD ~ factor(SPATIAL_AREA), 
        data = hm.enviro.dat,
        varwidth = TRUE,
        ylab = "Horse mussel abundance",
        xlab = "Spatial Area",
        main = "")


#Plot every covariate versus Y
MyX  <- c("Bathy", "MLD","BBPI" ,"FBPI","BS_Bulkshift","Easterness","LogSlope","MaxVel","MaxSal",       
          "MaxTemp","MeanGrainsize", "MeanVel","MeanSal","MeanTemp","MinSal","MinTemp", "Northerness","rdmv",         
          "SMF","WSV")
Myxyplot(hm.enviro.dat, MyX, "ABUND.STD", MyYlab = "Abundance")

# D Spatial/temporal aspects of sampling design -------


# E Interactions (is the quality of the data good enough to include  --------

#Loops to plot all variables for Spatial area
for(i in 10:29){

  p1 <- xyplot(ABUND.STD ~ hm.enviro.dat[i] | factor(SPATIAL_AREA),
       data = hm.enviro.dat, 
       xlab = colnames(hm.enviro.dat[i]),
       ylab = "Abundance",
       strip = function(bg = 'white', ...) 
         strip.default(bg = 'white', ...),
       scales = list(alternating = TRUE, 
                     x = list(relation = "free"),
                     y = list(relation = "same")),
       panel=function(x,y){
         panel.grid(h=-1, v= 2)
         panel.points(x, y, col = 1)
         #panel.loess(x,y,col=1,lwd=2) #Add smoother
         panel.abline(lm(y~x))        #Add regression line
       })
  print(p1)
}

#Loops to plot all variables for Benthoscape
for(i in 10:29){
  
  p2 <- xyplot(ABUND.STD ~ hm.enviro.dat[i] | factor(Benthoscape),
               data = hm.enviro.dat, 
               xlab = colnames(hm.enviro.dat[i]),
               ylab = "Abundance",
               strip = function(bg = 'white', ...) 
                 strip.default(bg = 'white', ...),
               scales = list(alternating = TRUE, 
                             x = list(relation = "free"),
                             y = list(relation = "same")),
               panel=function(x,y){
                 panel.grid(h=-1, v= 2)
                 panel.points(x, y, col = 1)
                 #panel.loess(x,y,col=1,lwd=2) #Add smoother
                 panel.abline(lm(y~x))        #Add regression line
               })
  print(p2)
}


# F Zero inflation Y ------------------------------------------------------

#Frequency plots - # of observations
par(mar = c(4, 4, 3, 2))
plot(table(round(hm.enviro.dat$ABUND.STD)),
     type = "h",
     xlim = c(0, 100),
     xlab = "Observed values", ylab = "Frequency")



# G Are categorical covariates balanced? ----------------------------------












