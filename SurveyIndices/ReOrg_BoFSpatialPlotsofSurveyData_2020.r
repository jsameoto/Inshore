###..............................###
### Spatial Figures - Survey Data ###
###    Full BoF and Approaches   ###
### J.Sameoto Nov 2017, Oct 2018 ###
### B.Wilson - Revised Dec 2020  ###
###        (using Github)        ###
###..............................###

# Spatial figures of Survey Density, Survey Biomass, Condition, Meat count for BoF: SPA 4, SPA 1A, SPA1B
#for SPA 3 and SPA 6 see area specific scripts for spatial plots 

#Script layout changed from original:

#Plots for Commercial data:
  #- Density
      #- all areas
  #- Biomass
      #- all areas
  #- Condition
      #- all areas
  #- Meat Count
      #- all areas
  #- Clappers

#Plots for Recruits data:
  #- Density
    #- all areas
  #- Biomass
    #- all areas
  #- Condition
    #- all areas
  #- Meat Count
    #- all areas
  #- Clappers

#Plots for Pre-rec data:
  #- Density
    #- all areas
  #- Biomass
    #- all areas
  #- Condition
    #- all areas
  #- Meat Count
    #- all areas
  #- Clappers

options(stringsAsFactors = FALSE)

# required packages
require (CircStats)
require (TeachingDemos)
require (PBSmapping)
require (akima)
require (gstat)
require (fields)
require (splancs)
require (RColorBrewer)
require (spatstat)
require (RPMG)
#require (RODBC)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
library(RCurl)



# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- un.bwilson
pwd <- pw.bwilson

surveyyear <- 2019  #This is the last survey year 
assessmentyear <- 2020 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/INSHORE SCALLOP/BoF/"

#set up directory to save plot
saveplot.dir <- "C:/Users/WILSONB/Documents/1_GISdata/" #Directory to save plots to #paste0("Y:/INSHORE SCALLOP/BoF/",assessmentyear,"/Assessment/Figures")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set year 
survey.year <- 2019  #removed maxyear in script and changed to survey year (Under )

#### Import Source functions and polygons ####
# source R functions
#source("Y:/Offshore scallop/Assessment/Assessment_fns/Maps/ScallopMap.R")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")
#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/gridPlot.r")

#### Import Mar-scal functions
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# BoF.poly <- read.csv("C:/Users/NasmithL/Documents/Mapping/Scallop Boundaries/Survey/XYBoFPoly_new.csv") #use for commercial plots of 1A !!!GET FROM LESLIE
#BoF.Survpoly <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/bofPoly.csv") #use for survey plots
#Survey.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SCSTRATADEFS.csv")
#spa4.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa4stratadefs.csv")
#spa3.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa3Poly.csv")
#BILU.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BILUpoly.csv")
#VMSpoly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
#spa6Poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84.csv")
#spa6line<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84_outterline.csv")
#inVMS<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_IN_R_final_MOD.csv")
#outvms<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_OUT_R_final_MOD.csv")
#attr(inVMS,"projection") <- "LL"
#attr(outvms,"projection") <- "LL"

# -----------------------------Import SHF data (live and dead)--------------------------------------------

##.. LIVE ..##
## NOTE: For BoF plots keep strata_id call included (omits GM and SMB); for document remove strata_id limits
#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 30, 31, 32, 35, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)    ",
  sep=""
)


#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
ScallopSurv  <- ScallopSurv[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commerical, recruit and prerecruit data columns

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60
  
table(ScallopSurv$year)
summary(ScallopSurv) #check data

#attr(ScallopSurv, "projection") #check default projection of data
#attr(ScallopSurv, "projection") <- "LL" # assign projection for data
#ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326)

##.. DEAD ..##
#Db Query:
quer3 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scdeadres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 30, 31, 32, 35, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)    ",
  sep=""
)

ScallopSurv.dead <- dbGetQuery(chan, quer3)
ScallopSurv.dead   <- ScallopSurv.dead[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commerical, recruit and prerecruit data columns

ScallopSurv.dead <- ScallopSurv.dead %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60


# -------------------------------Import WGTHGT DATA------------------------------------------

#List of tows that have detailed samples
quer4 <- paste(
  "SELECT CRUISE, TOW_NO 			                ",
  "FROM SCALLSUR.scwgthgt s			",
  " where s.strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 30, 31, 32, 35, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)    ",
  " GROUP BY CRUISE, TOW_NO",
  sep=""
)

sampled.dat <- dbGetQuery(chan, quer4)

# --------------------------------Import Biomass per tow data-----------------------------------------

# this is output from the meat weight/shell height modelling !NB: not all years needed depending on what you want to show
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)

max.yr <- max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-4),max.yr)
num.years <- length(Year)

BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA",area,"/BFliveweight",Year[i],".csv",sep=""), header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#check data structure
summary(BFliveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commerical, recruit and prerecruit data columns
ScallopSurv.kg <- BFliveweight %>%
  rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000
  
#attr(ScallopSurv.kg, "projection") #check default projection of data
#attr(ScallopSurv.kg, "projection") <- "LL" # assign projection for data

# --------------------------------Load Condition data since 2015-----------------------------------------

Year <- seq(2015,survey.year)
num.years <- length(Year)

con.dat <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA",area,"/BFConditionForMap",Year[i],".csv",sep="") ,header=T)
  con.dat <- rbind(con.dat,temp)
}
#check data structure
summary(con.dat)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), and ID (cruise.tow#) columns
con.dat <- con.dat %>%
  rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

# For Meat Count plot:
ScallopSurv.mtcnt <- ScallopSurv.kg %>% 
  select(ID, year, lat, lon, com.bm) %>% 
  merge(ScallopSurv %>% select(ID, com), by = "ID") %>% 
  mutate(meat.count = (0.5/(com.bm/com))) %>% 
  filter(!is.na(meat.count))
