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
source("Y:/Offshore scallop/Assessment/Assessment_fns/Maps/ScallopMap.R")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/gridPlot.r")

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
BoF.Survpoly <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/bofPoly.csv") #use for survey plots
Survey.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SCSTRATADEFS.csv")
spa4.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa4stratadefs.csv")
spa3.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa3Poly.csv")
BILU.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BILUpoly.csv")
VMSpoly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
spa6Poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84.csv")
spa6line<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84_outterline.csv")
inVMS<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_IN_R_final_MOD.csv")
outvms<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_OUT_R_final_MOD.csv")
attr(inVMS,"projection") <- "LL"
attr(outvms,"projection") <- "LL"

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

ScallopSurv <- mutate(ScallopSurv, year = year(TOW_DATE)) #Formats TOW_DATE as date

table(ScallopSurv$year)
summary (ScallopSurv) #check data

#Once data imported, convert to DD
ScallopSurv$lat <- convert.dd.dddd(ScallopSurv$START_LAT)
ScallopSurv$lon <- convert.dd.dddd(ScallopSurv$START_LONG)

#names(ScallopSurv)[2] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv <- rename(ScallopSurv, tow = TOW_NO) # ***NEW - eliminates the use of column number to make changes - require(tidyverse) ****

#ScallopSurv$tot <- rowSums(ScallopSurv[,11:50]) #all scallops, BIN_ID_0 to BIN_ID_195
ScallopSurv %>% 
  dplyr::select(BIN_ID_0:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv$tot # NEW **** Potential fix for summing all bin IDS across Rows, without using column numbers ****

ScallopSurv$tot <- ScallopSurv$tot/ 4267.2 # standardize number per tow to numbers per m^2

attr(ScallopSurv, "projection") #check default projection of data
attr(ScallopSurv, "projection") <- "LL" # assign projection for data
#ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326) #NEW ***Potential change: Convert ScallopSurv to sf object (requires sf) with latlong projection (EPSG 4326) can still be treated as dataframe, replaces assigning "LL" projection using attr() function, could potentially be named without the .sf ***

ScallopSurv$ID <- paste(ScallopSurv$CRUISE, ScallopSurv$tow, sep=".")

#create pre-rec, rec, comm fields:

#ScallopSurv$com <- apply(ScallopSurv[,27:50],1,sum) #>=80mm; BINS 80 to 195
ScallopSurv %>% #>=80mm; BINS 80 to 195 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_80:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv$com
ScallopSurv$com <- round(ScallopSurv$com, 0) #Round totals

#ScallopSurv$rec <- apply(ScallopSurv[,24:26],1,sum) #65-79; BINS 65 to 75
ScallopSurv %>% #65-79; BINS 65 to 75 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_65:BIN_ID_75) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv$rec
ScallopSurv$rec <- round(ScallopSurv$rec, 0) #Round totals

#ScallopSurv$pre <- apply(ScallopSurv[,11:23],1,sum) #0-64 mm; BINS 0 to 60
ScallopSurv %>% #0-64 mm; BINS 0 to 60 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_0:BIN_ID_60) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv$pre
ScallopSurv$pre <- round(ScallopSurv$pre, 0) #Round totals

ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326)

##.. DEAD ..##
#Db Query:
quer3 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scdeadres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 35, 37, 38, 39, 47,49, 50,  51, 52, 53,56)    ",
  sep=""
)

ScallopSurv.dead <- dbGetQuery(chan, quer3)
ScallopSurv.dead   <- ScallopSurv.dead[,1:51]
#ScallopSurv.dead$year <- as.numeric(substr(ScallopSurv.dead$CRUISE,3,6)) # add year column to SHF data. NOTE: this would NOT work for SFA29 cruises - would need to be ..,6,9)!!!
ScallopSurv.dead <- mutate(ScallopSurv.dead, year = year(TOW_DATE)) # ***Potential fix - require(lubridate), require(tidyverse), check if works with SFA29 cruises, should if TOW_DATE is formatted the same (POSIXct)

#Once data imported, convert to DD
ScallopSurv.dead$lat <- convert.dd.dddd(ScallopSurv.dead$START_LAT)
ScallopSurv.dead$lon <- convert.dd.dddd(ScallopSurv.dead$START_LONG)

#names(ScallopSurv.dead)[2] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv.dead <- rename(ScallopSurv.dead, tow = TOW_NO) # ***NEW - eliminates the use of column number to make changes - require(tidyverse) ****

#ScallopSurv.dead$tot <- rowSums(ScallopSurv.dead[,11:50]) #all scallops
ScallopSurv.dead %>% 
  dplyr::select(BIN_ID_0:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.dead$tot # NEW **** Potential fix for summing all bin IDS across Rows, without using column numbers ****

ScallopSurv.dead$tot <- ScallopSurv.dead$tot/ 4267.2 # standardize number per tow to numbers per m^2
attr(ScallopSurv.dead, "projection") #check default projection of data
attr(ScallopSurv.dead, "projection") <- "LL" # assign projection for data
ScallopSurv.dead$ID <- paste(ScallopSurv.dead$CRUISE, ScallopSurv.dead$tow, sep=".")

#create pre-rec, rec, comm fields:
#ScallopSurv.dead$com <- apply(ScallopSurv.dead[,27:50],1,sum) #>=80mm; BINS 80 to 195
ScallopSurv.dead %>% #>=80mm; BINS 80 to 195 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_80:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.dead$com
ScallopSurv.dead$com <- round(ScallopSurv.dead$com, 0) #Round totals

#ScallopSurv.dead$rec <- apply(ScallopSurv.dead[,24:26],1,sum) #65-79; BINS 65 to 75
ScallopSurv.dead %>% #65-79; BINS 65 to 75 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_65:BIN_ID_75) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.dead$rec
ScallopSurv.dead$rec <- round(ScallopSurv.dead$rec, 0) #Round totals

#ScallopSurv.dead$pre <- apply(ScallopSurv.dead[,11:23],1,sum) #0-64 mm; BINS 0 to 60
ScallopSurv.dead %>% #0-64 mm; BINS 0 to 60 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_0:BIN_ID_60) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.dead$pre
ScallopSurv.dead$pre <- round(ScallopSurv.dead$pre, 0) #Round totals


# -------------------------------Import WGTHGT DATA------------------------------------------

#List of tows that have detailed samples
quer4 <- paste(
  "SELECT CRUISE, TOW_NO 			                ",
  "FROM SCALLSUR.scwgthgt s			",
  " where s.strata_id in (1,2,3,4,5,6,7,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 35, 37, 38, 39, 49, 50, 51, 52, 53,56)    ",
  " GROUP BY CRUISE, TOW_NO",
  sep=""
)

sampled.dat <- dbGetQuery(chan, quer4)


# --------------------------------Import Biomass per tow data-----------------------------------------

# this is output from the meat weight/shell height modelling !NB: not all years needed depending on what you want to show
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)

max.yr <- max(na.omit(ScallopSurv$year)) ##***Already defined at beginning? as maxyear - with note to replace with survey year***
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

ScallopSurv.kg <- BFliveweight
ScallopSurv.kg$year <- ScallopSurv.kg$YEAR

#Once data imported, convert to DD
ScallopSurv.kg$lat <- convert.dd.dddd(ScallopSurv.kg$START_LAT)
ScallopSurv.kg$lon <- convert.dd.dddd(ScallopSurv.kg$START_LONG)

#names(ScallopSurv.kg)[4] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv.kg <- rename(ScallopSurv.kg, tow = TOW_NO) # ***NEW - eliminates the use of column number to make changes - require(tidyverse) ****

#ScallopSurv.kg$tot <- rowSums(ScallopSurv.kg[,13:52]) #all scallops
ScallopSurv.kg %>% 
  dplyr::select(BIN_ID_0:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.kg$tot # NEW **** Potential fix for summing all bin IDS across Rows, without using column numbers ****

ScallopSurv.kg$tot <- ScallopSurv.kg$tot/ 4267.2 # standardize number per tow to numbers per m^2
attr(ScallopSurv.kg, "projection") #check default projection of data
attr(ScallopSurv.kg, "projection") <- "LL" # assign projection for data
ScallopSurv.kg$ID <- paste(ScallopSurv.kg$CRUISE, ScallopSurv.kg$tow, sep=".")

#create pre-rec, rec, comm fields:
#ScallopSurv.kg$com.bm <- apply(ScallopSurv.kg[,29:52],1,sum)/1000 #>=80mm; BINS 80 to 195
ScallopSurv.kg %>% #>=80mm; BINS 80 to 195 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_80:BIN_ID_195) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.kg$com.bm
ScallopSurv.kg$com.bm <- round(ScallopSurv.kg$com.bm, 0)/1000 #Round totals and divide by 1000

#ScallopSurv.kg$rec.bm <- apply(ScallopSurv.kg[,26:28],1,sum)/1000 #65-79; BINS 65 to 75
ScallopSurv.kg %>% #65-79; BINS 65 to 75 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_65:BIN_ID_75) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.kg$rec.bm
ScallopSurv.kg$rec.bm <- round(ScallopSurv.kg$rec.bm, 0)/1000 #Round totals

#ScallopSurv.kg$pre.bm <- apply(ScallopSurv.kg[,13:25],1,sum)/1000 #0-64 mm; BINS 0 to 60
ScallopSurv.kg %>% #0-64 mm; BINS 0 to 60 # ***NEW*** - eliminates the need for column number selection - require(tidyverse)
  dplyr::select(BIN_ID_0:BIN_ID_60) %>%
  rowSums(na.rm=TRUE) -> ScallopSurv.kg$pre.bm
ScallopSurv.kg$pre.bm <- round(ScallopSurv.kg$pre.bm, 0)/1000 #Round totals

## Load Condition data since 2015 
Year <- seq(2015,survey.year) # ***changed maxyear to survey.year***
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

# Add 'year' and lat/lon
con.dat$year<- con.dat$YEAR
con.dat$lat <- convert.dd.dddd(con.dat$START_LAT)
con.dat$lon <- convert.dd.dddd(con.dat$START_LONG)
con.dat$ID <- paste(con.dat$CRUISE, con.dat$TOW_NO, sep=".")


# For Meat Count plot:
ScallopSurv.mtcnt <- merge(ScallopSurv.kg[,c('ID','year','lat','lon','com.bm')], ScallopSurv[,c('ID','com')], by=c("ID"))
ScallopSurv.mtcnt$meat.count<-(0.5/(ScallopSurv.mtcnt$com.bm/ScallopSurv.mtcnt$com))
ScallopSurv.mtcnt <- ScallopSurv.mtcnt[-which(is.na(ScallopSurv.mtcnt$meat.count)),]