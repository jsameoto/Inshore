###..............................###
### Spatial Figures - Survey Data ###
###    Full BoF and Approaches   ###
### J.Sameoto Nov 2017, Oct 2018 ###
### B.Wilson - Revised Dec 2020  ###
###        (using Github)        ###
###..............................###

# Spatial figures of Survey Density, Survey Biomass, Condition, Meat count for BoF: SPA 4, SPA 1A, SPA1B
#for SPA 3 and SPA 6 see area specific scripts for spatial plots 

#Note: Using revamped ScallopMap function - by David Keith

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
eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R", ssl.verifypeer = FALSE))) #load pectinid_projector function from Github


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
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 35, 37, 38, 39, 47, 49, 50, 51, 52, 53,56)    ",
  sep=""
)

#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
ScallopSurv  <- ScallopSurv[,1:51]

#ScallopSurv$year <- as.numeric(substr(ScallopSurv$CRUISE,3,6)) # add year column to SHF data. NOTE: this would NOT work for SFA29 cruises - would need to be ..,6,9)!!!
ScallopSurv <- mutate(ScallopSurv, year = year(TOW_DATE)) #***Potential fix - require(lubridate), require(tidyverse), check if works with SFA29 cruises, should if TOW_DATE is formatted the same (POSIXct). Creates column year based on the POSIXct year character in TOW_DATE.

# Error from year (b/c SFA29 surveys are alloted)
# Check using following that all cruises are ok for analysis
# temp <- ScallopSurv[!is.na(ScallopSurv$year),]
table(ScallopSurv$year)
summary (ScallopSurv) #check data
#Once data imported, convert to DD
ScallopSurv$lat <- convert.dd.dddd(ScallopSurv$START_LAT)
ScallopSurv$lon <- convert.dd.dddd(ScallopSurv$START_LONG)

#names(ScallopSurv)[2] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv <- rename(ScallopSurv, tow = TOW_NO) # ***NEW - eliminates the use of column number to make changes - require(tidyverse) ****

#ScallopSurv$tot <- rowSums(ScallopSurv[,11:50]) #all scallops, BIN_ID_0 to BIN_ID_195
ScallopSurv %>% 
  select(BIN_ID_0:BIN_ID_195) %>%
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
  select(BIN_ID_0:BIN_ID_195) %>%
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



# ------------------------------SURVEY DISTRIBUTION PLOTS (BoF, spa1a, spa1B, spa4/5)-------------------------------------------
# Breakout for each area

# ----BAY OF FUNDY (FULL EXPANSE PLOT)----

# SURVEY - Commercial Size >= 80 mm

#############NEW PLOT#################
##Setting up for plotting data contours... (re-vist another time for alternatives?)
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon","lat"), crs = 4326) #convert to sf
com.contours.sf <- ScallopSurv.sf %>%
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com)

#from original script using ScallopMap function and PBSMapping objects
com.contours <- contour.gen(subset(ScallopSurv,year==survey.year,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

#Convert PBSMapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly)
totCont.poly.sf <- totCont.poly.sf %>% 
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("MULTIPOLYGON") %>% #Convert multilines to polygons
  st_join(com.contours.sf[3]) %>% #combine with selected ScallopSurv data
  st_make_valid() %>% 
  st_buffer(0)

##########

#basemap
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = c(20, "c"), survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf, size = 0.2, fill = NA, color = "grey"), scale = list(scale = 'discrete', palette = viridis::viridis(100), breaks = c(1,5,10,50,100,200,300,400,500), limits = c(0,500), alpha = 0.8,leg.name = "Ted"))


p + 
  #geom_sf(data=totCont.poly.sf ,aes(fill= com), alpha = 0.8)+
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.1) +
  labs(title = "BoF Density (>= 80mm)",
       x = "Longitude",
       y = "Latitude") +
  theme_void() +
  theme(legend.position = "bottom")

###########OLD PLOT#############

xx <- 2019 #UPDATE 
com.contours <- contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data <- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()

ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Density (>= 80mm)',sep=" "),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=0.95, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# TEMPLATE TO AUTOMATE SAVING OF PLOTS (Changes to plot in code as well!)
#png (paste0('Y:/INSHORE SCALLOP/BoF/',xx,'/Figures/','ContPlot_BF_ComDensity',xx,'.png'),width=672,height=672,res=100)
#ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Density (>= 80mm)',sep=" "),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
#points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
#legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=0.95, bg='white')
#addPolys(Survey.poly,border='black',lwd=1, col="transparent")
#addPolys(spa4.poly,border='black',lwd=1, col="transparent")
#dev.off()

# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()

ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx, 'BoF Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()

ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# SURVEY BIOMASS - Commercial  Size >= 80 mm
xx <- 2019
com.contours <- contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()

ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Biomass (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off() 

# SURVEY BIOMASS - Recruit Size 65-79 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','rec.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()

ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Biomass (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off() 

# SURVEY BIOMASS - Prerecruit Size < 65 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','pre.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Biomass (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

##  CONDITION
xx <- 2019

#com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
com.contours <- contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnBu

windows()

ScallopMap('bof' ,bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Condition',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,con.dat,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Condition (g)",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# SURVEY MEAT COUNT - of commerical (>= 80mm) sized animals
xx <- 2019

mc.contours<-contour.gen(na.omit(subset(ScallopSurv.mtcnt,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Meat Count (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv.mtcnt,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# Spatial plot of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Clapper Density (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Clapper Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off() 

# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Clapper Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()  


### SURVEY - Clappers presented as a proportion of all live and dead animals:
###
live <- ScallopSurv[,c(1,2,52:59)]
dead <- ScallopSurv.dead[,c(1,2,52:59)]

prop.clappers <- merge(live,dead,by="ID")
prop.clappers$prop.dead.com <- prop.clappers$com.y/(prop.clappers$com.x+prop.clappers$com.y) #number of commercial sized clappers relative to total number of commercial size live and dead combined
prop.clappers$prop.dead.rec <- prop.clappers$rec.y/(prop.clappers$rec.x+prop.clappers$rec.y) #number of recruit sized clappers relative to total number of recruit size live and dead combined
prop.clappers$prop.dead.com[is.na(prop.clappers$prop.dead.com)] <- 0
prop.clappers$prop.dead.rec[is.na(prop.clappers$prop.dead.rec)]  <- 0
prop.clappers <- prop.clappers[,c(1:6,20:21)]
names(prop.clappers) <- c("ID", "CRUISE", "tow", "year", "lat", "lon", "prop.dead.com","prop.dead.rec")

#	write.csv(prop.clappers, file="Y:/INSHORE SCALLOP/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# Spatial plot of PROPORTION of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Clapper Proportion (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()

# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('bof',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'BoF Clapper Proportion (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

dev.off()


# -----------------------------SPA1A--------------------------------------------


# SURVEY - Commercial Size >= 80 mm
xx <- 2019 #UPDATE 
com.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Density (>= 80mm)',sep=" "),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=0.95, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx, 'spa1A Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY BIOMASS - Commercial  Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Biomass (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Recruit Size 65-79 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','rec.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Biomass (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Prerecruit Size < 65 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','pre.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Biomass (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


##  CONDITION
xx <- 2019

#com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnBu

windows()
ScallopMap( 'spa1A' ,bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Condition',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,con.dat,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Condition (g)",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY MEAT COUNT - of commerical (>= 80mm) sized animals
xx <- 2019

mc.contours<-contour.gen(na.omit(subset(ScallopSurv.mtcnt,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Meat Count (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv.mtcnt,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# Spatial plot of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Clapper Density (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Clapper Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Clapper Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


### SURVEY - Clappers presented as a proportion of all live and dead animals:
###
live <- ScallopSurv[,c(1,2,52:59)]
dead <- ScallopSurv.dead[,c(1,2,52:59)]

prop.clappers <- merge(live,dead,by="ID")
prop.clappers$prop.dead.com <- prop.clappers$com.y/(prop.clappers$com.x+prop.clappers$com.y) #number of commercial sized clappers relative to total number of commercial size live and dead combined
prop.clappers$prop.dead.rec <- prop.clappers$rec.y/(prop.clappers$rec.x+prop.clappers$rec.y) #number of recruit sized clappers relative to total number of recruit size live and dead combined
prop.clappers$prop.dead.com[is.na(prop.clappers$prop.dead.com)] <- 0
prop.clappers$prop.dead.rec[is.na(prop.clappers$prop.dead.rec)]  <- 0
prop.clappers <- prop.clappers[,c(1:6,20:21)]
names(prop.clappers) <- c("ID", "CRUISE", "tow", "year", "lat", "lon", "prop.dead.com","prop.dead.rec")

#	write.csv(prop.clappers, file="Y:/INSHORE SCALLOP/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# Spatial plot of PROPORTION of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Clapper Proportion (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1A',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1A Clapper Proportion (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# -------------------------------SPA1B------------------------------------------

# SURVEY - Commercial Size >= 80 mm
xx <- 2019 #UPDATE 
com.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)



lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Density (>= 80mm)',sep=" "),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=0.95, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx, 'spa1B Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY BIOMASS - Commercial  Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Biomass (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Recruit Size 65-79 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','rec.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Biomass (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Prerecruit Size < 65 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','pre.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Biomass (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


##  CONDITION
xx <- 2019

#com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnBu

windows()
ScallopMap( 'spa1B' ,bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Condition',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,con.dat,subset=year==xx,pch=16,cex=0.5)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Condition (g)",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY MEAT COUNT - of commerical (>= 80mm) sized animals
xx <- 2019

mc.contours<-contour.gen(na.omit(subset(ScallopSurv.mtcnt,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Meat Count (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv.mtcnt,subset=year==xx,pch=16,cex=0.5)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# Spatial plot of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Clapper Density (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Clapper Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Clapper Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


### SURVEY - Clappers presented as a proportion of all live and dead animals:
###
live <- ScallopSurv[,c(1,2,52:59)]
dead <- ScallopSurv.dead[,c(1,2,52:59)]

prop.clappers <- merge(live,dead,by="ID")
prop.clappers$prop.dead.com <- prop.clappers$com.y/(prop.clappers$com.x+prop.clappers$com.y) #number of commercial sized clappers relative to total number of commercial size live and dead combined
prop.clappers$prop.dead.rec <- prop.clappers$rec.y/(prop.clappers$rec.x+prop.clappers$rec.y) #number of recruit sized clappers relative to total number of recruit size live and dead combined
prop.clappers$prop.dead.com[is.na(prop.clappers$prop.dead.com)] <- 0
prop.clappers$prop.dead.rec[is.na(prop.clappers$prop.dead.rec)]  <- 0
prop.clappers <- prop.clappers[,c(1:6,20:21)]
names(prop.clappers) <- c("ID", "CRUISE", "tow", "year", "lat", "lon", "prop.dead.com","prop.dead.rec")

#	write.csv(prop.clappers, file="Y:/INSHORE SCALLOP/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# Spatial plot of PROPORTION of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Clapper Proportion (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa1B',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa1B Clapper Proportion (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("topleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# -------------------------------SPA4&5------------------------------------------

# SURVEY - Commercial Size >= 80 mm
xx <- 2019 #UPDATE 
com.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Density (>= 80mm)',sep=" "),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=0.95, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows(10,11)
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx, 'spa4 Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.001,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY BIOMASS - Commercial  Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Biomass (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Recruit Size 65-79 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','rec.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Biomass (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)

# SURVEY BIOMASS - Prerecruit Size < 65 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','pre.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Biomass (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


##  CONDITION
xx <- 2019

#com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnBu

windows()
ScallopMap( 'spa4' ,bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Condition',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,con.dat,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Condition (g)",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY MEAT COUNT - of commerical (>= 80mm) sized animals
xx <- 2019

mc.contours<-contour.gen(na.omit(subset(ScallopSurv.mtcnt,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'Meat Count (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='quick')
points(lat~lon,ScallopSurv.mtcnt,subset=year==xx,pch=16,cex=0.5)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# Spatial plot of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Clapper Density (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Clapper Density (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Prerecruit Size < 65 mm
xx <- 2019
pre.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Clapper Density (< 65mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


### SURVEY - Clappers presented as a proportion of all live and dead animals:
###
live <- ScallopSurv[,c(1,2,52:59)]
dead <- ScallopSurv.dead[,c(1,2,52:59)]

prop.clappers <- merge(live,dead,by="ID")
prop.clappers$prop.dead.com <- prop.clappers$com.y/(prop.clappers$com.x+prop.clappers$com.y) #number of commercial sized clappers relative to total number of commercial size live and dead combined
prop.clappers$prop.dead.rec <- prop.clappers$rec.y/(prop.clappers$rec.x+prop.clappers$rec.y) #number of recruit sized clappers relative to total number of recruit size live and dead combined
prop.clappers$prop.dead.com[is.na(prop.clappers$prop.dead.com)] <- 0
prop.clappers$prop.dead.rec[is.na(prop.clappers$prop.dead.rec)]  <- 0
prop.clappers <- prop.clappers[,c(1:6,20:21)]
names(prop.clappers) <- c("ID", "CRUISE", "tow", "year", "lat", "lon", "prop.dead.com","prop.dead.rec")

#	write.csv(prop.clappers, file="Y:/INSHORE SCALLOP/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# Spatial plot of PROPORTION of clappers #
# SURVEY - Commercial Size >= 80 mm
xx <- 2019
com.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Clapper Proportion (>= 80mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


# SURVEY - Recruit Size 65-79 mm
xx <- 2019
rec.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

windows()
ScallopMap('spa4',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste(xx,'spa4 Clapper Proportion (65-79mm)',sep=" "),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
addPolys(Survey.poly,border='black',lwd=1)
addPolys(spa4.poly,border='black',lwd=1)


########################################################################################################################################

# --------------PLOTS FOR ALL BOF - PREREC NUMBERS, REC NUMBERS, COM BIOMASS - USED IN UPDATE DOCUMENT ONLY------------------------------

##read in data for count - rec and prerec figure

##.. LIVE ..##
#Db Query:
#ScallopSurv.all <- sqlQuery(RODBCconn,
#	                           "SELECT *
#	                           FROM scallsur.scliveres")

#ROracle: 
ScallopSurv.all <- dbGetQuery(chan, "SELECT * FROM scallsur.scliveres")	

ScallopSurv.all$year <- as.numeric(substr(ScallopSurv.all$CRUISE,3,6)) # add year column to SHF data. NOTE: this would NOT work for SFA29 cruises - would need to be ..,6,9)!!!

summary (ScallopSurv.all) #check data
#Once data imported, convert to DD
ScallopSurv.all$lat <- convert.dd.dddd(ScallopSurv.all$START_LAT)
ScallopSurv.all$lon <- convert.dd.dddd(ScallopSurv.all$START_LONG)

names(ScallopSurv.all)[2] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv.all$tot <- rowSums(ScallopSurv.all[,11:50]) #all scallops
ScallopSurv.all$tot <- ScallopSurv.all$tot/ 4267.2 # standardize number per tow to numbers per m^2
attr(ScallopSurv.all, "projection") #check default projection of data
attr(ScallopSurv.all, "projection") <- "LL" # assign projection for data
ScallopSurv.all$ID <- paste(ScallopSurv.all$CRUISE, ScallopSurv.all$tow, sep=".")

#create pre-rec, rec, comm fields:
ScallopSurv.all$com <- apply(ScallopSurv.all[,27:50],1,sum) #>=80mm; BINS 80 to 195
ScallopSurv.all$rec <- apply(ScallopSurv.all[,24:26],1,sum) #65-79; BINS 65 to 75
ScallopSurv.all$pre <- apply(ScallopSurv.all[,11:23],1,sum) #0-64 mm; BINS 0 to 60


### Pre-rec Survey Distribution (0-64 mm) ###

com.contours<-contour.gen(subset(ScallopSurv.all,year==2019,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

#ylim=c(43.65,45.6),xlim=c(-67.2,-64.35) #USE THESE PLOT LIMS IN ScallopMap

CL <- contourLines(com.contours$image.dat,levels=lvls)
CP <- convCP(CL)
BoFcont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors=FALSE)

tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_prerec.tiff"), width = 11, height = 11, units = 'in', res = 300)

#windows()
ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF PreRec Density (< 65mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.all,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
addLines(inVMS, col='black',lwd=1, lty=2)
# May want to use red instead of black lines in plots (better clarity from tow positions)
#addLines(VMSpoly,col='red',lwd=1)
#addLines(inVMS, col='red',lwd=1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()


tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_prerec_FR.tiff"), width = 11, height = 11, units = 'in', res = 300)

#windows()
ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF PreRec Density (< 65mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.all,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
addLines(inVMS, col='black',lwd=1, lty=2)
# May want to use red instead of black lines in plots (better clarity from tow positions)
#addLines(VMSpoly,col='red',lwd=1)
#addLines(inVMS, col='red',lwd=1)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Nombre par trait",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()


### RECRUIT Survey Distribution(0-64 mm) ###

com.contours <- contour.gen(subset(ScallopSurv.all,year==2019,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500)

CL <- contourLines(com.contours$image.dat,levels=lvls)
CP <- convCP(CL)
BoFcont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors=FALSE)

tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_rec.tiff"), width = 11, height = 11, units = 'in', res = 300)

ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF Rec Density (< 65mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.all,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
#addLines(spa6Poly, border='black', lwd=1)
addLines(inVMS, col='black',lwd=1, lty=2)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()


tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_rec_FR.tiff"), width = 11, height = 11, units = 'in', res = 300)

ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF Rec Density (< 65mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.all,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
#addLines(spa6Poly, border='black', lwd=1)
addLines(inVMS, col='black',lwd=1, lty=2)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Nombre par trait",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()

### COMMERCIAL BIOMASS Survey Distribution (>=80mm in KG) ###

#merge liveweight files external to R and read in
#all.kg<-read.csv("Y:/INSHORE SCALLOP/BoF/dataoutput/All_liveweight2017.csv") ## DONE AS SEEN BELOW CURRENTLY (Merge files in R)

#read in separate liveweight files and merge for plotting

all.kg <- NULL

# Make a list with each years data in it, extract it as needed later
temp.1 <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",surveyyear,".csv",sep="") ,header=T)
temp.2 <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",surveyyear,".csv",sep="") ,header=T)
temp.3 <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",surveyyear,".csv",sep="") ,header=T)
all.kg <- rbind(temp.1,temp.2,temp.3)

#check data structure
summary(all.kg)


BF <- read.csv("Y:/INSHORE SCALLOP/BoF/dataoutput/BFliveweight2019.csv")
BI <- read.csv("Y:/INSHORE SCALLOP/BoF/dataoutput/BIliveweight2019.csv")
GM <- read.csv("Y:/INSHORE SCALLOP/BoF/dataoutput/GMliveweight2019.csv")

temp <- rbind(BF, BI)
all.kg <- rbind(temp, GM)

summary(all.kg)

ScallopSurv.kg <- all.kg
ScallopSurv.kg$year <- ScallopSurv.kg$YEAR

#Once data imported, convert to DD
ScallopSurv.kg$lat <- convert.dd.dddd(ScallopSurv.kg$START_LAT)
ScallopSurv.kg$lon <- convert.dd.dddd(ScallopSurv.kg$START_LONG)

names(ScallopSurv.kg)[4] <- c("tow") #change 'TOW_NO' to 'tow'
ScallopSurv.kg$tot <- rowSums(ScallopSurv.kg[,13:52]) #all scallops
ScallopSurv.kg$tot <- ScallopSurv.kg$tot/ 4267.2 # standardize number per tow to numbers per m^2
attr(ScallopSurv.kg, "projection") #check default projection of data
attr(ScallopSurv.kg, "projection") <- "LL" # assign projection for data
ScallopSurv.kg$ID <- paste(ScallopSurv.kg$CRUISE, ScallopSurv.kg$tow, sep=".")

#create pre-rec, rec, comm fields:
ScallopSurv.kg$com.bm <- apply(ScallopSurv.kg[,29:52],1,sum)/1000 #>=80mm; BINS 80 to 195
ScallopSurv.kg$rec.bm <- apply(ScallopSurv.kg[,26:28],1,sum)/1000 #65-79; BINS 65 to 75
ScallopSurv.kg$pre.bm <- apply(ScallopSurv.kg[,13:25],1,sum)/1000 #0-64 mm; BINS 0 to 60

com.contours <- contour.gen(subset(ScallopSurv.kg,year==2019,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)

CL <- contourLines(com.contours$image.dat,levels=lvls)
CP <- convCP(CL)
BoFcont.poly <- CP$PolySet
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors=FALSE)

tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_ComBiomass.tiff"), width = 11, height = 11, units = 'in', res = 300)

ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF Commercial Biomass'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
#addLines(spa6Poly, border='black', lwd=1)
addLines(inVMS, col='black',lwd=1, lty=2)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()


tiff(paste0(path.directory,assessmentyear, "/Assessment/Figures/ContPlot_BoFAll_ComBiomass_FR.tiff"), width = 11, height = 11, units = 'in', res = 300)

ScallopMap(ylim=c(43.65,45.6),xlim=c(-67.2,-64.35),bathcol=rgb(0,0,1,0.1),contour=list(BoFcont.poly,cont.data),title=paste('2019 BoF Commercial Biomass'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
points(lat~lon,ScallopSurv.kg,subset=year==2019,pch=16,cex=0.4)
addPolys(Survey.poly,border='black',lwd=1, col="transparent")
addLines(spa4.poly,border='black',lwd=1)
addLines(BILU.poly,border='black',lwd=1)
addLines(VMSpoly,col='black',lwd=1, lty=2)
#addLines(spa6Poly, border='black', lwd=1)
addLines(inVMS, col='black',lwd=1, lty=2)
legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg par trait",inset=0.02,bty='o',box.col='white',bg='white')

dev.off()

########################################################################################################################################
#---- STATIC BoF strata plot for appendix by F.Keyser November 2017 #----
require(ggplot2)
require(rgdal)
require(sp)
require(raster)
require(plyr)
maritimes_sp <- readOGR("Y:/Maps/shp/Maritimes_UTM.shp")
maritimes_sp <- spTransform(maritimes_sp, CRS("+proj=longlat +datum=WGS84"))
maritimes_sp <- crop(maritimes_sp, extent(-67.35, -62.9, 43.35, 46.2))
maritimes <- fortify(maritimes_sp)

scstrata <- read.csv("Y:/INSHORE SCALLOP/Survey/2017/data entry templates and examples/entry check functions/SCSTRATAINFO_August2017.csv")
names(scstrata) <- c("PID", "POS", "X", "Y", "DESCRIPTION", "AREA")
scstrata <- dplyr::arrange(scstrata, PID, POS)

# Remove Offshore banks
scstrata <- scstrata[!scstrata$DESCRIPTION %in% c("NE BANK", "SW BANK"),]

# All folowing code setting positional information for labels
scstratalabs <- aggregate(data=scstrata, cbind(X, Y) ~ DESCRIPTION, function(x) mean(x))
scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("Cape Spencer", "ANNAPOLIS BASIN (BA)", "MidBay North", "SPA 6C", "SPA 1 WEST A",
                                               "SCOTS BAY")] <- scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("Cape Spencer", "ANNAPOLIS BASIN (BA)", "MidBay North", "SPA 6C", "SPA 1 WEST A",
                                                                                                               "SCOTS BAY")] - 0.07
scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("SPA 1 WEST B")] <- scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("SPA 1 WEST B")] + 0.1
scstratalabs$X[scstratalabs$DESCRIPTION %in% c("BRIER ISLAND (BI)", "LURCHER (LU)")] <- scstratalabs$X[scstratalabs$DESCRIPTION %in% c("BRIER ISLAND (BI)", "LURCHER (LU)")] - 0.1

# Removes brackets after description (eg ANNAPOLIS BASIN (BA))
scstratalabs$DESCRIPTION <- gsub(scstratalabs$DESCRIPTION, pattern="\\s*\\([^\\)]+\\)", replacement="")
scstratalabs$DESCRIPTION <- toupper(scstratalabs$DESCRIPTION)

scstrataman <- data.frame(DESCRIPTION=c("2-8 MILES", "8-16 MILES"), X=c(-65.77, -65.85094), Y=c(44.82, 44.9))

##UPDATE SAVE LOCATION BEFORE PROCEEDING!!!!!!!!!
png("Y:/INSHORE SCALLOP/BoF/2017/Figures/ContPlot_BF_Strata2017.png", height=10, width=10, units="in", res=200)
ggplot() + 
  geom_polygon(data=scstrata, aes(X, Y, group=PID), fill=NA, colour="black") +
  geom_polygon(data=VMSpoly, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=BILU.poly, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=inVMS, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=outvms, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=maritimes, aes(long, lat, group=group), fill="wheat", colour="black") +
  coord_map(xlim=c(-67.25, -64.3), ylim=c(43.65, 45.6)) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  geom_label(data=scstratalabs[c(11,12,23:47),], aes(X, Y, label=DESCRIPTION), size=3, alpha=0.8) +
  geom_text(data=scstrataman, aes(X, Y, label=DESCRIPTION), alpha=0.8, angle=34, size=5) +
  xlab("LONGITUDE") + ylab("LATITUDE")
dev.off()

### Can make plot in ScallopMap instead of ggplot2 ***NOT TESTED OR ADVISED
# ScallopMap(area='custom', ylim=c(43.65,45.6), xlim=c(-67.25,-64.3),bathcol=rgb(0,0,1,0.1),plot.strata=F, plot.boundries = F,plot.bathy=T, bathy.source='usgs')
# addLines(scstrata,border='black',lwd=1)
# # addPolys(Survey.poly,border='black',lwd=1)
# # addPolys(spa4.poly,border='black',lwd=1)
# # addPolys(spa3.poly,border='black',lwd=1)
# # addPolys(BILU.poly,border='black',lwd=1)
# # addPolys(VMSpoly,border='black',lwd=1, lty="dashed")
# # addPolys(spa6Poly,border='black',lwd=1)
# # addPolys(inVMS,border='black',lwd=1, lty="dashed")
# # addPolys(outvms,border='black',lwd=1, lty="dashed")
# addLabels(scstrata$DESCRIPTION,placement="CENTROID",cex=1.2,col=2,font=2)
# text(labels = scstratalabs_full$DESCRIPTION, x=scstratalabs_full$X, y=scstratalabs_full$Y, cex = 0.5, offset=0.5, adj=c(1,2))
# text(dams, labels=as.character(dams$DAM_NAME), col="darkred",
#      cex=0.6, font=2, offset=0.5, adj=c(0,2))
# dev.off()
# # BoF.Survpoly <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/bofPoly.csv") #use for survey plots
# # Survey.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SCSTRATADEFS.csv")
# # spa4.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa4stratadefs.csv")
# # spa3.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa3Poly.csv")
# # BILU.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BILUpoly.csv")
# # VMSpoly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
# # spa6Poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84.csv")
# # inVMS<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_IN_R_final_MOD.csv")
# # outvms<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_OUT_R_final_MOD.csv")

########################################################################################################################################