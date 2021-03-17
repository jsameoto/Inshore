###..............................###
### Spatial Figures - Survey Data ###
###    Full BoF and Approaches   ###
### J.Sameoto Nov 2017, Oct 2018 ###
###B.Wilson - Overhauled Dec 2020###
###        (on Github)           ###
###..............................###

# Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count for BoF: 
#Full Bay
#SPA 1A
#SPA1B
#SPA 3
#SPA4 and 5
#and SPA 6

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
#library(RCurl)



# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- un.bwilson
pwd <- pw.bwilson

#set year 
survey.year <- 2019  #removed maxyear in script and changed to survey year
assessmentyear <- 2020 #year in which you are conducting the survey 
#area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/INSHORE SCALLOP/BoF/"

#set up directory to save plot
saveplot.dir <- "C:/Users/WILSONB/Documents/1_Inshore_Scallop/FigureTest/" #Directory to save plots to #paste0("Y:/INSHORE SCALLOP/BoF/",assessmentyear,"/Assessment/Figures")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')



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



#Calculating proportion of clappers:
live <- ScallopSurv %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

dead <- ScallopSurv.dead %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

prop.clappers <- merge(live,dead,by="ID") %>% 
  mutate(prop.dead.com = com.y/(com.x + com.y)) %>% #number of commercial sized clappers relative to total number of commercial size live and dead combined
  mutate(prop.dead.rec = rec.y/(rec.x + rec.y)) %>% #number of recruit sized clappers relative to total number of recruit size live and dead combined
  mutate_at(vars(prop.dead.com:prop.dead.rec), ~replace(., is.na(.), 0)) %>% 
  dplyr::select(ID, CRUISE = CRUISE.x, STRATA_ID = STRATA_ID.x, tow = tow.x, year = year.x, lat = lat.x, lon = lon.x, prop.dead.com, prop.dead.rec)

#	write.csv(prop.clappers, file="Y:/INSHORE SCALLOP/BoF/dataoutput/PropClappers.csv")  #Export if needed 

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

#SPA1A1B4and5
BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep=""), header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#SPA3
BIliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",Year[i],".csv",sep=""), header=T)
  BIliveweight <- rbind(BIliveweight,temp)
}

#SPA6
GMliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",Year[i],".csv",sep=""), header=T)
  GMliveweight <- rbind(GMliveweight,temp)
}

liveweight <- rbind(BFliveweight, if(exists("BIliveweight")) BIliveweight, if(exists("GMliveweight")) GMliveweight) #Combine SPA condition data together if data is available


#check data
head(liveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commerical, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000
  
# --------------------------------Load Condition data for survey year -----------------------------------------

#Year <- seq(2015,survey.year)
#num.years <- length(Year)

#SPA1A1B4and5
#BF.con.dat <- NULL
#for(i in 1:num.years)
#{
#   Make a list with each years data in it, extract it as needed later
#  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFConditionForMap",Year[i],".csv",sep="") ,header=T)
#  BF.con.dat <- rbind(BF.con.dat,temp)
#}

#SPA1A1B4and5
BF.con.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFConditionForMap",survey.year,".csv",sep="") ,header=T) %>% mutate(CRUISE = paste0("BF", survey.year))#Add Cruise information

#SPA3
BI.con.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIConditionForMap",survey.year,".csv",sep="") ,header=T)%>% mutate(CRUISE = paste0("BI", survey.year))#Add Cruise information

#SPA6
GM.con.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMConditionForMap",survey.year,".csv",sep="") ,header=T) %>% mutate(CRUISE = paste0("GM", survey.year))#Add Cruise information

con.dat <- rbind(BF.con.dat, if(exists("BI.con.dat")) BI.con.dat, if(exists("GM.con.dat")) GM.con.dat) #Combine SPA condition data together if data is available

#check data structure
head(con.dat)
table(con.dat$CRUISE)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), and ID (cruise.tow#) columns
con.dat <- con.dat %>%
  rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>%  #Convert to DD
  rename(tow = TOW_NO) %>% 
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) #No CRUISE column in con.dat - ID column is required for contour.gen function

# For Meat Count plot:
ScallopSurv.mtcnt <- ScallopSurv.kg %>% 
  dplyr::select(ID, STRATA_ID, year, lat, lon, com.bm) %>% 
  merge(ScallopSurv %>% dplyr::select(ID, com), by = "ID") %>% 
  mutate(meat.count = (0.5/(com.bm/com))) %>% 
  filter(!is.na(meat.count))

#Set plot themes (legend orientation/aesthetics)
#Set legend format for plots
plot.theme <-  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                     axis.title = element_text(size = 12),
                     axis.text = element_text(size = 10),
                     legend.title = element_text(size = 10, face = "bold"), 
                     legend.text = element_text(size = 8),
                     legend.position = c(.87,.32), #legend position
                     legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                     legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

plot.theme.1b <-  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                        axis.title = element_text(size = 12),
                        axis.text = element_text(size = 10),
                        legend.title = element_text(size = 10, face = "bold"), 
                        legend.text = element_text(size = 8),
                        legend.direction="horizontal",
                        legend.position = c(.72, .09),#legend position
                        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                        legend.box.margin = margin(1, 1, 1, 1))

plot.theme.3 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.87,.42), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))

plot.theme.4 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      #legend.direction="horizontal",
                      legend.position = c(.90,.22), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)

plot.theme.6 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.10,.70), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))


# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

#Create contour and specify plot aesthetics
com.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                                       dplyr::select(ID, lon, lat, com), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector


# ----DENSITY PLOTS -----

#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))



p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4
  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----
#Create contour and specify plot aesthetics
com.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, com), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(ScallopSurv %>% filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, com), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%                                     mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----

com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Set aesthetics for plot

labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(ScallopSurv.kg %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, com.bm), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS-----

com.contours <- contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BF2019") %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Condition"), x = "Longitude",
       y = "Latitude") + 
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

com.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "GM2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "GM2019"),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Condition"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BI2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BI2019"), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Condition"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# ----MEAT COUNT -----

mc.contours<-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                           dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)

#Set aesthetics for plot
n.breaks <- length(unique(totCont.poly.sf$col)) 
col <- rev(brewer.pal(length(lvls),"Spectral")) #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.45), name = expression(frac(Scallops,"500g"))) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BOF Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year,  !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

mc.contours<-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                           dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Meat Count (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
mc.contours <- contour.gen(ScallopSurv.mtcnt %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% 
                              dplyr::select(ID, lon, lat, meat.count, -STRATA_ID, -year), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',
                            blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>%
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Meat Count (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

com.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), 
                                scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

com.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

com.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1a",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1b",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

com.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))



p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours<-contour.gen(prop.clappers %>%  filter(year==survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #Only SPA3
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)


lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

#Create contour and specify plot aesthetics
rec.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                              dplyr::select(ID, lon, lat, rec), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----DENSITY PLOTS -----

#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----
#Create contour and specify plot aesthetics
rec.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, rec), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)



lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))



p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
rec.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, rec), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)



lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----

rec.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,rec.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Set aesthetics for plot

labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

rec.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,rec.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
rec.contours <- contour.gen(ScallopSurv.kg %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, rec.bm), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS-----

rec.contours <- contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BF2019") %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Condition"), x = "Longitude",
       y = "Latitude") + 
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

rec.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "GM2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "GM2019"),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Condition"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
rec.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BI2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BI2019"), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Condition"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

rec.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), 
                                scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                                                              mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                                                              dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

rec.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

rec.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1a",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1b",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

rec.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))



p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

#Create contour and specify plot aesthetics
rec.contours<-contour.gen(prop.clappers %>%  filter(year==survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #Only SPA3
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)


lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

#Create contour and specify plot aesthetics
pre.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                              dplyr::select(ID, lon, lat, pre), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----DENSITY PLOTS -----

#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----
#Create contour and specify plot aesthetics
pre.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, pre), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)



lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))



p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
pre.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, pre), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)



lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----

pre.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,pre.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Set aesthetics for plot

labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

pre.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,pre.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
pre.contours <- contour.gen(ScallopSurv.kg %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, pre.bm), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS-----

pre.contours <- contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BF2019") %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Condition"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BF2019"), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Condition"), x = "Longitude",
       y = "Latitude") + 
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

pre.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "GM2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "GM2019"),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Condition"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
pre.contours<-contour.gen(con.dat %>% filter(year==survey.year, CRUISE == "BI2019") %>% #Only SPA6
                            dplyr::select(ID,lon,lat,Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlOrBr"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == survey.year, CRUISE == "BI2019"), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Condition"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

pre.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), 
                                scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                                                              mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                                                              dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

pre.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

pre.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                            dplyr::select(ID,lon,lat,prop.dead.pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1a",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1b",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

pre.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, STRATA_ID %in% c(26,30,31,32,54,57)) %>% #Only SPA6
                            dplyr::select(ID,lon,lat,prop.dead.pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))



p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(26,30,31,32,54,57)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA6 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

#Create contour and specify plot aesthetics
pre.contours<-contour.gen(prop.clappers %>%  filter(year==survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #Only SPA3
                            dplyr::select(ID,lon,lat,prop.dead.pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)


lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(unique(CP$PolyData$level)),"YlGn"), level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA3 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)