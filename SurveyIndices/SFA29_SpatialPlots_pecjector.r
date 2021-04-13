
###................................................................................###
###                               Spatial Figures                                  ###
###                           SFA 29 2015 Assessment                               ###
###                             J.Sameoto Jan 2016                                 ###
###                     B.Wilson - Overhauled Mar 2021                             ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###


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

survey.year <- 2018  #This is the last survey year 
assessmentyear <- 2019 #year in which you are conducting the survey
cruise <- "'SFA292018'"

#for multiple cruises:
#cruise <- c('SFA292018','SFA292019') 
#cruise <- paste(cruise,collapse="','")

# get the year set up for later....  This should be set as the year of the assessment (e.g. 2017 survey is 2018 assessment)
#yr <- year(Sys.Date())
#yr.crnt <- yr-1
#years <- c(2001:(yr-1))

path.directory <- "Y:/INSHORE SCALLOP/SFA29/"

#set up directory to save plot
saveplot.dir <- "C:/Users/WILSONB/Documents/1_Inshore_Scallop/FigureTest/" #Directory to save plots to #paste0("Y:/INSHORE SCALLOP/SFA29/",assessmentyear,"/figures")


#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# source R functions
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")
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

#import SFA 29W boundaries for plots (sf objects)
sfa29.poly <- st_read("Y:/INSHORE SCALLOP/Databases/Scallsur/SFA29BottomTypes/SFA29_shp/SFA29_subareas_utm19N.shp") %>% 
  st_transform(crs = 4326)
	
sfa29strata <- st_read("Y:/INSHORE SCALLOP/Databases/Scallsur/SFA29BottomTypes/SFA29_shp/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp")

#Set working directory #
setwd(paste0(path.directory,"/",assessmentyear))

## Import data ##
## Survey Numbers Data ##
#sfa29surv.dat <- read.csv("dataoutput/SHFCF.sfa29.2014.csv") #flat file import option
#sfa29surv.dat <- sfa29surv.dat[,2:dim(sfa29surv.dat)[2]]

#Cruise List	
#cruise.list <- c('SFA292001', 'SFA292002','SFA292003','SFA292004','SFA292005','SFA292006',
#		'SFA292007','SFA292008', 'SFA292009' ,'SFA292010','SFA292011','SFA292012','SFA292013','SFA292014', 'SFA292015',
#		'SFA292016', 'SFA292017') # Must be UPDATED for Current Year! # 
	
	# -----------------------------Import SHF data (live and dead)--------------------------------------------
	
	# Import SHF data:
	
	##.. LIVE ..##
  #Db Query:
  quer1 <- paste(
    "SELECT *                                     ",
    "FROM scallsur.SCLIVERES                      ",                                                                                                                                  
	  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",								  
    "AND CRUISE =",cruise,
    sep=""
  )
	
#ROracle: 
ScallopSurv <- dbGetQuery(chan, quer1)
#ScallopSurv  <- ScallopSurv[,1:51] %>% #Remove duplicate column and filter Cruise SFA29 for survey year
  #filter(grepl(paste0("SFA29",survey.year), CRUISE, fixed = TRUE))

ScallopSurv <- ScallopSurv %>%
  mutate(year = as.numeric(substr(ScallopSurv$CRUISE,6,9))) %>% 
  #mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - 0-85 mm; BINS 0 to 85

	
##.. DEAD ..##
#Db Query:
quer2 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.SCDEADRES                      ",                                                                                                                                   
  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",								  
  "AND CRUISE =",cruise,
  sep=""
)

ScallopSurv.dead <- dbGetQuery(chan, quer2)


#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commerical, recruit and prerecruit data columns

ScallopSurv.dead <- ScallopSurv.dead %>% 
  mutate(year = as.numeric(substr(ScallopSurv$CRUISE,6,9))) %>%
  #mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - 0-85 mm; BINS 0 to 85


# -------------------------------Import WGTHGT DATA------------------------------------------
quer3 <- paste(
  "SELECT CRUISE, TOW_NO 			                ",
  "FROM SCALLSUR.scwgthgt s			",
  " where s.strata_id in (41, 42, 43, 44, 45)    ",
  " GROUP BY CRUISE, TOW_NO",
  sep=""
)

sampled.dat <- dbGetQuery(chan, quer3)
	
# --------------------------------Import Biomass per tow data-----------------------------------------
# Current year only
ScallopSurv.kg <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/",assessmentyear,"/dataoutput/SFA29liveweight",survey.year,".csv"))  #DEFINE
# 2014 to current year
ScallopSurv.kg <- ScallopSurv.kg %>% dplyr::select(-X) %>%  #removes index column X
	dplyr::rename(year = YEAR) %>%
	mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
	mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
	unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>% 
  mutate(com.bm = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec.bm = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0)/1000)# Pre-recruit scallop - 0-85 mm; BINS 0 to 85

# --------------------------------Load Condition data for survey year -----------------------------------------
	
con.dat <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/2019/dataoutput/ConditionforMap",survey.year,".csv")) #NOTE: 2014 file made by Jessica (..._JS.csv), will be slightly different than for 2015 assessment since Stephen calculated this using smaller dataset for mtwt sh model 
con.dat <- con.dat %>% dplyr::select(-X) %>%  #removes index column X
	 dplyr::rename(year = YEAR) %>%
	 mutate(CRUISE = paste0("SFA29", year)) %>% 
	 mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
	 mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
	 unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)
	
	
# For Meat Count plot:
ScallopSurv.mtcnt <- ScallopSurv.kg %>% 
	 dplyr::select(ID, year, lat, lon, com.bm) %>% 
	 merge(ScallopSurv %>% dplyr::select(ID, com), by = "ID") %>% 
	 mutate(meat.count = (0.5/(com.bm/com))) %>% 
	 filter(!is.na(meat.count))
	
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

#write.csv(prop.clappers,"dataoutput/SFA29_PropClappersbyTow.csv")

	
#Set plot themes (legend orientation/aesthetics)
#Set legend format for plots
plot.theme <-   theme(legend.key.size = unit(5,"mm"),
                      plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.90,.83), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(2, 3, 2, 3))


		
# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOT -----

com.contours <- contour.gen(ScallopSurv %>% filter(year == survey.year) %>% dplyr::select(ID, lon, lat, com), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
#sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.3), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (>= 100mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

#FR
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (>= 100mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.05", "0.05-0.1", "0.1-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.
#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Biomass (>= 100mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----CONDITION PLOT-----

com.contours <- contour.gen(con.dat %>% filter(year==survey.year) %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
#lvls=c(4,6,8,10,12,14,16) # large scale
#lvls=c(6,7,8,9,10,11,12,13) # good condition scale # update based on values observed


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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Condition"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT -----

mc.contours<-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year) %>% dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls <- c(10,15,20,25,30,34,40,45)
#lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
#cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45+")
col <- rev(brewer.pal(length(lvls),"Spectral")) #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.45), breaks = labels, name = expression(frac(Scallops,"500g"), limits = labels, drop = FALSE)) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Meat Count (>= 100"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----CLAPPERS -----

com.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year) %>%
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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year),aes(lon, lat), size = 0.5)+
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Density (>= 100mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----PROPORTION OF CLAPPERS -----

com.contours<-contour.gen(prop.clappers %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.10,0.20,0.30,0.5,0.6,0.8,1) #levels to be color coded

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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% 
  mutate(level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.10", "0.10-0.20", "0.20-0.30", "0.30-0.40", "0.40-0.50","0.50-0.60","0.60-0.80", "0.8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Proportion (>= 100mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOT -----

rec.contours<-contour.gen(ScallopSurv %>% filter(year == survey.year) %>% dplyr::select(ID, lon, lat, rec),
                          ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
#sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.5), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (90-99 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

#FR
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (90-99 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

rec.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,rec.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.05", "0.05-0.1", "0.1-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.
#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Biomass (90-99 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----CLAPPERS -----

rec.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year) %>%
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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Density (90-99 mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----PROPORTION OF CLAPPERS -----

rec.contours<-contour.gen(prop.clappers %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.10,0.20,0.30,0.5,0.6,0.8) #levels to be color coded

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
  mutate(level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.10", "0.10-0.20", "0.20-0.30", "0.30-0.40", "0.40-0.50","0.50-0.60","0.60-0.80", "0.8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Proportion (90-99mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOT -----

pre.contours<-contour.gen(ScallopSurv %>% filter(year == survey.year) %>% dplyr::select(ID, lon, lat, pre),
                          ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet
#sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.5), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (< 90 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

#FR
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Density (< 90 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----

pre.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,pre.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.05", "0.05-0.1", "0.1-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.
#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Biomass (< 90 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

pre.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year) %>%
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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year),aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Density (< 90 mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 10, height = 10, dpi = 300, units = "cm", limitsize = TRUE)

# ----PROPORTION OF CLAPPERS -----

pre.contours<-contour.gen(prop.clappers %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,prop.dead.pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.10,0.20,0.30,0.5,0.6,0.8, 1) #levels to be color coded

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
  mutate(level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.10", "0.10-0.20", "0.20-0.30", "0.30-0.40", "0.40-0.50","0.50-0.60","0.60-0.80", "0.80-1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('tl',0.5)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% filter(year == survey.year),aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "SFA29 Clapper Proportion (< 90 mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PropPreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

			
###...................###
### Plot CPUE in grid ###
###...................###

	## Import and prepare Log Data ##

	#log.2001to2011 <- read.csv(paste0(path.directory, assessmentyear, "/logs/logData_2002to2011_sfa29.csv"))
	#log.2012 <- read.csv(paste0(path.directory, assessmentyear, "/logs/logsSFA29_2012corrected.csv"))
	#log.2013 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogSFA29_2013.csv"))
	#log.2014 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogsSFA29_2014.csv")) #Fr db
	#log.2015 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogsSFA29_2015.csv")) #Fr db
	#log.2016 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs2016.csv"))
	# This includes the late season (October) FSC catch
	#log.2017 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs2017_dwnld_Feb2018_JS.csv"))
	
log.present <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs_", survey.year, ".csv"))
	#log.2019 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs_2019.csv"))
	
	# add YEAR and AREA columns for data years selected directly from the SCALLOP database
	#is using log data directly from SCALLOP database it's assumed that assigned_ared has been QA/QC'd and this is used as the AREA field
log.present <- log.present %>% 
  mutate(YEAR = as.numeric(substr(log.present$DATE_FISHED,1,4))) %>%  #assuming character and in format 'YYYY-XX-XX' or "YYYY/XX/XX'
	rename(AREA = ASSIGNED_AREA) %>% 
  mutate(DDSlat = convert.dd.dddd(LATITUDE)) %>% 
	mutate(DDSlon = convert.dd.dddd(LONGITUDE)) %>%
	mutate(ID = 1:nrow(log.present))

	#Filter out areas for privacy considerations (min 5 trips per area to include in presentation)
	
log.2018_priv <- log.present %>%
	 group_by(AREA) %>% 
	 filter(!n() <=5) %>% #Filter out any areas within the dataset that have less than 5
	 ungroup() %>% 
	 dplyr::select(ID, DDSlon, DDSlat, CPUE_KG)

lvls=seq(5,40,5)  #CPUE levels from 5 to 40 kg/h
lvls=seq(5,110,15)  #CPUE levels from 5 to 110 kg/h
	
log.2018.priv.sf <- st_as_sf(log.2018_priv, coords = c("DDSlon", "DDSlat"), crs = 4326)
	
hex <- st_make_grid(log.2018.priv.sf, cellsize=10, square=FALSE) %>%
	  st_sf() %>%
	  rowid_to_column('hex_id')
	plot(hex)
	
plot(log.2018.priv.sf)
	## assign year, select data, plot ##
	#xx <- 2019
	# For 2015: was subarea D closure line: 
	#Dclosure <- read.csv('Y:/INSHORE SCALLOP/SFA29/2016/Fishery/2015SubareaDClosureLine/SubareaDClosureLine2015.csv')
	#attr(Dclosure,"projection") <- "LL"
	
	## Remove B fishing from plot in 2018 due to privacy considerations
	#log.2018_noB <- log.2018 [!log.2018$ASSIGNED_AREA == "29B",]
	#gridlog<-subset(log.2018_noB, YEAR==xx, c('ID','DDSlon','DDSlat','CPUE_KG'))

	## Remove E fishing from plot in 2019 due to privacy considerations
	#log.2019_PA <- log.2019 [!log.2019$ASSIGNED_AREA == "29E",]
	#gridlog<-subset(log.2019_PA, YEAR==xx, c('ID','DDSlon','DDSlat','CPUE_KG'))

	
	#lvls=seq(5,40,5)  #CPUE levels from 5 to 40 kg/h
	#lvls=seq(5,110,15)  #CPUE levels from 5 to 110 kg/h
	
	cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGnBu"),border=NA,stringsAsFactors=FALSE)
windows()
	#png("figures/SFA29_CPUEgridplot2019.png",9,9,res=200,units='in') #NOTE: Not printing correctly to png, save from window
	ScallopMap('sfa29',bathcol=rgb(0,0,1,0.3),poly.lst=gridPlot(gridlog,sfa29.poly,lvls),title=paste(xx), plot.boundries = F,plot.bathy=T, bathy.source='usgs')
	legend("topright", c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/h",inset=0.01,bty='n',box.col='white', cex=0.9)
	addPolys(sfa29strata, border="black")
	text (-66.42, 43.62, "A", cex=2)   #labels for management subareas, position may need to be adjusted, or text colour can be faded
	text (-66.2, 43.58, "B", cex=2)
	text (-65.95, 43.28, "C", cex=2)
	text (-65.6, 43.31, "D", cex=2)
	text (-66.26, 43.35, "E", cex=2)
	#dev.off()
	
	
## Extra plots	(NOT REQUIRED)
	### Make histogram by area for distribution of cpue in 2017
	library (ggplot2)
	histolog<-subset(log.2018, YEAR==xx, c('AREA','CPUE_KG'))
	histolog <- subset (histolog, !AREA == '29B')
	
	cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) + 
	  geom_histogram (breaks = seq (0, 300, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
	  facet_wrap (~AREA, ncol = 1) +
	  scale_x_continuous(breaks = seq (0, 300, by = 50)) +
	  theme_bw() +
	  theme(panel.grid=element_blank()) 
	cpuehisto

## Plot catch rate by date for all areas 
	depdat <- subset (log.2018, YEAR==xx, c('AREA','CPUE_KG', 'DATE_FISHED'))
	depdat <- subset (depdat, !AREA == '29B')
	depdat$DATE_FISHED <- as.Date(depdat$DATE_FISHED)

		depplot <- ggplot (depdat, aes (y = CPUE_KG, x = DATE_FISHED)) +
	  geom_point () + labs (y = 'CPUE (kg/h)', x = 'Date') +
	  scale_x_date(date_breaks = "2 week", date_labels =  "%b-%d") + 
	  facet_wrap (~AREA, ncol = 2, scales = 'free') +
	  theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	         panel.background = element_blank(), axis.line = element_line(colour = "black"))
	depplot
	
## Range of values by area
	library (plyr)
	cpuevals <- ddply (log.2018,. (AREA),
	                   summarize,
	                   min = min (CPUE_KG), max = max (CPUE_KG), mean = mean (CPUE_KG), median = median (CPUE_KG))
	
## MARCH 2015 
### For NEW CF method: 


#		# SURVEY CONDITION FACTOR(g/dm3)
#			xx <- 2014
#			#subset survey data to be just those tows where detailed sampling was conducted
#			sfa29surv.detailed <- sfa29surv.dat
#			com.contours<-contour.gen(subset(sfa29surv.detailed,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
#
#			lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
#
#			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
#			CP <- convCP(CL)
#			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnB
#			png("figures/sfa29comCF2014.png",9,9,res=200,units='in')
#			ScallopMap('sfa29',plot.lines=F,bathcol=rgb(0,0,1,0.3),bathy.source='USGS',contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Condition'),cex=1.2)
#			points(lat~lon,sfa29surv.detailed,subset=year==xx,pch=16,cex=0.5)
#			addLines(sfa29strata)
#			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title=expression("" * g/dm^3*""),inset=0.02,bty='n',box.col='white', cex=1)
#			dev.off()
#
#		# SURVEY MEAT COUNT - of commerical (>= 100mm) sized animals
#			#sfa29.poly <- read.csv("Y:/INSHORE SCALLOP/SFA29/2015/SFA29_Boundaries_NoE.csv")
#			xx <- 2014
#			sfa29surv.dat$meat.count<-0.5/(sfa29surv.dat$Commercial_kg/sfa29surv.dat$com)
#			sfa29surv.mc <- sfa29surv.dat[-which(is.na(sfa29surv.dat$meat.count)),]
#			mc.contours<-contour.gen(na.omit(subset(sfa29surv.mc,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=F,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)
#
#			lvls=seq(10,45,5)
#			div=2
#
#			CL <- contourLines(mc.contours$image.dat,levels=lvls)
#			CP <- convCP(CL)
#			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
#			Ncol=length(lvls)+div
#			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)
#
#			png("figures/sfa29comMeatCnt2014.png",9,9,res=200,units='in')
#			ScallopMap('sfa29',plot.lines=F,bathcol=rgb(0,0,1,0.3),bathy.source='USGS',contour=list(sfa29cont.poly,cont.data),title="",cex=1.2)
#			points(lat~lon,sfa29surv.mc,subset=year==xx,pch=16,cex=0.5)
#			addLines(sfa29strata)
#			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='n',box.col='white', cex=1)
#			dev.off()
#
			
