
###................................................................................###
###                               Spatial Figures                                  ###
###                           SFA 29 2015 Assessment                               ###
###                             J.Sameoto Jan 2016                                 ###
###                     B.Wilson - Overhauled Mar 2021                             ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###

### This script is compatible with the SFA29W folder structure from 2021 onwards, if previous year outputs are needed, file paths will need to be entered manually..   

## While running each plot, check console for errors. Levels (lvls) may need adjusting and could cause errors when running.


# Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count and Clappers for SFA29.

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
library(scales)
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
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

survey.year <- 2023  #This is the last survey year 
assessmentyear <- 2024 #year in which you are providing advice for - (e.g. 2017 survey is 2018 assessment) - Save to folder year
cruise <- "'SFA292023'"

#for multiple cruises:
#cruise <- c('SFA292018','SFA292019') 
#cruise <- paste(cruise,collapse="','")

# get the year set up for later....  This should be set as the year of the assessment (e.g. 2017 survey is 2018 assessment)
#yr <- year(Sys.Date())
#yr.crnt <- yr-1
#years <- c(2001:(yr-1))

path.directory <- "Y:/Inshore/SFA29/"

#set up directory to save plot
saveplot.dir <- paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Figures/")


#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')


# ----Import Source functions and polygons---------------------------------------------------------------------


#### Import Mar-scal functions
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#### Import Mar-scal shapefiles

temp <- tempfile() # Find where tempfiles are stored
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
temp2 <- tempfile()# Figure out what this file was saved as
unzip(zipfile=temp, exdir=temp2) #unzip it

# Now read in the shapefiles
sfa29.poly <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% 
  st_transform(crs = 4326)
sfa29strata <- st_read(paste0(temp2, "/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp"))

#Set working directory #
#setwd(paste0(path.directory,"/",assessmentyear))

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

#Save out for notes for Survey Summary and WSAC
#write.csv(ScallopSurv %>% filter(year == survey.year), paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SFA29_totalpertow_sizeclass_live",survey.year,".csv"))

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

#Save out for notes for Survey Summary and WSAC
#write.csv(ScallopSurv.dead %>% filter(year == survey.year), paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SFA29_totalpertow_sizeclass_dead",survey.year,".csv"))

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

#Biomass data is read in from the previously generated "liveweightYYYY.csv" files in Y:\INSHORE SCALLOP\SFA29\YYYY\dataoutput directories.

# Current year only - DEFINE
#ScallopSurv.kg <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/",assessmentyear,"/dataoutput/SFA29liveweight",survey.year,".csv"))

#Multiple years - DEFINE
ScallopSurv.kg <- read.csv(paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29liveweight2014to",survey.year,".csv"))  
#ScallopSurv.kg <- read.csv("Y:/Inshore/SFA29/2020/dataoutput/SFA29liveweight2014to2019.csv")  

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

#Condition data is read in from the previously generated "ConditionforMapYYYY.csv" files in Y:\INSHORE SCALLOP\SFA29\YYYY\dataoutput directories.

con.dat <- read.csv(paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29ConditionforMap",survey.year,".csv")) #NOTE: 2014 file made by Jessica (..._JS.csv), will be slightly different than for 2015 assessment since Stephen calculated this using smaller dataset for mtwt sh model
#con.dat <- read.csv("Y:/Inshore/SFA29/2020/dataoutput/ConditionforMap2019.csv") 

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

# --------------------------------Calculating proportion of clappers -----------------------------------------	

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



# Lobster Bycatch formatting  ----------------------------------------------------

quer4 <- paste(
  "SELECT t.cruise, t.tow_no, tow_date, strata_id, start_lat, start_long, end_lat, end_long, tow_len, drag_width, geophys_id, lobsterpertow
FROM (
SELECT * 
FROM scallsur.SCTOWS 
WHERE cruise in (",cruise,")
) t
LEFT JOIN (
SELECT cruise, tow_no, count(*) AS LobsterPerTow
from scallsur.scbycatch_by_tow_raw
WHERE cruise in (",cruise,")
and speccd_id = 2550 
GROUP BY cruise, tow_no
) b
ON t.cruise = b.cruise
AND t.tow_no = b.tow_no",
  sep=""
)

bycatch.dat <- dbGetQuery(chan, quer4)

bycatch.dat <- bycatch.dat %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) #Convert to DD

bycatch.dat[is.na(bycatch.dat)] <- 0 #Assumes all NAs are 0

#Save out for notes for Survey Summary and WSAC
#write.csv(bycatch.dat, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/LOBSTER_totalpertow",survey.year,".csv"))

# Set plot themes (legend orientation/aesthetics) ------------------------


#Set legend format for plots
plot.theme <-   theme(legend.key.size = unit(6,"mm"),
                      plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 10),
                      legend.position = c(.90,.77), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(2, 3, 2, 3),
                      panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

plot.theme.2 <-   theme(legend.key.size = unit(6,"mm"),
                      plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 10),
                      legend.position = c(.88,.77), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(2, 3, 2, 3),
                      panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

		
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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)),
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (>= 100mm)"),
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme


#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_ComDensity",survey.year,".png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#FR figure - need to edit longitude West = Ouest:
#Figuring out where ticks should go? Doesn't plot properly unless coord_sf() is added...       
pt <- p + coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)
lon.loc <- c(66.4, 66.2, 66.0, 65.8, 65.6)
lony <- paste0(lon.loc,expression("*{degree}*O"))
# And then replot the figure
p2 <- pt +
  scale_x_continuous(breaks =-lon.loc,labels=parse(text = lony))
  #scale_y_continuous(breaks = lat.loc,labels=parse(text = latty))
p2

#FR
p2 + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (>= 100mm)"), 
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme.2

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----

com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded
#lvls=c(0.01,0.05,0.1,2,6,10,14,18)
#lvls=c(0.01,0.05,0.1,5,10,15,20,25)

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
#labels <- c("0.01-0.05", "0.05-0.1", "0.1-2", "2-6", "6-10", "10-14", "14-18", "18+")
#labels <- c("0.01-0.05", "0.05-0.1", "0.1-5", "5-10", "10-15", "15-20", "20-25", "25+")

col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.
#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Biomass (>= 100mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_ComBiomass",survey.year,".png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----CONDITION PLOT-----

com.contours <- contour.gen(con.dat %>% filter(year==survey.year) %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

#lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
lvls=c(4,6,8,10,12,14,16) # large scale
#lvls=c(4,5,6,7,8,9,10,11,12) # good condition scale # update based on values observed
#lvls=c(4,6,8,10,12,14,16,18,20)

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
#labels <- c("4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
labels <- c("4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16+")
#labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16-18", "18-20", "20+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

#Plot with Pecjector
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap",scale.bar = c('bl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs( x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Condition"),
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_Condition",survey.year,".png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT -----

mc.contours<-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year) %>% dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls <- c(10,15,20,25,30,35,40,45)
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
cfd <- scale_fill_manual(values = alpha(col, 0.45), breaks = labels, name = expression(frac(Meats,"500g"), limits = labels, drop = FALSE)) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Meat Count (>= 100)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year),aes(lon, lat), size = 0.5)+
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",
       y = "Latitude") + #title = paste(survey.year, "", "SFA29 Clapper Density (>= 100mm)")
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_ComClappers",survey.year,".png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----PROPORTION OF CLAPPERS -----

com.contours<-contour.gen(prop.clappers %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7) #levels to be color coded

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
labels <- c("0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5","0.5-0.6","0.6-0.7", "0.7+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",
       y = "Latitude") + #title = paste(survey.year, "", "SFA29 Clapper Proportion (>= 100mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_PropComClappers2019.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (90-99 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_RecDensity2019.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#FR figure - need to edit longitude West = Ouest:
#Figuring out where ticks should go? Doesn't plot properly unless coord_sf() is added...       
pt <- p + coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)
lon.loc <- c(66.4, 66.2, 66.0, 65.8, 65.6)
lony <- paste0(lon.loc,expression("*{degree}*O"))
# And then replot the figure
p2 <- pt +
  scale_x_continuous(breaks =-lon.loc,labels=parse(text = lony))
#scale_y_continuous(breaks = lat.loc,labels=parse(text = latty))
p2

#FR
p2 + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (90-99 mm)"), 
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme.2
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Biomass (90-99 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----CLAPPERS -----

rec.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year) %>%
                            dplyr::select(ID,lon,lat,rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

#lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded
lvls=c(0.5, 1, 5, 10, 15, 20, 25)

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
#labels <- c("1-5", "5-10", "10-15", "15-20", "20-23", "23-30", "30-50", "50-100", "100+")
labels <- c("1", "1-5", "5-10", "10-15", "15-20", "20-25","+25")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",
       y = "Latitude") + #title = paste(survey.year, "", "SFA29 Clapper Density (90-99 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_RecClappers",survey.year,".png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----PROPORTION OF CLAPPERS -----

rec.contours<-contour.gen(prop.clappers %>% filter(year==survey.year) %>% 
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7) #levels to be color coded

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
labels <- c("0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5","0.5-0.6","0.6-0.7", "0.7+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% filter(year == survey.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",
       y = "Latitude") + #title = paste(survey.year, "", "SFA29 Clapper Proportion (90-99mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (< 90 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme


#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_PreDensity2019.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#FR figure - need to edit longitude West = Ouest:
#Figuring out where ticks should go? Doesn't plot properly unless coord_sf() is added...       
pt <- p + coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)
lon.loc <- c(66.4, 66.2, 66.0, 65.8, 65.6)
lony <- paste0(lon.loc,expression("*{degree}*O"))
# And then replot the figure
p2 <- pt +
  scale_x_continuous(breaks =-lon.loc,labels=parse(text = lony))
#scale_y_continuous(breaks = lat.loc,labels=parse(text = latty))
p2

#FR
p2 + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Density (< 90 mm)"), 
  guides(fill = guide_legend(title="Nombre par trait", override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme.2
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

#ENG
p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Biomass (< 90 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


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
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% filter(year == survey.year),aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",
       y = "Latitude") + #title = paste(survey.year, "", "SFA29 Clapper Density (< 90 mm)"), 
  guides(fill = guide_legend(override.aes= list(alpha = .8))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# LOBSTER BYCATCH ----------------------------------------------------

rm(cfd) #Remove aesthetics from previous plots before running pecjector

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('bl',0.5,-1,-1)))


#define for plotting ****MAY NEED TO ADJUST INTERVALS****
labels <- c("0", "1-5", "6-15", "16-20", paste0("21-",max(bycatch.dat$LOBSTERPERTOW)))
lvls <- c(0,1,5,15,20,max(bycatch.dat$LOBSTERPERTOW))

bycatch.dat <- bycatch.dat %>% 
  mutate(brk = cut(LOBSTERPERTOW, breaks = lvls, labels = labels, include.lowest = TRUE))


p + #Plot survey data and format figure.
  geom_spatial_point(data = bycatch.dat, aes(lon, lat, size = brk, shape = brk)) +
  scale_shape_manual("Number of Lobster\n per Tow",values = c(4, 19, 19, 19,19), labels = labels)+ #Formatting the shape
  scale_size_manual("Number of Lobster\n per Tow", values = c(2, 2, 4, 6, 8), labels = labels)+# Formatting the size for each break
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude",y = "Latitude") +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE) +
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        legend.position = c(.86,.82), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)),
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

ggsave(filename = paste0(saveplot.dir,'LobsterSpatialPlot_SFA29',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ------------------------------END OF CONTOUR PLOTS  -------------------------------------------



