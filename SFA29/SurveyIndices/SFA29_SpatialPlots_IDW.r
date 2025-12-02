
###................................................................................###
###                         Spatial Figures (using IDW)                            ###
###                           SFA 29W Assessment                                   ###
###                     G.English & J.Sameoto Jan 2016                             ###
###                                                                                ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###

### This script is compatible with the SFA29W folder structure from 2021 onwards, if previous year outputs are needed, file paths will need to be entered manually..   

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
require(concaveman)
require(ggspatial)
#library(RCurl)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
# uid <- keyring::key_list("Oracle")[1,2]
# pwd <- keyring::key_get("Oracle", uid)
uid <- un.englishg
pwd <- pw.englishg

survey.year <- 2025  #This is the last survey year 
assessmentyear <- 2026 #year in which you are providing advice for - (e.g. 2017 survey is 2018 assessment) - Save to folder year
cruise <- "'SFA292025'"

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
#set up a directory for french figures
saveplot.dir.fr <- paste0(saveplot.dir,"FrenchFigures_indicies/")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')


# ----Import Source functions and polygons---------------------------------------------------------------------
#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")

#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#### Import Mar-scal shapefiles

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/Inshore_Spatial_Layers_Mar2025.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
mgmt.zones.detailed <- st_read(paste0(temp2, "/Inshore_Spatial_Layers_Mar2025/Scallop_Strata.shp")) %>% 
  filter(Scal_Area == "SFA29W")

bathy_sf <- st_read(paste0(temp2,"/Inshore_Spatial_Layers_Mar2025/bathymetry_15m.shp")) 

Land <- st_read(paste0(temp2,"/Inshore_Spatial_Layers_Mar2025/Atl_region_land.shp")) %>% 
  st_transform(crs = 32620) 

	# -----------------------------Import SHF data (live and dead)--------------------------------------------
	
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
# write.csv(ScallopSurv %>% filter(year == survey.year), paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SFA29_totalpertow_sizeclass_live",survey.year,".csv"))

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
# write.csv(ScallopSurv.dead %>% filter(year == survey.year), paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SFA29_totalpertow_sizeclass_dead",survey.year,".csv"))

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
write.csv(bycatch.dat, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/LOBSTER_totalpertow",survey.year,".csv"))


# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------

#create shapefile out of survey tow locations. It will get re-written during IDW, but just a placeholder for function below.
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

hull <- concaveman(Surv.sf)
idw_sf <- st_buffer(hull, dist = 2000)

# Ensure correct CRS
st_crs(idw_sf) <- 32620

# -------------- Set consistent plot objects/themes ------------------------------

#Set standard layers: bathymetry on bottom layer, and land, survey tows, etc above IDW layer
bathy <-   ggplot() + #goes before IDW layer
  geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)

# Function to generate map layers to go after IDW layer
p <- function(mgmt_zone, surv_sf, land) {
  list(
    # Management Zones 
    geom_sf(data = mgmt.zones.detailed, color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
    
    # Survey Points (white outline, black fill)
    geom_sf(data = surv_sf, color = "white", fill = "black", shape = 21, size = 1.5),
    
    # Land (grey fill)
    geom_sf(data = land, fill = "grey60"),
    
    # Custom X and Y axis formatting (easier for french translation)
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)),
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)),
    
    # Set theme for the plot
    theme(plot.title = element_text(size = 14, hjust = 0.5), # Plot title size and position
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"), 
          legend.text = element_text(size = 12),
          legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), # Legend background color and transparency
          legend.box.margin = margin(6, 8, 6, 8),
          legend.position = c(.85,.77), # Fixed legend position
          legend.justification = c(0.5, 0.5)), # Keep legend within bounds, 
    
    # Add scale bar with selectable location
    annotation_scale(
      location = "bl", width_hint = 0.5, 
      pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")
    ),
    
    # Add north arrow with selectable location
    annotation_north_arrow(
      location = "bl", which_north = "true", 
      height = unit(1.25, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),
      style = north_arrow_fancy_orienteering
    )
  )
}

# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

##### DENSITY (ENGLISH) ----

bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) + #max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### DENSITY (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) + #max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

##### BIOMASS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


##### BIOMASS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(con.dat,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_condition <- log(Surv.sf$Condition)
Surv.sf$log_condition[which(Surv.sf$log_condition==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_condition~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Condition)

##### CONDITION (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(6,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### CONDITION (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(6,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.mtcnt,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_mtct <- log(Surv.sf$meat.count)
Surv.sf$log_mtct[which(Surv.sf$log_mtct==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_mtct~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$meat.count)

##### MEAT COUNT (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,35), oob = scales::squish) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### MEAT COUNT (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,35), oob = scales::squish) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com)
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

##### CLAPPERS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### CLAPPERS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ---- PROPORTION OF CLAPPERS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$prop.dead.com)
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.com)

##### PROPORTION OF CLAPPERS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Proportion of Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### PROPORTION OF CLAPPERS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Proportion of Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

##### DENSITY (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### DENSITY (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

##### BIOMASS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### BIOMASS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec)
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

##### CLAPPERS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### CLAPPERS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ---- PROPORTION OF CLAPPERS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop.dead.rec <- log(Surv.sf$prop.dead.rec)
Surv.sf$log_prop.dead.rec[which(Surv.sf$log_prop.dead.rec==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop.dead.rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.rec)

##### PROPORTION OF CLAPPERS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Proportion of Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### PROPORTION OF CLAPPERS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Proportion of Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #pre-recruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

##### DENSITY (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### DENSITY (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #pre-recruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

##### BIOMASS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (<65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### BIOMASS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Biomass (<65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS PLOTS -----
#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre)
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

##### CLAPPERS (ENGLISH) ----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (<65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

##### CLAPPERS (FRENCH) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Clappers (<65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# LOBSTER BYCATCH ----------------------------------------------------

#define for plotting ****MAY NEED TO ADJUST INTERVALS****
labels <- c("0", "1-5", "6-15", "16-20", paste0("21-",max(bycatch.dat$LOBSTERPERTOW)))
lvls <- c(0,1,5,15,20,max(bycatch.dat$LOBSTERPERTOW))

bycatch.dat <- bycatch.dat %>% 
  mutate(brk = cut(LOBSTERPERTOW, breaks = lvls, labels = labels, include.lowest = TRUE)) %>% 
  filter(STRATA_ID != 46) #Filter out exploratory tows for future framework (strata 46)


ggplot() + #Plot survey data and format figure.
  geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  geom_sf(data = mgmt.zones.detailed, color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid") +
  geom_sf(data = Land, fill = "grey60") +
  geom_spatial_point(data = bycatch.dat, aes(lon, lat, size = brk, shape = brk)) +
  scale_shape_manual("Number of Lobster\n per Tow",values = c(4, 19, 19, 19,19), labels = labels)+ #Formatting the shape
  scale_size_manual("Number of Lobster\n per Tow", values = c(2, 2, 4, 6, 8), labels = labels)+# Formatting the size for each break
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(x = "Longitude",y = "Latitude") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) + # Custom X and Y axis formatting (easier for french translation)
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + # Add scale bar with selectable location
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.25, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + # Add north arrow with selectable location
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

#save
ggsave(filename = paste0(saveplot.dir,'LobsterSpatialPlot_SFA29',survey.year,'.png'), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ------------------------------END OF SPATIAL PLOTS  -------------------------------------------

#Checking for anomalies 
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat")) #clappers
# Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat")) #alive
Surv.sf %>% 
  select(tow,pre, rec, com) %>% 
  filter(pre > 50 | rec >50 | com > 50)
