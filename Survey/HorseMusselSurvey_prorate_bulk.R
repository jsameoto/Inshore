#Prorating horsemussel bycatch
library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)
library(magrittr)
#require(s2)

#HM data entry files were copied over from Y:\INSHORE SCALLOP\Survey\YYYY\data entry templates and examples for 2018 2019, and 2021.

#NOTE: Subsampling and volume recording methods are different for 2018 and 2019/2021. No subsampling occured in 2019 and in 2021, subsamples were done by drag (lined).

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts and Horse Mussel Measuring Device (HMMD) volume (i.e. bucket vol.) recorded, but no SHF (2 lined drags)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs in GM2018 for clappers)


#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", "WILSONBR")

dir <- "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/"
survey.year <- 2021
Year <- c(2018:survey.year)
Year <- Year[! Year %in% 2020]

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/archive/2016/contour.gen.r") 
direct <- getwd()
for(fun in funcs) 
{
  temp <- direct
  download.file(fun,destfile = basename(fun))
  source(paste0(direct,"/",basename(fun)))
  file.remove(paste0(direct,"/",basename(fun)))
}

# Read in Tow data from database ------------------------------------------

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sctows s			",
  sep=""
)

tows.dat <- dbGetQuery(chan, quer2)
#str(tows.dat)

tows.dat <- tows.dat %>%
  mutate(YEAR = year(TOW_DATE)) %>% 
  filter(YEAR %in% Year) %>% 
  rename(TOW = TOW_NO) %>% 
  dplyr::select(CRUISE, TOW, TOW_TYPE_ID, STRATA_ID, START_LAT, START_LONG, END_LAT, END_LONG, TOW_LEN,NUM_LINED_FREQ) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)

tows.dat$NUM_LINED_FREQ <- as.numeric(tows.dat$NUM_LINED_FREQ)

# LIVE --------------------------------------------------------------------

# Load live horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/BF",Year[i],"_horsemussellive.csv",sep=""), header=T)
  BF.hm.live <- rbind(BF.hm.live,temp)
}

#SPA3
BI.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/BI",Year[i],"_horsemussellive.csv",sep=""), header=T)
  BI.hm.live <- rbind(BI.hm.live,temp)
}

#SPA6
GM.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/GM",Year[i],"_horsemussellive.csv",sep=""), header=T)
  GM.hm.live <- rbind(GM.hm.live,temp)
}

#SFA29
SFA29.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/SFA29",Year[i],"_horsemussellive.csv",sep=""), header=T)
  SFA29.hm.live <- rbind(SFA29.hm.live,temp)
}

hm.live <- rbind(BF.hm.live, BI.hm.live, GM.hm.live, SFA29.hm.live) %>%  #Combine SPA data together if data is available
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

colnames(hm.live) <- str_replace(colnames(hm.live), "X", "BIN_ID_") #Rename bin headers


# Join the tow data and horse mussel data ---------------------------------------------------------------

#Check if number of sctows records = number of horse mussel records
nrow(tows.dat) == nrow(hm.live)

hm.live <- left_join(hm.live, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#replace all NAs in bin and total columns with 0s
hm.live[, 4:43][is.na(hm.live[, 4:43])] <- 0

#replace all NAs in PRORATE.FACTOR with 1
hm.live$PRORATE.FACTOR[is.na(hm.live$PRORATE.FACTOR)] <- 1


# Prorating ---------------------------------------------------------------

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts and Horse Mussel Measuring Device (HMMD) volume (i.e. bucket vol.) recorded, but no SHF (2 lined drags)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs once in GM2018 for clappers - FEW clappers)

#Check sample methods
nrow(hm.live %>% filter(SAMPLE.METHOD == 0))
nrow(hm.live %>% filter(SAMPLE.METHOD == 1))
nrow(hm.live %>% filter(SAMPLE.METHOD == 2))
nrow(hm.live %>% filter(SAMPLE.METHOD == 3))

#Check tows where number of lined drags = 1 and subsampled by HMMD - should result in none (would have to adjust prorate factor if this happened)
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1)) # 0
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1)) #21 tows have only one lined drag sampled.

     
#HM are sampled from lined gear only (2 lined drags) - If lined drag detaches, treat sample as subsampled by drag.
hm.live <- hm.live %>% #filter(!is.na(SAMPLE.METHOD)) %>%  #remove samples with method NA (removed a 1 samples from 2018)
  mutate(PRORATE.FACTOR = if_else(NUM_LINED_FREQ == 1, 2, PRORATE.FACTOR)) #If the number of lined gear sampled is 1, set prorate factor to 2 (this replaces the prorate factor that is already entered during data entry as 2 because only one lined gear was sampled), otherwise, the prorate factor remains the same as it was entered during data entry.

#How many tows with horse mussels were subsampled because a lined drag came loose?
nrow(hm.live %>% filter(PRORATE.FACTOR == 2 & TOTAL > 0 & SAMPLE.METHOD == 0))

#PRORATE to 800m tow length
for(i in 1:nrow(hm.live)) {
  for(j in 4:42){
    hm.live[i,j] <- (hm.live[i,j]*hm.live$PRORATE.FACTOR[i])*(800/(hm.live$TOW_LEN[i]))
  }
}

hm.live <- hm.live %>%
  mutate(START_LAT = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(START_LONG = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  mutate(END_LAT = convert.dd.dddd(END_LAT)) %>% #Convert to DD
  mutate(END_LONG = convert.dd.dddd(END_LONG)) %>% #Convert to DD
  mutate(across(BIN_ID_0:BIN_ID_180, round, 2)) %>%
  mutate(PRORATED.TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_190) %>% rowSums(na.rm = TRUE) %>% round(0)) %>%
  mutate(PRESENT.ABSENT = if_else(TOTAL >= 1, 1, 0)) %>% # Absent == 0, Present = 1
  dplyr::select(!c(TOTAL, TOW_LEN, NUM_LINED_FREQ))

#How many tows had presence of horse mussel?
nrow(hm.live %>% filter(PRESENT.ABSENT == 1))

#How many samples were subsampled?
nrow(hm.live %>% filter(PRORATE == "y"))
nrow(hm.live %>% filter(PRORATE.FACTOR >= 2))

#Saves files by cruise
for(i in unique(hm.live$CRUISE)){
  write.csv(hm.live %>% filter(CRUISE == i), paste0(dir,"Prorated/",i, "_horsemussellive_prorated.csv"))
}

# Spatial plot - live density --------------------------------------------------

com.contours <- contour.gen(hm.live %>% dplyr::select(ID, START_LONG, START_LAT, PRORATED.TOTAL),
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
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGnBu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.live, aes(START_LONG, START_LAT), size = 0.5) +
  labs(title = paste(survey.year, "", "Live M.modiolus Density"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(dir,'ContPlot_LiveHM_Density 2018-',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# DEAD --------------------------------------------------------------------

# Load DEAD horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/BF",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  BF.hm.dead <- rbind(BF.hm.dead,temp)
}

#SPA3
BI.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/BI",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  BI.hm.dead <- rbind(BI.hm.dead,temp)
}

#SPA6
GM.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/GM",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  GM.hm.dead <- rbind(GM.hm.dead,temp)
}

#SFA29
SFA29.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,Year[i],"/SFA29",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  SFA29.hm.dead <- rbind(SFA29.hm.dead,temp)
}

hm.dead <- rbind(BF.hm.dead, BI.hm.dead, GM.hm.dead, SFA29.hm.dead) %>%  #Combine SPA data together if data is available
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

colnames(hm.dead) <- str_replace(colnames(hm.dead), "X", "BIN_ID_") #Rename bin headers


# Join the tow data and horse mussel data ---------------------------------------------------------------

#Check if number of sctows records = number of horse mussel records
nrow(tows.dat) == nrow(hm.dead)

hm.dead <- left_join(hm.dead, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#replace all NAs in bin and total columns with 0s
hm.dead[, 4:43][is.na(hm.dead[, 4:43])] <- 0

#replace all NAs in PRORATE.FACTOR with 1
hm.dead$PRORATE.FACTOR[is.na(hm.dead$PRORATE.FACTOR)] <- 1


# Prorating ---------------------------------------------------------------

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts and Horse Mussel Measuring Device (HMMD) volume (i.e. bucket vol.) recorded, but no SHF (2 lined drags)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs in GM2018 for clappers)

#Check sample methods
nrow(hm.dead %>% filter(SAMPLE.METHOD == 0))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 1))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 2))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 3))
nrow(hm.dead %>% filter(is.na(SAMPLE.METHOD))) # *1 NA

#Check tows with number of lined drags = 1 and subsampled by HMMD - none (would have to adjust prorate factor if this happend)
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1))
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1))

#HM are sampled from lined gear only (2 lined drags)
# If lined drag detaches - treat sample as subsampled by drag.
hm.dead <- hm.dead %>%
  mutate(PRORATE.FACTOR = if_else(NUM_LINED_FREQ == 1, 2, PRORATE.FACTOR)) #If the number of lined gear sampled is 1, set prorate factor to 2 (this replaces the prorate factor that is already entered during data entry as 2 because only one lined gear was sampled), otherwise, the prorate factor remains the same as it was entered during data entry.

#How many tows with horse mussels were prorated because a lined drag came loose, but were not entered as being subsampled?
nrow(hm.dead %>% filter(PRORATE.FACTOR == 2 & TOTAL > 0 & SAMPLE.METHOD == 0))

#PRORATE to 800m tow length
for(i in 1:nrow(hm.dead)) {
  for(j in 4:42){
    hm.dead[i,j] <- (hm.dead[i,j]*hm.dead$PRORATE.FACTOR[i])*(800/(hm.dead$TOW_LEN[i]))
  }
}

hm.dead <- hm.dead %>%
  mutate(START_LAT = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(START_LONG = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  mutate(END_LAT = convert.dd.dddd(END_LAT)) %>% #Convert to DD
  mutate(END_LONG = convert.dd.dddd(END_LONG)) %>% #Convert to DD
  mutate(across(BIN_ID_0:BIN_ID_180, round, 2)) %>%
  mutate(PRORATED.TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_190) %>% rowSums(na.rm = TRUE) %>% round(0)) %>%
  mutate(PRESENT.ABSENT = if_else(TOTAL >= 1, 1, 0)) %>% # Absent == 0, Present = 1
  dplyr::select(!c(TOTAL, TOW_LEN, NUM_LINED_FREQ))

#How many tows had presence of dead horse mussel?
nrow(hm.dead %>% filter(PRESENT.ABSENT == 1))

#How many samples were prorated?
nrow(hm.dead %>% filter(PRORATE == "y"))

#Check columns in dead are the same as live
table(hm.dead$PRORATE.FACTOR == hm.live$PRORATE.FACTOR)
table(hm.dead$SAMPLE.METHOD == hm.live$SAMPLE.METHOD) #One NA record

#Which ones miss-match??
#anti_join(hm.dead, hm.live, by = "PRORATE.FACTOR")

#Saves files by cruise
for(i in unique(hm.dead$CRUISE)){
  write.csv(hm.dead %>% filter(CRUISE == i), paste0(dir,"Prorated/",i, "_horsemusseldead_prorated.csv"))
}


# Spatial plot - live density --------------------------------------------------

com.contours <- contour.gen(hm.dead %>% dplyr::select(ID, START_LONG, START_LAT, PRORATED.TOTAL),
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
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGnBu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.dead, aes(START_LONG, START_LAT), size = 0.5) +
  labs(title = paste(survey.year, "", "Dead M.modiolus Density"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(dir,'ContPlot_DeadHM_Density 2018-',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# Horse mussel video data -------------------------------------------------

#Tow lengths
log.2017 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2017_camera_logfiles/Comb_2017_logfiles.csv")

log.2017 <- log.2017 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  filter(!is.na(Station)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326) %>%
  st_transform(crs = 32620) %>% #Convert to utm for length calculation
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  dplyr::mutate(len_m = sf::st_length(geometry))

#Extract start lat long from geometry
log.2017$lon<- NULL
log.2017$lat<- NULL
for (i in 1:nrow(log.2017)){
  log.2017 <- log.2017 %>%
    st_transform(crs = 4326)  #Convert to get dd
  log.2017$lon[i] <- sf::st_coordinates(log.2017[i,1])[1,1]
  log.2017$lat[i] <- sf::st_coordinates(log.2017[i,1])[1,2]
}


log.2018 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2018_camera_logfiles/Comb_2018_logfiles.csv")

log.2018 <- log.2018 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326) %>%
  st_transform(crs = 32620) %>%  #Convert to utm for length calculation
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")%>% 
  dplyr::mutate(len_m = sf::st_length(geometry))

#Extract start lat long from geometry
log.2018$lon<- NULL
log.2018$lat<- NULL
for (i in 1:nrow(log.2018)){
  log.2018 <- log.2018 %>%
    st_transform(crs = 4326)  #Convert to get dd
  log.2018$lon[i] <- sf::st_coordinates(log.2018[i,1])[1,1]
  log.2018$lat[i] <- sf::st_coordinates(log.2018[i,1])[1,2]
}

log.2019 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2019_camera_logfiles/Comb_2019_logfiles.csv")

log.2019 <- log.2019 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)  %>%
  mutate(lat = sf::st_coordinates(.)[1,1], 
         lon = sf::st_coordinates(.)[1,2]) %>% #Pull start lat and long from geometry
  st_transform(crs = 32620) %>% #Convert to utm for length calculation
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")%>% 
  dplyr::mutate(len_m = sf::st_length(geometry))

#Extract start lat long from geometry
log.2019$lon<- NULL
log.2019$lat<- NULL
for (i in 1:nrow(log.2019)){
  log.2019 <- log.2019 %>%
    st_transform(crs = 4326)  #Convert to get dd
  log.2019$lon[i] <- sf::st_coordinates(log.2019[i,1])[1,1]
  log.2019$lat[i] <- sf::st_coordinates(log.2019[i,1])[1,2]
}

log.2019$Station <- as.factor(log.2019$Station)
log.2018$Station <- as.factor(log.2018$Station)


#combine logfiles
log.dat <- rbind(log.2017,log.2018,log.2019)


mapview::mapview(log.2017)+
  mapview::mapview(log.2018)+
  mapview::mapview(log.2019)


# Horse mussel count data -------------------------------------------------

hm.dat <- read.csv("Z:/Projects/BoF_Mapping_Project/Analysis/Community_analysis_and_SpeciesPresence/HorseMussel_Presence/BOF_Horsemussel_2017-2019.csv")%>% 
  filter(!is.na(Area..cm2.)) %>% 
  mutate(Area_m2 = as.numeric(Area..cm2.)* 0.0001) 

image.area <- hm.dat %>% 
  group_by(Station) %>% 
  summarize(avg_imgArea = mean(Area_m2))

hm.tot <- hm.dat %>% 
  group_by(Station) %>% 
  summarize(totHM = sum(Horse.Mussel.Count))

hm.camera <- merge(hm.tot, log.dat, by = "Station", all = TRUE)
hm.camera <- merge(hm.camera, image.area, by = "Station", all = TRUE) %>% 
  mutate(totHM = totHM*(800*0.5926667*2/(len_m)))

hm.camera$totHM[is.na(hm.camera$totHM)] <- 0 #convert NAs to 0

hm.camera <- hm.camera %>% 
  mutate(ID = Station) %>% 
  filter(!is.na(totHM)) #Look into why these are NA - no image area?
str(hm.camera)

hm.camera$totHM <- unclass(hm.camera$totHM)
hm.camera$PRESENT.ABSENT <- if_else(hm.camera$totHM >= 1, 1, 0)
str(hm.camera)
hm.camera <- hm.camera %>% 
  dplyr::select(!geometry)


write.csv(hm.camera, "Z:/Projects/BoF_Mapping_Project/Analysis/Community_analysis_and_SpeciesPresence/HorseMussel_Presence/Presence_atStation_lvl.csv")
  
# Spatial plot - camera data density --------------------------------------------------

com.contours <- contour.gen(hm.camera %>% dplyr::select(ID, lon, lat, totHM),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.008)

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
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGnBu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.camera, aes(lon, lat), size = 0.5) +
  labs(title = paste("Camera Survey M.modiolus Density"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(dir,'ContPlot_CameraSurvey_HM_Density 2018-',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Alternative Spatial plot - camera data density --------------------------------------------------

library(terra)
library(raster)

b_box <- hm.camera %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  st_buffer(0.06) %>% #create sf object around data points
  st_union() %>% 
  st_as_sf()

hm.camera.sp <- hm.camera %>% #Save data as new object 
  filter(totHM != 0, !is.na(totHM)) #Remove NAs and zero values #Check.

coordinates(hm.camera.sp) <- c("lon", "lat")
grd <- as.data.frame(spsample(hm.camera.sp, "regular", n = 12000))
names(grd) <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(hm.camera.sp) <- CRS("+init=epsg:4326")
proj4string(grd) <- proj4string(hm.camera.sp)

S.idw <- gstat::idw(totHM ~ 1, hm.camera.sp, newdata=grd, idp = 3.5)
plot(S.idw)

S.idw <- raster(S.idw)
S.idw <- S.idw %>% 
  mask(mask = b_box)

plot(b_box)
plot(S.idw)
class(S.idw)

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGnBu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('tl',0.5)))

p + #Plot survey data and format figure.
  geom_raster(data = S.idw, aes(x=x, y=y, fill = var1.pred))+
  scale_fill_gradientn(colours=rev(brewer.pal(length(labels),"Spectral")), na.value=alpha("white", 0))+
  geom_spatial_point(data = hm.camera, aes(lon, lat), size = 0.5) +
  labs(title = paste("Camera survey M.modiolus Density"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

