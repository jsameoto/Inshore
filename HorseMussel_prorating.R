 #Prorating horsemussel bycatch
library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)


#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", "WILSONBR")

path.directory <- "Y:/INSHORE SCALLOP/Survey/"
survey.year <- 2021
Year <- c(2018:survey.year)
Year <- Year[! Year %in% 2020]

# LIST ALL SUBSAMPLED TOWS USING CRUISE.TOW FORMAT
sub.sampled.tows <- c("SFA292021.103") # Were there any tows that were subsampled? Check field notes.

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/archive/2016/contour.gen.r") 
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
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

# Load live horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/BF",Year[i],"/BF",Year[i],"_horsemussellive.csv",sep=""), header=T)
  BF.hm.live <- rbind(BF.hm.live,temp)
}

#SPA3
BI.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/BI",Year[i],"/BI",Year[i],"_horsemussellive.csv",sep=""), header=T)
  BI.hm.live <- rbind(BI.hm.live,temp)
}

#SPA6
GM.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/GM",Year[i],"/GM",Year[i],"_horsemussellive.csv",sep=""), header=T)
  GM.hm.live <- rbind(GM.hm.live,temp)
}

#SFA29
SFA29.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/SFA29",Year[i],"/SFA29",Year[i],"_horsemussellive.csv",sep=""), header=T)
  SFA29.hm.live <- rbind(SFA29.hm.live,temp)
}

hm.live <- rbind(BF.hm.live, if(exists("BI.hm.live")) BI.hm.live, if(exists("GM.hm.live")) GM.hm.live, if(exists("SFA29.hm.live")) SFA29.hm.live) %>%  #Combine SPA data together if data is available
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number
  
                
colnames(hm.live) <- str_replace(colnames(hm.live), "X", "BIN_ID_") #Rename bin headers
hm.live[is.na(hm.live)] <- 0 #convert NAs to 0
hm.live <- left_join(hm.live, tows.dat, by = "ID") %>% #Join the tow data and horse mussel data
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#HM are sampled from lined gear only (2 lined drags) - tow length (m) * single dredge width (0.5926667m)*2 = total area sampled
#CHECK IF TOWS WERE SUBSAMPLED!!##
hm.live <- hm.live %>% 
  mutate(NUM_LINED_SAMPLED = if_else(TOW & CRUISE %in% sub.sampled.tows, 1, NUM_LINED_FREQ))#If TOW = any of the subsampled tows, then NUM_LINED_SAMPLED = 1 (assuming that is how the sample was subsampled, else NUM_LINED_SAMPLED = the number of lined drags (2 unless gear detatched))

#PRORATE to 800m tow length
for(i in 1:nrow(hm.live)) {
  for(j in 4:40){
    hm.live[i,j] <- hm.live[i,j]/(hm.live$TOW_LEN[i]*0.5926667*hm.live$NUM_LINED_SAMPLED[i])*800*0.5926667*NUM_LINED_SAMPLED[i]
  }
}

hm.live <- hm.live %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  mutate(across(BIN_ID_0:BIN_ID_180, round, 2)) %>% 
  mutate(TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_180) %>% rowSums(na.rm = TRUE) %>% round(0)) 


# Spatial plot - density --------------------------------------------------

com.contours <- contour.gen(hm.live %>% dplyr::select(ID, lon, lat, TOTAL),
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
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.live, aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Live M.modiolus Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
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


# Load dead horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/BF",Year[i],"/BF",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  BF.hm.dead <- rbind(BF.hm.dead,temp)
}

#SPA3
BI.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/BI",Year[i],"/BI",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  BI.hm.dead <- rbind(BI.hm.dead,temp)
}

#SPA6
GM.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/GM",Year[i],"/GM",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  GM.hm.dead <- rbind(GM.hm.dead,temp)
}

#SFA29
SFA29.hm.dead <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,Year[i],"/data entry templates and examples/SFA29",Year[i],"/SFA29",Year[i],"_horsemusseldead.csv",sep=""), header=T)
  SFA29.hm.dead <- rbind(SFA29.hm.dead,temp)
}

hm.dead <- rbind(BF.hm.dead, if(exists("BI.hm.dead")) BI.hm.dead, if(exists("GM.hm.dead")) GM.hm.dead, if(exists("SFA29.hm.dead")) SFA29.hm.dead) %>%  #Combine SPA data together if data is available
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number


colnames(hm.dead) <- str_replace(colnames(hm.dead), "X", "BIN_ID_") #Rename bin headers
hm.dead[is.na(hm.dead)] <- 0 #convert NAs to 0
hm.dead <- left_join(hm.dead, tows.dat, by = "ID") %>% #Join the tow data and horse mussel data
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#HM are sampled from lined gear only (2 lined drags) - tow length (m) * single dredge width (0.5926667m)*2 = total area sampled
#CHECK IF TOWS WERE SUBSAMPLED!!##
hm.dead <- hm.dead %>% 
  mutate(NUM_LINED_SAMPLED = if_else(TOW & CRUISE %in% sub.sampled.tows, 1, NUM_LINED_FREQ))#If TOW = any of the subsampled tows, then NUM_LINED_SAMPLED = 1 (assuming that is how the sample was subsampled, else NUM_LINED_SAMPLED = the number of lined drags (2 unless gear detatched))

#PRORATE to 800m tow length
for(i in 1:nrow(hm.dead)) {
  for(j in 4:40){
    hm.dead[i,j] <- hm.dead[i,j]/(hm.dead$TOW_LEN[i]*0.5926667*hm.dead$NUM_LINED_SAMPLED[i])*800*0.5926667*NUM_LINED_SAMPLED[i]
  }
}

hm.dead <- hm.dead %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  mutate(across(BIN_ID_0:BIN_ID_180, round, 2)) %>% 
  mutate(TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_180) %>% rowSums(na.rm = TRUE) %>% round(0)) 


# Spatial plot - density --------------------------------------------------

com.contours <- contour.gen(hm.dead %>% dplyr::select(ID, lon, lat, TOTAL),
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
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.dead, aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Live M.modiolus Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
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




# Horse mussel video data -------------------------------------------------

#Tow lengths
log.2017 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2017_camera_logfiles/Comb_2017_logfiles.csv")

log.2017 <- log.2017 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  filter(!is.na(Station)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326) %>%
  st_transform(crs = 32620) %>% 
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  dplyr::mutate(len_m = sf::st_length(geometry))

log.2018 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2018_camera_logfiles/Comb_2018_logfiles.csv")

log.2018 <- log.2018 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)  %>%
  st_transform(crs = 32620) %>% 
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")%>% 
  dplyr::mutate(len_m = sf::st_length(geometry))


log.2019 <- read.csv("Z:/Projects/BoF_Mapping_Project/Data/Survey_Data/Logs/Camera_GPS_Logs/2019_camera_logfiles/Comb_2019_logfiles.csv")

log.2019 <- log.2019 %>%
  filter(!is.na(GPS.Longitude)) %>% 
  st_as_sf(coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)  %>%
  st_transform(crs = 32620) %>% 
  group_by(Station) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")%>% 
  dplyr::mutate(len_m = sf::st_length(geometry))

log.2019$Station <- as.factor(log.2019$Station)

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

hm.camera <- left_join(hm.tot, log.dat, by = "Station")
hm.camera <- left_join(hm.camera, image.area, by = "Station")

hm.camera <- hm.camera %>% 
  mutate(ID = Station)


# Spatial plot - density --------------------------------------------------

com.contours <- contour.gen(hm.camera %>% dplyr::select(ID, lon, lat, totHM),
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
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
p + #Plot survey data and format figure.
  geom_spatial_point(data = hm.camera, aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "M.modiolus Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
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
