 #Prorating horsemussel bycatch
library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)
library(magrittr)


#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", "WILSONBR")

path.directory <- "Y:/INSHORE SCALLOP/Survey/"
survey.year <- 2021

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
  filter(YEAR %in% survey.year) %>% 
  rename(TOW = TOW_NO) %>% 
  dplyr::select(CRUISE, TOW, TOW_TYPE_ID, STRATA_ID, START_LAT, START_LONG, END_LAT, END_LONG, TOW_LEN,NUM_LINED_FREQ) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)

tows.dat$NUM_LINED_FREQ <- as.numeric(tows.dat$NUM_LINED_FREQ)


# LIVE --------------------------------------------------------------------


# Load live horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.live <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/BF",survey.year,"/BF",survey.year,"_horsemussellive.csv",sep=""), header=T)


#SPA3
BI.hm.live <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/BI",survey.year,"/BI",survey.year,"_horsemussellive.csv",sep=""), header=T)


#SPA6
GM.hm.live <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/GM",survey.year,"/GM",survey.year,"_horsemussellive.csv",sep=""), header=T)

#SFA29
SFA29.hm.live <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/SFA29",survey.year,"/SFA29",survey.year,"_horsemussellive.csv",sep=""), header=T)

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
# NA --- Not counted or measured, but noted (occurs in GM2018 for clappers) -> IGNORE

#Check sample methods
nrow(hm.live %>% filter(SAMPLE.METHOD == 0))
nrow(hm.live %>% filter(SAMPLE.METHOD == 1))
nrow(hm.live %>% filter(SAMPLE.METHOD == 2))
nrow(hm.live %>% filter(SAMPLE.METHOD == 3))

#Check tows with number of lined drags = 1 and subsampled by HMMD - none (would have to adjust prorate factor if this happened)
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1))
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1))

#HM are sampled from lined gear only (2 lined drags) - If lined drag detaches, treat sample as subsampled by drag.
hm.live <- hm.live %>% #filter(!is.na(SAMPLE.METHOD)) %>%  #remove samples with method NA (removed a 1 samples from 2018)
  mutate(PRORATE.FACTOR = if_else(NUM_LINED_FREQ == 1, 2, PRORATE.FACTOR)) #If the number of lined gear sampled is 1, set prorate factor to 2 (this replaces the prorate factor that is already entered during data entry as 2 because only one lined gear was sampled), otherwise, the prorate factor remains the same as it was entered during data entry.

#How many tows with horse mussels were prorated because a lined drag came loose?
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

#How many samples were prorated?
nrow(hm.live %>% filter(PRORATE == "y"))

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
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
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




# DEAD --------------------------------------------------------------------

# Load dead horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.dead <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/BF",survey.year,"/BF",survey.year,"_horsemusseldead.csv",sep=""), header=T)


#SPA3
BI.hm.dead <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/BI",survey.year,"/BI",survey.year,"_horsemusseldead.csv",sep=""), header=T)


#SPA6
GM.hm.dead <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/GM",survey.year,"/GM",survey.year,"_horsemusseldead.csv",sep=""), header=T)

#SFA29
SFA29.hm.dead <- read.csv(paste0(path.directory,survey.year,"/data entry templates and examples/SFA29",survey.year,"/SFA29",survey.year,"_horsemusseldead.csv",sep=""), header=T)

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
# NA --- Not counted or measured, but noted (occurs in GM2018 for clappers) -> IGNORE

#Check sample methods
nrow(hm.dead %>% filter(SAMPLE.METHOD == 0))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 1))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 2))
nrow(hm.dead %>% filter(SAMPLE.METHOD == 3))

#Check tows with number of lined drags = 1 and subsampled by HMMD - none (would have to adjust prorate factor if this happened)
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1))
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1))

#HM are sampled from lined gear only (2 lined drags) - If lined drag detaches, treat sample as subsampled by drag.
hm.dead <- hm.dead %>% #filter(!is.na(SAMPLE.METHOD)) %>%  #remove samples with method NA (removed a 1 samples from 2018)
  mutate(PRORATE.FACTOR = if_else(NUM_LINED_FREQ == 1, 2, PRORATE.FACTOR)) #If the number of lined gear sampled is 1, set prorate factor to 2 (this replaces the prorate factor that is already entered during data entry as 2 because only one lined gear was sampled), otherwise, the prorate factor remains the same as it was entered during data entry.

#How many tows with horse mussels were prorated because a lined drag came loose?
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
#How many tows had presence of horse mussel?
nrow(hm.dead %>% filter(PRESENT.ABSENT == 1))

#How many samples were prorated?
nrow(hm.dead %>% filter(PRORATE == "y"))
#nrow(hm.dead %>% filter(SAMPLE.METHOD == 0))
#hm.dead %>% filter(SAMPLE.METHOD == 1)
#hm.dead %>% filter(SAMPLE.METHOD == 2)
#hm.dead %>% filter(SAMPLE.METHOD == 3)

#Saves files by cruise
for(i in unique(hm.dead$CRUISE)){
  write.csv(hm.dead %>% filter(CRUISE == i), paste0(dir,"Prorated/",i, "_horsemusseldead_prorated.csv"))
}


# Spatial plot - dead density --------------------------------------------------

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
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), 
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

