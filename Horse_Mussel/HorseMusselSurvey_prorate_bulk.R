#Prorating horsemussel bycatch
library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(lubridate)
library(magrittr)
#require(s2)

#HM data entry files were copied over from Y:\INSHORE SCALLOP\Survey\YYYY\data entry templates and examples for 2018 2019, and 2021 and column modifications and data corrections were made to the copies (**These changes were not made on original files!!**).

#NOTE: Subsampling and volume recording methods are different for 2018 and 2019/2021. No subsampling occured in 2019 and in 2021, subsamples were done by drag (lined).

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts and Horse Mussel Measuring Device (HMMD) volume (i.e. bucket vol.) recorded, but no SHF (2 lined drags)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs once in GM2018 for clappers)


#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

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


#Read in the inshore strata for plotting
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57.shp"))
#mapview::mapview(strata)

#Read in inshore boundaries
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
SFA29 <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% mutate(ID = seq(1,5,1)) %>%  #TO FIX IN COPY ON GITHUB (ET_ID missing so adding it here)
  mutate(ET_ID = case_when(ID == 1 ~ 41, 
                           ID == 2 ~ 42,
                           ID == 3 ~ 43,
                           ID == 4 ~ 44,
                           ID == 5 ~ 45)) %>% 
  dplyr::select(Id = ID, ET_ID) %>% st_transform(crs = 4326)


ebsa <- st_read("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Shapefiles/HorseMussel_shapefiles/Modiolus_EBSA/modiolus_EBSA.shp") %>% 
  st_transform(4326)


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

#Check for duplicate IDs
n_occur <- data.frame(table(hm.live$ID))
n_occur[n_occur$ID > 1,] #no dups
dim(hm.live[duplicated(hm.live$ID),])[1]

#Check IDs match
dim(left_join(hm.live, tows.dat, by = "ID"))[1] == dim(inner_join(hm.live, tows.dat, by = "ID"))[1]


#Now Join the tow data with horse mussel live data
hm.live <- left_join(hm.live, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#replace all NAs in bin and total columns with 0s UNLESS SAMPLE.METHOD == 1, where SHF were not recorded - Keep NAs.
hm.live <- hm.live %>%
  mutate(across(BIN_ID_0:TOTAL, ~if_else(SAMPLE.METHOD != 1, replace_na(.,0), NULL)))

#During data entry, PRORATE.FACTOR is only entered if catch was subsampled (i.e. from 2 (sampleable) lined drags, only one was sampled/or catch was subsampled using a Horse mussel measuring device (HMMD). *Note prorate factor is NOT entered if gear becomes detached and is not samplable.

#replace all NAs in PRORATE.FACTOR with 1.
hm.live$PRORATE.FACTOR[is.na(hm.live$PRORATE.FACTOR)] <- 1

#Save as seperate object (raw) to compare to prorated copy.
hm.live.raw <- hm.live

# Prorating ---------------------------------------------------------------

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts recorded, but no SHF (2 lined drags) (i.e. presence and abundance). Sometimes Horse Mussel Measuring Device (HMMD) volume is recorded (i.e. bucket vol.)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs once in GM2018 for clappers - FEW clappers)

#Check sample methods
hm.live %>% filter(SAMPLE.METHOD == 0)
hm.live %>% filter(SAMPLE.METHOD == 1)
hm.live %>% filter(SAMPLE.METHOD == 2)
hm.live %>% filter(SAMPLE.METHOD == 3)
hm.live %>% filter(is.na(SAMPLE.METHOD))

#Check tows where number of lined drags = 1 and subsampled by HMMD - should result in none (would have to adjust prorate factor if this happened)
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1)) # 0
nrow(hm.live %>% filter(NUM_LINED_FREQ == 1)) #21 tows have only one lined drag sampled.

#How many tows with horse mussels were subsampled because a lined drag came loose?
hm.live %>% filter(NUM_LINED_FREQ == 1 & TOTAL > 0)


#Standardized Total Abundance - Prorating for subsample of gear (2 lined drags), prorating for amount of gear sampled (NUM_LINED_FREQ - either 1 or 2), and standardizing to 800m tow length. This assumes that the individual drag width is equivelent for all drags (assumes gear width of 4ft).
hm.GM2021.raw <- hm.live.raw %>% filter(CRUISE == "GM2021") #Test with GM2021 data
hm.live <- hm.GM2021.raw
head(hm.live)

hm.live$ABUND.STD <- (hm.live$TOTAL*hm.live$PRORATE.FACTOR)*(2/hm.live$NUM_LINED_FREQ)*(800/(hm.live$TOW_LEN))

hm.live$ABUND.STD <- hm.live$ABUND.STD*(17.5/4)


##SHF - Prorating for subsample of gear (2 lined drags), prorating for amount of gear sampled (NUM_LINED_FREQ - either 1 or 2), and standardizing to 800m tow length. This assumes that the individual drag width is equivelent for all drags (assumes gear width of 4ft). 
for(i in 1:nrow(hm.live)) {
  for(j in 4:42){
    hm.live[i,j] <- (hm.live[i,j]*hm.live$PRORATE.FACTOR[i])*(2/hm.live$NUM_LINED_FREQ[i])*(800/(hm.live$TOW_LEN[i]))
    hm.live[i,j] <- hm.live[i,j] *(17.5/4)
  }
}

hm.test <- hm.live %>% filter(ABUND.STD > 0)
write.csv(hm.live, "Y:/Inshore/Databases/Scallsur/ScallsurUpdates/2022/Db_Update_HorseMusselAddition/testing/Feb082022/hm_standardize_R_version.csv")

#Tidy up lat and long coords, add presence/absence etc.
#hm.GM2021 <- hm.GM2021 %>%
#  mutate(PRESENT.ABSENT = if_else(TOTAL >= 1, 1, 0)) %>% # Absent == 0, Present = 1
#  mutate(START_LAT = convert.dd.dddd(START_LAT)) %>% #Convert to DD
#  mutate(START_LONG = convert.dd.dddd(START_LONG)) %>% #Convert to DD
#  mutate(END_LAT = convert.dd.dddd(END_LAT)) %>% #Convert to DD
#  mutate(END_LONG = convert.dd.dddd(END_LONG)) %>% #Convert to DD
#  mutate(MID_LAT = apply(cbind(END_LAT, START_LAT),1,mean)) %>% #Find midpoint lat
#  mutate(MID_LONG = apply(cbind(END_LONG, START_LONG),1,mean)) %>% #Find midpoint long
#  mutate(PRORATED.TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_190) %>% rowSums(na.rm = TRUE)) #Adding all prorated shell height frequencies to check against ABUND.STD

#Tidy up lat and long coords, add presence/absence etc.
hm.live <- hm.live %>%
  mutate(PRESENT.ABSENT = if_else(TOTAL >= 1, 1, 0)) %>% # Absent == 0, Present = 1
  mutate(START_LAT = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(START_LONG = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  mutate(END_LAT = convert.dd.dddd(END_LAT)) %>% #Convert to DD
  mutate(END_LONG = convert.dd.dddd(END_LONG)) %>% #Convert to DD
  mutate(MID_LAT = apply(cbind(END_LAT, START_LAT),1,mean)) %>% #Find midpoint lat
  mutate(MID_LONG = apply(cbind(END_LONG, START_LONG),1,mean)) %>% #Find midpoint long
  mutate(PRORATED.TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_190) %>% rowSums(na.rm = TRUE)) #Adding all prorated shell height frequencies to check against ABUND.STD

# check against PRORATE TOTAL (total prorated shell height frequencies) and ABUND.STD. 

#Histogram plot of difference between PRORATE TOTAL and ABUND.STD. Exclude SAMPLE.METHOD == 1 (No SHF, only abundance)
ggplot(hm.live %>% filter(!SAMPLE.METHOD == 1), aes(x = PRORATED.TOTAL - ABUND.STD)) +
  geom_histogram()

ggplot(hm.live, aes(x = PRORATED.TOTAL - ABUND.STD)) +
  geom_histogram()

head(hm.live[hm.live$ABUND.STD > 0,]) 

#How many tows had presence of horse mussel?
nrow(hm.live %>% filter(PRESENT.ABSENT == 1))

#was catch subsampled?
nrow(hm.live %>% filter(PRORATE == "y"))

hm.live %>% filter(PRORATE.FACTOR >= 2)


# CHECK SPATIAL POINTS ----------------------------------------------------
#Check by Cruise and year
#NOTE - Only start and end points are recorded in sctows - some tows may appear shorter than others - this could be due to curved track lines during tows. Midpoints are caluclated assuming straight lines between start and end points.

year <- 2021

#CHECK BF
Cruise <- paste0("BF", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.live, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.live, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.live, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK GM
Cruise <- paste0("GM", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.GM2021, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.GM2021, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.GM2021, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK BI
Cruise <- paste0("BI", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.live, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.live, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.live, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK SFA29W
Cruise <- paste0("SFA29", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.live, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.live, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.live, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

# Final edits --------------------------------------------------

hm.live <- hm.live %>%
mutate(across(BIN_ID_0:BIN_ID_190, round, 6)) %>% #Round to 6 sigfigs
dplyr::select(!c(TOTAL, TOW_LEN, NUM_LINED_FREQ, PRORATED.TOTAL, PRORATE.FACTOR, PRORATE)) %>% #remove columns
  dplyr::select(ID,	CRUISE,	TOW,	STRATA_ID,	SAMPLE.METHOD,	START_LAT,	START_LONG,	END_LAT,	END_LONG,	MID_LAT,	MID_LONG,
               BIN_ID_0:BIN_ID_190, ABUND.STD,	PRESENT.ABSENT)

#Save out data for loading later:
saveRDS(hm.live, file = paste0(dir,"Prorated/horsemussellive_prorated.rds"))

#SAVE AS SINGLE DATASET
write.csv(hm.live, paste0(dir,"Prorated/horsemussellive_prorated_2018-2021.csv"))

#Saves files by cruise
#for(i in unique(hm.live$CRUISE)){
#  write.csv(hm.live %>% filter(CRUISE == i), paste0(dir,"Prorated/",i, "_horsemussellive_prorated.csv"))
#}

# Spatial plot - live density --------------------------------------------------

com.contours <- contour.gen(hm.GM2021 %>% filter(SAMPLE.METHOD != 1) %>% dplyr::select(ID, MID_LONG, MID_LAT, ABUND.STD),
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
               add_layer = list(land = "grey", scale.bar = c('tl',0.5,-1,-1)), # bathy = "ScallopMap"
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

#Plot without contours
p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", scale.bar = c('tl',0.5,-1,-1))) # bathy = "ScallopMap"
               

#Creating strata shapefiles:
smb.strata <- strata %>% filter(STRATA_ID==22)

brier.strata <-strata %>% filter(STRATA_ID %in% c(23, 56)) %>% 
  st_union() %>% 
  st_make_valid()

lurcher.strata <- strata %>% filter(STRATA_ID==24) 
sfa29.strata <- strata %>% filter(STRATA_ID %in% c(41:45)) %>% 
  st_union() %>% 
  st_make_valid()

spa6.strata <- strata %>% filter(STRATA_ID %in% c(30:32)) %>% 
  st_union() %>% 
  st_make_valid()

spa4and5.strata <- strata %>% filter(STRATA_ID %in% c(1:21, 47, 48)) %>%
  st_set_precision(100) %>% 
  st_union() %>% 
  st_make_valid()
  

upperbay.strata <- strata %>% filter(STRATA_ID %in% c(35, 49, 50, 51, 52)) %>% 
  st_union() %>% 
  st_make_valid()

innerbay.strata <- strata %>% filter(STRATA_ID %in% c(37:38, 53:55)) %>%
  st_set_precision(100000) %>% 
  st_union() %>% 
  st_make_valid()

midbaysouth.strata <- strata %>% filter(STRATA_ID == 39)

sfa29.strata <- SFA29 %>%
  st_union()


#PLOT

hm.live$PRESENT.ABSENT <- as.factor(hm.live$PRESENT.ABSENT)
hm.live <- hm.live %>% filter(!is.na(PRESENT.ABSENT))
  
p + #Plot survey data and format figure.
  #geom_spatial_point(data = hm.live, aes(START_LONG, START_LAT, colour = PRESENT.ABSENT, shape = PRESENT.ABSENT), size = 1) +
  #scale_color_manual(values=c("#69b3a2", "purple", "black"))+
  #geom_sf(data = ebsa, size = 0.9, colour = "firebrick", fill = NA) +
  geom_sf(data = smb.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =brier.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =lurcher.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =spa6.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =spa4and5.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =upperbay.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =midbaysouth.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =innerbay.strata, size = 0.6, colour = "black", fill = NA) +
  geom_sf(data =sfa29.strata, size = 0.6, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "Live M.modiolus Density"),
  #guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.50,-64.30), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.80,.52), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8)) + #Legend bkg margin (top, right, bottom, left)
        guides(shape = guide_legend(override.aes = list(size = 3)))

ggsave(filename = paste0(dir,'ContPlot_LiveHM_Density 2018-',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#ggsave(filename ="Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Documents/DataExploration_report/Figures/temp_HorseMussel_blank.png", plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


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

#Check for duplicate IDs
n_occur <- data.frame(table(hm.dead$ID))
n_occur[n_occur$Freq > 1,] #no dups

#Check IDs match
dim(left_join(hm.dead, tows.dat, by = "ID"))[1] == dim(inner_join(hm.dead, tows.dat, by = "ID"))[1]


#Now Join the tow data with horse mussel dead data
hm.dead <- left_join(hm.dead, tows.dat, by = "ID") %>% 
  dplyr::select(!c(CRUISE.y, COMMENTS, TOW.y, TOW_TYPE_ID)) %>% #remove columns
  rename(CRUISE = CRUISE.x) %>% 
  rename(TOW = TOW.x)

#replace all NAs in bin and total columns with 0s UNLESS SAMPLE.METHOD == 1, where SHF were not recorded - Keep NAs.
hm.dead <- hm.dead %>%
  mutate(across(BIN_ID_0:TOTAL, ~if_else(SAMPLE.METHOD != 1, replace_na(.,0), NULL)))

#During data entry, PRORATE.FACTOR is only entered if catch was subsampled (i.e. from 2 (sampleable) lined drags, only one was sampled/or catch was subsampled using a Horse mussel measuring device (HMMD). *Note prorate factor is NOT entered if gear becomes detached and is not samplable.

#replace all NAs in PRORATE.FACTOR with 1.
hm.dead$PRORATE.FACTOR[is.na(hm.dead$PRORATE.FACTOR)] <- 1

#Save as seperate object (raw) to compare to prorated copy.
hm.dead.raw <- hm.dead

# Prorating ---------------------------------------------------------------

#SAMPLE.METHOD
# 0 --- No subsample (2 lined drags)
# 1 --- Counts recorded, but no SHF (2 lined drags) (i.e. presence and abundance). Sometimes Horse Mussel Measuring Device (HMMD) volume is recorded (i.e. bucket vol.)
# 2 --- Subsampled by HMMD (2 lined drags)
# 3 --- Subsampled by drag (1 lined drag)
# NA --- Not counted or measured, but noted (occurs once in GM2018 for clappers - FEW clappers)

#Check sample methods
hm.dead %>% filter(SAMPLE.METHOD == 0)
hm.dead %>% filter(SAMPLE.METHOD == 1)
hm.dead %>% filter(SAMPLE.METHOD == 2)
hm.dead %>% filter(SAMPLE.METHOD == 3)
hm.dead %>% filter(is.na(SAMPLE.METHOD))

#Check tows where number of lined drags = 1 and subsampled by HMMD - should result in none (would have to adjust prorate factor if this happened)
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1) %>% filter(SAMPLE.METHOD == 1)) # 0
nrow(hm.dead %>% filter(NUM_LINED_FREQ == 1)) #21 tows have only one lined drag sampled.

#How many tows with horse mussels were subsampled because a lined drag came loose?
hm.dead %>% filter(NUM_LINED_FREQ == 1 & TOTAL > 0)

hm.dead.GM2021.raw <- hm.dead.raw %>% filter(CRUISE == "GM2021") #Test with GM2021 data
hm.dead <- hm.dead.GM2021.raw
head(hm.dead )


#Standardized Total Abundance - Prorating for subsample of gear (2 lined drags), prorating for amount of gear sampled (NUM_LINED_FREQ - either 1 or 2), and standardizing to 800m tow length. This assumes that the individual drag width is equivelent for all drags (assumes gear width of 4ft). 
hm.dead$ABUND.STD <- (hm.dead$TOTAL*hm.dead$PRORATE.FACTOR)*(2/hm.dead$NUM_LINED_FREQ)*(800/(hm.dead$TOW_LEN))
hm.dead$ABUND.STD <- hm.dead$ABUND.STD*(17.5/4)
##SHF - Prorating for subsample of gear (2 lined drags), prorating for amount of gear sampled (NUM_LINED_FREQ - either 1 or 2), and standardizing to 800m tow length. This assumes that the individual drag width is equivelent for all drags (assumes gear width of 4ft). 
for(i in 1:nrow(hm.dead)) {
  for(j in 4:42){
    hm.dead[i,j] <- (hm.dead[i,j]*hm.dead$PRORATE.FACTOR[i])*(2/hm.dead$NUM_LINED_FREQ[i])*(800/(hm.dead$TOW_LEN[i]))
    hm.dead[i,j] <- hm.dead[i,j]*(17.5/4)
  }
}

#hm.test.dead <- hm.dead %>% filter(ABUND.STD > 0)
write.csv(hm.dead, "Y:/Inshore/Databases/Scallsur/ScallsurUpdates/2022/Db_Update_HorseMusselAddition/testing/Feb082022/hm_dead_standardize_R_version.csv")


#Tidy up lat and long coords, add presence/absence etc.
hm.dead <- hm.dead %>%
  mutate(PRESENT.ABSENT = if_else(TOTAL >= 1, 1, 0)) %>% # Absent == 0, Present = 1
  mutate(START_LAT = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(START_LONG = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  mutate(END_LAT = convert.dd.dddd(END_LAT)) %>% #Convert to DD
  mutate(END_LONG = convert.dd.dddd(END_LONG)) %>% #Convert to DD
  mutate(MID_LAT = apply(cbind(END_LAT, START_LAT),1,mean)) %>% #Find midpoint lat
  mutate(MID_LONG = apply(cbind(END_LONG, START_LONG),1,mean)) %>% #Find midpoint long
  mutate(PRORATED.TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_190) %>% rowSums(na.rm = TRUE)) #Adding all prorated shell height frequencies to check against ABUND.STD


# check against PRORATE TOTAL (total prorated shell height frequencies) and ABUND.STD. 

#Histogram plot of difference between PRORATE TOTAL and ABUND.STD. Exclude SAMPLE.METHOD == 1 (No SHF, only abundance)
ggplot(hm.dead %>% filter(!SAMPLE.METHOD == 1), aes(x = PRORATED.TOTAL - ABUND.STD)) +
  geom_histogram()

ggplot(hm.dead, aes(x = PRORATED.TOTAL - ABUND.STD)) +
  geom_histogram()

head(hm.dead[hm.dead$ABUND.STD > 0,]) 

#How many tows had presence of dead horse mussel?
nrow(hm.dead %>% filter(PRESENT.ABSENT == 1))

#was catch subsampled?
nrow(hm.dead %>% filter(PRORATE == "y"))

hm.dead %>% filter(PRORATE.FACTOR >= 2)


# CHECK SPATIAL POINTS ----------------------------------------------------
#Check by Cruise and year
#NOTE - Only start and end points are recorded in sctows - some tows may appear shorter than others - this could be due to curved track lines during tows. Midpoints are caluclated assuming straight lines between start and end points.

year <- 2018

#CHECK BF
Cruise <- paste0("BF", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.dead, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.dead, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.dead, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK GM
Cruise <- paste0("GM", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.dead, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.dead, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.dead, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK BI
Cruise <- paste0("BI", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.dead, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.dead, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.dead, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

#CHECK SFA29W
Cruise <- paste0("SFA29", year) #BI, GM or SFA29

hm.start.sf <-  st_as_sf(hm.dead, coords = c("START_LONG", "START_LAT"), crs = 4326)
hm.mid.sf <- st_as_sf(hm.dead, coords = c("MID_LONG", "MID_LAT"), crs = 4326)
hm.end.sf <- st_as_sf(hm.dead, coords = c("END_LONG", "END_LAT"), crs = 4326)


mapview::mapview(hm.start.sf %>% filter(CRUISE %in% Cruise), col.regions = "green") +
  mapview::mapview(hm.mid.sf %>% filter(CRUISE %in% Cruise), col.regions = "orange") +
  mapview::mapview(hm.end.sf %>% filter(CRUISE %in% Cruise), col.regions = "blue")

# Final edits --------------------------------------------------

hm.dead <- hm.dead %>%
  mutate(across(BIN_ID_0:BIN_ID_190, round, 6)) %>% #Round to 6 sigfigs
  dplyr::select(!c(TOTAL, TOW_LEN, NUM_LINED_FREQ, PRORATED.TOTAL)) #remove columns

#Save out data for loading later:
saveRDS(hm.dead, file = paste0(dir,"Prorated/horsemusseldead_prorated.rds"))

#SAVE AS SINGLE DATASET

#Saves files by cruise
#for(i in unique(hm.dead$CRUISE)){
#  write.csv(hm.dead %>% filter(CRUISE == i), paste0(dir,"Prorated/",i, "_horsemusseldead_prorated.csv"))
#}

# Spatial plot - Dead density --------------------------------------------------

com.contours <- contour.gen(hm.dead %>% dplyr::select(ID, START_LONG, START_LAT, ABUND.STD),
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



# CHECKING PRORATE FUNCTION ON LINED SCALLOP DATA --------------------------------------------

# Read in lined Tow data from database - use e.g. BF2021 ------------------------------------------

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Loading standardized live lined scallop from database
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sclinedlive_std			",
  "WHERE CRUISE = 'BF2021'",
  sep=""
)

scall.lined.std <- dbGetQuery(chan, quer2)

scall.lined.std <- scall.lined.std %>%
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)

scall.lined.std <- scall.lined.std %>% 
  mutate(TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)) %>% 
  arrange(TOW_NO)

#From tows data (loaded at start of script) - filter out BF2021
BF.2021.tows <- tows.dat %>% 
  filter(CRUISE == "BF2021") %>%
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE) %>% 
  dplyr::select(c(ID, TOW_LEN, NUM_LINED_FREQ))

#Join to get tow length data
scall.lined.std <- left_join(scall.lined.std, BF.2021.tows, by = "ID")

scall.lined.std<- scall.lined.std %>% 
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE))

scall.lined.std$TOTAL_4ft <- (scall.lined.std$tot)*(4/17.5)


#Read in raw data shf file for proration and comparison
raw.scal.dat <- read.csv("Y:/INSHORE SCALLOP/Survey/2021/data entry templates and examples/BF2021/BF2021_dhf.csv")

#format recruit sizes
lined.dat.rec <- raw.scal.dat %>%
  filter(GEAR == 1 & c == 0)
colnames(lined.dat.rec) <- str_replace(colnames(lined.dat.rec), "X", "BIN_ID_")

#format Commercial sizes
lined.dat.com <- raw.scal.dat %>%
  filter(GEAR == 1) %>% 
  filter(c == 1)
names(lined.dat.com)[5:24] <- as.character(paste0("BIN_ID_", seq(100, 195, by = 5))) #Rename bin headers

#Combine both commercial and recruit
lined.dat.raw <- cbind(lined.dat.com %>% dplyr::select(CRUISE, TOW, GEAR, DEPTH), lined.dat.rec %>% dplyr::select(., BIN_ID_0:BIN_ID_95), lined.dat.com %>% dplyr::select(., BIN_ID_100:BIN_ID_195)) %>% 
  unite(ID, c("CRUISE", "TOW"), sep = ".", remove = FALSE)

lined.dat.raw <- lined.dat.raw %>% 
  mutate(TOTAL = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE))

lined.dat.raw <- left_join(lined.dat.raw, BF.2021.tows, by = "ID")

lined.dat.raw$ABUND.STD <- (lined.dat.raw$TOTAL)*(2/lined.dat.raw$NUM_LINED_FREQ)*(800/(lined.dat.raw$TOW_LEN))



##SHF - Prorating for subsample of gear (2 lined drags), prorating for amount of gear sampled (NUM_LINED_FREQ - either 1 or 2), and standardizing to 800m tow length. This assumes that the individual drag width is equivelent for all drags (assumes gear width of 4ft). 
for(i in 1:nrow(lined.dat.raw)) {
  for(j in 4:42){
    lined.dat.raw[i,j] <- (lined.dat.raw[i,j]*hm.live$PRORATE.FACTOR[i])*(2/hm.live$NUM_LINED_FREQ[i])*(800/(hm.live$TOW_LEN[i]))
  }
}


#Look at total from database and total (now prorated) from raw data.
compare <- data.frame(cbind(lined.dat.raw$ABUND.STD, scall.lined.std$TOTAL_4ft))

compare <- compare %>% 
  mutate(Prop = X1/X2)

summary(compare)

#Now compare database shf to prorated raw shf.
#Histogram plot of  database shf and prorated raw shf
ggplot(lined.dat, aes(x = TOTAL)) +
  geom_histogram()

ggplot(scall.lined.std , aes(x = RAW.TOTAL)) +
  geom_histogram()