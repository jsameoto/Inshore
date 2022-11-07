

#Load in Prorated HM datasheets (using R script "HM_prorate_and_compare.R) and plot numbers per tow.

library(tidyverse)
library(ROracle)
require(maptools)
require(sf)
require(raster)
#require(lubridate)
#library(magrittr)

#ROracle - credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

dir <- "Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/"
#dir <- "C:/Users/WILSONB/Documents/1_Projects/HM_project_temp/Prorated/"
survey.year <- 2022
Year <- c(2018:survey.year)
Year <- Year[! Year %in% 2020]

#### Import Source functions####
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
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


#Load VMS rasters:

ss.scallop.vms <- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Scallop_VMS_Percentiles.tif")
ss.scallop.vms <- projectRaster(ss.scallop.vms, crs = 4326)

ss.gf.mobile.vms<- raster("Z:/Projects/BoF_Mapping_Project/Data/GIS_Layers/Other/FishingEffort_Rastor/Fisheries_VMS_TIFF/SS_Groundfish_Mobile_VMS_Percentiles.tif")
ss.gf.mobile.vms<-projectRaster(ss.gf.mobile.vms, crs = 4326)


#Read in Conservation areas
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/Conservation_areas/DFO_EBSA.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
HM.ebsa <- st_read(paste0(temp2, "/DFO_EBSA.shp")) %>% filter(Name == "Modiolus reefs, NS Shore") %>% st_transform(crs = 4326) %>% dplyr::select(Name)

# Load live horse mussel data for all years and areas (2018-present) ------------------------------------------

#SPA1A1B4and5
BF.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,"/Prorated/BF",Year[i],"_live_standardize_R_version.csv",sep=""), header=T)
  BF.hm.live <- rbind(BF.hm.live,temp)
}

#SPA3
BI.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,"/Prorated/BI",Year[i],"_live_standardize_R_version.csv",sep=""), header=T)
  BI.hm.live <- rbind(BI.hm.live,temp)
}

#SPA6
GM.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,"/Prorated/GM",Year[i],"_live_standardize_R_version.csv",sep=""), header=T)
  GM.hm.live <- rbind(GM.hm.live,temp)
}

#SFA29
SFA29.hm.live <- NULL
for(i in 1:length(Year))
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(dir,"/Prorated/SFA29",Year[i],"_live_standardize_R_version.csv",sep=""), header=T)
  SFA29.hm.live <- rbind(SFA29.hm.live,temp)
}

hm.live <- rbind(BF.hm.live, BI.hm.live, GM.hm.live, SFA29.hm.live) %>%  #Combine SPA data together if data is available
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE)  #Creates ID column with cruise and tow number

colnames(hm.live) <- str_replace(colnames(hm.live), "X", "BIN_ID_") #Rename bin headers


#  Set up data for spatial plot ------------------------------------------

hm.live <- hm.live %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE))

hm.live.sf <- st_as_sf(hm.live, coords = c("lon","lat"), crs = 4326)


com.contours <- contour.gen(hm.live %>% 
                              dplyr::select(ID, lon, lat, tot),
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
cfd <- scale_fill_manual(values = alpha(col, 1.0), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector



#Plot with Pecjector:

p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p+
  geom_sf(data = hm.live.sf, colour = "black", size = 0.5)+
  geom_sf(data = HM.ebsa, colour = "deeppink4", fill = NA, size = 1)+
  coord_sf(xlim = c(-67.50,-64.05), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                       axis.title = element_text(size = 12),
                       axis.text = element_text(size = 10),
                       legend.title = element_text(size = 10, face = "bold"), 
                       legend.text = element_text(size = 8),
                       legend.position = c(.84,.32), #legend position
                       legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
                       legend.box.margin = margin(6, 8, 6, 8))


#save
ggsave(filename = paste0(dir,'ContPlot_HM_Density_and_tows_2018-2022.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#Plot with Pecjector - just survey locations:

p <- pecjector(area = "inshore",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)))

p+
  geom_sf(data = hm.live.sf, colour = "black", size = 0.5)+
  #geom_sf(data = HM.ebsa, colour = "deeppink4", fill = NA, size = 1)+
  coord_sf(xlim = c(-67.50,-64.05), ylim = c(43.10,45.80), expand = FALSE)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.84,.32), #legend position
        legend.box.background = element_rect(colour = "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))


#save
ggsave(filename = paste0(dir,'ScallopSurvey_tows_2018-2022.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

