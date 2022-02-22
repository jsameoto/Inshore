
# required packages
require (PBSmapping)
require (RColorBrewer)
library(scales)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
library(raster)
library(viridis)
library(stars)




#Load functions:

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/archive/2016/contour.gen.r")
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

#Load SDM Layer 

#Binned raster
#sdm <-raster("Y:/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/sdm_bin_hml/w001001.adf", values = TRUE)

#raster (not binned)
sdm <-read_stars("Y:/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/sdm_sfa29/w001001.adf", values = TRUE)
sdm <- st_warp(sdm, crs = 4326)

bin.sdm <- cut(sdm, c(0,0.3,0.6,1.0))
plot(bin.sdm)


#sdm <- projectRaster(sdm, crs = 4326, method = "ngb") #st_transform
#mapview::mapview(sdm) 
#plot(sdm)

#sdm.reclas <- reclassify(sdm, c(0,0.3,0.3, 0.3,0.6,0.6,0.6,1,1.0), include.lowest = TRUE)
#sdm.reclas <- projectRaster(sdm.reclas, crs = 4326, method = "ngb") #st_transform

#plot(sdm.reclas)
#mapview::mapview(sdm)+
#mapview::mapview(sdm.reclas)



#Run base layer:
p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey30", scale.bar = c('bl',0.5,-1,-1)))

p + geom_stars(data = bin.sdm, aes(x=x,y=y, fill = w001001.adf))+
  scale_fill_manual(values = c('darkblue', 'grey62', 'firebrick3'), na.value = 'transparent', name = "Habitat Suitability", breaks = levels(bin.sdm$w001001.adf))+
  geom_sf(data = sfa29.poly, size = 1, colour = "black", fill = NA)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID !="E"), aes(label = ET_ID),size = 6, colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 6, colour = "black")+
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        legend.position = c(.85,.80), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3))
  #geom_sf_label(data = sfa29.poly, aes(label = ET_ID),size = 3, colour = "black", fill = "white")
  
#Greyscale
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library("gridSVG")
library("gridExtra")

p + geom_stars(data = bin.sdm, aes(x=x,y=y, fill = w001001.adf))+
  scale_fill_manual(values = c('darkblue', 'grey62', 'firebrick3'), na.value = 'transparent', name = "Habitat Suitability", breaks = levels(bin.sdm$w001001.adf))+
  geom_sf(data = sfa29.poly, size = 1, colour = "black", fill = NA)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID !="E"), aes(label = ET_ID),size = 6, colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 6, colour = "black")+
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        legend.position = c(.85,.80), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3))







#Plots with Raster - working
#p + geom_raster(data = sdm, aes(x=x,y=y, fill = as.factor(w001001)))+
#  scale_fill_manual(values = c('darkblue','darksalmon', 'red4'), breaks = c(0.3,0.6,1.0), na.value = 'transparent') #c(1,2,3)
  #scale_fill_gradient2(low='darkslateblue',mid = 'grey62', high='firebrick2', midpoint = 0.5, na.value = 'transparent')
  



#p + geom_raster(data = sdm, aes(x=x,y=y, fill = w001001))+
  #scale_fill_viridis(discrete=TRUE, option="magma")
#  scale_fill_binned(breaks = c(0.3, 0.6, 1.0), na.values = "transparent")#, values = c('darkslateblue','grey62', 'firebrick2'),na.value = "transparent")
 
  
