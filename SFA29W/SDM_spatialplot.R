
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
require(forcats)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library("gridSVG")
library("gridExtra")
library(magick)

#Set year to save figures to appropriate directory:
assessment.year <- 2022

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

#raster (not binned)
sdm <-read_stars("Y:/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/sdm_sfa29/w001001.adf", NA_value = NA)
sdm <- st_warp(sdm, crs = 4326)

bin.sdm <- cut(sdm, c(0,0.3,0.6,1.0), right = FALSE)

# Load Pecjector and Convert.dd.ddd functions then Run pecjector

p <- pecjector(area =list(x=c( -66.7, -65.43), y=c(43.72, 43.1), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey40", scale.bar = c('bl',0.1,-1,-1)))
#Figuring out where ticks should go? Doesn't plot properly unless coord_sf() is added...       
pt <- p + coord_sf(xlim = c(-66.7, -65.43), ylim = c(43.72, 43.1), expand = FALSE)
lon.loc <- c(66.6, 66.2, 65.8)
# Convert to deg-min-sec
lon.tmp <- convert.dd.dddd(lon.loc,'deg.min.sec')
# Repeat
lat.loc <- c(43.20, 43.40, 43.60)
lat.tmp <- convert.dd.dddd(lat.loc,'deg.min.sec')
lony <- paste0(substr(lon.tmp$Degree_Min$Degree_Minutes,1,2),expression("*{degree}*"), substr(lon.tmp$Degree_Min$Degree_Minutes,4,5),expression("*{minute}*"), substr(lon.tmp$Degree_Min_Sec$Degree_Minute_Seconds,7,8),expression("*{second}*W"))
latty <- paste0(substr(lat.tmp$Degree_Min$Degree_Minutes,1,2),expression("*{degree}*"), substr(lat.tmp$Degree_Min$Degree_Minutes,4,5),expression("*{minute}*"), substr(lat.tmp$Degree_Min_Sec$Degree_Minute_Seconds,7,8),expression("*{second}*N"))
# And then replot the figure
p2 <- pt +
  scale_x_continuous(breaks =-lon.loc,labels=parse(text = lony)) +
  scale_y_continuous(breaks = lat.loc,labels=parse(text = latty))
p2

# Coloured EN Version -------------------------------------------------------

  p2 +
  geom_stars(data = bin.sdm, aes(x=x,y=y, fill = w001001.adf))+
  scale_fill_manual(values = c('firebrick3', 'grey68', 'darkblue'), na.value = 'transparent', name = "Habitat Suitability", breaks = c("[0.6,1)", "[0.3,0.6)", "[0,0.3)"),labels = c("[0.6,1)"="High [0.6,1)", "[0.3,0.6)"= "Medium [0.3,0.6) ", "[0,0.3)"="Low [0,0.3)"))+
  geom_sf(data = sfa29.poly, size = 1, colour = "black", fill = NA)+
  geom_text(aes(label = "Nova Scotia"), x = -65.6, y = 43.65, size = 4, color = "black")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="A"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_x = -0.12 )+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="C"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.1)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "D"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.09)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold")+
  coord_sf(xlim = c(-66.7, -65.43), ylim = c(43.72, 43.1), expand = FALSE)+
  theme(legend.key = element_rect(colour = "black", size=1),
        legend.key.size = unit(5,"mm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 7),
        legend.position = c(.14,.365), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

#save
ggsave(filename = paste0("Y:/Inshore/SFA29/", assessment.year,"/Assessment/Figures/ScallopSDM_binned_coloured.png"), plot = last_plot(), scale = 2.5, width = 7, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

# Coloured FR Version -------------------------------------------------------

#West = Ouest
lony.fr <- paste0(substr(lon.tmp$Degree_Min$Degree_Minutes,1,2),expression("*{degree}*"), substr(lon.tmp$Degree_Min$Degree_Minutes,4,5),expression("*{minute}*"), substr(lon.tmp$Degree_Min_Sec$Degree_Minute_Seconds,7,8),expression("*{second}*O"))
# And then replot the figure
p3 <- pt +
  scale_x_continuous(breaks =-lon.loc,labels=parse(text = lony.fr)) +
  scale_y_continuous(breaks = lat.loc,labels=parse(text = latty))
p3

p3 +
  geom_stars(data = bin.sdm, aes(x=x,y=y, fill = w001001.adf))+
  scale_fill_manual(values = c('firebrick3', 'grey68', 'darkblue'), na.value = 'transparent', name = "Qualité de l'habitat", breaks = c("[0.6,1)", "[0.3,0.6)", "[0,0.3)"),labels = c("[0.6,1)"="Élevée [0.6,1)", "[0.3,0.6)"= "Moyenne [0.3,0.6) ", "[0,0.3)"="Faible [0,0.3)"))+
  geom_sf(data = sfa29.poly, size = 1, colour = "black", fill = NA)+
  geom_text(aes(label = "Nouvelle-Écosse"), x = -65.6, y = 43.65, size = 4, color = "black")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="A"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_x = -0.12 )+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="C"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.1)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "D"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.09)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold")+
  coord_sf(xlim = c(-66.7, -65.43), ylim = c(43.72, 43.1), expand = FALSE)+
  theme(legend.key = element_rect(colour = "black", size=1),
        legend.key.size = unit(5,"mm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 7),
        legend.position = c(.14,.365), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

#save
ggsave(filename = paste0("Y:/Inshore/SFA29/", assessment.year,"/Assessment/Figures/ScallopSDM_binned_coloured_FR.png"), plot = last_plot(), scale = 2.5, width = 7, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

# Greyscale EN Version -------------------------------------------------------

#Convert to sf object
bin.sdm.sf <- st_as_sf(bin.sdm, na.rm = FALSE)

#Group all bin classes (dissolving the bin classes)
bin.sdm.dissolve <- bin.sdm.sf %>%
  group_by( w001001.adf) %>%
  summarise()

#Cropping shapefile:
bin.sdm.int <-st_intersection(bin.sdm.dissolve, st_union(sfa29.poly$geometry))
plot(bin.sdm.int)

#"[0.6,1)", "[0.3,0.6)", "[0,0.3)"

#Formatting for plotting (changing NAs to "No_information" and renaming the bin names)
bin.sdm.nas <- bin.sdm.int %>%
  mutate(w001001.adf = case_when(w001001.adf == "[0,0.3)" ~ "Low", 
                                 w001001.adf == "[0.3,0.6)" ~ "Medium",
                                 w001001.adf == "[0.6,1)" ~ "High")) %>% 
  mutate(w001001.adf = fct_explicit_na(w001001.adf, na_level = "No_information")) %>% 
  mutate(sdm = w001001.adf)

#bin.sdm.nas.FR <- bin.sdm.int %>%
#  mutate(w001001.adf = case_when(w001001.adf == "[0,0.3)" ~ "Faible", 
                                # w001001.adf == "[0.3,0.6)" ~ "Moyenne",
                                # w001001.adf == "[0.6,1)" ~ "Élevée")) %>% 
#  mutate(w001001.adf = fct_explicit_na(w001001.adf, na_level = "Aucune information")) %>% 
#  mutate(sdm = w001001.adf)
                                 
#EN Plot
#p <- pecjector(area =list(x=c( -66.7, -65.4), y=c(43.7, 43.1), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               #add_layer = list(land = "grey40", scale.bar = c('bl',0.2,-1,-1)))
p2 +
  geom_sf_pattern(data = bin.sdm.nas, aes(pattern = w001001.adf, fill = w001001.adf, colour = w001001.adf),
                  colour = NA,
                  pattern_colour = NA,
                  pattern_fill = alpha("black", 0.6),
                  pattern_density = 0.25,
                  pattern_spacing = 0.010,
                  pattern_key_scale_factor = 0.5) + 
  scale_colour_manual(values= c("white","transparent","transparent","transparent"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="High [0.6,1)", "Medium"="Medium [0.3,0.6)", "Low"="Low [0,0.3)", "No_information" = "No information"), name = "Habitat Suitability")+
  scale_fill_manual(values = c("white","grey70","black","transparent"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="High [0.6,1)", "Medium"="Medium [0.3,0.6)", "Low"="Low [0,0.3)", "No_information" = "No information"),name = "Habitat Suitability") +
  scale_pattern_discrete(choices = c(Low = "none", Medium = "none", High = "none", No_information = "crosshatch"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="High [0.6,1)", "Medium"="Medium [0.3,0.6)", "Low"="Low [0,0.3)", "No_information" = "No information"),name = "Habitat Suitability")+
  geom_sf(data = sfa29.poly, size = 1.15, colour = "black", fill = NA)+
  geom_text(aes(label = "Nova Scotia"), x = -65.6, y = 43.65, size = 4, color = "black")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="A"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_x = -0.12 )+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 5, colour = "white",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 2.5, colour = "white",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="C"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.1)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "D"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.09)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 5, fontface = "bold",fontface = "bold", colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 2.5, fontface = "bold",fontface = "bold", colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold")+
  coord_sf(xlim = c(-66.7, -65.43), ylim = c(43.72, 43.1), expand = FALSE)+
  theme(legend.key = element_rect(colour = "black", size=0.5),
        legend.key.size = unit(5,"mm"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 7),
        legend.position = c(.14,.39), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

#FR version

p3 +
  geom_sf_pattern(data = bin.sdm.nas, aes(pattern = w001001.adf, fill = w001001.adf, colour = w001001.adf),
                  colour = NA,
                  pattern_colour = NA,
                  pattern_fill = alpha("black", 0.6),
                  pattern_density = 0.25,
                  pattern_spacing = 0.010,
                  pattern_key_scale_factor = 0.5) + 
  scale_colour_manual(values= c("white","transparent","transparent","transparent"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="Élevée [0.6,1)", "Medium"="Moyenne [0.3,0.6)", "Low"="Faible [0,0.3)", "No_information" = "Aucune information"), name = "Qualité de l'habitat")+
  scale_fill_manual(values = c("white","grey70","black","transparent"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="Élevée [0.6,1)", "Medium"="Moyenne [0.3,0.6)", "Low"="Faible [0,0.3)", "No_information" = "Aucune information"),name = "Qualité de l'habitat") +
  scale_pattern_discrete(choices = c(Low = "none", Medium = "none", High = "none", No_information = "crosshatch"), breaks = c("High", "Medium", "Low", "No_information"),labels = c("High"="Élevée [0.6,1)", "Medium"="Moyenne [0.3,0.6)", "Low"="Faible [0,0.3)", "No_information" = "Aucune information"),name = "Qualité de l'habitat")+
  geom_sf(data = sfa29.poly, size = 1.15, colour = "black", fill = NA)+
  geom_text(aes(label = "Nouvelle-Écosse"), x = -65.6, y = 43.65, size = 4, color = "black")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="A"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_x = -0.12 )+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 5, colour = "white",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 2.5, colour = "white",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "B"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.13, nudge_x = -0.0109)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID =="C"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.1)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "D"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold", nudge_y = 0.09)+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 5, fontface = "bold",fontface = "bold", colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 2.5, fontface = "bold",fontface = "bold", colour = "white")+
  geom_sf_text(data = sfa29.poly %>% filter(ET_ID == "E"), aes(label = ET_ID),size = 4, colour = "black",fontface = "bold")+
  coord_sf(xlim = c(-66.7, -65.43), ylim = c(43.72, 43.1), expand = FALSE)+
  theme(legend.key = element_rect(colour = "black", size=0.5),
        legend.key.size = unit(5,"mm"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 7),
        legend.position = c(.14,.39), #legend position
        legend.box.background = element_rect(colour = "white", fill= "white"), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

#annotation_scale(width_hint = scale.width,pad_x = unit(xpad + 1.5, "cm"), pad_y = unit(ypad + 1.5, "cm")) + 
#annotation_north_arrow(location = scal.loc, which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
 #                      pad_x = unit(xpad + 1.5, "cm"), pad_y = unit(ypad+1.9, "cm"),style = north_arrow_fancy_orienteering)+

#save
ggsave(filename = paste0("Y:/Inshore/SFA29/", assessment.year,"/Assessment/Figures/ScallopSDM_binned_greyscale_FR.png"), plot = last_plot(), scale = 2.5, width = 7, height = 5, dpi = 300, units = "cm", limitsize = TRUE)
