###................................................................................###
###                       Scallop Health - Spatial Figures                         ###
###                           Full BoF and Approaches                              ###
###                                 Dec 2021                                       ###
###                                                                                ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###

# Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count and Clappers for BoF: 
#Full Bay
#SPA 1A
#SPA1B
#SPA 3
#SPA4 and 5
#and SPA 6

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
require (rmapshaper)
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

#set year 
survey.year <- 2023  #removed maxyear in script and changed to survey year
assessmentyear <- 2023 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/BoF/"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,assessmentyear,"/Assessment/Figures/")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# ----Import Source functions and polygons---------------------------------------------------------------------


#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")

#### Import Mar-scal functions 
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
# Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
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
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles

#Management zones
mgmt.zones <- rbind(st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A"), st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B"), st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp"))%>% mutate(ET_ID = "2"), st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp"))%>% mutate(ET_ID = "3"), st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp"))%>% mutate(ET_ID = "4"), st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp"))%>% mutate(ET_ID = "5"), st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A"), st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B"), st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C"),  st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")) %>% 
  st_transform(crs= 4326)

grey.zone <- st_read(paste0(temp2, "/GreyZone_lines_NAD83.shp"))
spa3.poly <- st_read(paste0(temp2, "/SPA3_modelledArea.shp"))
inVMS <- st_read(paste0(temp2, "/SPA6_VMSstrata_OUT_2015.shp"))
outVMS <- st_read(paste0(temp2, "/SPA6_VMSstrata_IN_2015.shp"))
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
#SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")

SPA6 <- rbind(SPA6A, SPA6B, SPA6D) %>% #SPA6C
  st_transform(crs = 4326)


# -------------------------------Import WGTHGT DATA------------------------------------------

#List of tows that have detailed samples
#NOTE:  *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

quer4 <- paste(
  "SELECT * 			                ",
  "FROM SCALLSUR.scwgthgt s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)

sampled.dat <- dbGetQuery(chan, quer4)
sampled.dat <- sampled.dat[,1:17]

# -------------------------------Format for myco plots-----------------------------------------

sampled.dat <- sampled.dat %>%
  mutate(year = year(TOW_DATE)) %>%
  filter(year %in% c(2018:survey.year)) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>% 
  mutate(MYCO_INFECTED = as.factor(MYCO_INFECTED)) %>% 
  mutate(MEAT_COLOUR = as.factor(MEAT_COLOUR)) %>% 
  mutate(MEAT_COLOUR = case_when(MEAT_COLOUR == "Normal white colour" ~ "Normal", 
                                 MEAT_COLOUR == "moderate (light brown/gray)" ~ "Moderate",
                                 MEAT_COLOUR == "severe (dark brown/gray)" ~ "Severe"))

table(sampled.dat$MYCO_INFECTED)

myco.dat <- sampled.dat %>% 
  group_by(CRUISE, tow) %>% 
  count(MYCO_INFECTED)

#convert to wide table format
myco.datw <- pivot_wider(myco.dat, 
                         names_from = MYCO_INFECTED,
                         values_from = n,
                         values_fill = 0)

myco.datw <- myco.datw %>%
  mutate(tot = N+Y) %>% 
  mutate(prop = Y/(tot)) %>% #calculates proportion of infected meats
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

tow.dat <- sampled.dat %>% group_by(ID, tow, STRATA_ID, lat, lon, year) %>% 
  summarise()

myco.datw <- merge(myco.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)

#Saves files by cruise
#for(i in unique(myco.datw$CRUISE)){
#  write.csv(myco.datw %>% filter(year == survey.year & CRUISE == i), paste0("Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/",i,"towsdd_MYCOprop.csv"))
#}

#write.csv(myco.datw, "Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/BF2023towsdd_MYCOprop.csv")

# -------------------------------Format for Grey meats plots-----------------------------------------

greymeat.dat <- sampled.dat %>% 
  group_by(CRUISE, tow) %>% 
  count(MEAT_COLOUR)

#convert to wide table format
greymeat.datw <- pivot_wider(greymeat.dat, 
                             names_from = MEAT_COLOUR,
                             values_from = n,
                             values_fill = 0)

greymeat.datw <- greymeat.datw %>%
  mutate(prop = (Moderate + Severe)/(Normal + Moderate + Severe)) %>% 
  mutate(NUM_GREYMEAT = Moderate + Severe) %>% 
  mutate(NUM_GREYMEAT = as.numeric(NUM_GREYMEAT)) %>% 
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

greymeat.datw <- merge(greymeat.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)

#Saves files by cruise
for(i in unique(greymeat.datw$CRUISE)){
  write.csv(greymeat.datw %>% filter(year == survey.year & CRUISE == i), paste0("Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/",i,"towsdd_QUALITYprop.csv"))
}

#write.csv(greymeat.datw, "Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/BI2021towsdd_QUALITY.csv")

# --------------Set plot themes (legend orientation/aesthetics)------------------------------

#Set legend format for plots
plot.theme <-  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                     axis.title = element_text(size = 12),
                     axis.text = element_text(size = 10),
                     legend.title = element_text(size = 10, face = "bold"), 
                     legend.text = element_text(size = 8),
                     legend.position = c(.87,.32), #legend position
                     legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                     legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

plot.theme.1b <-  theme(legend.key.size = unit(5,"mm"),
                        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                        axis.title = element_text(size = 12),
                        axis.text = element_text(size = 10),
                        legend.title = element_text(size = 8, face = "bold"), 
                        legend.text = element_text(size = 7),
                        legend.direction="horizontal",
                        legend.position = c(.70, .09),#legend position
                        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                        legend.box.margin = margin(1, 1, 1, 1))

plot.theme.3 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.87,.44), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))

plot.theme.4 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      #legend.direction="horizontal",
                      legend.position = c(.91,.22), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)

plot.theme.6 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.10,.68), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))

#Need to run to use pecjector - will look into 
sf::sf_use_s2(FALSE)

# -------------- MYCOBACTERIUM ------------------------------------------------------------


# Proportion of Myco ------------------------------------------------------

#For FULL BAY, SPA1A, SPA1B, SPA4&5

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>%
                              dplyr::select(ID, lon, lat, prop),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(0,0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 0.1) #levels to be color coded
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
labels <- c("0-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06-0.08", "0.08-0.1", "0.1+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion Infected", limits = labels) #set custom fill arguments for pecjector


# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = list(x=c(-66.40,-64.80), y=c(44.37,45.30), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('br',0.5, -1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.48,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, prop), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(0,0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 0.1) #levels to be color coded
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
labels <- c("0-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06-0.08", "0.08-0.1", "0.1+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion Infected", limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% 
                              filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, prop), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(0,0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 0.1) #levels to be color coded
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
labels <- c("0-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06-0.08", "0.08-0.1", "0.1+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion Infected", limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area =list(x=c(-66.4, -67.5), y=c(44.4, 45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA)) #
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32, 54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  #geom_sf(data = outVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  #geom_sf(data = inVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA6 Proportion of Myco Infections"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----All SPAs -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year) %>% dplyr::select(ID, lon, lat, prop),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(0,0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 0.1) #levels to be color coded
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
labels <- c("0-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06-0.08", "0.08-0.1", "0.1+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion Infected", limits = labels) #set custom fill arguments for pecjector


#Plot with Pecjector

p <- pecjector(area = list(x=c(-67.08,-64.4), y=c(45.62, 43.65), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object
#adding transparency slows down plot creating and saving significantly....

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Proportion of Myco Infections"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.65,45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.82,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BFAll_MycoProportion',survey.year,'_new.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)




# #Infected per Tow  ------------------------------------------------------

#For FULL BAY, SPA1A, SPA1B, SPA4&5

myco.datw$Y <- as.numeric(myco.datw$Y)

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% dplyr::select(tow, lon, lat, Y),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(myco.datw %>% filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% dplyr::select(Y)) #check range to determine levels

lvls=c(0, 1, 2, 3, 4, 5) 
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
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac("N Infected",tow)), limits = labels) #set custom fill arguments for pecjector

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = list(x=c(-66.40,-64.80), y=c(44.37,45.30), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('br',0.5, -1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.48,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, Y), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls=c(0, 1, 2, 3, 4, 5) 
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
labels <- c("0-1", "1-2", "3-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% 
                              filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, Y), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls=c(0, 1, 2, 3, 4, 5) 
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
labels <- c("0-1", "1-2", "3-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area =list(x=c(-66.4, -67.5), y=c(44.4, 45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA)) 

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32, 54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  #geom_sf(data = outVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  #geom_sf(data = inVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA6 Myco Infections per Tow"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----ALL SPAs -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year) %>% dplyr::select(tow, lon, lat, Y),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(myco.datw$Y) #check range to determine levels
lvls=c(0,1, 2, 3, 4, 5) 

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
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac("N Infected",tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:

p <- pecjector(area = list(x=c(-67.08,-64.4), y=c(45.62, 43.65), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object
#adding transparency slows down plot creating and saving significantly....

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Myco Infections per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.65,45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.82,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# -------------- GREY MEATS ------------------------------------------------------------


# Proportion of Grey meats ------------------------------------------------------

#Create contour and specify plot aesthetics
#For FULL BAY, SPA1A, SPA1B, SPA4&5

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>%
                              dplyr::select(ID, lon, lat, prop),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

#lvls <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3) #levels to be color coded
lvls <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
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
#labels <- c("0-0.01", "0.01-0.05","0.05-0.1", "0.1-0.15", "0.15-0.2", "0.2-0.25", "0.25-0.3", "0.3+")
labels <- c("0-0.01", "0.01-0.02","0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector


# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = list(x=c(-66.40,-64.80), y=c(44.37,45.30), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('br',0.5, -1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.48,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, prop), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5) #levels to be color coded
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
labels <- c("0-0.05", "0.05-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% 
                              filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, prop), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5) #levels to be color coded
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
labels <- c("0-0.05", "0.05-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area =list(x=c(-66.4, -67.5), y=c(44.4, 45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA)) #
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32, 54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  #geom_sf(data = outVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  #geom_sf(data = inVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA6 Proportion of Grey Meats"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----All SPAs -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year) %>% dplyr::select(ID, lon, lat, prop),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5) #levels to be color coded
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
labels <- c("0-0.05", "0.05-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector


#Plot with Pecjector

p <- pecjector(area = list(x=c(-67.08,-64.4), y=c(45.62, 43.65), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object
#adding transparency slows down plot creating and saving significantly....

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Proportion of Grey Meats"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.65,45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.82,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BFAll_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Number of Grey meats (moderate + severe) per Tow  ------------------------------------------------------

#For FULL BAY, SPA1A, SPA1B, SPA4&5

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% dplyr::select(tow, lon, lat, NUM_GREYMEAT),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01) 

range(greymeat.datw %>% filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% dplyr::select(NUM_GREYMEAT)) #check range to determine levels

lvls=c(0, 1, 2, 3, 4, 5) 
#lvls=c(0.1, 0.3, 0.5, 0.7, 0.9, 1)  #for really low numbers
#lvls=c(0, 1, 2) 
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

labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
#labels <- c("0-0.1", "0.1-0.3","0.3-0.5", "0.5-0.7", "0.7-0.9", "0.9-1.0", "1+")
#col.nu <- c("grey90", "grey80", "grey70", "grey60", "grey50", "grey40","grey30", "grey20")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#save
#ggsave(filename = paste0("Y:/Inshore/BoF/2018/Figures/ContPlot_BF_GreyMeats_per_Tow2018.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = list(x=c(-66.40,-64.80), y=c(44.37,45.30), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('br',0.5, -1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1, -1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.48,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% #only SPA3
                              dplyr::select(ID, lon, lat, NUM_GREYMEAT), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls=c(0, 1, 2, 3, 4, 5) 
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
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% 
                              filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)) %>% #Only SPA6
                              dplyr::select(ID, lon, lat, NUM_GREYMEAT), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)


lvls=c(0, 1, 2, 3, 4, 5) 
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
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area =list(x=c(-66.4, -67.5), y=c(44.4, 45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA)) 

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32, 54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  #geom_sf(data = outVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  #geom_sf(data = inVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA6 Grey Meats per Tow"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----ALL SPAs -----

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year) %>% dplyr::select(tow, lon, lat, NUM_GREYMEAT),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(greymeat.datw$Y) #check range to determine levels
lvls=c(0,1, 2, 3, 4, 5) 

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
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

#Plot with Pecjector for each area:

p <- pecjector(area = list(x=c(-67.08,-64.4), y=c(45.62, 43.65), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object
#adding transparency slows down plot creating and saving significantly....

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "Grey Meats per Tow"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.08,-64.4), ylim = c(43.65,45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.82,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BFAll_GreyMeats_per_Tow',survey.year,'_new.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



