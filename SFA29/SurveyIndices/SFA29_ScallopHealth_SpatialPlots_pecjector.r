###................................................................................###
###                       Scallop Health - Spatial Figures                         ###
###                                  SFA 29                                        ###
###                                 Dec 2021                                       ###
###................................................................................###

# Spatial figures of Mycobacterium infections (numbers per tow and proportions) and Gray Meats (number per tow and proportions): 


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
survey.year <- 2021  #removed maxyear in script and changed to survey year
assessmentyear <- 2022 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/SFA29/"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,assessmentyear,"/Assessment/Figures/")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# ----Import Source functions and polygons---------------------------------------------------------------------


#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")

#### Import Mar-scal functions 
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/archive/2016/contour.gen.r") 
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
sfa29.poly <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% 
  st_transform(crs = 4326)
sfa29strata <- st_read(paste0(temp2, "/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp"))

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
  "where strata_id in (41, 42, 43, 44, 45)    ",
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

#If the myco infections are not present (i.e. Y column does not exist), then total column == N+0 and proportion will be 0)
if(myco.datw %>% names %>% str_detect("Y") %>% any() == FALSE){
  myco.datw <- myco.datw %>%
    mutate(tot = N) %>% 
    mutate(prop = 0/(tot)) %>% #calculates proportion of infected meats
    unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)
}else{
  myco.datw <- myco.datw %>%
         mutate(tot = N+Y) %>% 
         mutate(prop = Y/(tot)) %>% #calculates proportion of infected meats
         unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)
}


tow.dat <- sampled.dat %>% group_by(ID, tow, STRATA_ID, lat, lon, year) %>% 
  summarise()

myco.datw <- merge(myco.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)

#Saves file
#write.csv(myco.datw %>% filter(year == survey.year), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",survey.year,".csv"), row.names = FALSE)

#Save files for each year
#for(i in unique(myco.datw$year)){
#write.csv(myco.datw %>% filter(year == i), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",i,".csv"#),row.names = FALSE)
#}


# -------------------------------Format for Grey meats plots-----------------------------------------


table(sampled.dat$MEAT_COLOUR)

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
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

greymeat.datw <- merge(greymeat.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)

#Save file
#write.csv(greymeat.datw %>% filter(year == survey.year), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_QUALITYprop",survey.year,".csv"))

#Save files for each year
#for(i in unique(greymeat.datw$year)){
#write.csv(greymeat.datw %>% filter(year == i), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",i,".csv"#),row.names = FALSE)
#}


# --------------Set plot themes (legend orientation/aesthetics)------------------------------

#Set plot themes (legend orientation/aesthetics)
#Set legend format for plots
plot.theme <- theme(legend.key.size = unit(6,"mm"),
                      plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 10),
                      legend.position = c(.90,.77), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(2, 3, 2, 3),
                    panel.border = element_rect(colour = "black", fill=NA, size=1))


# -------------- MYCOBACTERIUM ------------------------------------------------------------

##SKIP IF Y = 0

# Proportion of Myco ------------------------------------------------------

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% 
                              filter(year == survey.year) %>%
                              dplyr::select(ID, lon, lat, prop),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(0, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 0.1) #levels to be color coded
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
labels <- c("0-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.05", "0.05-0.06", "0.06-0.08", "0.08-0.1", "0.1+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Proportion of Myco Infections"), 
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Infected per Tow  ------------------------------------------------------

myco.datw$Y <- as.numeric(myco.datw$Y)

#Create contour and specify plot aesthetics
com.contours <- contour.gen(myco.datw %>% filter(year == survey.year) %>% dplyr::select(tow, lon, lat, Y),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(myco.datw %>% filter(year == survey.year) %>% dplyr::select(Y)) #check range to determine levels

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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0-1", "1-2", "3-3", "3-4", "4-5", "5+")
col <- brewer.pal(length(lvls),"RdPu") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac("N Infected",tow)), limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey",bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = myco.datw %>% 
                       filter(year == survey.year), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Myco Infections per Tow")
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# -------------- GREY MEATS ------------------------------------------------------------


# Proportion of Grey meats ------------------------------------------------------

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% 
                              filter(year == survey.year) %>%
                              dplyr::select(ID, lon, lat, prop),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(greymeat.datw %>% filter(year == survey.year) %>%  dplyr::select(prop))

lvls <- c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3) #levels to be color coded
#lvls <- c(0, 0.01, 0.02, 0.03, 0.04)
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
labels <- c("0-0.01","0.01-0.05" ,"0.05-0.1", "0.1-0.15", "0.15-0.2", "0.2-0.25", "0.25-0.3", "0.3+")
#labels <- c("0-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04+")
col <- brewer.pal(length(lvls),"Greys") #set colours #Cant see the lower numbers
col.nu <- c("grey90", "grey80", "grey70", "grey60", "grey50", "grey40","grey30", "grey20")
cfd <- scale_fill_manual(values = alpha(col.nu, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey",bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year),
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs( x = "Longitude", y = "Latitude") +#title = paste(survey.year, "", "BoF Proportion of Grey Meats"),
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        legend.position = c(.90,.80), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#ggsave(filename = paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_GreyMeatProportion2019.png"), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Number of Grey meats (moderate + severe) per Tow  ------------------------------------------------------

greymeat.datw$NUM_GREYMEAT <- as.numeric(greymeat.datw$NUM_GREYMEAT)

#Create contour and specify plot aesthetics
com.contours <- contour.gen(greymeat.datw %>% filter(year == survey.year) %>% dplyr::select(tow, lon, lat, NUM_GREYMEAT),ticks='define',nstrata=7, str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

range(greymeat.datw %>% filter(year == survey.year) %>% dplyr::select(NUM_GREYMEAT)) #check range to determine levels

lvls=c(0, 1, 2, 3, 4, 5, 6, 7) 
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
  st_intersection(sfa29.poly %>% group_by(Id) %>% st_union()) %>% #Dissolve polygon segments and crop contours to sfa29.poly 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6","6+")
#labels <- c("0-1", "1-2","2+")
col <- brewer.pal(length(lvls),"Greys") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5,-1,-1)), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_spatial_point(data = greymeat.datw %>% 
                       filter(year == survey.year), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(x = "Longitude", y = "Latitude") + #title = paste(survey.year, "", "SFA29 Grey Meats per Tow"), 
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# END of Scallop Health Spatial Plots -------------------------------------


