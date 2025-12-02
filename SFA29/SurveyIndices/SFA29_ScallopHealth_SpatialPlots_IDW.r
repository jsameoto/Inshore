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
#set year 
survey.year <- 2025  #removed maxyear in script and changed to survey year
assessmentyear <- 2026 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/SFA29/"
cruise <- "'SFA292025'"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,assessmentyear,"/Assessment/Figures/") 
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

# -------------------------------Format for Myco plots-----------------------------------------

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

# #Saves file
# write.csv(myco.datw %>% filter(year == survey.year), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",survey.year,".csv"), row.names = FALSE)
# 
# #Save files for each year
# for(i in unique(myco.datw$year)){
# write.csv(myco.datw %>% filter(year == i), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",i,".csv"),row.names = FALSE)
# }


# -------------------------------Format for Discoloured scallops plots-----------------------------------------

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
# 
# #Save file
# write.csv(greymeat.datw %>% filter(year == survey.year), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_QUALITYprop",survey.year,".csv"))
# 
# #Save files for each year
# for(i in unique(greymeat.datw$year)){
# write.csv(greymeat.datw %>% filter(year == i), paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29towsdd_MYCOprop",i,".csv"),row.names = FALSE)
# }

# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------

#create shapefile out of survey tow locations. It will get re-written during IDW, but just a placeholder for function below.
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

# -------------- MYCOBACTERIUM ------------------------------------------------------------

##SKIP IF Y = 0

# Proportion of Myco ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

## ENGLISH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.75)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

## FRENCH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.75)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Myco per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_Y <- log(Surv.sf$Y) 
Surv.sf$log_Y[which(Surv.sf$log_Y==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_Y~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Y)

## ENGLISH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

## FRENCH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# -------------- DISCOLOURED SCALLOPS ------------------------------------------------------------

# Proportion of Discoloured scallops ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

## ENGLISH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured scallop \n(proportion)", limits = c(0,0.75)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

## FRENCH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75)) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Number of Discoloured scallops (moderate + severe) per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_grey <- log(Surv.sf$NUM_GREYMEAT) 
Surv.sf$log_grey[which(Surv.sf$log_grey==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=500) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_grey~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$NUM_GREYMEAT)

## ENGLISH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured scallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SFA29_GreyMeats_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE) 

## FRENCH ####
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone, Surv.sf, Land) +
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)+
  labs(#title = paste(survey.year, "", "SFA29W Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SFA29_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ---------------------------- END of Scallop Health Spatial Plots -------------------------------------
