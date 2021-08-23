
## OLEX LAT LONG DATA CONVERSION ##
## AND SPA AND STRATA MATCHING ##

###  VERIFY THE COORDINATES MATCH THE RECORDS IN FIELD NOTEBOOK  ##

#CONSIDERATIONS 
# - Data points for large pelagics - how are these formatted in olex output? - may be entered as "Grønnramme" in between tracklines
# - Tracklines that are ended during a tow and started again will need to be identified and start and end coords will need to be checked.
# - if anything is entered during the survey that are not tracklines - will need to check (i.e. notes of whales etc.)
# - check number of tows and number of track logs match
# - Can use lat long to match strata ID and SPAs to auto fill these columns.

# Load libraries and functions and Strata/management shapefiles

library(data.table)
library(splitstackshape)
library(tidyverse)
library(sf)
library(rmapshaper)
library(mapview)

funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#Read in inshore boundaries
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A")
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B")
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp")) %>% mutate(ET_ID = "2")
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp")) %>% mutate(ET_ID = "3")
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp")) %>% mutate(ET_ID = "4")
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp")) %>% mutate(ET_ID = "5")
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")

SFA29 <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% mutate(ID = seq(1,5,1)) %>%  #TO FIX IN COPY ON GITHUB (ET_ID missing so adding it here)
  mutate(ET_ID = case_when(ID == 1 ~ "A", 
                           ID == 2 ~ "B",
                           ID == 3 ~ "C",
                           ID == 4 ~ "D",
                           ID == 5 ~ "E")) %>% 
  select(Id = ID, ET_ID) %>% st_transform(crs = 4326)

#Because SPA6A, 6B, 6D overlap - need to cut out (#TO FIX IN COPY ON GITHUB)
SPA6A <- rmapshaper::ms_erase(SPA6A,SPA6B)%>% 
  select(Id, ET_ID) %>% 
  st_make_valid()
#plot(SPA6A)

SPA6B <- rmapshaper::ms_erase(SPA6B,SPA6D)%>% 
  select(Id, ET_ID) %>% 
  st_make_valid()
#plot(SPA6B)

SPA_BoF <- rbind(SPA1A, SPA1B, SPA2, SPA3, SPA4, SPA5, SPA6A, SPA6B, SPA6C, SPA6D) %>% 
  st_transform(crs = 4326)

rm(SPA1A, SPA1B, SPA2, SPA3, SPA4, SPA5, SPA6A, SPA6B, SPA6C, SPA6D) #declutter environment

#STRATA FILE FROM GITHUB
# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the strata shapefile
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57-31.shp"))


###########################################################################################################

#Import olex data:

#Norwegian translation according to Google:
#Grønnramme - basic framework
#Navn - name
#Rute uten navn- Route without name
#Garnstart - start
#Garnstopp - stop
#Brunsirkel - brown circle (points along trackline?)

zz <- read.csv(gzfile('Y:/INSHORE SCALLOP/Survey/OLEX tow tracks/2021/aug172021.gz'))

str(zz)
zz$Ferdig.forenklet <- as.character(zz$Ferdig.forenklet)

#Split characters separated by spaces into columns.
zz <- cSplit(zz, "Ferdig.forenklet", sep = " ", type.convert = FALSE) 

#zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp")) %>% 
#  select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
#  mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
#  mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)

#Occasionally a "Grønnramme" occurs where a "Garnstopp" should be or between tow tracks. 
#RUN but Check if number of "Garnstart" == "Garnstopp"!! 
zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp", "Grønnramme")) %>% 
  select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
  mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
  mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)

View(zz)

#Select the row where the track data starts (i.e. the first "Garnstart"). Check for "Grønnramme".
zz <- zz[120:nrow(zz)] #[Row# where "Garnstart" first occurs: to end of data]

#Convert decimal degrees to decimal minutes seconds.
zz$Latitude.deg <- convert.dd.dddd(zz$Latitude, format = 'deg.min')
zz$Longitude.deg <- convert.dd.dddd(zz$Longitude, format = 'deg.min')*-1

zz.start <- zz %>% filter(Ferdig.forenklet_4 == "Garnstart") %>% 
  rename(Start_lat = Latitude.deg) %>% 
  rename(Start_long = Longitude.deg) %>% 
  select(Start_lat, Start_long, Start_lat_dec = Latitude, Start_long_dec = Longitude)

#If required to have 3 decimal places - values copied down in fieldbook are not rounded so run to get 3 decimal places not rounded:
zz.start$Start_lat <- trunc(zz.start$Start_lat*10^3)/10^3
zz.start$Start_long <- trunc(zz.start$Start_long*10^3)/10^3

zz.end <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstopp")) %>% #"Grønnramme"
  rename(End_lat = Latitude.deg) %>% 
  rename(End_long = Longitude.deg) %>% 
  select(End_lat, End_long, End_lat_dec = Latitude, End_long_dec = Longitude)

zz.end$End_lat <- trunc(zz.end$End_lat*10^3)/10^3
zz.end$End_long <- trunc(zz.end$End_long*10^3)/10^3

coords <- cbind(zz.start, zz.end) %>% 
  mutate(ID = seq(1,nrow(zz.start),1))  #NOTE - ID IS NOT TOW NUMBER. it is only used to compare records when matching strata #s and SPAs.

# Match Strata ID and SPA # to lat and long data (use start lat long)
coords.sf <- st_as_sf(coords, coords = c("Start_long_dec", "Start_lat_dec"), crs = 4326)
plot(coords.sf)

strata.match <- st_intersection(strata, coords.sf)
strata.match <- strata.match %>% dplyr::select(STRATA_ID, ID)

spa.match <- st_intersection(SPA_all, coords.sf)
spa.match <- spa.match %>% dplyr::select(ET_ID, ID)

#All points should have strata, and spa matches. If there are discrepincies - check 

coords.sf <- coords.sf %>% 
  st_join(spa.match, by = "ID", suffix = c("", ".y")) %>% 
  st_join(strata.match,by = "ID", suffix = c("", ".y")) %>% 
  select(ID, Start_lat, Start_long, End_lat, End_long, ET_ID, STRATA_ID)

##########################################################
#plot to check

mapview::mapview(coords.sf) +
  mapview::mapview(strata) +
  mapview::mapview(SPA_all)

coords.sf <- coords.sf%>% 
  st_drop_geometry()

write.csv(coords.sf, "Y:/INSHORE SCALLOP/Survey/OLEX tow tracks/Olex-latlong_conversion/GM2021_coords_check.csv")

####################################################################################


