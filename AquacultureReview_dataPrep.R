
#This script is built to help provide information for Aquaculture Site Review Requests provided that site boundary information is provided (typically by the Coastal Oceanography & Ecosystem Research Section (COERS) - contact Lindsay Brager)), and overlap with commercial fishing occurs. The script looks at mean and total CPUE over the last 5 years and determines overlap with the proposed Aquaculture site boundary.  The script includes options for quick plotting (mapview) to visualize the area the surrounding CPUE data, as well as plotting using pecjector with defined extents.

#July 16, 2021

#NOTES - 

# required packages
require (fields)
require (splancs)
require (RColorBrewer)
require (RPMG)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
library(mapview)
library(data.table)

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", "WILSONBR")

#### Import Source functions####

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
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

#Directory
dir <- "Y:/Admin/Request and Review Tracking/Aquaculture_Reviews/2021/Beaver_Harbour/" #to save logs and read in ll data
#set year
last.fishing.yr <- 2020
start.year <- last.fishing.yr - 5 #Get data from latest survey year back 5 years.

#READ IN STRATA
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57-31.shp"))

#SET Cruise parameters
cruise <- "SPA6"

#Run for determining subareas for data query.
if(cruise == "SPA1A") {
  sub.area <- paste0("'1A'")
  strata <- strata %>% filter(STRATA_ID %in% c(6,7,12, 13, 14, 15, 16, 17, 18, 19, 20, 39))
}
if(cruise == "SPA1B") {
  sub.area <- paste0("'1B'")
  strata <- strata %>% filter(STRATA_ID %in% c(37, 38, 53, 35, 49, 52, 50, 51))
}
if(cruise == "SPA3") {
  sub.area <- paste0("'SPA3'")
  strata <- strata %>% filter(STRATA_ID %in% c(22, 23, 24))

}
if(cruise == "SPA4") {
  sub.area <- paste0("'SPA4', 'SPA5'")
  strata <- strata %>% filter(STRATA_ID %in% c(1,8,2,9,3,10,4,5,47,21))
}
sub.area <- NULL
if(cruise == "SPA6") {
  sub.area <- paste0("'6A','6B','6C','6D'")
  strata <- strata %>% filter(STRATA_ID %in% c(30,31,32,54))
}



#READ IN SITE INFO - PROVIDED BY Coastal Oceanography & Ecosystem Research Section (COERS) (Lindsay Brager)
site.name <- "BeaverHarbour"
data <- read.table(paste0(dir,"rams_head_pez_pelagic_speed26.6_cms_ttrack_3h.ll"))


site.boundary <- data %>% 
  mutate(ID = seq(1,nrow(data),1)) %>% 
  mutate(Site = site.name)

site.boundary.sf <- st_as_sf(site.boundary, coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") # %>% 
  #st_cast("POLYGON")
plot(site.boundary.sf)

#Define boundary to filter out data points with errors in coordinates
#boundary <- convert.coords(plot.extent = cruise)
#bbox <- st_bbox(boundary[[1]])
#plot(bbox)

mapview::mapview(site.boundary.sf)+
  mapview(strata)
#mapview::mapview(bbox)+

#### Select Commercial data ####
quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in (",sub.area,")       ",
  " 	AND  s.date_fished >= to_date('",start.year, "-09-30','YYYY-MM-DD') and s.date_fished < to_date('",last.fishing.yr,"-10-01','YYYY-MM-DD') ",
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=uid,  password = pwd,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)
dim(logs)

logs <- logs %>% 
  mutate(YEAR = as.Date(logs$DATE_FISHED, format="%Y-%m-%d")) %>%  #assuming character and in format 'YYYY-XX-XX' or "YYYY/XX/XX'
  rename(AREA = ASSIGNED_AREA) %>% 
  mutate(DDSlat = convert.dd.dddd(LATITUDE/100)) %>% 
  mutate(DDSlon = -convert.dd.dddd(LONGITUDE/100)) %>%
  mutate(ID = 1:nrow(logs)) %>% 
  mutate(EFFORT_HOURS = (AVG_TOW_TIME * NUM_OF_TOWS)/60)

#Filter out areas for privacy considerations (min 5 trips per area to include in presentation)
log.priv <- logs %>%
  group_by(AREA) %>% 
  filter(!n() <=5) %>% #Filter out any areas within the dataset that have less than 5
  ungroup() %>%
  dplyr::select(ID, AREA, DDSlon, DDSlat, DAY_CATCH_KG, EFFORT_HOURS)

log.priv.sf <- st_as_sf(log.priv, coords = c("DDSlon", "DDSlat"), crs = 4326) # Create sf object
plot(log.priv.sf) #Note any coordinate errors

#Create log and Print out records removed by rule of 5
cat(nrow(logs)-nrow(log.priv), "records are removed - rule of 5\n", file = paste0(dir,site.name,"_AquacultureReview.log")) 

num.rec.in.site <- nrow(log.priv.sf %>% st_crop(site.boundary.sf)) #How many records are in the site boundaries
cat(num.rec.in.site, "- usable commercial record(s) within the site boundary\n", file = paste0(dir,site.name,"_AquacultureReview.log"), append = TRUE)

spa.site.overlap <- log.priv.sf %>% #What subareas overlap with the site boundary?
  st_crop(site.boundary.sf) %>%
  summarise(AREA) %>% 
  unique() %>%
  st_set_geometry(NULL) %>% 
  pull()

cat(spa.site.overlap, "- subarea(s) overlap with the site boundary\n", file = paste0(dir,site.name,"_AquacultureReview.log"), append = TRUE)

#Remove any data points outside bounding box (defined by SPA strata)
log.priv.sf <- log.priv.sf %>% 
  st_crop(strata)

# print # of records removed
cat(nrow(log.priv)-nrow(log.priv.sf), "record(s) removed due to coordinate errors\n", file = paste0(dir,site.name,"_AquacultureReview.log"), append = TRUE)

##GRID DATA:
#hex <- st_make_grid(log.priv.sf , cellsize= 0.015, square=FALSE) #Hexagon grid shape
grid <- st_make_grid(log.priv.sf , cellsize= 0.015, square=TRUE) #Square grid shape

#hex <- st_as_sf(data.table(id_hex=1:length(hex), geom=sf::st_as_text(hex)), wkt='geom', crs = 4326) #id_hex = 1:#of elements in grid
grid <- st_as_sf(data.table(id_grd=1:length(grid), geom=sf::st_as_text(grid)), wkt='geom', crs = 4326)

#plot(st_geometry(hex))
plot(st_geometry(grid))
plot(log.priv.sf, add = TRUE)

#join <- suppressMessages(hex %>% 
  #st_join(log.priv.sf, join=st_contains) %>%
 # group_by(id_hex) %>% 
  #dplyr::summarise(sum_CPUE_KG=sum(CPUE_KG, na.rm=T),
  #                 mean_CPUE_KG=mean(CPUE_KG, na.rm=T)) %>% 
  #filter(sum_CPUE_KG != 0))

grid.data <- suppressMessages(grid %>% #supresses message - "although coordinates are longitude/latitude, st_union assumes that they are planar"
  st_join(log.priv.sf, join=st_contains) %>%
  group_by(id_grd) %>% 
  dplyr::summarise(sum_CATCH=sum(DAY_CATCH_KG, na.rm=T),
                   sum_EFFORT=sum(EFFORT_HOURS, na.rm=T)) %>% 
  filter(sum_CATCH != 0))
  
plot(grid.data[2])
plot(grid.data[3])

#Whole SPA
mapview::mapview(grid.data, zcol="sum_CATCH") + 
  mapview::mapview(grid.data, zcol = "sum_EFFORT")+
  mapview::mapview(site.boundary.sf)

tot.catch.eff <- grid.data %>% 
  dplyr::summarise(tot_CATCH=sum(sum_CATCH, na.rm=T),
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
tot.catch.eff


#Within Site boundary
site.grid <- grid.data %>% 
  st_crop(site.boundary.sf)

mapview::mapview(site.grid,  zcol="sum_CATCH") +
  mapview::mapview(site.grid,  zcol="sum_EFFORT") +
mapview::mapview(site.boundary.sf)

site.catch.eff <- site.grid %>% 
  dplyr::summarise(tot_CATCH=sum(sum_CATCH, na.rm=T),
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
site.catch.eff

prop.catch <- round(site.catch.eff$tot_CATCH/tot.catch.eff$tot_CATCH *100, digits = 2) #percent of catch within site boundary
prop.catch

prop.effort <- round(site.catch.eff$tot_EFFORT/tot.catch.eff$tot_EFFORT *100, digits = 2) #percent of effort within site boundary
prop.effort

tot.catch.eff
site.tot.catch.eff

prop.catch
prop.effort

cat(paste("Summary: The site boundary falls within",cruise, "and overlaps with subarea(s)", spa.site.overlap,".\n From ",start.year, "to", last.fishing.yr,", total landings within the site boundary were", site.catch.eff$tot_CATCH, "metric tonnes (", prop.catch, "% of total catch in", cruise,").\n This represents",num.rec.in.site, "out of", nrow(log.priv.sf), "usable log records from", start.year, "to", last.fishing.yr, "\n and represents ", prop.effort,"% of the total effort in", cruise, "."), file = paste0(dir,site.name,"_AquacultureReview.log"), append = TRUE)

      
#######PECJECTOR PLOTS#################################################################################################################

#lvls<-c(lvls,max(lvls)*100)
#labels <- c("1-15", "15-30", "30-45", "45-60","60-75", "75-90", "90-105", "105-120", "120-135","135+") #edit lables accordingly
#col <- rev(brewer.pal(length(labels),"RdYlBu"))
#cfd <- scale_fill_manual(values = alpha(col, 0.8), breaks = labels, name = expression(frac(Kg,hr)), limits = labels)


#Pecjector
#p <- pecjector(area =Proposed.area, repo ='github',c_sys="ll", gis.repo = 'github', plot=F, plot_as = 'ggplot',
#               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('br',0.5)),add_custom = list(obj = avg#.cpue %>% arrange(levels) %>% mutate(brk = labels[1:length(levels)]) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

#p + #Plot survey data and format figure.
#  geom_sf(data = site.boundary.sf %>% dplyr::select(geometry), fill = NA, colour = "black")+
  #geom_sf(data = avg.cpue %>% dplyr::select(brk), aes(fill = factor(brk))) +
  #scale_fill_manual(values = brewer.pal(length(lvls),"YlGnBu"), name = "Kg/hr", labels = labels) +
#  labs(title = paste(start.year, "-", last.surv.yr, "", "CPUE"), x = "Longitude", y = "Latitude") +
#  theme(legend.position = c(.86,.28),legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
#        legend.box.margin = margin(2, 3, 2, 3))

##################################################################################################################################