
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

#set year
last.fishing.yr <- 2020
start.year <- last.fishing.yr - 5 #Get data from latest survey year back 5 years.

cruise <- "SPA6"

#Run for determining subareas for data query.
if(cruise == "SPA1A") {
  sub.area <- paste0("'1A'")
}
if(cruise == "SPA1B") {
  sub.area <- paste0("'1B'")
}
if(cruise == "SPA3") {
  sub.area <- paste0("'SPA3'")
}
if(cruise == "SPA4") {
  sub.area <- paste0("'SPA4', 'SPA5'")
}
sub.area <- NULL
if(cruise == "SPA6") {
  sub.area <- paste0("'6A','6B','6C','6D'")
}


#READ IN SITE INFO - PROVIDED BY Coastal Oceanography & Ecosystem Research Section (COERS) (Lindsay Brager)
data <- read.table("Y:/Admin/Request and Review Tracking/Aquaculture_Reviews/2021/Beaver_Harbour/rams_head_pez_pelagic_speed26.6_cms_ttrack_3h.ll")

site.boundary <- data %>% 
  mutate(ID = seq(1,nrow(data),1)) %>% 
  mutate(Site = "BeaverHarbour") #Change

site.boundary.sf <- st_as_sf(site.boundary, coords = c("V1", "V2"), crs = 4326) %>% #May need to adjust Lat long headers in coords = c()
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") # %>% 
  #st_cast("POLYGON")
plot(site.boundary.sf)

#Define boundary to filter out data points with errors in coordinates
boundary <- convert.coords(plot.extent = cruise)
bbox <- st_bbox(boundary[[1]])
#plot(bbox)

mapview::mapview(site.boundary.sf)+
mapview::mapview(bbox)

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

cat(nrow(logs)-nrow(log.priv), "records are removed - rule of 5") #Print out records removed by rule of 5

log.priv.sf <- st_as_sf(log.priv, coords = c("DDSlon", "DDSlat"), crs = 4326) # Create sf object
plot(log.priv.sf) #Note any coordinate errors


num.rec.in.site <- nrow(log.priv.sf %>% st_crop(site.boundary.sf)) #How many records are in the site boundaries

spa.site.overlap <- log.priv.sf %>% #What subareas overlap with the site boundary?
  st_crop(site.boundary.sf) %>%
  summarise(AREA) %>% 
  unique() %>%
  st_set_geometry(NULL) %>% 
  pull()
  

#Remove any data points outside bounding box (defined above)
log.priv.sf <- log.priv.sf %>% 
  st_crop(bbox)
cat(nrow(log.priv.sf)-nrow(log.priv), "records are removed - coordinate errors")


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

mapview::mapview(grid.data, zcol="sum_CATCH") + 
  mapview::mapview(grid.data, zcol = "sum_EFFORT")+
  mapview::mapview(site.boundary.sf)

site.grid <- grid.data %>% 
  st_crop(site.boundary.sf)

mapview::mapview(site.grid,  zcol="sum_CATCH") +
  mapview::mapview(site.grid,  zcol="sum_EFFORT") +
mapview::mapview(site.boundary.sf)

tot.catch.eff <- grid.data %>% 
  dplyr::summarise(tot_CATCH=sum(sum_CATCH, na.rm=T),
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))
                   
tot.catch.eff

site.tot.catch.eff <- site.grid %>% 
  dplyr::summarise(tot_CATCH=sum(sum_CATCH, na.rm=T),
                   tot_EFFORT=sum(sum_EFFORT, na.rm=T))

site.tot.catch.eff

prop.catch <- site.tot.catch.eff$tot_CATCH/tot.catch.eff$tot_CATCH *100

prop.effort <- round(site.tot.catch.eff$tot_EFFORT/tot.catch.eff$tot_EFFORT *100, digits = 2)

tot.catch.eff
site.tot.catch.eff

prop.catch
prop.effort

print(paste("The site boundary falls within",cruise, "and overlaps with subarea(s)", spa.site.overlap,". Landings within the site boundary were", site.tot.catch.eff$tot_CATCH, "metric tonnes. This represents",num.rec.in.site, "out of", nrow(log.priv.sf), "usable log records from", start.year, "to", last.fishing.yr, "and represents ", prop.effort,"% of the total effort in", cruise, "."))

      
      
########################################################################################3
lvls <- seq(0,140,15)
#lvls=seq(5,110,15)
lvls <- seq(0, 25000, 5000)

tot.catch <- join %>% 
  mutate(brk = cut(sum_CATCH, breaks = lvls)) %>%
  group_by(brk) %>%
  summarise() %>% 
  mutate(levels = 1:length(brk)) %>% 
  mutate(brk = fct_reorder(brk, levels))

join %>% st_intersection(site.boundary.sf) %>%  #Find Cells that are inside the site boundary.
plot()


#Quick plot to view data
BluYelRed <- colorRampPalette(c('blue','yellow', 'red'))
mapview::mapview(join , zcol="sum_CATCH", col.regions = BluYelRed, at = seq(0, 25000, 5000)) +
  mapview::mapview(join , zcol="sum_EFFORT", col.regions = BluYelRed, at = seq(1,1000,250)) +
  mapview::mapview(site.boundary.sf)


#######PECJECTOR PLOTS#################################################################################################################

#lvls<-c(lvls,max(lvls)*100)
labels <- c("1-15", "15-30", "30-45", "45-60","60-75", "75-90", "90-105", "105-120", "120-135","135+") #edit lables accordingly
col <- rev(brewer.pal(length(labels),"RdYlBu"))
cfd <- scale_fill_manual(values = alpha(col, 0.8), breaks = labels, name = expression(frac(Kg,hr)), limits = labels)


#Pecjector
p <- pecjector(area =Proposed.area, repo ='github',c_sys="ll", gis.repo = 'github', plot=F, plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('br',0.5)),add_custom = list(obj = avg.cpue %>% arrange(levels) %>% mutate(brk = labels[1:length(levels)]) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p + #Plot survey data and format figure.
  geom_sf(data = site.boundary.sf %>% dplyr::select(geometry), fill = NA, colour = "black")+
  #geom_sf(data = avg.cpue %>% dplyr::select(brk), aes(fill = factor(brk))) +
  #scale_fill_manual(values = brewer.pal(length(lvls),"YlGnBu"), name = "Kg/hr", labels = labels) +
  labs(title = paste(start.year, "-", last.surv.yr, "", "CPUE"), x = "Longitude",
       y = "Latitude") +
  theme(legend.position = c(.86,.28),legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3))

##################################################################################################################################