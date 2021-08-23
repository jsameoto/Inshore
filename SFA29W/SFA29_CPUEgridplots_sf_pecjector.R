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
#require (RODBC)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
#library(RCurl)


survey.year <- 2018  #This is the last survey year 
assessmentyear <- 2019 #year in which you are providing advice for - (e.g. 2017 survey is 2018 assessment) - Save to folder year
cruise <- "'SFA292018'"

#for multiple cruises:
#cruise <- c('SFA292018','SFA292019') 
#cruise <- paste(cruise,collapse="','")

# get the year set up for later....  This should be set as the year of the assessment (e.g. 2017 survey is 2018 assessment)
#yr <- year(Sys.Date())
#yr.crnt <- yr-1
#years <- c(2001:(yr-1))

path.directory <- "Y:/INSHORE SCALLOP/SFA29/"

#### Import Source functions####

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
# ------------------------------Plot CPUE in grid -------------------------------------------------------

require(data.table)

## Import and prepare Log Data ##

#log.2001to2011 <- read.csv(paste0(path.directory, assessmentyear, "/logs/logData_2002to2011_sfa29.csv"))
#log.2012 <- read.csv(paste0(path.directory, assessmentyear, "/logs/logsSFA29_2012corrected.csv"))
#log.2013 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogSFA29_2013.csv"))
#log.2014 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogsSFA29_2014.csv")) #Fr db
#log.2015 <- read.csv(paste0(path.directory, assessmentyear, "/logs/dlogsSFA29_2015.csv")) #Fr db
#log.2016 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs2016.csv"))
# This includes the late season (October) FSC catch
#log.2017 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs2017_dwnld_Feb2018_JS.csv"))

log.present <- read.csv(paste0(path.directory, assessmentyear, "/logs/SFA29logs_", survey.year, ".csv"))
#log.2019 <- read.csv (paste0(path.directory, assessmentyear, "/logs/SFA29logs_2019.csv"))


# add YEAR and AREA columns for data years selected directly from the SCALLOP database
#is using log data directly from SCALLOP database it's assumed that assigned_ared has been QA/QC'd and this is used as the AREA field
#*NOTE* - Longitude is not negative in log.present

log.present <- log.present %>% 
  mutate(YEAR = as.numeric(substr(log.present$DATE_FISHED,1,4))) %>%  #assuming character and in format 'YYYY-XX-XX' or "YYYY/XX/XX'
  dplyr::rename(AREA = ASSIGNED_AREA) %>% 
  mutate(DDSlat = convert.dd.dddd(LATITUDE)) %>% 
  mutate(DDSlon = -convert.dd.dddd(LONGITUDE)) %>%
  mutate(ID = 1:nrow(log.present))

#Filter out areas for privacy considerations (min 5 trips per area to include in presentation)

log.2018_priv <- log.present %>%
  group_by(AREA) %>% 
  filter(!n() <=5) %>% #Filter out any areas within the dataset that have less than 5
  ungroup() %>% 
  dplyr::select(ID, DDSlon, DDSlat, CPUE_KG)



log.2018.priv.sf <- st_as_sf(log.2018_priv, coords = c("DDSlon", "DDSlat"), crs = 4326)
plot(log.2018.priv.sf)


hex <- st_make_grid(log.2018.priv.sf, cellsize= 0.015, square=FALSE)
grid <- st_make_grid(log.2018.priv.sf, cellsize= 0.015, square=TRUE)

hex <- st_as_sf(data.table(id_hex=1:2394, geom=sf::st_as_text(hex)), wkt='geom', crs = 4326)
grid <- st_as_sf(data.table(id_grd=1:2046, geom=sf::st_as_text(grid)), wkt='geom', crs = 4326)

plot(st_geometry(hex))
plot(st_geometry(grid))
plot(log.2018.priv.sf, add = TRUE)

join <- hex %>% 
  st_join(log.2018.priv.sf, join=st_contains) %>%
  group_by(id_hex) %>% 
  dplyr::summarise(sum_CPUE_KG=sum(CPUE_KG, na.rm=T),
                   mean_CPUE_KG=mean(CPUE_KG, na.rm=T)) %>% 
  filter(sum_CPUE_KG != 0)

join <- grid %>% 
  st_join(log.2018.priv.sf, join=st_contains) %>%
  group_by(id_grd) %>% 
  dplyr::summarise(sum_CPUE_KG=sum(CPUE_KG, na.rm=T),
                   mean_CPUE_KG=mean(CPUE_KG, na.rm=T)) %>% 
  filter(sum_CPUE_KG != 0)


#lvls=seq(5,40,5)  #CPUE levels from 5 to 40 kg/h
lvls=seq(5,110,15)  #CPUE levels from 5 to 110 kg/h

avg.cpue <- join %>% 
  mutate(brk = cut(mean_CPUE_KG, breaks = lvls))
plot(avg.cpue[5])

lvls<-c(lvls,max(lvls)*100)
labels <- c("5-20", "20-35", "35-50", "50-65", "65-80", "80-95", "95-110", "100+")
col <- brewer.pal(length(lvls),"YlGnBu")
#cfd <- scale_fill_manual(values = col, breaks = labels, name = expression(frac(Kg,hr)), limits = labels) 


p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.5)))#, add_custom = list(obj = test, size = 1, fill = "cfd", color = "black"))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_sf(data = avg.cpue %>% dplyr::select(brk), aes(fill = factor(brk))) +
  scale_fill_manual(values = brewer.pal(length(lvls),"YlGnBu"), name = "Kg/hr", labels = labels) +
  #geom_sf(data = sfa29.poly, size = 0.5, colour = "black", fill = NA) +
  labs(title = paste(survey.year, "", "CPUE"), x = "Longitude",
       y = "Latitude") +
  theme(legend.position = c(.90,.83),legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(2, 3, 2, 3))

