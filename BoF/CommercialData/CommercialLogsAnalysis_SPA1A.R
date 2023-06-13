###.....................................###
###  Commercial Catch Rates and Plots   ###
###             SPA 1A                  ###
###       J. Raper June 2020            ###
###.....................................###

#Summarizes commercial log data from SPA1A for a given year and updates CPUE_1A.csv file. 
#Creates the following plots:
# SPA1A_CPUEyyyy.png
# SPA1A_CPUEandEffortyyyy.png
# SPA1A_CatchandEffortyyyy.png
# SPA1A_TACandLandingsyyyy.png
# SPA1A_CPUEgridplotyyyy.png
# SPA1A_Effortgridplotyyyy.png
# SPA1A_Catchgridplotyyyy.png


#BEFORE RUNNING THIS SCRIPT:

# compare SCALLOP db landings with area cap monitoring report and address any issues (differences > 2 mt for each fleet/area should be investigated)
# update SPA1A_TACandLandings_YYYY.xlsx with TAC and landings from monitoring report and save in the current assessment folder: Y:\INSHORE SCALLOP\BoF\YYYY\Assessment\Data\CommercialData
# ensure date format in your SQL Developer profile is set to YYYY-MM-DD


options(stringsAsFactors=FALSE)
library(ROracle)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(sf)

source("Y:/Inshore/BoF/Assessment_fns/convert.dd.dddd.r")


#### Import Mar-scal functions for Pectinid Projector

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


#### DEFINE ####

direct <- "Y:/Inshore/BoF"
fishingyear <- 2022 #most recent year of commercial fishing data to be used (e.g. if fishing season is 2019/2020, use 2020)
assessmentyear <- 2022 #year in which you are conducting the assessment
#un.ID=un.raperj #ptran username
#pwd.ID=pw.raperj #ptran password
un.ID=un.sameotoj #ptran username
pwd.ID=pw.sameotoj#ptran password

#Date range for logs to be selected 
start.date.logs <- "2021-10-01"  #YYYY-MM-DD use Oct 1 
ends.date.logs <- "2022-10-01"  #YYYY-MM-DD use Oct 1 


#### Read files ####

#TAC and Landings time series
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1A_TACandLandings_", 
                       fishingyear, ".xlsx"), sheet = "TACandLandings")

#CPUE_1A csv file from previous year
CPUE_1A <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_1A_", (fishingyear-1), ".csv")) 

#Polygons for spatial plots
poly.sf <- st_read("Y:/Inshore/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA1A_polygon_NAD83")
poly.strata <- st_read("Z:/People/Amy/2012 survey prep/AmyArc", layer = "SCSTRATADEFS_Polygons")


#### Select data ####

#As of 2022, don't need to update date range here - done above in Define section 

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('1A')       ",
  " 	AND  s.date_fished >= to_date('",start.date.logs,"','YYYY-MM-DD') and s.date_fished < to_date('",ends.date.logs,"','YYYY-MM-DD') ", #update years
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=un.ID,  password = pwd.ID,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)
dim(logs)



#Convert coordinates:
logs$lat <- convert.dd.dddd(logs$LATITUDE/100) #coordinates in SCALLOP db are stored without the decimal point, have to divide by 100 to get degrees decimal minutes for convert.dd.dddd
logs$lon <- (-1)*convert.dd.dddd(logs$LONGITUDE/100)

#Add year column:
logs$DATE_FISHED <- as.Date(logs$DATE_FISHED, format="%Y-%m-%d")  
logs$YEAR <- as.numeric(format(logs$DATE_FISHED, "%Y")) 

#Check data:
table(logs$YEAR) #check years, fishing season in 1A spans 2 calendar years
table(logs$FLEET) #check fleets, 1A should only have records from Full Bay


#If any issues here, STOP and address with log scan and/or VMS checks. Make any corrections in the SCALLOP db (and send to CDD), then restart this script
#JS Insert 2022 - remove MidBay 
logs <- logs[logs$FLEET!='Mid-Bay',]


#Change year in logs to reflect fishing year instead of calendar year
logs$YEAR <- as.numeric(fishingyear) 


#### Calculate CPUE ####

logs$effort_h <- (logs$AVG_TOW_TIME*logs$NUM_OF_TOWS)/60

effort.dat <- aggregate(logs$effort_h, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum) #effort from usable logs
names(effort.dat) <- c("fleet","area","effort.hr")

catch.dat <-  aggregate(logs$DAY_CATCH_KG, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum) #catch from usable logs
names(catch.dat) <- c("fleet", "area", "catch.kg")

cpue <- merge(effort.dat, catch.dat, by.x=c("fleet","area"), by.y=c("fleet","area"))

#CPUE is reported as long as rule of 5 is met for Privacy Act considerations, otherwise returns NA.
if(nrow(logs) > 5){
  cpue$cpue.kgh <- cpue$catch.kg/cpue$effort.hr 
} else {
  cpue$cpue.kgh <- NA
  }


#### Calculate Fleet Effort ####

landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("landings.fleet.mt", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))

catch.fleet.mt <- landings$landings.fleet.mt[which(landings$year == fishingyear)]
effort.fleet.1000h <- catch.fleet.mt/cpue$cpue.kgh


#### Update CPUE_1A csv file ####

#Convert catch and effort units
cpue$catch.dat.mt <- cpue$catch.kg/1000
cpue$effort.dat.1000h <- cpue$effort.hr/1000

#Append current year data to CPUE_1A
comm.dat <- cbind(cpue, year = fishingyear, season = paste0(fishingyear-1, "/", fishingyear), catch.fleet.mt = catch.fleet.mt, effort.fleet.1000h = effort.fleet.1000h)
comm.dat <- comm.dat[,c("fleet", "area", "season", "year", "cpue.kgh", "catch.dat.mt", "effort.dat.1000h", "catch.fleet.mt", "effort.fleet.1000h")]
comm.dat <- rbind(CPUE_1A, comm.dat)


#Save to current asssessment folder
write.csv(comm.dat, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_1A_", fishingyear, ".csv"), row.names = FALSE)


#### Time Series Plots ####

#CPUE time series:
ggplot(comm.dat, aes(x = year, y = cpue.kgh)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_continuous("Year", breaks = seq(1998,fishingyear,2)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,60), breaks = seq(0,60,10)) +
  geom_hline(aes(yintercept=median(cpue.kgh[1:(dim(comm.dat)[1]-1)])), linetype = "dashed") + # median line is all years except current year
  geom_text(x = 1999, y = 18, label = "median", size = 4)
#save
#ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUE",fishingyear, ".png"), 24,20,res=400,units='cm')
#updated 2022 
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUE",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#CPUE and Effort (fishery-level) time series:
ggplot(comm.dat) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
  geom_point(aes(x = year, y = cpue.kgh)) + 
  geom_line(aes(x = year, y = cpue.kgh)) + 
  geom_point(aes(x = year, y = effort.fleet.1000h*2), color = "red") + 
  geom_line(aes(x = year, y = effort.fleet.1000h*2), color = "red", linetype = "dashed") +
  scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./2, name = "Effort (1000h)", breaks = seq(0,25,5))) + # 
  scale_x_continuous("Year", breaks=seq(1998,fishingyear,2)) 
#save
#ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUEandEffort",fishingyear, ".png"), 24,20,res=400,units='cm')
#updated 2022 
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUEandEffort",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')


#TAC and Landings
ggplot(landings) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_bar(aes(x = year, y = landings.fleet.mt), stat = "identity", color = "black", fill = "white") +
  geom_line(aes(x = year, y = TAC)) +
  scale_y_continuous("Landings (meats, t)", breaks=seq(0,1200,200)) + # 
  scale_x_continuous("Year", breaks=seq(1998,fishingyear,2)) +
  geom_text(x = 2005.5, y = 600, label = "TAC", size = 5)
#save
#ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_TACandLandings",fishingyear, ".png"), 24,20,res=400,units='cm')
#updated 2022 
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_TACandLandings",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#### Catch and effort with linear model ####
 ggplot(logs, aes(x = effort_h, y = DAY_CATCH_KG)) +
   theme_bw() + theme(panel.grid=element_blank()) +
   geom_point() +
   geom_smooth(method='lm') +
   scale_y_continuous("Catch (kg)") +
   scale_x_continuous("Effort (h)")
#save
#ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CatchandEffort",fishingyear, ".png"), 24,20,res=400,units='cm')
#updated 2022 
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CatchandEffort",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#### SPATIAL PLOTS ####
 
 
 #Pecjector basemap with SPA 1A boundaries
 p <- pecjector(area =list(x=c(-66.5,-64.5), y=c(44.2,45.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = c(50,'c'), scale.bar = c('tl',0.5)))
 
 
 #CPUE Grid Plot
 
 #Create raster of mean cpue
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','CPUE_KG'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "CPUE_KG", fun = mean)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "mean.cpue")
 
 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = mean.cpue), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.5), ylim = c(44.2,45.4), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="CPUE (kg/h)") +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.87,.32), #legend position
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
         legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)
 #save
 #ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUEgridplot",fishingyear, ".png"), 9,9,res=200,units='in')
#new in 2022
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_CPUEgridplot",fishingyear, ".png"), width = 9,height = 9,dpi= 200,units='in')
 
 
 # Make histogram by area for distribution of cpue
 # histolog<-subset(logs, YEAR == fishingyear, c('ASSIGNED_AREA','CPUE_KG'))
 # 
 # cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) + 
 # geom_histogram (breaks = seq (0, 1300, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
 # facet_wrap (~ASSIGNED_AREA, ncol = 1) +
 # scale_x_continuous(breaks = seq (0, 1300, by = 50)) +
 # theme_bw() +
 # theme(panel.grid=element_blank()) 
 # cpuehisto
 # 
 # tapply(histolog$CPUE_KG, histolog$ASSIGNED_AREA, summary)
 
 
 #Catch Grid Plot
 
 #Create raster of cumulative catch
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','DAY_CATCH_KG'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "DAY_CATCH_KG", fun = sum)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "tot.catch")
 
 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = tot.catch), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.5), ylim = c(44.2,45.4), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="Catch (kg)", breaks = c(1000, 2000, 3000, 4000, 5000, 6000)) +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.87,.32), 
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
         legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)
  #save
 # ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_Catchgridplot",fishingyear, ".png"), 9,9,res=200,units='in')
 #new 2022
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_Catchgridplot",fishingyear, ".png"), width=9,height=9,dpi=200,units='in')
  
 
 #Effort Grid Plot
 
 #Create raster of cumulative effort
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','effort_h'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "effort_h", fun = sum)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "tot.effort")
 
 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = tot.effort), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.5), ylim = c(44.2,45.4), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="Effort (h)") +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.87,.32), 
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
         legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)
 #save
# ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_Effortgridplot",fishingyear, ".png"), 9,9,res=200,units='in')
#new 2022
 ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1A_Effortgridplot",fishingyear, ".png"), width=9,height=9,dpi=200,units='in')
 

