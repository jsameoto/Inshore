library(ROracle)
require(sf)
require(raster)
require(dplyr)

options(stringsAsFactors = FALSE)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- un.bwilson
pwd <- pw.bwilson

#### Import Source functions####
# source R functions
source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set survey.year and cruise - *Note: requires single quotations within double quotations*
survey.year <- "'2019'"
cruise <- "'SFA292019'"
appendingfile_year <- "2019" # for importing the current spreadsheet to append to.
updatefile_year <- "2019" #For saving file

#Db Query:
quer2 <- paste(
  "SELECT CRUISE, TOW_NO, CRUISE||'.'||TOW_NO AS ID, start_lat, start_long 			                ",
  "FROM SCALLSUR.SCTOWS			",
  "WHERE to_char(TOW_DATE,'yyyy')=",
  survey.year,
  "AND CRUISE =",cruise,
  sep=""
)


#Pull data using ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)

#Convert lat/lon to DD:
ScallopSurv$lat <- convert.dd.dddd(ScallopSurv$START_LAT)
ScallopSurv$lon <- convert.dd.dddd(ScallopSurv$START_LONG)
#duplicate lat lon columns: (To keep consitent with previous records)
ScallopSurv$DDSlat <- ScallopSurv$lat
ScallopSurv$DDSlon <- ScallopSurv$lon 

#Convert dataframe to sf:
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326)

#Read in Bathy (with raster):  #Former process involved importing raster (UTM zone 20 into Arcmap, and extracting imported points from database. ArcMap converts data points to UTM zone 20 in order to extract.)  *DO NOT TRANSFORM THE RASTER OR DATA*. raster::extract() gives warning "Transforming SpatialPoints to the CRS of the Raster".

bathy <- raster("Y:/INSHORE SCALLOP/BoF/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf")

olex.depth <- raster::extract(bathy, ScallopSurv.sf) #extract bathy data from ScallopSurv.sf point locations.
#warning given: Transforming SpatialPoints to the CRS of the Raster

#Append depth to survey data
ScallopSurv.dpth <-cbind(ScallopSurv.sf, olex.depth) %>% 
  mutate(FID = row_number()) %>%
  dplyr::select(FID, CRUISE, TOW_NO, ID, START_LAT, START_LONG, DDSlat, DDSlon, RASTERVALU = olex.depth) %>% #sort to match towsdd_Std column formatting
  st_set_geometry(NULL) #removes geometry
  
#Load previous towsdd_stdDepth.csv file to append to.
towsdd <- read.csv(paste0("Y:/INSHORE SCALLOP/StandardDepth/towsdd_StdDepth_", appendingfile_year, ".csv"))

##############################################################################################################
###Checks - will remove this section once workflow has been confirmed to be equivilent to previous workflow ##
##############################################################################################################
towsdd.test <- towsdd %>% 
  filter(CRUISE == "SFA292019") %>% ## Will need to change CRUISE if checking other years/cruises.
  arrange(TOW_NO)

#check depth values
format(round(towsdd.test$RASTERVALU,5), nsmall = 5) == format(round(ScallopSurv.dpth$RASTERVALU,5), nsmall = 5) #Will return false without reducing sigfigs... see minor difference without rounding in calculation below.
towsdd.test$diff <- towsdd.test$RASTERVALU - ScallopSurv.dpth$RASTERVALU
plot(abs(towsdd.test$diff))

#check latitude
format(round(towsdd.test$DDSlat,7), nsmall = 7) == format(round(ScallopSurv.dpth$DDSlat,7), nsmall = 7)
towsdd.test$diff <- towsdd.test$DDSlat - ScallopSurv.dpth$DDSlat
plot(abs(towsdd.test$diff))
#################################################################################################

#Appending to towsdd
towsdd.updt <- rbind(towsdd, ScallopSurv.dpth)

#Save with year in filename
setwd("Y:/INSHORE SCALLOP/StandardDepth")
write.csv(towsdd.updt, paste0("towsdd_StdDepth_", updatefile_year,"test.csv")) #remove test when finalized**

#**manually move last years file to Archived folder under Y:/INSHORE SCALLOP/StandardDepth **


