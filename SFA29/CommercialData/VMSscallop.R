###.........................................................###
# RETREIVE VMS BY YEAR AS FUNCTION OF LIST OF VRNs IN SCALLOP #
#  calculate speed, add SDM, prorate catch, spatial plots     #
# J. Sameoto												                          #
# Dec 2020; revamped Jan 2022
###.........................................................###

	options(stringsAsFactors=FALSE)

	#Packages and functions needed 
	library(ROracle)
  library(tidyverse)
	library(plyr)
	library(ggplot2)
	library(dplyr)
	library(sp)
	library(sf)
	library(raster)      
	library(devtools)
	library(Mar.datawrangling)
  library(Mar.utils)
	library(rgdal)
	library(RColorBrewer)
	library(foreign) 
	library(plyr)
  library(lubridate)
# library(spData)        # load geographic data
# library(spDataLarge)   # load larger geographic data
#install_github('Maritimes/Mar.datawrangling')

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


#required special function:
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)  #Function to specify number after the decimal point

#define what you have ess drive mapped as: 
ess <- "Y" 
#subset data to select only VMS records in SFA29W
poly.sf <- st_read(paste0(ess,":/Inshore/Databases/Scallsur/SFA29BottomTypes/SFA29_shp"),layer = "SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84")
poly.subareas <- st_read(paste0(ess,":/Inshore/Databases/Scallsur/SFA29BottomTypes/SFA29_shp"),layer = "SFA29_subareas_utm19N")
#Set datum for sfa29w polygon as WGS84 then convert to UTM Zone 19 
poly.sf <- st_transform(poly.sf, crs = sp::CRS(SRS_string = "EPSG:32619"))



####
#...DEFINE... 
#

#ORACLE Username and Password
  uid <- un.sameotoj
  pwd <- pw.sameotoj
  surveyyear <- 2021  #This is the last survey year 
  assessmentyear <- 2022 #year in which you are conducting the survey 
  path.directory <- paste0(":/Inshore/SFA29/")
  startdate <- "2021-01-01"   # Start DateTime for VMS 
  stopdate <- "2021-12-31"    # Stop DateTime for VMS 
 
#open database connection 
  chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')
  
#Old 
#	source(paste0(ess,":/Offshore scallop/Assessment/Assessment_fns/Maps/ScallopMap.r"))

#boundaries needed for plots 
#	sfa29strata <- read.csv(paste0(ess,":/INSHORE SCALLOP/Databases/Scallsur/SFA29BottomTypes/SFA29_shp/SFA29_BoundariesFollowing12nmdd.csv"),header=T)
#	attr(sfa29strata,"projection") <- "LL"


# ----- SQL/Database Pulls ---- 
	
# Define List of VRNs from Monitoring Documents #
	quer1 <- paste(
		"SELECT DISTINCT (vr_number) vrn 			",
		"FROM scallop.scallop_log_marfis 			",
		"WHERE TO_CHAR(date_fished,'yyyy')='",surveyyear,"'",
		"ORDER BY VR_NUMBER  ",
	sep=""
		)	
	vrn.list <- dbGetQuery(chan, quer1)
	vrn.list <- na.omit(vrn.list)
#	vrn.list <- as.character(as.matrix(vrn.list))
#	vrn.list <- paste(vrn.list,collapse="','")

	
# ---- Select VMS ---- using Mike McMahon's Mar.utils package ----
#note require SELECT permissions to views/tables undelying the function - if doesn't work e.g. error of table doesn't
# grab the new VMS data
	
	start <- Sys.time()
	vms.new.raw <- VMS_get_recs(uid, pwd, "ptran", dateStart = startdate, dateEnd = stopdate, 
	                            usepkg = 'roracle',vrn = c(vrn.list$VRN), rowNum = 1000000)  
	print(Sys.time()-start)
  dim(vms.new.raw) #be sure to check your dim(vms.new.raw) -- recall above pull will only take rowNum =1000000 so if you've hit this ceiling, up the rowNum in the query and try again
  dat <- vms.new.raw
  names(dat) <- tolower(names(dat))
  names(dat)[grep("position_utc_date", names(dat))] <- "vmstime"
  dat$vmsdate <- lubridate::date(dat$vmstime)
  names(dat)[grep("vr_number", names(dat))] <- "vrn"
  dat <- dat[order(dat$vrn, dat$vmstime), ]  # Order dataframe by vrn and DateTime 
  names(dat)[grep("longitude", names(dat))] <- "lon"
  names(dat)[grep("latitude", names(dat))] <- "lat"
  head(dat)
  paste("Number of VMS records for ",surveyyear,": ",dim(dat)[1],sep="")
  #[1] "Number of VMS records for 2020: 733352"; #2021 729580
	if(dim(dat)[1]>1000000){ 
	  print("PROBLEM! You've hit your ceiling for records - edit rowNum in VME_get_rec() to ensure you're getting all rows") 
	  } else { print("You're good to go -- your records is under the ceiling limit specified")}
 
# ----- Select VMS OLD WAY direct from VMS database ---- 
  vrn.list.for.sql <- paste(as.character(as.matrix(vrn.list)),collapse="','")
  
	begTime <- Sys.time()
	quer2 <- paste(
	  "SELECT rownum vesid, p.lon, p.lat, NVL(v.vessel_name,p.vrn) vessel_name, p.vrn, 'V_'||p.vrn vr_number, ",
	  "TO_CHAR(p.pdate,'YYYY') YEAR, TO_CHAR(p.pdate,'YYYY-MM-DD') vmsdate, p.pdate vmstime, ",
	  "p.hailout, to_number(TO_CHAR(to_number(TO_CHAR(pdate,'J')) + to_number(TO_CHAR(pdate,'HH24'))/24.,'99999999.999999')) julian_date, ",
	  "p.speed_knots, 1 obs,",
	  "v.vessel_name || ' '|| p.vrn || ' (LOA ' || v.loa || ') '|| TO_CHAR(p.pdate,'YYYY/MM/DD HH24:MI')|| ' ' || speed_knots ves_id, ",
	  "'http://foip.mar.dfo-mpo.ca/pls/foip/foip$lic_vessels.p_vrn:1in_vrn=' || p.vrn licence_href ",
	  "FROM mfd_obfmi.vms_pos p,                         ",
	  "	  mfd_obfmi.marfis_vessels_syn v               ",
	  "WHERE                                             ",
	  "	  p.vrn = v.vr_number(+)                       ",
	  "AND  p.vrn IN ('",vrn.list.for.sql,"')                    ",
	  "AND p.pdate >= to_date('", startdate,"','YYYY-MM-DD')",
	  "AND p.pdate <= to_date('", stopdate, "','YYYY-MM-DD') ",
	  "AND p.lon BETWEEN -85 AND 180                     ", 
	  "AND p.lat < 90                                    ", 
	  sep=""
	)   

	dat.sql <- dbGetQuery(chan, quer2)
	dbDisconnect(chan)
	
	runTime <- Sys.time()-begTime
	runTime
	
	dat <- dat.sql
	names(dat) <- tolower(names(dat))
	dat$vmstime <- as.POSIXct(dat$vmstime)
	dat$vmsdate <- as.POSIXct(dat$vmsdate)
	dat <- dat[order(dat$vrn, dat$vmstime), ]  # Order dataframe by vrn and DateTime 
	
	paste("Number of VMS records for ",surveyyear,": ",dim(dat)[1],sep="")
	#In 2018: 735539
	#In 2019: 738889
	#In 2020: 736888
	#in 2021: 734981
	
# ---- Clean scallop VMS data selected from VMS_pos ---- 
# Remove duplicates and cross against SCALLOP MONITORING DOCS on VRN and Date to ID Scallop Only trips
# Extract unique rows/ Remove duplicates # 
	tot.dat <- dim(dat)[1]
	dat <- dat[!duplicated(dat),] #removes any rows that are fully duplicated
	dups <- dat[duplicated(dat[,c("vrn","vmstime")]),]
	dups <- dups[order(dups$vrn, dups$vmstime),]
	dat <- dat[!duplicated(dat[,c("vrn","vmstime")]),] # removes any rows where vrn and vmstime are duplicated
	dups.rm <- tot.dat - dim(dat)[1]
	paste("Number of row removed:", dups.rm, sep=" ")
	#In 2018: 243
	#In 2019: 89
 #In 2020 136 removed 
 #In 2021 904 removed ; 898 Mikes way 
	
#
# ---- Cross against logs to pull out SCALLOP trips ----
# Pull out scallop logs #

	chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')
	
	quer.logs <- paste(
		"SELECT *		",
		"FROM scallop.scallop_log_marfis 			",
		"WHERE TO_CHAR(date_fished,'yyyy')='",surveyyear,"'",
	sep=""
		)	
	
	scal.log <- dbGetQuery(chan, quer.logs)
	dbDisconnect(chan)
	
	names(scal.log) <- tolower(names(scal.log)) 
	scal.log$date_fished <- as.POSIXct(scal.log$date_fished)
	scal.log1 <- data.frame(scal.log[,c("vr_number","date_fished")])
	scal.log1 <- unique(scal.log1) #Unique vrn and date fished from logs
	head(scal.log1)
	scal.log1$date_fished <- as.Date(ymd_hms(scal.log1$date_fished)) #get rid of hms 
	

# Select VMS data that has matching VRN and DATE FISHED from logs #
  dat$vmsdate <- as.Date(dat$vmsdate) 
	head(dat)
	str(dat)
	vms.dat <- merge(dat, scal.log1, by.x = c("vrn", "vmsdate"), by.y = c("vr_number","date_fished")) #Creates dataframe with SCALLOP VMS ONLY: vms.dat
	head(vms.dat)
	# dim(vms.dat)[1] #number of records in dataset
	paste("Number of SCALLOP VMS records for ",surveyyear,": ",dim(vms.dat)[1] ,sep="")
	#"Number of SCALLOP VMS records for 2018: 230756"
	#"Number of SCALLOP VMS records for 2019: 229112"
	# Number of SCALLOP VMS records for 2020: 228368
	# Number of SCALLOP VMS records for 2021: 228401   (old numbers: 234122; 235841
	
	vms.vrn <- unique(vms.dat$vrn) # vessels from vms data
	aa <- length(vms.vrn)
	log.vrn <- unique(scal.log$vr_number) #vessels from mon docs
	bb <- length(log.vrn)
	xx <- bb - aa
	paste("Vessels with MonDocs and no matching VMS:",xx,sep="")
	#In 2018  "Vessels with MonDocs and no matching VMS:35"
  #In 2019:   "Vessels with MonDocs and no matching VMS:23"
	#"Vessels with MonDocs and no matching VMS:26 or 27" #in 2020
	#"Vessels with MonDocs and no matching VMS:121" #in 2021 with Mike's pull; 21 from JS pull 
	
	missing.vms <- is.element(el=log.vrn, set=vms.vrn) # log vrn with no matching VMS 
	length(log.vrn[missing.vms==FALSE]) #check of length; should match print out of vessels with mondocs and no matching vms
	MonDocs.No.VMS <- log.vrn[missing.vms==FALSE] #List of VRNs with mon docs and no matching VMS 
	
	vms.dat <- vms.dat[order(vms.dat$vrn, vms.dat$vmstime), ]  # Order dataframe by vrn and DateTime 
	vms.dat$lon <- specify_decimal(vms.dat$lon,6)  # Specify number of decimals for longitude
	vms.dat$lat <- specify_decimal(vms.dat$lat,6)  # Specify number of decimals for latitude
	
	head(vms.dat)
	tail(vms.dat)
	
# Write Outputs #	
#write list of VRNs with mon docs and no matching VMS for the year 
	write.table(MonDocs.No.VMS, paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/VRNsMissingVMS",surveyyear,".txt",sep=""),quote=FALSE, row.names=FALSE, sep=",")
#all vms records associated with inshore scallop for the year #NOTE this is for ALL inshore scallop areas - not just 29W - so only write out if you really want it since its LARGE
#	write.table(vms.dat, paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/scallopVMS",surveyyear,".txt",sep=""),quote=FALSE, row.names=FALSE, sep="\t") 
	write.table(vms.dat, paste0("D:/VMS/data/VMSInshoreAll/scallopVMS",surveyyear,".txt",sep=""),quote=FALSE, row.names=FALSE, sep="\t") 
	
# CHECKS FOR ANY VMS FROM MISSING VESSEL & MON DOCS in the associated year - i.e. just missing vms for those vessls on those scallop fishing day or no records for that vessel at all             
	vrn.t <- MonDocs.No.VMS[1]
  chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')
  
		quer3 <- paste(
		"SELECT *			",
		"FROM mfd_obfmi.vms_pos p   ", 
		"WHERE  p.vrn IN ('",vrn.t,"') ",
		" AND p.pdate >= to_date('", startdate, "','YYYY-MM-DD')",
		" AND p.pdate <= to_date('", stopdate, "','YYYY-MM-DD') ",
	sep=""
		)	
 	
	vrn.list.missingVMS <- dbGetQuery(chan, quer3)
	dbDisconnect(chan)
	
	dim(vrn.list.missingVMS)	
	head(vrn.list.missingVMS)
	table(vrn.list.missingVMS$VRN) #list of number of VMS records in the vms database for a given year by the vrn's the appear to be missing from scallop trips 

#
# ---- Select VMS that is an SFA 29 West trip ----
#    Uses  assigned_area  as true area     #
#        prepare data for PERL             #


	log.area <- unique(scal.log[,c("vr_number","date_fished","assigned_area")])
	log.area <- log.area [!duplicated(log.area ),] #removes any rows that are fully duplicated
	dups <- log.area[duplicated(log.area[,c("vr_number","date_fished")]),] 
	log.area   <- log.area[!duplicated(log.area[,c("vr_number","date_fished")]),] #removes any rows that where fishing on a single day in more than one assigned area
	
	vms.dat <- merge(vms.dat, log.area , by.x = c("vrn", "vmsdate"), by.y = c("vr_number","date_fished"))
	vms.dat <- vms.dat[order(vms.dat$vrn, vms.dat$vmstime), ] #order rows
	vms.dat$uid <- seq(from=1,to=dim(vms.dat)[1],by=1) #add unique ID
	names(vms.dat)[grep("speed_knots", names(vms.dat))] <- "inst_speed"  #Replace "speed_knots" 
	dim(vms.dat) 

	vms.29 <- vms.dat[vms.dat$assigned_area%in%c('29A','29B','29C','29D','29E'),] #select VMS from SFA29 West trips 
	dim(vms.29)
	#34323 in 2017 
	# 31268  in  2018
  # 27984 in 2019
	# 35613 in 2020 
	# 35408 in 2021 ; 35529 from JS pull 
	
	unique(vms.29$assigned_area)
	table(vms.29$assigned_area)
	# Plot Year of VMS that fished SFA 29W to check data #
	plot(lat~lon,vms.29, ylim=c(42,46), xlim=c(-66.5,-65))
	
	head(vms.29)
	unique(vms.29$vrn)
 #subset.vms.29 <- 	vms.29 %>% filter(vrn %in% c("101440", "101469", "101593", "101594", "104257", "105290")) 

#	subset.vms.29 <- subset.vms.29 %>% mutate(LATITUDE = as.numeric(lat), LONGITUDE = as.numeric(lon))
	
#	subset.vms.29.cln <- VMS_clean_recs(df = subset.vms.29,  lat.field = "LATITUDE",
#	                             lon.field = "LONGITUDE",
#	                             objField = "vrn",
#	                             timeField = "vmstime",
#	                             minDist_m = 0,
#	                             maxBreak_mins = 240)  #break time is 4 hour: 60*4 = 240 minutes 
	
#	saveRDS(subset.vms.29, file = "subset.vms.29.rds")
	
	
## --- Calculate Speed via R ---- 
	## UNDER DEVELOPMENT - MODIFY CODE FROM DAVE 
	vms.29 <- vms.29 %>% mutate(LATITUDE = as.numeric(lat), LONGITUDE = as.numeric(lon))
	str(vms.29)
	head(vms.29)
	dim(vms.29)
	# Now get rid of back to back points within 50 meters of each other, and with more than 4 hours between pooling
	# I'm not actually gonna do this...
	vms.29.cln <- VMS_clean_recs(df = vms.29,  lat.field = "LATITUDE",
	                             lon.field = "LONGITUDE",
	                             objField = "vrn",
	                             timeField = "vmstime",
	                             minDist_m = 0,
	                             maxBreak_mins = 240)  #break time is 4 hour: 60*4 = 240 minutes 
	
	head(vms.29.cln) 
  unique(vms.29.cln$trek)
	dim(vms.29.cln)
	#28396  with minDist_m = 50 
	#35408   with minDist_m = 0
	
	# to be comparable need to add option to subset data to minimum breaks -- i.e. pair down data to coarses temporal resolution. 
	# in my perl script - this is the minimumInterogationTime -- which for 29W we set to 48 minutes (i.e. 1 hour polling)
	
	
#----- Calculate speed for VMS -RUN PERL THROUGH R  ----
#   Requires that Perl be installed on local machine  #
#
	#ONLY NEED TO DO IF VMS IS FROM DIRECT DATABASE PULL - NOT Mar.Util and are running Perl 	
	# reorder columns and Format for PERL #
	vms.29 <- vms.29[c( "vesid", "lon", "lat", "vessel_name", "vrn", "year", "vmsdate", "vmstime", "hailout", "julian_date", "inst_speed", "obs", "ves_id", "licence_href","assigned_area","uid")]
	
#	vms.29 <- vms.29 %>% select(vrn      lat       lon             vmstime speed_knots         update_date    vmsdate)
	
	write.table(vms.29, paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/SFA29vms_",surveyyear,".txt",sep=""),quote=FALSE, row.names=FALSE, sep="\t") #write inshore scallop vms data to table for year = YR
	#place here: D:\VMS\data\vms\vmsFormatted\sfa29
	
#Creates run.bat file and runs. Must specify folder that contains both PERL script and VMS script for which speed will be calculated. 
	setwd("D:/VMS/scripts/perl/")
	#note: move file: E:\Documents and Settings\VMS\data\vms\vmsFormatted\sfa29\SFA29vms_YYYY.txt  to same folder as perl script. Update line 4 of perl script with file name SFA29vms_YYYY.txt 
	cat(  "perl onehour_add_tripID.pl > SFA29vms_2021out.txt", file="run.bat") #update output file name
	system( "run.bat",  intern=FALSE, wait=TRUE, show.output.on.console = TRUE)

	#move output here D:\VMS\scripts\perl\out_60min\sfa29_byYr
	
	#vms.29.out <- read.delim2("D:/VMS/scripts/perl/out_60min/sfa29_byYr/SFA29vms_2020out.txt") #read in vms file with speed
	vms.29.out <- read.delim2(paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/SFA29vms_",surveyyear,"out.txt"))
	dim(vms.29.out)
	
	names(vms.29.out)
  str(vms.29.out)	
  vms.29.out$SPEED_KNOTS <- as.numeric(vms.29.out$SPEED_KNOTS )
  vms.29.out$DISTANCE_NM <- as.numeric(vms.29.out$DISTANCE_NM )
  vms.29.out$lon <- as.numeric(vms.29.out$lon)
  vms.29.out$lat <- as.numeric(vms.29.out$lat)
  plot(lat~lon,vms.29.out)
  
#remove 0 lat or 0 lon points 
  vms.29.out <- vms.29.out[vms.29.out$lat>0,]
  vms.29.out <- vms.29.out[vms.29.out$lon<0,]
  dim(vms.29.out)
  plot(lat~lon,vms.29.out)
       
#make sf object    
#  crs_wgs84 <- sp::CRS(SRS_string = "EPSG:4326")
  vms.29.out.sf <- st_as_sf(vms.29.out,  coords = c("lon", "lat"),crs = 4326) #WGS84 
  plot(vms.29.out.sf)
  #check CRS:
  st_crs(vms.29.out.sf)
  #convert CRS to UTM zone 19:
  vms.29.out.sf.utm <- st_transform(vms.29.out.sf, sp::CRS(SRS_string = "EPSG:32619"))
  st_coordinates(vms.29.out.sf.utm)
  st_crs(vms.29.out.sf.utm)
  crs(vms.29.out.sf.utm)


  
###
###    ---- Spatially intersect VMS with Speed with SDM Rater ----  
###
# In ArcGIS: Intersect vms.29.out (i.e. SFA29vms_YYYYout.txt) with SDM layer in GIS: Use ArcGIS Workspace: SFA29vms2002toYYYY.mxd, intersect with integer raster: sdm29bins # 
#  Notes: shp and raster MUST both be in WGS UTM Zone 19N (Use Project tool to convert shp to WGS UTM Zone 19N)
#		  Use Extract Values to Points tool (SA) (Input Pt Feature: vmsYYYY_utm.shp; raster: sdm29bins  #
#         Export vms data with SDM as dbf (Under export in attribute table choose 'Save type as: dBASE Table #
  
#import SDM raster  
# 3 different ways to inport raster, going with raster package original raster function pull but other code left for interest
#1. import with stars: 
#sdm.raster.stars <- stars::read_stars("Y:/INSHORE SCALLOP/Databases/Scallsur/SFA29BottomTypes/SDM/sdm29bins",proxy = TRUE)
#2. import with specifying driver with rgdal and raster:
#     dpath <- "Y:/INSHORE SCALLOP/Databases/Scallsur/SFA29BottomTypes/SDM/sdm29bins"
#     x <- new("GDALReadOnlyDataset", dpath)
#     getDriver(x)
#     getDriverLongName(getDriver(x))
#     xx <- asSGDF_GROD(x)
#     r <- raster::raster(xx)
# 3. raster import function from raster package: 
  sdm.raster <- raster(paste0(ess,":/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/sdm29bins"))  
  crs(sdm.raster) #confirm its in UTM zone 19N -- should be 
  
#Intersect  vms.29.out_sf with raster sdm.raster
  start <- Sys.time()
  sdm.values <- raster::extract(sdm.raster, vms.29.out.sf.utm)
  end <- Sys.time() - start
  vms.29.out.sf.utm$SDM <- sdm.values
  dim(vms.29.out.sf.utm)  
  head(vms.29.out.sf.utm)
  sum(!is.na(vms.29.out.sf.utm$SDM))
#check with plots 
  ggplot() + geom_histogram(aes(vms.29.out.sf.utm$SDM))
 
  ggplot(vms.29.out.sf.utm) + geom_histogram(aes(vms.29.out.sf.utm$SDM)) + facet_wrap(~assigned_area)
  
# Import SFA 29 VMS with speed and SDM and use UID to merge to original vms #
 # sdm <- read.dbf(paste0(ess,":/INSHORE SCALLOP/SFA29/2021/Assessment/Data/CommercialData/VMS/shp/SFA29vms_2020SDM.dbf"))
#  sdm$RASTERVALU[sdm$RASTERVALU==-9999] <- NA
#  dim(sdm)
#  ggplot(sdm) + geom_histogram(aes(sdm$RASTERVALU)) + facet_wrap(~assigned_a)
#  sum(!is.na(sdm$RASTERVALU))
#	setwd("Y:/INSHORE SCALLOP/SFA29/2020/") #update
#	sdm <-  read.dbf("VMS/SFA29vms_2019SDM.dbf") #update
#	sdm <- sdm[,c("uid","RASTERVALU")] # uid (join field)
#	head(sdm)
#  length(unique(sdm$uid))
	# Join sdm to vms on uid #
#	vms.sdm <- merge(vms.29.out, sdm, by.x=c("uid"), by.y=c("uid"))#
#	dim(vms.sdm)
#  names(vms.sdm)[22] <- "SDM"#
#	head(vms.sdm)
#  names(vms.sdm)


#check CRS:
crs(poly.sf)
crs(poly.subareas)
crs(vms.29.out.sf.utm)

#test plots 
plot(poly.sf)
plot(poly.subareas)
plot(vms.29.out.sf.utm)

#intersection - returns sf object but just of those points that are within the polygon 
dim(vms.29.out.sf.utm)
outdata <- st_intersection(vms.29.out.sf.utm, poly.sf)
dim(outdata)
str(outdata)

#plot sf vms that overlaps with SFA 29W 
plot(outdata)
head(outdata)

#check summaries
table(outdata$SFA29W)
table(outdata$SDM)


# only VMS records in SFA 29W boundary 
vms.sdm.29 <- outdata
#from 2002 to 2014: records 223979; 2015: 5455;  7371 in 2017; 2018 was 6799;  2019 was 6141; 2020 was 7395; 2021 7581   
dim(vms.sdm.29) 
#convert back to lat/lon WGS 84 
vms.sdm.29 <- st_transform(vms.sdm.29, crs = sp::CRS(SRS_string = "EPSG:4326")) #WGS84    
head(vms.sdm.29) 
#remove geometry to get just dataframe back:
vms.sdm.29 <- cbind(st_drop_geometry(vms.sdm.29),st_coordinates(vms.sdm.29))
class(vms.sdm.29)
dim(vms.sdm.29)
str(vms.sdm.29)
head(vms.sdm.29)
#Replace column labels of X and Y with lon and lat 
names(vms.sdm.29)[grep("X", names(vms.sdm.29))] <-'lon'
names(vms.sdm.29)[grep("Y", names(vms.sdm.29))] <- 'lat'

#### ID Fishing VMS using speed filter ###
	lowerlimit <- 0.191
	upperlimit <- 1.175

	vms.sdm.29speed  <-  vms.sdm.29[vms.sdm.29$SPEED_KNOTS>=lowerlimit & vms.sdm.29$SPEED_KNOTS<=upperlimit,] # VMS fishing 2015 after speed filter: 2985;  
  dim(vms.sdm.29speed) #in 3715 in 2020; 2019 is 3031; 3388 records in 2018; 3650 in 2017.... 3912 in 2021 

###..................................................................................###
### ---- Prorate VMS effort on Logbook Catch - note assuming a single area and year ----
###..................................................................................###
	
	logs <- scal.log #scal.log logs from year YYYY selected from SCALLOP db above. 
	logs$CNT <- 1
	logs <- logs[!duplicated(logs),] # removes full duplicates
	logs$year <- as.numeric(strftime(logs$date_fished, format="%Y"))
	logs$date_fished <- as.Date(logs$date_fished)
	
	#Calculate effort for vms comparison 
	mon.doc.data <- logs 
	mon.doc.data$effort <- (mon.doc.data$avg_tow_time*mon.doc.data$num_of_tows)/60
	mon.doc.trip.effort <- as.data.frame(tapply(mon.doc.data$effort,mon.doc.data$trip_id, FUN=sum))
	names(mon.doc.trip.effort) <- "effort"
	
	logs <- aggregate(logs$CNT, by=list(logs$trip_id, logs$vr_number, logs$year, logs$sum_slip_weight_lbs, logs$date_fished), FUN=sum)
	names(logs) = c("TRIP_ID_LOGS","VR_NUMBER","YEAR","SUM_SLIP_WEIGHT_LBS", "DATE_FISHED", "CNT")
	logs <- logs[logs$CNT==1,] # logs from one subarea only 
	logs$SUM_SLIP_KG <- logs$SUM_SLIP_WEIGHT_LBS*0.45359237
	logs$DATE_FISHED <- as.character(logs$DATE_FISHED)
	
	vms29 <- merge(vms.sdm.29speed, logs, by.x=c("year", "vrn", "vmsdate"), by.y=c("YEAR","VR_NUMBER","DATE_FISHED"))
	dim(vms29)
	#[1] 3633   27
	#  3774   in 2021 
	
	#Effort by trip
	vms.effort <- aggregate(vms29$TIME_DIFF_S, by=list(vms29$TRIP_ID_LOGS, vms29$SUM_SLIP_WEIGHT_LBS), FUN=sum)
	names(vms.effort) <- c("TRIP_ID_LOGS", "SUM_SLIP_WEIGHT_LBS", "VMSEFFORT_TOTALHRS")
	vms.effort$VMSEFFORT_TOTALHRS <- vms.effort$VMSEFFORT_TOTALHRS/3600
	
	#merge total effort by trip to individual vms records 
	vms29 <- merge(vms29, vms.effort, by.x=c("TRIP_ID_LOGS","SUM_SLIP_WEIGHT_LBS"), by.y=c("TRIP_ID_LOGS","SUM_SLIP_WEIGHT_LBS"))
	dim(vms29)
	vms29$PropEffort <- ((vms29$TIME_DIFF_S)/3600) /vms29$VMSEFFORT_TOTALHRS #proportion of effort of a whole trip by vms record
	vms29$PropCatch_kg <- vms29$PropEffort*vms29$SUM_SLIP_KG #proportion of total trip catch by vms record 
	
#check for significant outliers and remove 
  plot(vms29$PropCatch_kg)
  summary(vms29$PropCatch_kg)
  vms29 <- vms29[vms29$PropCatch_kg < 400,]
  dim(vms29) #2019: 2977 ;  3347 in 2018; 3504 in 2017
  names(vms29)

# Export vms with prorated catch # 
  write.csv(vms29, paste0(ess, path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/SFA29vms_",surveyyear,"_proratedCatch.csv"), row.names = FALSE)
#read in all data since 2002 and append recent year data to full dataset#
  vms.prorated.catch <- read.csv(paste0(ess, path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/VMSproratedCatch/SFA29vms.2002to",(surveyyear-1),".proratedCatch.csv"))
  table(vms.prorated.catch$year)
  head(vms.prorated.catch)
  head(vms29)
  
  vms29$Subarea <- substr(vms29$assigned_area,3,3)
  vms29$year <- year(as.Date(vms29$vmsdate))
  vms29.format <- vms29[,c("TRIP_ID",	"TRIP_ORD",	"TIME_DIFF_S",	"DISTANCE_NM",	"SPEED_KNOTS",	"vesid",	"lon",	"lat", "vrn", "vessel_name", "year", "vmsdate", "vmstime", "SUM_SLIP_KG", "VMSEFFORT_TOTALHRS", "PropEffort", "PropCatch_kg", "Subarea", "SDM")]
  
  names(vms29.format) <- tolower(names(vms29.format))
  vms.prorated.catch.all <- rbind(vms.prorated.catch, vms29.format)
  table(vms.prorated.catch.all$year)
  
  write.csv(vms.prorated.catch.all, paste0(ess, path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/VMSproratedCatch/SFA29vms.2002to",(surveyyear),".proratedCatch.csv"),  row.names = FALSE)
  #note you will used vms.prorated.catch.all for further analysis lower down in the script 
  
  
### Compare VMS trip effort to log trip effort ###
	par(mfrow=c(1,2))  # all plots on one page
	hist(vms.effort$VMSEFFORT_TOTALHRS, ylim=c(0,30), xlim=c(0,120), xlab="VMS effort by trip (hr)", main =paste0("SFA 29 ", surveyyear))
	hist(mon.doc.trip.effort$effort, ylim=c(0,1000), xlim=c(0,120), xlab="Log effort by trip (hr)",main =paste0("SFA 29 ",surveyyear))
	#save as CompareLogvsVMSeffortbyTripYYYY.png If you want 
	

###
###  ---- Spatial Plots of VMS  ----
###
#Read in and plot all years of SFA 29W VMS with speed 
	# YEAR  i : 2002=1, 2003=2, ... , 2012=11, 2013=12, 2014=13 , 2015=14, 2016=15, 2017=16, 2018=17 , 2019 = 18
	i <- length(2002:surveyyear) 
	temp <- list.files(path=paste0(ess, path.directory, assessmentyear,"/Assessment/Data/CommercialData/VMS/sfa29_byYr/"), pattern="*.txt")
	#setwd(paste0(ess, path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/sfa29_byYr/"))  #note this is needed for lappy to work on all .txt files in the folder
	# Read in and merge all files into a list	
	all.29vms <- lapply(temp, function(x) read.delim2(paste0(ess, path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/sfa29_byYr/",x))) 

#merge all years VMS in SFA 29W into a data.frame 
	all.29vms <- ldply(all.29vms, data.frame)
	dim(all.29vms)
	head(all.29vms)
	str(all.29vms)
	table(all.29vms$year) #records by year - check if makes sense 

	#convert to sf object 
	all.29vms.sf <- st_as_sf(all.29vms,  coords = c("lon", "lat"), crs = 4326)
	#check CRS:
	st_crs(all.29vms.sf)

#Subset to just records within SFA 29W boundary - have both datasets in UTM Zone 19N for intersection 
	all.29vms.sf.utm <- st_transform(all.29vms.sf, 32619)
	crs(all.29vms.sf.utm)
	all.29vms.sf.IN <- st_intersection(all.29vms.sf.utm, poly.sf)
	dim(all.29vms.sf.IN)
	
### ID "Fishing" VMS using speed filter ###
	lowerlimit <- 0.191
	upperlimit <- 1.175
	
	all.29vms.sf.IN$SPEED_KNOTS <- as.numeric(all.29vms.sf.IN$SPEED_KNOTS)
	all.29vms.sf.IN.fishing  <-  all.29vms.sf.IN[all.29vms.sf.IN$SPEED_KNOTS>=lowerlimit & all.29vms.sf.IN$SPEED_KNOTS<=upperlimit,] # VMS fishing 106774 from 2002 to 2014
	dim(all.29vms.sf.IN.fishing)
	
	#plot theme to apply to all plots
	plot.theme <-	theme(axis.text = element_text(size = 8),
	                    panel.border = element_blank(), panel.grid.major = element_blank(),
	                    plot.margin = margin(0,0.5,0,0, "cm"))
	
# NO speed filter - Plot All Years of VMS that fished SFA 29W to check data  
	#Pecjector basemap #ylim=c(43.1,43.8);	xlim=c(-66.5,-65.45) # is from SFA 29 from scallopmap 
	p <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)), add_custom = list(obj = all.29vms.sf.IN, size = 1, fill = NA, color = alpha("black", alpha=0.1)))
	
	p +
	  geom_sf(data = poly.subareas, fill=NA, colour="grey")+
	  coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
	  plot.theme
	
ggsave(paste0("SFA29vms_2002to",surveyyear,"_all.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

	
# NO speed filter - Plot All Years of VMS IN SFA 29W with current year (YR) identified - NO speed filter#
	xx <- surveyyear 	#Be sure YR is the year you want 
	p1 <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey",  bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)))
	p1 +
	  geom_sf(data = poly.subareas, fill=NA, colour="grey", size = 1) +
	  geom_sf(data = subset(all.29vms.sf.IN, year<xx), size = 0.5, colour = alpha("black", alpha=0.2)) + #all years prior to current year 
	  geom_sf(data = subset(all.29vms.sf.IN, year==xx), size = 0.5, colour = alpha("red", alpha=0.2))+ #current year
	coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
	  plot.theme 

ggsave(paste0("SFA29vms_2002to",(surveyyear-1),"v",xx,"_all.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# NO speed filter - Plot Current Years of VMS IN SFA 29W vs Previous Year - NO speed filter 
	xx <- surveyyear 	#Be sure YR is the year you want 
	yy <- surveyyear - 1
	p2 <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey",  bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)))
	
	p2 +
	  geom_sf(data = poly.subareas, fill=NA, colour="grey", size = 1) +
	  geom_sf(data = subset(all.29vms.sf.IN, year==yy), size = 0.5, colour = alpha("black", alpha=0.2)) + #all years prior to current year 
	  geom_sf(data = subset(all.29vms.sf.IN, year==xx), size = 0.5, colour = alpha("red", alpha=0.2)) +#current year 
	  coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
	  plot.theme 
	
ggsave(paste0("SFA29vms_",yy,"v",xx,"_all.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

	
# SPEED FILTERED "FISHING" - Plot All Years of VMS that fished SFA 29W to check data 
	#Pecjector basemap #ylim=c(43.1,43.8);	xlim=c(-66.5,-65.45) # is from SFA 29 from scallopmap 
p3 <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)), add_custom = list(obj = all.29vms.sf.IN.fishing, size = 1, fill = NA, color = alpha("black", alpha=0.1)))
	
p3 +
	  geom_sf(data = poly.subareas, fill=NA, colour="grey", size = 1)+
  coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
  plot.theme

ggsave(paste0("SFA29vms_2002to",surveyyear,"_filtered.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

	
# SPEED FILTERED "FISHING" Plot All Years of VMS IN SFA 29W with current year (YR) identified in red 
xx <- surveyyear 	#Be sure YR is the year you want 

p4 <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)))

p4 +
	 geom_sf(data = poly.subareas, fill=NA, colour="grey", size = 1) +
	 geom_sf(data = subset(all.29vms.sf.IN.fishing, year<xx), size = 0.5, colour = alpha("black", alpha=0.2)) + #all years prior to current year 
	 geom_sf(data = subset(all.29vms.sf.IN.fishing, year==xx), size = 0.5, colour = alpha("red", alpha=0.2))+ #current year
  coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
  plot.theme

ggsave(paste0("SFA29vms_2002to",(surveyyear-1),"v",xx,"_filtered.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# SPEED FILTERED "FISHING" Plot Current Years of VMS IN SFA 29W vs Previous Year  
xx <- surveyyear 	#Be sure YR is the year you want 
yy <- surveyyear - 1

p5 <- pecjector(area =list(x=c(-66.5,-65.45), y=c(43.1,43.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = "ScallopMap", scale.bar = c('bl',0.25,-1,-1)))

	p5 +
	  geom_sf(data = poly.subareas, fill=NA, colour="grey") +
	  geom_sf(data = subset(all.29vms.sf.IN.fishing, year==yy), size = 0.5, colour = alpha("black", alpha=0.2)) + #all years prior to current year 
	  geom_sf(data = subset(all.29vms.sf.IN.fishing, year==xx), size = 0.5, colour = alpha("red", alpha=0.2)) +#current year 
	  coord_sf(xlim = c(-66.5,-65.45), ylim = c(43.1,43.8), expand = FALSE)+
	  plot.theme

ggsave(paste0("SFA29vms_",yy,"v",xx,"_filtered.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
	
#ggsave(paste0("SFA29vms_",yy,"v",xx,"_filtered.png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), dpi=300)   
	
	
#### 
	#vms.prorated.catch.all is dat.2002toYYYY from 2 of 2 script 
	
	#areas
	areas <- read.csv(paste0(ess,":/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/SDM_Binned_Areas.csv"))
	areas$subarea <- substr(areas$Subarea,6,6)
	areas$sdm <- areas$strata.id
	areas <- areas[,2:ncol(areas)]
	areas$sdm <- areas$sdm/10
	areas$sdmstrata <- NA
	areas$sdmstrata[areas$sdm < 0.3] <- "low"
	areas$sdmstrata[areas$sdm >= 0.3 & areas$sdm < 0.6] <- "med"
	areas$sdmstrata[areas$sdm >= 0.6] <- "high"
	areassdm <- areas 
	
	areas <- aggregate(areas$Area.sq.km, by=list(areas$subarea, areas$sdmstrata),sum)
	names(areas) <- c("subarea","sdmstrata","Area.sq.km")
	
	#landings by subarea 
	landings <- read.csv(paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/SFA29_totalLandings_YearSubarea.csv"))
	landings$landingskg <- landings$Landingsmt*1000
	landings$subarea <- substring(landings$Area,3,3)
	names(landings) <- tolower(names(landings)) 
	
	
	#vms.prorated.catch.all is dat.2002toYYYY which is dat below: 
	table(vms.prorated.catch.all$sdm, useNA = c("always"))
	
	vms.prorated.catch.all$effort_hr <- vms.prorated.catch.all$vmseffort_totalhrs*vms.prorated.catch.all$propeffort  #effort in hours by vms record 
	vms.prorated.catch.nona <- vms.prorated.catch.all[vms.prorated.catch.all$sdm!=-9999,]
	vms.prorated.catch.nona <- vms.prorated.catch.nona[!is.na(vms.prorated.catch.nona$sdm),]
	vms.prorated.catch.nona.noE <- vms.prorated.catch.nona[vms.prorated.catch.nona$subarea!="E",]
	vms.prorated.catch.nona.noE$sdm <- vms.prorated.catch.nona.noE$sdm/10
	
	eff.area <- aggregate(vms.prorated.catch.nona.noE$effort_hr,by=list(vms.prorated.catch.nona.noE$subarea, vms.prorated.catch.nona.noE$sdm, vms.prorated.catch.nona.noE$year),sum)  #sum vms effort by subarea by sdm by year 
	names(eff.area) <- c("subarea","sdm","year","totalefforthr")
	catch.area <- aggregate(vms.prorated.catch.nona.noE$propcatch_kg,by=list(vms.prorated.catch.nona.noE$subarea, vms.prorated.catch.nona.noE$sdm, vms.prorated.catch.nona.noE$year),sum)  #sum vms catch by subarea by sdm by year 
	names(catch.area) <- c("subarea","sdm","year","totalcatchkg")
	
	eff.area <- merge(eff.area, catch.area, by=c("subarea","sdm","year")) #vms catch and effort by subarea by sdm by year 
	eff.area$sdmstrata <- NA
	eff.area$sdmstrata[eff.area$sdm < 0.3] <- "low"
	eff.area$sdmstrata[eff.area$sdm >= 0.3 & eff.area$sdm < 0.6] <- "med"
	eff.area$sdmstrata[eff.area$sdm >= 0.6] <- "high"
	
	#vms catch and effort by high, med and low SDM strata by subarea by year 
	effort.strata <- aggregate(eff.area$totalefforthr,by=list(eff.area$sdmstrata, eff.area$year, eff.area$subarea), sum)
	names(effort.strata) <- c("sdmstrata","year","subarea","effort.hr")
	catch.strata <- aggregate(eff.area$totalcatchkg,by=list(eff.area$sdmstrata, eff.area$year, eff.area$subarea), sum)
	names(catch.strata) <- c("sdmstrata","year","subarea","catch.kg")
	
	vms.catch <- aggregate(catch.strata$catch.kg, by=list(catch.strata$subarea,catch.strata$year),sum) #total catch by subarea from VMS 
	names(vms.catch) <- c("subarea","year","catch.kg.tot")
	
	vms.catch <- merge(vms.catch, landings, by=c("subarea","year"), all.y=TRUE)
	vms.catch <- vms.catch[vms.catch$subarea!='E',]
	vms.catch$catch.kg.tot[which(is.na(vms.catch$catch.kg.tot))] <- 0 #replace vms catch as NA with zero 
	vms.catch$propcatch <- vms.catch$catch.kg.tot/vms.catch$landingskg # proportion of vms catch by subarea by year 
	
	effort.strata <- merge(effort.strata, catch.strata, by=c("subarea","sdmstrata","year"))
	effort.strata <- merge(effort.strata, vms.catch, by=c("subarea","year"))
	
	effort.strata$corr.effort.hr <- effort.strata$effort.hr/effort.strata$propcatch  #calculate corrected vms effort
	
	effort.strata <- merge(effort.strata, areas, by=c("subarea","sdmstrata"),all.x=TRUE) #merge with area 
	
	effort.strata$effort.std <- effort.strata$corr.effort.hr/effort.strata$Area.sq.km  #calc effort per sq km NOTE must use CORRECTED VMS effort 
	
	effort.strata$subarea <- ordered(effort.strata$subarea,c("C","D","A","B"))
	effort.strata$sdmstrata <- ordered(effort.strata$sdmstrata,c("high","med","low"))
	effort.strata <- effort.strata[order(effort.strata$subarea, effort.strata$sdmstrata, effort.strata$year), ]  # Order dataframe 
	dim(effort.strata)
	
	#...add in records where there was zero vms effort in a sdm strata in a subarea for years where there was fishing in those subareas   
	#Subarea A missing high in 2003, 2005, 2008, 2013, 2015, 2016
	zerorow1 <-  data.frame(subarea=rep('A',6),sdmstrata=rep('high',6),year=c(2003,2005,2008,2013,2015,2016), effort.hr=rep(0,6),catch.kg=rep(NA,6), catch.kg.tot=rep(NA,6), area=rep(NA,6), landingsmt=rep(NA,6), landingskg=rep(NA,6) ,propcatch=rep(NA,6), corr.effort.hr=rep(NA,6), Area.sq.km=rep(NA,6) ,effort.std=rep(0,6))
	
	#Subarea A missing medium and low in 2003
	zerorow2 <- data.frame(subarea=rep('A',2),sdmstrata=c('low','med'),year=rep(2003,2), effort.hr=rep(0,2),catch.kg=rep(NA,2), catch.kg.tot=rep(NA,2), area=rep(NA,2), landingsmt=rep(NA,2), landingskg=rep(NA,2) ,propcatch=rep(NA,2), corr.effort.hr=rep(NA,2), Area.sq.km=rep(NA,2) ,effort.std=rep(0,2))
	
	#Subarea C missing Low, medium and high in 2014
	zerorow3 <- 	data.frame(subarea=rep('C',3),sdmstrata=c('low','med','high'),year=rep(2014,3), effort.hr=rep(0,3),catch.kg=rep(NA,3), catch.kg.tot=rep(NA,3), area=rep(NA,3), landingsmt=rep(NA,3), landingskg=rep(NA,3) ,propcatch=rep(NA,3), corr.effort.hr=rep(NA,3), Area.sq.km=rep(NA,3) ,effort.std=rep(0,3))
	
	#Subarea D missing low, med, high in 2014
	zerorow4 <- 	data.frame(subarea=rep('D',3),sdmstrata=c('low','med','high'),year=rep(2014,3), effort.hr=rep(0,3),catch.kg=rep(NA,3), catch.kg.tot=rep(NA,3), area=rep(NA,3), landingsmt=rep(NA,3), landingskg=rep(NA,3) ,propcatch=rep(NA,3), corr.effort.hr=rep(NA,3), Area.sq.km=rep(NA,3) ,effort.std=rep(0,3)) 
	
	#Subarea A missing medium and low in 2015 and 2016
	zerorow5 <-  data.frame(subarea=rep('A',4),sdmstrata=c('low','med'),year=c(2015,2015,2016,2016), effort.hr=rep(0,4),catch.kg=rep(NA,4), catch.kg.tot=rep(NA,4), area=rep(NA,4), landingsmt=rep(NA,4), landingskg=rep(NA,4) ,propcatch=rep(NA,4), corr.effort.hr=rep(NA,4), Area.sq.km=rep(NA,4) ,effort.std=rep(0,4))
	
	#Subarea B closed in 2018 B - but 0.013 mt removed but no VMS records associated with this small amount of catch; 0 for high, med, low in 2018 in B  
	zerorow6 <- data.frame(subarea=rep('B',3),sdmstrata=c('low','med','high'),year=rep(2018,3), effort.hr=rep(0,3),catch.kg=rep(NA,3), catch.kg.tot=rep(NA,3), area=rep(NA,3), landingsmt=rep(NA,3), landingskg=rep(NA,3) ,propcatch=rep(NA,3), corr.effort.hr=rep(NA,3), Area.sq.km=rep(NA,3) ,effort.std=rep(0,3)) 
	
	addzero <- 	rbind(zerorow1,zerorow2, zerorow3, zerorow4, zerorow5, zerorow6)
	effort.strata <- rbind(effort.strata, addzero)
	dim(effort.strata)
	
	#reorder dataframe 
	effort.strata <- effort.strata[order(effort.strata$subarea, effort.strata$sdmstrata, effort.strata$year), ]  # Order dataframe 
	
	#export effort by sdm strata within each subarea A, B, C, D
	write.csv(effort.strata,paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/effort.strata.",surveyyear,".csv"))
	
	
# Corrected effort by SDM bins 
	effort.bins <- merge(eff.area, vms.catch, by=c("subarea","year"),all.x=TRUE) 
	effort.bins$effort.cor <- effort.bins$totalefforthr/effort.bins$propcatch #calculate corrected vms effort - this on the sdm bin level 
	effort.bins <- merge(effort.bins,areassdm ,by=c("subarea","sdm"),all.x=TRUE )
	effort.bins$effort.std  <- effort.bins$effort.cor/effort.bins$Area.sq.km  #CHECK calc effort per sq km NOTE must use CORRECTED VMS effort  
	effort.bins <- effort.bins[order(effort.bins$subarea, effort.bins$sdm, effort.bins$year), ]  # Order dataframe 
	
	#...add in records where there was zero vms effort in a sdm in a subarea in a year 
	#sdm zeros from 2003 to 2015 in A, C, and D:
	sdmzeros.2003to2015 <- data.frame(subarea=c(rep('A',10),rep('C',10),rep('D',10),rep('A',10)), sdm=rep(seq(0,0.9,0.1),4), year=c(rep(2003,10),rep(2014,10),rep(2014,10),rep(2015,10)), totalefforthr=rep(NA,40), totalcatchkg=rep(NA,40) ,sdmstrata.x=rep(NA,40), catch.kg.tot=rep(NA,40), area=rep(NA,40), landingsmt=rep(NA,40), landingskg=rep(NA,40), propcatch=rep(NA,40), effort.cor=rep(NA,40), Bottom.type=rep(NA,40), Area.sq.km=rep(NA,40), NH=rep(NA,40), Strata=rep(NA,40), strata.id=rep(NA,40), sdmstrata.y=rep(NA,40), effort.std=rep(NA,40))
	
	#sdm zeros for 2016: 
	sdmzeros.A.2016 <- data.frame(subarea=c(rep('A',10)), sdm=seq(0,0.9,0.1), year=c(rep(2016,10)), totalefforthr=rep(NA,10), totalcatchkg=rep(NA,10) ,sdmstrata.x=rep(NA,10), catch.kg.tot=rep(NA,10), area=rep(NA,10), landingsmt=rep(NA,10), landingskg=rep(NA,10), propcatch=rep(NA,10), effort.cor=rep(NA,10), Bottom.type=rep(NA,10), Area.sq.km=rep(NA,10), NH=rep(NA,10), Strata=rep(NA,10), strata.id=rep(NA,10), sdmstrata.y=rep(NA,10), effort.std=rep(NA,10))
	
	#subarea B for 2018 - closed; Added 
	sdmzeros.B.2018 <- data.frame(subarea=c(rep('B',10)), sdm=seq(0,0.9,0.1), year=c(rep(2018,10)), totalefforthr=rep(NA,10), totalcatchkg=rep(NA,10) ,sdmstrata.x=rep(NA,10), catch.kg.tot=rep(NA,10), area=rep(NA,10), landingsmt=rep(NA,10), landingskg=rep(NA,10), propcatch=rep(NA,10), effort.cor=rep(NA,10), Bottom.type=rep(NA,10), Area.sq.km=rep(NA,10), NH=rep(NA,10), Strata=rep(NA,10), strata.id=rep(NA,10), sdmstrata.y=rep(NA,10), effort.std=rep(NA,10))
	
	#for ALL years - add zeros for sdm values 0.8 and and 0.9 in subarea A - Must be done bc no 0.8 or 0.9 areas in A (see areas sdm). Must be done for each year - so why using surveyyear 
	range.year <- seq(2002,surveyyear,1) 
	tot.length <- (length(range.year)*2)
	sdmzeros.A <- data.frame(subarea=rep('A',tot.length), sdm=c(rep(0.8,length(range.year)),rep(0.9,length(range.year))), year=rep(range.year,2), totalefforthr=rep(NA,tot.length), totalcatchkg=rep(NA,tot.length) ,sdmstrata.x=rep(NA,tot.length), catch.kg.tot=rep(NA,tot.length), area=rep(NA,tot.length), landingsmt=rep(NA,tot.length), landingskg=rep(NA,tot.length), propcatch=rep(NA,tot.length), effort.cor=rep(NA,tot.length), Bottom.type=rep(NA,tot.length), Area.sq.km=rep(NA,tot.length), NH=rep(NA,tot.length), Strata=rep(NA,tot.length), strata.id=rep(NA,tot.length), sdmstrata.y=rep(NA,tot.length), effort.std=rep(NA,tot.length))
	
	#rbind to final dataframe 
	sdmzeros <- rbind(sdmzeros.2003to2015,sdmzeros.A.2016, sdmzeros.B.2018 ,  sdmzeros.A)
	effort.bins <- rbind(effort.bins,sdmzeros)
	dim(effort.bins)
	effort.bins <- effort.bins[order(effort.bins$subarea, effort.bins$sdm, effort.bins$year), ]  # ReOrder dataframe 
	
	#write.csv(effort.bins,"VMS/effort.sdmbins.csv")
	#export effort by sdm bin within each subarea A, B, C, D
	write.csv(effort.strata,paste0(ess,path.directory,assessmentyear,"/Assessment/Data/CommercialData/VMS/effort.sdmbins.",surveyyear,".csv"))
	
	
### ---- VMS Effort Plots ---- 
	
	# NOTE - ZERO EFFORT FOR 2014 and 2015 when areas closed - Dec 24 2018 note from JS CHECK effort.strata 
	#Fishing effort by SDM strata by year 
	
	
	# Assign SDM level (low,med,high) 
	#xx <- effort.strata #plot all data
#	windows(11,11)
#	xx <- effort.strata[!(effort.strata$subarea=='A'&effort.strata$sdmstrata=='high'),] #remove subarea A sdm high from plot 
#	xyplot(effort.std ~ year|subarea, groups = sdmstrata, data=xx, scales=list(tick.number=10),	
#	       pch = c(3,2,1), col = c("green","red","black"), type="b", lty=c(3,2,1), 
#	       ylim=c(-2,60), 
#	       as.table = FALSE,
#	       xlab="Year", ylab=expression(paste("VMS Effort  ", (h/km^2),sep="")), main="",
#	       key= list(x=.30,y=.95,corner=c(1,1), transparent=TRUE, lines =list(lty=c(3,2,1),col=c("green","red","black"), 
#	                                                                          pch=c(3,2,1),type="b"),divide=1,cex=0.8, text=list(c("High","Medium","Low"))) 
#	) 
	#save as VMSeffortpersqkm.png

	# VMS Effort per sq km by Year by Subarea 
	#reorder subarea levels 
	effort.strata$subarea	<- ordered(effort.strata$subarea, levels=c("A", "B", "C", "D"))
	effort.strata.no.A.high <- effort.strata[!(effort.strata$subarea=='A'&effort.strata$sdmstrata=='high'),] #remove subarea A sdm high from plot 
	ggplot(effort.strata.no.A.high) + geom_point(aes(x=year, y = effort.std, group=sdmstrata, color=sdmstrata, shape=sdmstrata)) + 
	  geom_line(aes(x=year, y = effort.std, group=sdmstrata, color=sdmstrata, linetype=sdmstrata)) + facet_wrap(~subarea) + 
	  labs(x = "Year", y = expression(paste("VMS Effort  ", (h/km^2),sep=""))) + 
	  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
	  scale_color_manual(values=c('blue','red','black')) + 
	  theme(
	    legend.position = c(.125, .99),
	    legend.justification = c("right", "top"),
	    legend.box.just = "right",
	    legend.margin = margin(6, 6, 6, 6)) 
	ggsave(paste0("VMSeffortpersqkm.",(surveyyear),".png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), dpi=300)   
	
	
	
#... Fishing Effort per sq km ... 
	# Subarea A
	ggplot(effort.bins[effort.bins$subarea=="A",], (aes(x=as.factor(sdm), y=effort.std))) + geom_col() + facet_wrap(~year) +
	labs(x ="Habitat suitability", y = expression(paste("Fishing intensity  ", (h/km^2),sep=""))) + 
	  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
	ggsave(paste0("VMSFishingIntensity_Bins_A.",(surveyyear),".png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), dpi=300, width = 12, height = 6, units = c("in"))   
	

	# Subarea B
	ggplot(effort.bins[effort.bins$subarea=="B",], (aes(x=as.factor(sdm), y=effort.std))) + geom_col() + facet_wrap(~year) +
	  labs(x ="Habitat suitability", y = expression(paste("Fishing intensity  ", (h/km^2),sep=""))) + 
	  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
	ggsave(paste0("VMSFishingIntensity_Bins_B.",(surveyyear),".png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), dpi=300, width = 12, height = 6, units = c("in"))  
	
	
	# Subarea C
	ggplot(effort.bins[effort.bins$subarea=="C",], (aes(x=as.factor(sdm), y=effort.std))) + geom_col() + facet_wrap(~year) +
	  labs(x ="Habitat suitability", y = expression(paste("Fishing intensity  ", (h/km^2),sep=""))) + 
	  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
	ggsave(paste0("VMSFishingIntensity_Bins_C.",(surveyyear),".png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"),dpi=300, width = 12, height = 6, units = c("in"))  
	
	
	# Subarea D
	ggplot(effort.bins[effort.bins$subarea=="D",], (aes(x=as.factor(sdm), y=effort.std))) + geom_col() + facet_wrap(~year) +
	  labs(x ="Habitat suitability", y = expression(paste("Fishing intensity  ", (h/km^2),sep=""))) + 
	  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
	ggsave(paste0("VMSFishingIntensity_Bins_D.",(surveyyear),".png"), path=paste0(ess,path.directory,assessmentyear,"/Assessment/Figures/CommercialData/"), dpi=300, width = 12, height = 6, units = c("in"))  
	
	
	
	# EXTRA - just to look #
	# VMS proportion of total catch by subarea  - indication of why the correction of vms using landings is required 
	ggplot(vms.catch) + geom_point(aes(x=year, y = propcatch)) + 
	  geom_line(aes(x=year, y = propcatch)) + facet_wrap(~subarea) + 
	  labs(x="Year", y="VMS proportion of total catch by subarea")
	  
	
	
	