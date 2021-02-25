
###............................###
###    Spatial Figures         ###
###    SFA 29 2015 Assessment  ###
###    J.Sameoto Jan 2016      ###
###............................###

#Note: Using revamped ScallopMap function - by David Keith 

	options(stringsAsFactors = FALSE) 
	
	# Make sure Maps is on your "C:/"
	    ### NOT NECESSARY
	
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
	require(lubridate)
	
	library(RODBC)
	RODBCconn <- odbcConnect("ptran", uid=username, pwd=password)
		
	
		# source R functions
		source("Y:/Offshore scallop/Assessment/Assessment_fns/Maps/ScallopMap.r")
		source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")
		source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")
		source("Y:/Offshore scallop/Assessment/Assessment_fns//archive/2016/gridPlot.r")

#import SFA 29W boundaries for plots
	sfa29.poly <- read.csv("Y:/INSHORE SCALLOP/SFA29/sfa29Poly.csv")
	sfa29strata <- read.csv("Y:/Maps/data/SFA29_BoundariesFollowing12nmdd.csv",header=T)
	attr(sfa29strata,"projection") <- "LL"

	# get the year set up for later....  This should be set as the year of the assessment (e.g. 2017 survey is 2018 assessment)
	yr <- year(Sys.Date())
	yr.crnt <- yr-1
	years <- c(2001:(yr-1))
	
	#Set working directory #
	setwd(paste0("Y:/INSHORE SCALLOP/SFA29/",yr))

	## Import data ##
	## Survey Numbers Data ##
		#sfa29surv.dat <- read.csv("dataoutput/SHFCF.sfa29.2014.csv") #flat file import option
		#sfa29surv.dat <- sfa29surv.dat[,2:dim(sfa29surv.dat)[2]]

	#Cruise List	
	#cruise.list <- c('SFA292001', 'SFA292002','SFA292003','SFA292004','SFA292005','SFA292006',
	#		'SFA292007','SFA292008', 'SFA292009' ,'SFA292010','SFA292011','SFA292012','SFA292013','SFA292014', 'SFA292015',
	#		'SFA292016', 'SFA292017') # Must be UPDATED for Current Year! # 

		## UPDATE::Only updating 2 most recent years (saves loading all data) (use previous year to make sure code is working)
	cruise.list <- c('SFA292018','SFA292019')
	cruise.list <- paste(cruise.list,collapse="','")
	
##################
# Import SHF data:
		
	##.. LIVE ..##
	#Db Query:
	quer1 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.SCLIVERES                      ",                                                                                                                                   
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",								  
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )

		ScallopSurv  <- sqlQuery(RODBCconn, quer1, believeNRows=FALSE)  
	ScallopSurv$year <- as.numeric(substr(ScallopSurv$CRUISE,6,9)) # add year column to SHF data. NOTE:  for SFA29 cruises - would need to be ..,6,9)!!!

	#Once data imported, convert to DD
	ScallopSurv$lat <- convert.dd.dddd(ScallopSurv$START_LAT)
	ScallopSurv$lon <- convert.dd.dddd(ScallopSurv$START_LONG)
		
		names(ScallopSurv)[2] <- c("tow") #change 'TOW_NO' to 'tow'
		ScallopSurv$tot <- rowSums(ScallopSurv[,11:50]) #all scallops 
		ScallopSurv$tot <- ScallopSurv$tot/ 4267.2 # standardize number per tow to numbers per m^2 (800mx5.334m)
		attr(ScallopSurv, "projection") #check default projection of data 
		attr(ScallopSurv, "projection") <- "LL" # assign projection for data 
		ScallopSurv$ID <- paste(ScallopSurv$CRUISE, ScallopSurv$tow, sep=".")
		
	#create pre-rec, rec, comm fields: 
	ScallopSurv$com <- apply(ScallopSurv[,31:50],1,sum) #>=100mm; BINS 100 to 195
	ScallopSurv$rec <- apply(ScallopSurv[,29:30],1,sum) #90-99; BINS 90 to 95
	ScallopSurv$pre <- apply(ScallopSurv[,11:28],1,sum) #0-85 mm; BINS 0 to 85
 
	attr(ScallopSurv,"projection") 	
	
	##.. DEAD ..##
	#Db Query:
	quer2 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.SCDEADRES                      ",                                                                                                                                   
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",								  
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )
		
	ScallopSurv.dead  <- sqlQuery(RODBCconn, quer2, believeNRows=FALSE)  
	ScallopSurv.dead$year <- as.numeric(substr(ScallopSurv.dead$CRUISE,6,9)) # add year column to SHF data. NOTE:  for SFA29 cruises - would need to be ..,6,9)!!!

	#Once data imported, convert to DD
	ScallopSurv.dead$lat <- convert.dd.dddd(ScallopSurv.dead$START_LAT)
	ScallopSurv.dead$lon <- convert.dd.dddd(ScallopSurv.dead$START_LONG)
		
		names(ScallopSurv.dead)[2] <- c("tow") #change 'TOW_NO' to 'tow'
		ScallopSurv.dead$tot <- rowSums(ScallopSurv.dead[,11:50]) #all scallops 
		ScallopSurv.dead$tot <- ScallopSurv.dead$tot/ 4267.2 # standardize number per tow to numbers per m^2 (800mx5.334m)
		attr(ScallopSurv.dead, "projection") #check default projection of data 
		attr(ScallopSurv.dead, "projection") <- "LL" # assign projection for data 
		ScallopSurv.dead$ID <- paste(ScallopSurv.dead$CRUISE, ScallopSurv.dead$tow, sep=".")
		
	#create pre-rec, rec, comm fields: 
	ScallopSurv.dead$com <- apply(ScallopSurv.dead[,31:50],1,sum) #>=100mm; BINS 100 to 195
	ScallopSurv.dead$rec <- apply(ScallopSurv.dead[,29:30],1,sum) #90-99; BINS 90 to 95
	ScallopSurv.dead$pre <- apply(ScallopSurv.dead[,11:28],1,sum) #0-85 mm; BINS 0 to 85

#####################################
#####################################

	#List of tows that have detailed samples
	#UN <- username
	#PW <- password
	#RODBCconn <- odbcConnect("PTRAN", uid=UN, pwd=PW)

	quer3 <- paste(
		"SELECT CRUISE, TOW_NO 			                ",
		"FROM SCALLSUR.scwgthgt s			",
		" where s.strata_id in (41, 42, 43, 44, 45)    ",
		" GROUP BY CRUISE, TOW_NO",
		sep=""
		)
	sampled.dat <- sqlQuery(RODBCconn, quer3, believeNRows=FALSE)

	
	##########
	# Import Biomass per tow data:
	# Flat file: 
	# Current year only
	#ScallopSurv.kg <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/",yr,"/dataoutput/SFA29liveweight",yr.crnt,".csv"))  #DEFINE
	# 2014 to current year
	ScallopSurv.kg <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/",yr,"/dataoutput/SFA29liveweight2014to",yr.crnt,".csv"))
	ScallopSurv.kg <- ScallopSurv.kg[,c(-1,-2)] #removes index column X
	names(ScallopSurv.kg)[52] <- "year" #make match so matches code below and other input scallop survey data files 
	#create pre-rec, rec, comm fields and convert grams to kg : fields are kg per tow
	## MAKE SURE THESE FIELDS MATCH THE DATAFRAME
	ScallopSurv.kg$com.bm <- (apply(ScallopSurv.kg[,32:51],1,sum))/1000 #>=100mm; BINS 100 to 195
	ScallopSurv.kg$rec.bm <- (apply(ScallopSurv.kg[,30:31],1,sum))/1000 #90-99; BINS 90 to 95
	ScallopSurv.kg$pre.bm <- (apply(ScallopSurv.kg[,12:29],1,sum))/1000 #0-89 mm; BINS 0 to 85
	#convert to DD
	ScallopSurv.kg$lat <- convert.dd.dddd(ScallopSurv.kg$START_LAT)
	ScallopSurv.kg$lon <- convert.dd.dddd(ScallopSurv.kg$START_LONG)
	#ScallopSurv.kg$ID <- paste(ScallopSurv.kg$CRUISE, ScallopSurv.kg$TOW_NO, sep=".")
		
	###########
	#Import for Condition plot: 
	#Flat file:
	con.dat <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/",yr,"/dataoutput/ConditionforMap",yr.crnt,".csv")) #DEFINE NOTE: 2014 file made by Jessica (..._JS.csv), will be slightly different than for 2015 assessment since Stephen calculated this using smaller dataset for mtwt sh model 
	con.dat <- con.dat[,c(-1)]
	names(con.dat)[6] <- "year" #Check index on names that you are replacing YEAR with year 
	con.dat$lat <- convert.dd.dddd(con.dat$START_LAT)
	con.dat$lon <- convert.dd.dddd(con.dat$START_LONG)
	con.dat$CRUISE <- paste0("SFA29", con.dat$year)
	con.dat$ID <- paste(con.dat$CRUISE,con.dat$TOW_NO,sep=".") #DEFINE

	#############
	# For Meat Count plot: 
	ScallopSurv.mtcnt <- merge(ScallopSurv.kg[,c('ID','year','lat','lon','com.bm')], ScallopSurv[,c('ID','com')], by=c('ID'))
	ScallopSurv.mtcnt$meat.count<-(0.5/(ScallopSurv.mtcnt$com.bm/ScallopSurv.mtcnt$com))
	ScallopSurv.mtcnt <- ScallopSurv.mtcnt[-which(is.na(ScallopSurv.mtcnt$meat.count)),]

		
##............................##
## SURVEY DISTRIBUTION PLOTS  ##
##............................##
		# SURVEY - Commercial Size >= 100 mm
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			attr(sfa29cont.poly,"projection") <-"LL"
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29comDensity",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (>= 100mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs', boundaries='inshore')
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()
			
			
		# SURVEY - Recruit Size 90-99 mm
			xx <- yr-1
			rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,50,100,200,300,400,500)

			CL <- contourLines(rec.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29recDensity",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (90-99 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()


		# SURVEY - Prerecruit Size < 90 mm
			xx <- yr-1
			pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,50,100,200,300,400,500)

			CL <- contourLines(pre.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29preDensity",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (< 90 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')	
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

### EN FRANCAIS !!!
			# SURVEY - Commercial Size >= 100 mm
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
			
			lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
			
			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			attr(sfa29cont.poly,"projection") <-"LL"
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)
			
			png(paste0("figures/sfa29comDensity",xx,'_FR',".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (>= 100mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs', boundaries='inshore')
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Nombre par trait",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()
			
			
			# SURVEY - Recruit Size 90-99 mm
			xx <- yr-1
			rec.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
			
			lvls=c(1,5,10,50,100,200,300,400,500)
			
			CL <- contourLines(rec.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)
			
			png(paste0("figures/sfa29recDensity",xx,"_FR.png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (90-99 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Nombre par trait",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()
			
			
			# SURVEY - Prerecruit Size < 90 mm
			xx <- yr-1
			pre.contours<-contour.gen(subset(ScallopSurv,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
			
			lvls=c(1,5,10,50,100,200,300,400,500)
			
			CL <- contourLines(pre.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)
			
			png(paste0("figures/sfa29preDensity",xx,"_FR.png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (< 90 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')	
			points(lat~lon,ScallopSurv,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Nombre par trait",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

		# SURVEY BIOMASS - Commercial  Size >= 100 mm
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','com.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29comBiomass",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Biomass (>= 100mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')	
			points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

		# SURVEY BIOMASS - Recruit Size 90-99 mm
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','rec.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29recBiomass",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Biomass (90-99 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')	
			points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

		# SURVEY BIOMASS - Prerecruit Size < 90 mm
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv.kg,year==xx,c('ID','lon','lat','pre.bm')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(0.01,0.05,0.1,1,2,3,4,5) #levels to be color coded

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29preBiomass",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Biomass (< 90 mm)'),plot.boundries = F, plot.bathy=T, bathy.source='usgs')	
			points(lat~lon,ScallopSurv.kg,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()




		# SURVEY CONDITION (g of 100mm shell height scallop) 
			xx <- yr-1
			#subset survey data to be just those tows where detailed sampling was conducted - note from dim(con.dat) this is already done.
			com.contours<-contour.gen(subset(con.dat,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
			#lvls=c(4,6,8,10,12,14,16) # large scale
			#lvls=c(6,7,8,9,10,11,12,13) # good condition scale # update based on values observed

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnBu

			png(paste0("figures/sfa29comCF",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Condition'),plot.boundries = F,plot.bathy=T, bathy.source='quick')
			points(lat~lon,con.dat,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Condition (g)",inset=0.02,bty='o',box.col='white', cex=1,  bg='white')
			dev.off()



		# SURVEY MEAT COUNT - of commerical (>= 100mm) sized animals
			# SURVEY MEAT COUNT - of commerical (>= 80mm) sized animals
			xx <- yr-1
	
			mc.contours<-contour.gen(na.omit(subset(ScallopSurv.mtcnt,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

			lvls=seq(10,45,5)
			div=2

			CL <- contourLines(mc.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			Ncol=length(lvls)+div
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

			png(paste0("figures/sfa29comMeatCnt",xx,".png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('Meat Count (>= 100mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv.mtcnt,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

			
			
			
###...................###
### Plot CPUE in grid ###
###...................###

	## Import and prepare Log Data ##
	log.2001to2011 <- read.csv("logs/logData_2002to2011_sfa29.csv")
	log.2012 <- read.csv("logs/logsSFA29_2012corrected.csv")
	log.2013 <- read.csv("logs/dlogSFA29_2013.csv")
	log.2014 <- read.csv("logs/dlogsSFA29_2014.csv") #Fr db
	log.2015 <- read.csv("logs/dlogsSFA29_2015.csv") #Fr db
	log.2016 <- read.csv ("logs/SFA29logs2016.csv")
	# This includes the late season (October) FSC catch
	log.2017 <- read.csv ("logs/SFA29logs2017_dwnld_Feb2018_JS.csv")
	log.2018 <- read.csv ("logs/SFA29logs_2018.csv")
	log.2019 <- read.csv ("logs/SFA29logs_2019.csv")
	
	# add YEAR and AREA columns for data years selected directly from the SCALLOP database
	#is using log data directly from SCALLOP database it's assumed that assigned_ared has been QA/QC'd and this is used as the AREA field
	log.2018$YEAR <- as.numeric(substr(log.2018$DATE_FISHED,1,4)) #assumed data structure of field DATE_FISHED is character and in format 'YYYY-XX-XX' or "YYYY/XX/XX'
	log.2019$YEAR <- as.numeric(substr(log.2019$DATE_FISHED,1,4)) 
	
	log.2018$AREA <- log.2018$ASSIGNED_AREA
	log.2019$AREA <- log.2019$ASSIGNED_AREA
	
	log.2018$DDSlat<-convert.dd.dddd(log.2018$LATITUDE)
	log.2018$DDSlon<--convert.dd.dddd(log.2018$LONGITUDE)
	log.2018$ID<-1:nrow(log.2018)
	
	log.2019$DDSlat<-convert.dd.dddd(log.2019$LATITUDE)
	log.2019$DDSlon<--convert.dd.dddd(log.2019$LONGITUDE)
	log.2019$ID<-1:nrow(log.2019)

	# Check for privacy considerations (min 5 trips per area to include in presentation)
	require (plyr)
	trips.pa <- ddply (log.2019,. (AREA), summarize, 
	                   n.trips = length (unique(TRIP_ID)))
	# Remove observations in E in 2019
	
	# if merging multiple years of  data, need to subset columns from those datasets selected directly from SCALLOP database
	#log.2014 <- log.2014[,c(1:4,6:27,29,39:40)]
	#log.2015 <- log.2015[,c(1:4,6:27,29,39:40)]
	
	# merge data if required
	#sfa29Logs <- rbind(log.2001to2011, log.2012, log.2013, log.2014, log.2015)
	#sfa29Logs$DDSlat<-convert.dd.dddd(sfa29Logs$LATITUDE)
	#sfa29Logs$DDSlon<--convert.dd.dddd(sfa29Logs$LONGITUDE)
	#sfa29Logs$ID<-1:nrow(sfa29Logs)


	## assign year, select data, plot ##
	xx <- 2019
	# For 2015: was subarea D closure line: 
	#Dclosure <- read.csv('Y:/INSHORE SCALLOP/SFA29/2016/Fishery/2015SubareaDClosureLine/SubareaDClosureLine2015.csv')
	#attr(Dclosure,"projection") <- "LL"
	
	## Remove B fishing from plot in 2018 due to privacy considerations
	log.2018_noB <- log.2018 [!log.2018$ASSIGNED_AREA == "29B",]
	gridlog<-subset(log.2018_noB, YEAR==xx, c('ID','DDSlon','DDSlat','CPUE_KG'))

	## Remove E fishing from plot in 2019 due to privacy considerations
	log.2019_PA <- log.2019 [!log.2019$ASSIGNED_AREA == "29E",]
	gridlog<-subset(log.2019_PA, YEAR==xx, c('ID','DDSlon','DDSlat','CPUE_KG'))

	
	lvls=seq(5,40,5)  #CPUE levels from 5 to 40 kg/h
	lvls=seq(5,110,15)  #CPUE levels from 5 to 110 kg/h
	
	cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGnBu"),border=NA,stringsAsFactors=FALSE)
windows()
	#png("figures/SFA29_CPUEgridplot2019.png",9,9,res=200,units='in') #NOTE: Not printing correctly to png, save from window
	ScallopMap('sfa29',bathcol=rgb(0,0,1,0.3),poly.lst=gridPlot(gridlog,sfa29.poly,lvls),title=paste(xx), plot.boundries = F,plot.bathy=T, bathy.source='usgs')
	legend("topright", c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="kg/h",inset=0.01,bty='n',box.col='white', cex=0.9)
	addPolys(sfa29strata, border="black")
	text (-66.42, 43.62, "A", cex=2)   #labels for management subareas, position may need to be adjusted, or text colour can be faded
	text (-66.2, 43.58, "B", cex=2)
	text (-65.95, 43.28, "C", cex=2)
	text (-65.6, 43.31, "D", cex=2)
	text (-66.26, 43.35, "E", cex=2)
	#dev.off()
	
	
## Extra plots	(NOT REQUIRED)
	### Make histogram by area for distribution of cpue in 2017
	library (ggplot2)
	histolog<-subset(log.2018, YEAR==xx, c('AREA','CPUE_KG'))
	histolog <- subset (histolog, !AREA == '29B')
	
	cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) + 
	  geom_histogram (breaks = seq (0, 300, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
	  facet_wrap (~AREA, ncol = 1) +
	  scale_x_continuous(breaks = seq (0, 300, by = 50)) +
	  theme_bw() +
	  theme(panel.grid=element_blank()) 
	cpuehisto

## Plot catch rate by date for all areas 
	depdat <- subset (log.2018, YEAR==xx, c('AREA','CPUE_KG', 'DATE_FISHED'))
	depdat <- subset (depdat, !AREA == '29B')
	depdat$DATE_FISHED <- as.Date(depdat$DATE_FISHED)

		depplot <- ggplot (depdat, aes (y = CPUE_KG, x = DATE_FISHED)) +
	  geom_point () + labs (y = 'CPUE (kg/h)', x = 'Date') +
	  scale_x_date(date_breaks = "2 week", date_labels =  "%b-%d") + 
	  facet_wrap (~AREA, ncol = 2, scales = 'free') +
	  theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	         panel.background = element_blank(), axis.line = element_line(colour = "black"))
	depplot
	
## Range of values by area
	library (plyr)
	cpuevals <- ddply (log.2018,. (AREA),
	                   summarize,
	                   min = min (CPUE_KG), max = max (CPUE_KG), mean = mean (CPUE_KG), median = median (CPUE_KG))
	
## MARCH 2015 
### For NEW CF method: 


#		# SURVEY CONDITION FACTOR(g/dm3)
#			xx <- 2014
#			#subset survey data to be just those tows where detailed sampling was conducted
#			sfa29surv.detailed <- sfa29surv.dat
#			com.contours<-contour.gen(subset(sfa29surv.detailed,year==xx,c('ID','lon','lat','Condition')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
#
#			lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
#
#			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
#			CP <- convCP(CL)
#			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA, stringsAsFactors=FALSE)  #previously was YlOrBr YlGnB
#			png("figures/sfa29comCF2014.png",9,9,res=200,units='in')
#			ScallopMap('sfa29',plot.lines=F,bathcol=rgb(0,0,1,0.3),bathy.source='USGS',contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Condition'),cex=1.2)
#			points(lat~lon,sfa29surv.detailed,subset=year==xx,pch=16,cex=0.5)
#			addLines(sfa29strata)
#			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title=expression("" * g/dm^3*""),inset=0.02,bty='n',box.col='white', cex=1)
#			dev.off()
#
#		# SURVEY MEAT COUNT - of commerical (>= 100mm) sized animals
#			#sfa29.poly <- read.csv("Y:/INSHORE SCALLOP/SFA29/2015/SFA29_Boundaries_NoE.csv")
#			xx <- 2014
#			sfa29surv.dat$meat.count<-0.5/(sfa29surv.dat$Commercial_kg/sfa29surv.dat$com)
#			sfa29surv.mc <- sfa29surv.dat[-which(is.na(sfa29surv.dat$meat.count)),]
#			mc.contours<-contour.gen(na.omit(subset(sfa29surv.mc,year==xx,c('ID','lon','lat','meat.count'))),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=F,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)
#
#			lvls=seq(10,45,5)
#			div=2
#
#			CL <- contourLines(mc.contours$image.dat,levels=lvls)
#			CP <- convCP(CL)
#			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
#			Ncol=length(lvls)+div
#			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)
#
#			png("figures/sfa29comMeatCnt2014.png",9,9,res=200,units='in')
#			ScallopMap('sfa29',plot.lines=F,bathcol=rgb(0,0,1,0.3),bathy.source='USGS',contour=list(sfa29cont.poly,cont.data),title="",cex=1.2)
#			points(lat~lon,sfa29surv.mc,subset=year==xx,pch=16,cex=0.5)
#			addLines(sfa29strata)
#			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Scallops/500g",inset=0.02,bty='n',box.col='white', cex=1)
#			dev.off()
#
			
			
			
			
# Spatial plot of clappers #
# SURVEY - Commercial Size >= 100 mm
				
			xx <- yr-1
			com.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

			CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29comDensity",xx,"_clappers.png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (>= 100mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()

			#Exploratory: summary(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','com')))
			
		# SURVEY - Recruit Size 90-99 mm
			xx <- yr-1
			rec.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

			CL <- contourLines(rec.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29recDensity",xx,"_clappers.png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (90-99mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()


		# SURVEY - Prerecruit Size < 90 mm
			xx <- yr-1
			pre.contours<-contour.gen(subset(ScallopSurv.dead,year==xx,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

			lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

			CL <- contourLines(pre.contours$image.dat,levels=lvls)
			CP <- convCP(CL)
			sfa29cont.poly <- joinPolys(CP$PolySet,sfa29.poly)
			cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)

			png(paste0("figures/sfa29preDensity",xx,"_clappers.png"),9,9,res=200,units='in')
			ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(sfa29cont.poly,cont.data),title=paste('SFA 29 Density (< 90 mm)'),plot.boundries = F,plot.bathy=T, bathy.source='usgs')
			points(lat~lon,ScallopSurv.dead,subset=year==xx,pch=16,cex=0.5)
			addLines(sfa29strata)
			legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="#/tow",inset=0.02,bty='n',box.col='white', cex=1)
			dev.off()


			
			

##............................##
## CLAPPERS AS A PROPORTION OF LIVE SCALLOPS  ##
##............................##
		
		
		live <- ScallopSurv[,c(1,2,3,8,51:58)]#!check indexing!
		dead <- ScallopSurv.dead[,c(1,2,3,8, 51:58)] #!check indexing!
		
		prop.clappers <- merge(live,dead,by="ID")
		prop.clappers$prop.dead.com <- prop.clappers$com.y/(prop.clappers$com.x+prop.clappers$com.y) #number of commercial sized clappers relative to total number of commercial size live and dead combined
		prop.clappers$prop.dead.rec <- prop.clappers$rec.y/(prop.clappers$rec.x+prop.clappers$rec.y) #number of recruit sized clappers relative to total number of recruit size live and dead combined
		prop.clappers$prop.dead.com[is.na(prop.clappers$prop.dead.com)] <- 0
		prop.clappers$prop.dead.rec[is.na(prop.clappers$prop.dead.rec)]  <- 0
		prop.clappers <- prop.clappers[,c(1:3, 5:8,24:25)]
		names(prop.clappers) <- c("ID", "CRUISE", "tow", "strata_id", "year", "lat", "lon", "prop.dead.com","prop.dead.rec")
		
		write.csv(prop.clappers,"dataoutput/SFA29_PropClappersbyTow.csv")
		
		# Spatial plot of PROPORTION of clappers #
		# SURVEY - Commercial Size >= 100 mm
		xx <- yr-1
		com.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
		
		lvls=c(0.01,0.10,0.20,0.30,0.5,0.6) #levels to be color coded
		
		CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
		CP <- convCP(CL)
		totCont.poly <- CP$PolySet
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)
		
		png(paste0("figures/sfa29comDensity",xx,"_clappersProportion.png"),9,9,res=200,units='in')
		ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste('Clapper Proportion (>= 100mm)'),plot.boundries = T,plot.bathy=T, bathy.source='usgs')
		points(lat~lon,prop.clappers,subset=year==xx,pch=16,cex=0.45)
		addLines(sfa29strata)
		legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
		dev.off()
		
		
		# SURVEY - Recruit Size 90-99 mm
		xx <- yr-1
		rec.contours<-contour.gen(subset(prop.clappers,year==xx,c('ID','lon','lat','prop.dead.rec')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
		
		lvls=c(0.01,0.10,0.20,0.30,0.5,0.6) #levels to be color coded
		
		CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
		CP <- convCP(CL)
		totCont.poly <- CP$PolySet
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA, stringsAsFactors=FALSE)
		
		png(paste0("figures/sfa29recDensity",xx,"_clappersProportion.png"),9,9,res=200,units='in')
		ScallopMap('sfa29',bathcol=rgb(0,0,1,0.1),contour=list(totCont.poly,cont.data),title=paste('Clapper Proportion (90-99mm)'),plot.boundries = T,plot.bathy=T, bathy.source='usgs')
		points(lat~lon,prop.clappers,subset=year==xx,pch=16,cex=0.45)
		addLines(sfa29strata)
		legend("topright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,title="Proportion",inset=0.02,bty='o',box.col='white', cex=1, bg='white')
		dev.off()
		
		
		
		