### check.tows.spatial.r
#### This function can be used to check for keypunching errors in spatial (coordinates) and strata columns for the Inshore survey tow files.
##### It checks the CONVERTED cruise tow file only (e.g. CRUISEtow_CONVERTED.csv, not any other files). 
##### It will tell you which tows are too short (<200m), too long (>2 km), or are in the wrong strata. It can also plot the tows on the strata polygons so you can examine the tracks visually. 
##### Once you find and fix errors in CRUISEtow_CONVERTED.csv using this function, you must copy/paste the corrections into any other associated files. 
##### Beware of stratas that encompass one another/overlap. This function is not capable of identifying which strata a tow belongs in when the strata polyons overlap. 
##### For example, off Grand Manan, strata 31 encompasses 32 entirely. The function will identify the tows that are within both strata, but will not check for a keypunching error for these tows.
##### You can either pull tow data from the Y drive, or work from a temporary desktop directory (if you save the CRUISEtow.csv file on your desktop). 
##### Default is to pull from Y: so be careful!!
##### This function DOES NOT: 
############# automatically correct tow files (must do manually in excel),
############# or run in-depth geoprocessing to calculate exact lengths (only used to identify tiny or extremely long tows),
############# or explicitly identify tows that cross a strata line (must do this by plotting and looking at them yourself). 

check.tows.spatial <- function(cruise="BF2017", direct="Y:/Inshore/Survey/", previouscruisefolder="SPA145", previouscruisename="BF2016", desktop="NULL", year="2017", plot=TRUE, df=TRUE) {
  
  # Clean up any open plot devices.
  if(!is.null(dev.list())) dev.off()
  require(dplyr)
  require(plyr)
  require(reshape2)
  require(rgeos)
  require(ggplot2)
  require(geosphere)
  require(sp)
  require(readr)
  
  # this gets the convert.dd.dddd.r functions from Github (previously #source("Y:/Inshore/BoF/Assessment_fns/convert.dd.dddd.r"))
  funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  # Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
  dir <- getwd()
  for(fun in funcs) 
  {
    temp <- dir
    download.file(fun,destfile = basename(fun))
    source(paste0(dir,"/",basename(fun)))
    file.remove(paste0(dir,"/",basename(fun)))
  }
  
  #####
  
  # grab the strata polygons file. this was downloaded from database on August 29, 2017 and saved to Y:
  strata <- read.csv(paste0(direct, year, "/data entry templates and examples/entry check functions/SCSTRATAINFO_August2017.csv"))
  # this creates strata labels
  strata_lab <- ddply(.data=strata, .(STRATA_ID, DESCRIPTION, AREA),
                      summarize,
                      LONGITUDE = mean(LONGITUDE), 
                      LATITUDE = mean(LATITUDE))
  
  # grab the Authoritative SPA polygons file. These polygons were created by Leslie Nasmith in 2014 and is saved on the Y drive as a CSV in August 2017.
  area <- read.csv(paste0(direct, year, "/data entry templates and examples/entry check functions/BayofFundyFishingBoundaries_WGS84.csv"))
  area$AREA_ID <- parse_number(area$Area)
  # this creates strata labels
  area_lab <- ddply(.data=area, .(AREA_ID, Area),
                    summarize,
                    LONGITUDE = mean(Longitude), 
                    LATITUDE = mean(Latitude))
  
  
  # this reads the tow file from the desktop if desktop path is specified, or from Y: if no desktop path provided
  if(desktop=="NULL" & !df=="TRUE"){
    bftows <- read.csv(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "tow_CONVERTED.csv"))}
  if(!desktop=="NULL" & !df=="TRUE")
    bftows <- read.csv(paste0(desktop, cruise, "tow_CONVERTED.csv"))
  
  # this reads the tow file from the desktop if desktop path is specified, or from Y: if no desktop path provided
  if(desktop=="NULL" & df=="TRUE"){
    bftows <- read.csv(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "tow_CONVERTED.csv"))
    assign(paste0("tows_", cruise), bftows, env=.GlobalEnv)}
  if(!desktop=="NULL" & df=="TRUE"){
    bftows <- read.csv(paste0(desktop, cruise, "_tow.csv"))
    assign(paste0("tows_", cruise), bftows, env=.GlobalEnv)}
  
  
  # converting all lats and longs to decimal degrees
  bftows$Start_lat <- convert.dd.dddd(format="dec.deg", x=bftows$Start_lat)
  bftows$Start_long <- convert.dd.dddd(format="dec.deg", x=bftows$Start_long)
  bftows$End_lat <- convert.dd.dddd(format="dec.deg", x=bftows$End_lat)
  bftows$End_long <- convert.dd.dddd(format="dec.deg", x=bftows$End_long)
  bftows$Start_long <- -bftows$Start_long
  bftows$End_long <- -bftows$End_long
  
  
  # calculating the distance of each tow
  bftows$dist.calc <- geosphere::distGeo(matrix(c(bftows$Start_long, bftows$Start_lat), ncol=2), matrix(c(bftows$End_long, bftows$End_lat), ncol=2))
  
  # flag the tow if the distance is greater than 2 km
  bftows$diff <- bftows$dist.calc - bftows$Tow_len
  bftows$flag <- ifelse(bftows$diff > 100, "check", 
                        ifelse(bftows$diff < -100, "check", "ok"))
  
  # make sure we're still sorted by Tow number
  bftows <- dplyr::arrange(bftows, Oracle.tow..)
  
  # data tidying for ggplotting
  towpath_lat <- dplyr::select(bftows, Oracle.tow.., Tow_type_id, Tow_len, Strata_id, Start_lat, End_lat, Tow_dir, dist.calc, flag)
  towpath_lat <- reshape2::melt(towpath_lat, id.vars=c("Oracle.tow..", "Tow_type_id", "Tow_len", "Strata_id", "Tow_dir", "dist.calc", "flag"))
  names(towpath_lat)[8:9] <- c("lats", "LATITUDE")
  towpath_lon <- dplyr::select(bftows, Oracle.tow.., Tow_type_id, Tow_len, Strata_id, Start_long, End_long, Tow_dir, dist.calc, flag)
  towpath_lon <- reshape2::melt(towpath_lon, id.vars=c("Oracle.tow..", "Tow_type_id", "Tow_len", "Strata_id", "Tow_dir", "dist.calc", "flag"))
  names(towpath_lon)[8:9] <- c("lons", "LONGITUDE")
  towpath <- join(towpath_lat, towpath_lon, type="full")
  towpath_start <- subset(towpath, lats=="Start_lat" & lons=="Start_long")
  towpath_end <- subset(towpath, lats=="End_lat" & lons=="End_long")
  towpath <- rbind(towpath_start, towpath_end)
  
  # To check if tow tracks cross a strata line, you have to plot them and take a look. 
  # This takes a little bit to run (50 strata to plot = about 30 seconds?) so be sure to set plot = FALSE if you want to skip this step. 
  # if plotting is required:
  if(plot=="TRUE"){
    # if working off of directory:
    if(desktop=="NULL"){
      plot.list <- NULL
      for(i in unique(strata$STRATA_ID)){
        p <- ggplot() + geom_polygon(data=strata, aes(LONGITUDE, LATITUDE, group=STRATA_ID), fill=NA, colour="black") +
          geom_text(data=strata_lab, aes(LONGITUDE, LATITUDE, label=STRATA_ID), size=3, colour="blue") +
          coord_map() + 
          theme_bw() + theme(panel.grid=element_blank()) +
          geom_path(data=towpath, aes(LONGITUDE, LATITUDE, group=Oracle.tow.., colour=flag)) +
          geom_text(data=bftows, aes(Start_long, Start_lat, label=Oracle.tow..), size=3) +
          geom_text(data=bftows, aes(End_long, End_lat, label=Strata_id), size=2) +
          xlim(min(strata[strata$STRATA_ID %in% i,]$LONGITUDE)-0.05, max(strata[strata$STRATA_ID %in% i,]$LONGITUDE)+0.05) +
          ylim(min(strata[strata$STRATA_ID %in% i,]$LATITUDE)-0.05, max(strata[strata$STRATA_ID %in% i,]$LATITUDE)+0.05)
        plot.list[[i]] <- p
      }
      
      pdf(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "_strata_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
    
    # if working off desktop:
    if(!desktop=="NULL"){
      plot.list <- NULL
      for(i in unique(strata$STRATA_ID)){
        p <- ggplot() + geom_polygon(data=strata, aes(LONGITUDE, LATITUDE, group=STRATA_ID), fill=NA, colour="black") +
          geom_text(data=strata_lab, aes(LONGITUDE, LATITUDE, label=STRATA_ID), size=3, colour="blue") +
          coord_map() + 
          theme_bw() + theme(panel.grid=element_blank()) +
          geom_path(data=towpath, aes(LONGITUDE, LATITUDE, group=Oracle.tow.., colour=flag)) +
          geom_text(data=bftows, aes(Start_long, Start_lat, label=Oracle.tow..), size=3) +
          geom_text(data=bftows, aes(End_long, End_lat, label=Strata_id), size=2) +
          xlim(min(strata[strata$STRATA_ID %in% i,]$LONGITUDE)-0.05, max(strata[strata$STRATA_ID %in% i,]$LONGITUDE)+0.05) +
          ylim(min(strata[strata$STRATA_ID %in% i,]$LATITUDE)-0.05, max(strata[strata$STRATA_ID %in% i,]$LATITUDE)+0.05)
        plot.list[[i]] <- p
      }
      
      pdf(paste0(desktop, cruise, "_strata_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
  }
  
  # this prints the dataframe of tows that need checking based on the calculated length being too long.
  print("FLAGGED TOWS")
  print(subset(bftows, flag=="check"))
  flagged.tows <- subset(bftows, flag=="check")
  
  if(df=="TRUE"){
    assign(paste0("flagged.tows_", cruise), flagged.tows, envir = .GlobalEnv)
  }
  
  if(desktop=="NULL"){
    write.csv(flagged.tows, paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "_flagged_tows.csv"))
  }
  if(!desktop=="NULL"){
    write.csv(flagged.tows, paste0(desktop, cruise, "_flagged_tows.csv"))
  }
  
  # this checks to make sure tows were assigned to the right strata. Compares the strata ID in the CRUISEtow.csv file to the strata ID where it is located
  strata.test <- NULL
  for(i in unique(strata$STRATA_ID)){
    points <- SpatialPoints(matrix(c(bftows$Start_long[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))], 
                                     bftows$Start_lat[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))]), ncol=2), 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    coord_list <- split(strata[strata$STRATA_ID %in% i, c(1,3,4)], strata[strata$STRATA_ID %in% i,]$STRATA_ID)
    coord_list <- lapply(coord_list, function(x) { x["STRATA_ID"] <- NULL; x })
    ps <- sapply(coord_list, Polygon)
    p1 <- Polygons(ps, ID = 1) 
    
    # create SpatialPolygons object
    my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
    
    test <- data.frame(Oracle.tow.. = bftows$Oracle.tow..[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))], gContains(my_spatial_polys, points, byid=TRUE), strata=i)
    strata.true <- subset(test, X1=="TRUE", select=c("Oracle.tow..", "strata"))
    strata.test <- rbind(strata.test, strata.true)
  }
  strata.test <- join(bftows, strata.test, type="full")
  strata.test$same <- strata.test$strata==strata.test$Strata_id
  
  # this deals with the overlapping/encompassing strata issue but showing you which ones are inside multiple strata. Be careful with these!
  double.strata <- strata.test[strata.test$Oracle.tow.. %in% unique(strata.test$Oracle.tow..[duplicated(strata.test$Oracle.tow..)]),]
  assign(paste0("double.strata_", cruise), double.strata, envir = .GlobalEnv)
  print("Watch out for these tows because stratas overlap:")
  print(double.strata)
  
  strata.test <- subset(strata.test, same==FALSE)
  strata.test <- subset(strata.test, !unique(double.strata$Oracle.tow..))
  assign(paste0("strata.test_", cruise), strata.test, envir = .GlobalEnv)
  
  # prints the tows where the strata is mis-labelled
  print("These tows have mislabelled stratas")
  print(strata.test)
  
  # To check tow tracks against Management Area (SPA) line, plot them and take a look. 
  # Set plot = FALSE if you want to skip this step. 
  # if plotting is required:
  if(plot=="TRUE"){
    # if working off of directory:
    if(desktop=="NULL"){
      plot.list <- NULL
      for(i in unique(area$AREA_ID)){
        p <- ggplot() + geom_polygon(data=area, aes(Longitude, Latitude, group=AREA_ID, fill=Area), colour="black") +
          geom_text(data=area_lab, aes(LONGITUDE, LATITUDE, label=Area), size=4, colour="blue") +
          coord_map() + 
          theme_bw() + theme(panel.grid=element_blank()) +
          geom_path(data=towpath, aes(LONGITUDE, LATITUDE, group=Oracle.tow.., colour=flag)) +
          scale_colour_manual(values=c("black", "white")) +
          geom_text(data=bftows, aes(Start_long, Start_lat, label=Oracle.tow..), size=3) +
          geom_text(data=bftows, aes(End_long, End_lat, label=Strata_id), size=2) +
          xlim(min(area[area$AREA_ID %in% i,]$Longitude)-0.05, max(area[area$AREA_ID %in% i,]$Longitude)+0.05) +
          ylim(min(area[area$AREA_ID %in% i,]$Latitude)-0.05, max(area[area$AREA_ID %in% i,]$Latitude)+0.05)
        plot.list[[i]] <- p
      }
      
      pdf(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "_SPA_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
    
    # if working off desktop:
    if(!desktop=="NULL"){
      plot.list <- NULL
      for(i in unique(area$AREA_ID)){
        p <- ggplot() + geom_polygon(data=area, aes(Longitude, Latitude, group=AREA_ID, fill=Area), colour="black") +
          geom_text(data=area_lab, aes(LONGITUDE, LATITUDE, label=Area), size=4, colour="blue") +
          coord_map() + 
          theme_bw() + theme(panel.grid=element_blank()) +
          geom_path(data=towpath, aes(LONGITUDE, LATITUDE, group=Oracle.tow.., colour=flag)) +
          scale_colour_manual(values=c("black", "white")) +
          geom_text(data=bftows, aes(Start_long, Start_lat, label=Oracle.tow..), size=3) +
          geom_text(data=bftows, aes(End_long, End_lat, label=Strata_id), size=2) #+
        #xlim(min(area[area$AREA_ID %in% i,]$Longitude)-0.05, max(area[area$AREA_ID %in% i,]$Longitude)+0.05) +
        #ylim(min(area[area$AREA_ID %in% i,]$Latitude)-0.05, max(area[area$AREA_ID %in% i,]$Latitude)+0.05)
        plot.list[[i]] <- p
      }
      
      pdf(paste0(desktop, cruise, "_SPA_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
  }
  
  # this tells you which SPA each tow is in based on Leslie's Authoritative SPA polygons in WGS84
  
  # based on start location first, then by end location. compare them to determine if they cross an area line.
  area.test <- NULL
  for(i in unique(area$AREA_ID)){
    
    points <- SpatialPoints(matrix(c(bftows$Start_long[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))], 
                                     bftows$Start_lat[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))]), ncol=2), 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    coord_list <- split(area[area$AREA_ID %in% i, c(3,2,5)], area[area$AREA_ID %in% i,]$AREA_ID)
    coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
    ps <- sapply(coord_list, Polygon)
    p1 <- Polygons(ps, ID = 1) 
    
    # create SpatialPolygons object
    my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
    
    test <- data.frame(Oracle.tow.. = bftows$Oracle.tow..[!(is.na(bftows$Start_long) & is.na(bftows$Start_lat))], gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
    area.true <- subset(test, X1=="TRUE", select=c("Oracle.tow..", "AREA_ID"))
    area.test <- rbind(area.test, area.true)
  }
  area.test <- join(bftows, area.test, type="full")
  area.test <- join(area.test, unique(area[,4:5]), type="left")
  area.test$SPA <- gsub(area.test$Area, pattern="SPA", replacement="")
  area.test$SPA <- gsub(area.test$SPA, pattern="A", replacement="")
  area.test$SPA <- gsub(area.test$SPA, pattern="B", replacement="")
  
  # by end location
  area.test.end <- NULL
  for(i in unique(area$AREA_ID)){
    
    points <- SpatialPoints(matrix(c(bftows$End_long[!(is.na(bftows$End_long) & is.na(bftows$End_lat))], 
                                     bftows$End_lat[!(is.na(bftows$End_long) & is.na(bftows$End_lat))]), ncol=2), 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    coord_list <- split(area[area$AREA_ID %in% i, c(3,2,5)], area[area$AREA_ID %in% i,]$AREA_ID)
    coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
    ps <- sapply(coord_list, Polygon)
    p1 <- Polygons(ps, ID = 1) 
    
    # create SpatialPolygons object
    my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
    
    test <- data.frame(Oracle.tow.. = bftows$Oracle.tow..[!(is.na(bftows$End_long) & is.na(bftows$End_lat))], gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
    area.true.end <- subset(test, X1=="TRUE", select=c("Oracle.tow..", "AREA_ID"))
    area.test.end <- rbind(area.test.end, area.true.end)
  }
  area.test.end <- join(bftows, area.test.end, type="full")
  area.test.end <- join(area.test.end, unique(area[,4:5]), type="left")
  area.test.end$SPA <- gsub(area.test.end$Area, pattern="SPA", replacement="")
  area.test.end$SPA <- gsub(area.test.end$SPA, pattern="A", replacement="")
  area.test.end$SPA <- gsub(area.test.end$SPA, pattern="B", replacement="")
  
  colnames(area.test)[which(names(area.test) == "SPA")] <- "SPA.start"
  colnames(area.test.end)[which(names(area.test.end) == "SPA")] <- "SPA.end"
  
  area.test.both <- join(area.test, area.test.end, type="left")
  
  area.test.both$test <- area.test.both$SPA.start==area.test.both$SPA.end
  
  print("these ones cross a management area line")
  area.test.cross <- subset(area.test.both, test=="FALSE")
  print(area.test.cross)
  
  if(df=="TRUE"){
    assign(paste0("area.test_", cruise), area.test, env=.GlobalEnv)
    assign(paste0("area.test.end_", cruise), area.test.end, env=.GlobalEnv)
    assign(paste0("area.test.cross_", cruise), area.test.cross, env=.GlobalEnv)
  }
  
  ## based only on starting location
  print("area.test")
  print(area.test)
  
  if(substr(cruise, 1, 2) == "GM" | 
     unique(as.character(area.test$Area)) %in% c("SPA6A") |
     unique(as.character(area.test$Area)) %in% c("SPA6B") |
     unique(as.character(area.test$Area)) %in% c("SPA6C") |
     unique(as.character(area.test$Area)) %in% c("SPA6D")){
    
    ## if in area 6 (grand manan) you need to change the strata to VMS strata
    inVMS<-read.csv("Y:/INSHORE SCALLOP/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
    outVMS<-read.csv("Y:/INSHORE SCALLOP/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")
    otherVMS <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84.csv")
    
    if(plot=="TRUE") {
      p <- ggplot() + 
        geom_polygon(data=otherVMS, aes(X, Y, group=PID), fill="yellow", alpha=0.5)+
        geom_polygon(data=inVMS, aes(X, Y, group=SID), fill="red",alpha=0.5) +
        geom_polygon(data=outVMS, aes(X, Y, group=SID), fill="blue", alpha=0.5) + 
        geom_path(data=towpath, aes(LONGITUDE, LATITUDE, group=Oracle.tow..)) +
        theme_bw() + theme(panel.grid=element_blank()) +
        coord_map() +
        xlim(-67.2, -66.4) +
        ylim(44.4, 45.1)
      pdf(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "_VMS_check.pdf"),onefile=T,width=22,height=12)
      print(p)
      dev.off()
    } 
    
    inVMS$polyID <- "in"
    outVMS$polyID <- "out"
    
    VMS <- rbind(inVMS, outVMS)
    VMS$ID <- paste0(as.numeric(as.factor(VMS$polyID)), ".", VMS$SID)
    
    VMS.test <- NULL
    for(i in unique(VMS$ID)){
      
      points <- SpatialPoints(matrix(c(bftows$Start_long, bftows$Start_lat), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      coord_list <- split(VMS[VMS$ID %in% i, c(4,5,7)], VMS[VMS$ID %in% i,]$ID)
      coord_list <- lapply(coord_list, function(x) { x["ID"] <- NULL; x })
      ps <- sapply(coord_list, Polygon)
      p1 <- Polygons(ps, ID = 1) 
      
      # create SpatialPolygons object
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
      
      test <- data.frame(Oracle.tow.. = bftows$Oracle.tow.., gContains(my_spatial_polys, points, byid=TRUE), ID=i)
      VMS.true <- subset(test, X1=="TRUE", select=c("Oracle.tow..", "ID"))
      VMS.test <- rbind(VMS.test, VMS.true)
    }
    
    names(VMS.test)[2] <- "VMSid"
    VMS.test <- ddply(.data=VMS.test, .(Oracle.tow..),
                      summarize,
                      VMSid = min(as.numeric(as.character(VMSid))))
    VMS.test <- join(bftows, VMS.test, type="left")
    VMS.test$VMSid <- as.numeric(as.character(VMS.test$VMSid))
    VMS.test[is.na(VMS.test$VMSid),]$VMSid <- 10
    VMS.test[VMS.test$VMSid < 2,]$VMSid <- 1
    VMS.test[VMS.test$VMSid > 1.9 & VMS.test$VMSid < 3,]$VMSid <- 2
    VMS.test$VMS <- NA
    VMS.test[VMS.test$VMSid == 1,]$VMS <- "inner"
    VMS.test[VMS.test$VMSid == 2,]$VMS <- "outer"
    VMS.test[VMS.test$VMSid == 10,]$VMS <- "other"
    
    names(VMS.test)[34] <- "VMSstart"
    VMS.test <- VMS.test[,c(1:32, 34)]
    
    VMS.test.end <- NULL
    for(i in unique(VMS$ID)){
      
      points <- SpatialPoints(matrix(c(bftows$End_long, bftows$End_lat), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      coord_list <- split(VMS[VMS$ID %in% i, c(4,5,7)], VMS[VMS$ID %in% i,]$ID)
      coord_list <- lapply(coord_list, function(x) { x["ID"] <- NULL; x })
      ps <- sapply(coord_list, Polygon)
      p1 <- Polygons(ps, ID = 1) 
      
      # create SpatialPolygons object
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
      
      test <- data.frame(Oracle.tow.. = bftows$Oracle.tow.., gContains(my_spatial_polys, points, byid=TRUE), ID=i)
      VMS.true.end <- subset(test, X1=="TRUE", select=c("Oracle.tow..", "ID"))
      VMS.test.end <- rbind(VMS.test.end, VMS.true.end)
    }
    
    names(VMS.test.end)[2] <- "VMSid"
    VMS.test.end <- ddply(.data=VMS.test.end, .(Oracle.tow..),
                          summarize,
                          VMSid = min(as.numeric(as.character(VMSid))))
    VMS.test.end <- join(bftows, VMS.test.end, type="left")
    VMS.test.end$VMSid <- as.numeric(as.character(VMS.test.end$VMSid))
    VMS.test.end[is.na(VMS.test.end$VMSid),]$VMSid <- 10
    VMS.test.end[VMS.test.end$VMSid < 2,]$VMSid <- 1
    VMS.test.end[VMS.test.end$VMSid > 1.9 & VMS.test.end$VMSid < 3,]$VMSid <- 2
    VMS.test.end$VMS <- NA
    VMS.test.end[VMS.test.end$VMSid == 1,]$VMS <- "inner"
    VMS.test.end[VMS.test.end$VMSid == 2,]$VMS <- "outer"
    VMS.test.end[VMS.test.end$VMSid == 10,]$VMS <- "other"
    
    names(VMS.test.end)[34] <- "VMSend"
    VMS.test.end <- VMS.test.end[,c(1:32, 34)]
    
    VMS.test <- join(VMS.test, VMS.test.end, type="full")
    VMS.test$VMS <- ifelse(VMS.test$VMSstart==VMS.test$VMSend, VMS.test$VMSstart, "check")
    
    if(df=="TRUE"){
      assign(paste0("VMS.test_", cruise), VMS.test, env=.GlobalEnv)
    }
    
    if(desktop=="NULL"){
      write.csv(VMS.test, paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "VMS_strata.csv"))
    }
    
    if(!desktop=="NULL"){
      write.csv(VMS.test, paste0(desktop, cruise, "/", cruise, "VMS_strata.csv"))
    }
  }
  
  ## check repeat tows. make sure they're close enough together
  repeatsthisyear <- subset(towpath, Tow_type_id==5)
  repeatslastyear <- read.csv(paste0(direct, as.numeric(year)-2, "/", previouscruisefolder, "/", previouscruisename, "tow_CONVERTED.csv"))
  
  # converting all lats and longs to decimal degrees
  repeatslastyear$Start_lat <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$Start_lat)
  repeatslastyear$Start_long <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$Start_long)
  repeatslastyear$End_lat <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$End_lat)
  repeatslastyear$End_long <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$End_long)
  repeatslastyear$Start_long <- -repeatslastyear$Start_long
  repeatslastyear$End_long <- -repeatslastyear$End_long
  
  # calculating the distance of each tow
  repeatslastyear$dist.calc <- geosphere::distGeo(matrix(c(repeatslastyear$Start_long, repeatslastyear$Start_lat), ncol=2), matrix(c(repeatslastyear$End_long, repeatslastyear$End_lat), ncol=2))
  
  # flag the tow if the distance is greater than 2 km
  repeatslastyear$flag <- ifelse(repeatslastyear$dist.calc > 2000 | repeatslastyear$dist.calc < 200, "check", "ok")
  
  repeatslastyear <- dplyr::arrange(repeatslastyear, Oracle.tow..)
  
  # data tidying for ggplotting
  towpath_lat <- dplyr::select(repeatslastyear, Oracle.tow.., Tow_type_id, Tow_len, Strata_id, Start_lat, End_lat, Tow_dir, dist.calc, flag)
  towpath_lat <- reshape2::melt(towpath_lat, id.vars=c("Oracle.tow..", "Tow_type_id", "Tow_len", "Strata_id", "Tow_dir", "dist.calc", "flag"))
  names(towpath_lat)[8:9] <- c("lats", "LATITUDE")
  towpath_lon <- dplyr::select(repeatslastyear, Oracle.tow.., Tow_type_id, Tow_len, Strata_id, Start_long, End_long, Tow_dir, dist.calc, flag)
  towpath_lon <- reshape2::melt(towpath_lon, id.vars=c("Oracle.tow..", "Tow_type_id", "Tow_len", "Strata_id", "Tow_dir", "dist.calc", "flag"))
  names(towpath_lon)[8:9] <- c("lons", "LONGITUDE")
  towpath <- plyr::join(towpath_lat, towpath_lon, type="full")
  towpath_start <- subset(towpath, lats=="Start_lat" & lons=="Start_long")
  towpath_end <- subset(towpath, lats=="End_lat" & lons=="End_long")
  towpath_lastyear <- rbind(towpath_start, towpath_end)
  
  if(plot=="TRUE"){
    # if working off directory:
    if(desktop=="NULL"){
      plot.list <- NULL
      for(i in 1:length(unique(repeatsthisyear$Oracle.tow..))){
        p <- ggplot() + geom_path(data=repeatsthisyear, aes(-abs(LONGITUDE), LATITUDE, group=Oracle.tow..)) +
          geom_text(data=repeatsthisyear[repeatsthisyear$lats=="Start_lat",], aes(-abs(LONGITUDE), LATITUDE, label=Oracle.tow..), size=4) +
          geom_path(data=towpath_lastyear, aes(-abs(LONGITUDE), LATITUDE, group=Oracle.tow..), colour="blue") +
          geom_text(data=towpath_lastyear[towpath_lastyear$lats=="Start_lat",], aes(-abs(LONGITUDE), LATITUDE, label=Oracle.tow..), size=4) +
          xlim(min(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LONGITUDE) - 0.03, max(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LONGITUDE) + 0.03) +
          ylim(min(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LATITUDE) - 0.01, max(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LATITUDE) + 0.01) +
          theme_bw() + 
          theme(panel.grid=element_blank())
        plot.list[[i]] <- p
      }
      
      pdf(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "_repeat_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
    
    # if working off desktop:
    if(!desktop=="NULL"){
      plot.list <- NULL
      for(i in unique(repeatsthisyear$Oracle.tow..)){
        p <- ggplot() + geom_path(data=repeatsthisyear, aes(-abs(LONGITUDE), LATITUDE, group=Oracle.tow..), lwd=2) +
          geom_text(data=repeatsthisyear[repeatsthisyear$lats=="Start_lat",], aes(-abs(LONGITUDE), LATITUDE, label=Oracle.tow..)) +
          geom_path(data=towpath_lastyear[towpath_lastyear$flag=="ok",], aes(-abs(LONGITUDE), LATITUDE, group=Oracle.tow..), lwd=2, colour="blue") +
          geom_text(data=towpath_lastyear[towpath_lastyear$lats=="Start_lat",], aes(-abs(LONGITUDE), LATITUDE, label=Oracle.tow..)) +
          xlim(min(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LONGITUDE) - 0.03, max(repeatsthisyear[repeatsthisyear$Oracle.tow..==repeatsthisyear$Oracle.tow..[i],]$LONGITUDE) + 0.03) +
          ylim(min(repeatsthisyear[repeatsthisyear$Oracle.tow..==i,]$LATITUDE) - 0.01, max(repeatsthisyear[repeatsthisyear$Oracle.tow..==i,]$LATITUDE) + 0.01) +
          theme_bw() + 
          theme(panel.grid=element_blank())
        plot.list[[i]] <- p
      }
      pdf(paste0(desktop, cruise, "_repeat_check.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
    
  }
}


