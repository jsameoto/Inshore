##data entry checks

mwsh <- read.csv("Y:/INSHORE SCALLOP/Survey/2019/data entry templates and examples/BI2019/BI2019_HGTWGT.csv")
str(mwsh)

require(ggplot2)

mwsh$Weight <- as.numeric(as.character(mwsh$Weight))

summary(mwsh)

# adjust tow numbers to test it out # if an entire tow is away from the rest, make sure that the height and weight columns weren't flipped!!
ggplot() + geom_text(data=mwsh
                     [mwsh$Tow>109 & mwsh$Tow<140,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=Num)) 

ggplot() + geom_text(data=mwsh[mwsh$Tow==111 
                               #& mwsh$Tow <50
                               ,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

ggplot() + geom_text(data=mwsh[mwsh$Tow>109& mwsh$Tow<135,], aes(Height, Weight, colour=as.factor(Tow), label=Num)) + facet_wrap(~Tow, scales="free")

ggplot() + geom_text(data=mwsh[mwsh$Tow==113,], aes(Height, Weight, colour=as.factor(Tow), label=Num))



bycatch <- read.csv("Y:/INSHORE SCALLOP/Survey/2019/data entry templates and examples/BI2019/BI2019_bycatch.csv")

## NO NA's ALLOWED! REPLACE THESE WITH APPROPRIATE CODES (unknown sex=0!)
print(bycatch[is.na(bycatch$Species_code) & !is.na(bycatch$Tow_num) |
                is.na(bycatch$Measure_code) & !is.na(bycatch$Tow_num) |
                is.na(bycatch$Sex) & !is.na(bycatch$Tow_num),])

ggplot() + geom_point(data=bycatch, aes(Sex, as.factor(Species_code)))
# bycatch$Sex[which(bycatch$Species_code==203 & bycatch$Sex==21)] <- 1
# bycatch[which(bycatch$Species_code==203 & bycatch$Sex==0),]
# bycatch$Sex[which(bycatch$Species_code==203 & bycatch$Sex==0)] <- 1
# bycatch[which(bycatch$Species_code==204 & bycatch$Sex==0),]
# bycatch[bycatch$Species_code==160,]
# bycatch[bycatch$Species_code==1220,]$Species_code <- 122
# bycatch[bycatch$Species_code==1991,]$Species_code <- 1191

# We only record ocean pout, so the species code should be 640. Change records of 845 or other (642, 598) to 640. Note that in 2012, they were miscoded as 845. 
# bycatch[which(bycatch$Species_code%in% c(845, 642, 598)),]

ggplot() + geom_point(data=bycatch, aes(as.factor(Sex), Measurement)) + facet_wrap(~Species_code, scales="free")
#bycatch[which(bycatch$Species_code==1191 & bycatch$Tow_num==53 & bycatch$Measurement>100),]$Measurement <-24
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement==30.5),]$Measurement <- 30
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement>30 & bycatch$Tow_num==244),]$Measurement <- 26

#write.csv(bycatch, file="Y:/INSHORE SCALLOP/Survey/2018/data entry templates and examples/entry check functions/BF2018_bycatch_FK.csv")


dhf <-  read.csv("Y:/INSHORE SCALLOP/Survey/2019/data entry templates and examples/BI2019/BI2019_dhf.csv")
dhf <- reshape2::melt(dhf, id.vars=c("CRUISE", "TOW", "GEAR", "DEPTH", "c"))
dhf <- dplyr::arrange(dhf, TOW)
dhf$variable <- gsub('X', '', dhf$variable)
dhf$bin <- ifelse(dhf$c %in% c(0,2), dhf$variable, 
                  ifelse(dhf$c %in% c(1,3), as.numeric(dhf$variable)+100, NA))
dhf <- dplyr::arrange(dhf, TOW, GEAR, c)

dhf1 <- dhf[dhf$TOW>0 & dhf$TOW < 25,]
ggplot() + geom_point(data=dhf1, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf2 <- dhf[dhf$TOW>24 & dhf$TOW < 50,]
ggplot() + geom_point(data=dhf2, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf3 <- dhf[dhf$TOW>49 & dhf$TOW < 75,]
ggplot() + geom_point(data=dhf3, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf4 <- dhf[dhf$TOW>74 & dhf$TOW < 100,]
ggplot() + geom_point(data=dhf4, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf5 <- dhf[dhf$TOW>99 & dhf$TOW < 125,]
ggplot() + geom_point(data=dhf5, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf6 <- dhf[dhf$TOW>124 & dhf$TOW < 150,]
ggplot() + geom_point(data=dhf6, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf7 <- dhf[dhf$TOW>231 & dhf$TOW < 242,]
ggplot() + geom_point(data=dhf7, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>241 & dhf$TOW < 252,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>251 & dhf$TOW < 262,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>261 & dhf$TOW < 272,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>271 & dhf$TOW < 282,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>281 & dhf$TOW < 292,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>291 & dhf$TOW < 302,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>301 & dhf$TOW < 312,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>311 & dhf$TOW < 322,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>321 & dhf$TOW < 332,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>331 & dhf$TOW < 342,]
ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")


### tow data spatial checks
### use SCSTRATAINFO to make sure that they don't cross strata
### calculate distances between tow start/end to make sure they are within 1km-ish

# cruise <- "BI2019"
# direct <- "Y:/INSHORE SCALLOP/Survey/"
# desktop="C:/Users/keyserf/Desktop/"
year <- 2019
source(paste0(direct, year, "/data entry templates and examples/entry check functions/check.tows.spatial.r"))

check.tows.spatial(cruise="BI2019", year=2019, direct="Y:/INSHORE SCALLOP/Survey/", desktop="NULL", 
                   previouscruisefolder = "data entry templates and examples/BI2018", previouscruisename = "BI2018", plot=TRUE, df=TRUE)

area <- read.csv(paste0(direct, year, "/data entry templates and examples/entry check functions/BayofFundyFishingBoundaries_WGS84.csv"))
area$AREA_ID <- as.numeric(area$Area)
# 
# flagged.tows_BI2019[flagged.tows_BI2019$Oracle.tow..==84,]
# 
# ggplot() + 
#  geom_polygon(data=area, aes(Longitude, Latitude, group=AREA_ID, fill=Area), colour="black") +
#   #geom_segment(data=flagged.tows_BF2018[flagged.tows_BF2018$Oracle.tow..==275,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)
# geom_point(data=flagged.tows_BI2019[flagged.tows_BI2019$Oracle.tow..==84,], aes(x=Start_long, y=Start_lat))+
# geom_point(data=flagged.tows_BI2019[flagged.tows_BI2019$Oracle.tow..==84,], aes(x=End_long, y=End_lat))

check.tows.spatial(cruise="GM2017", year=2017, direct="Y:/INSHORE SCALLOP/Survey/", desktop="NULL", 
                   previouscruisefolder = "SPA6", previouscruisename = "GM2016", plot=TRUE, df=TRUE)

check.tows.spatial(cruise="BI2017", year=2017, direct="Y:/INSHORE SCALLOP/Survey/", desktop="NULL", 
                   previouscruisefolder = "SPA3", previouscruisename = "BI2016", plot=TRUE, df=TRUE)

## Need a way to handle overlapping strata! E.g. 31 and 32. THIS WAS ADDRESSED KIND OF...
# ggplot() + geom_polygon(data=strata[strata$STRATA_ID %in% c(31,32),], aes(LONGITUDE, LATITUDE, group=STRATA_ID), fill=NA, colour="black")

test <- subset(double.strata, strata==32)
length(unique(test$Oracle.tow..))

test2<- subset(unique(double.strata[,1:30]), Strata_id==32)
length(unique(test2$Oracle.tow..))

### data validation

bitows <- read.csv(paste0(direct, year, "/data entry templates and examples/", cruise, "/", cruise, "tow_CONVERTED.csv"))

start_long_less6500 <- subset(bitows, Start_long < abs(6500))
min(start_long_less6500$Start_long)

end_long_less6500 <- subset(bitows, End_long < abs(6500))
min(end_long_less6500$End_long)

# area1 <- read.table(paste0(direct, "/", year, "/data entry templates and examples/BF2017/data validation/R/area1 tows.txt"), fill=T)
# area4 <- read.table(paste0(direct, "/", year, "/data entry templates and examples/BF2017/data validation/R/area4 tows.txt"), fill=T)
# area5 <- read.table(paste0(direct, "/", year, "/data entry templates and examples/BF2017/data validation/R/area5 tows.txt"), fill=T)
# 
# area1$area <- 1
# area1 <- area1[-c(1:4),]
# area4$area <- 4
# area4 <- area4[-c(1:4),]
# area5$area <- 5
# area5 <- area5[-c(1:4),]
# 
# area <- rbind(area1, area4, area5)
# area <- area[,c(1:4, 14)]
# area$V2 <- as.numeric(as.character(area$V2))
# area <- subset(area, is.na(V2)==FALSE)
# tows <- data.frame(V2=1:334)
# 
# area <- join(area, tows, type="full")
# 
# area <- arrange(area, V2)
# names(area) <- c("CRUISE", "Oracle.tow..", "STRATA", "NA", "area.val")

# write.csv(area, paste0(direct, "/", year, "/data entry templates and examples/BF2017/data validation/R/area.csv"))

# area.check <- join(area, area.test, type="left")
# area.check$val.check <- area.check$area==area.check$SPA.start
# 
# subset(area.check, val.check=="FALSE"|is.na(val.check))
# ## ALL GOOD
# 
# write.csv(area.check, paste0(direct, "/", year, "/data entry templates and examples/BF2017/data validation/R/area_checked.csv"))

end_lat_less4300 <- subset(bftows, End_lat < abs(4300))
min(end_lat_less4300$End_lat)

end_lat_more4600 <- subset(bftows, End_lat > abs(4600))
min(end_lat_more4600$End_lat)

end_long_less6500 <- subset(bftows, End_long < abs(6500))
min(end_long_less6500$End_long)


# VMS strata check
VMS.test[VMS.test$VMS=="check",]
ggplot() + 
  geom_polygon(data=otherVMS, aes(X, Y, group=PID), fill="yellow", alpha=0.5)+
  geom_polygon(data=inVMS, aes(X, Y, group=SID), fill="red",alpha=0.5) +
  geom_polygon(data=outVMS, aes(X, Y, group=SID), fill="blue", alpha=0.5) + 
  geom_path(data=towpath[towpath$Oracle.tow..%in% c(VMS.test[VMS.test$VMS=="check",]$Oracle.tow..),], 
            aes(LONGITUDE, LATITUDE, group=Oracle.tow..)) +
  geom_text(data=bftows[bftows$Oracle.tow..%in% c(VMS.test[VMS.test$VMS=="check",]$Oracle.tow..),],
            aes(Start_long, Start_lat, label=Oracle.tow..))+
  theme_bw() + theme(panel.grid=element_blank()) +
  coord_map() +
  xlim(-67.2, -66.4) +
  ylim(44.4, 45.1)

### FINAL
source(paste0(direct, year, "/data entry templates and examples/entry check functions/check.tows.spatial.r"))
check.tows.spatial(cruise="GM2017", year=2017, direct="Y:/INSHORE SCALLOP/Survey/", 
                   desktop="NULL", previouscruisefolder="SPA6", 
                   previouscruisename="GM2016",plot=TRUE, df=TRUE)
check.tows.spatial(cruise="BF2018", year=2018, direct="Y:/INSHORE SCALLOP/Survey/", 
                   desktop="NULL", previouscruisefolder="data entry templates and examples/BF2017", 
                   previouscruisename="BF2017",plot=TRUE, df=TRUE)
check.tows.spatial(cruise="BI2018", year=2018, direct="Y:/INSHORE SCALLOP/Survey/", 
                   desktop="NULL", previouscruisefolder="data entry templates and examples/BI2017", 
                   previouscruisename="BI2017",plot=TRUE, df=TRUE)

checktows_BF2018 <- dplyr::select(tows_BF2018, Oracle.tow.., Tow_len, Start_lat, Start_long, End_lat, End_long, Strata_id)
checktows_BI2019 <- dplyr::select(tows_BI2019, Oracle.tow.., Tow_len, Start_lat, Start_long, End_lat, End_long, Strata_id)
checktows_GM2017 <- dplyr::select(tows_GM2017, Oracle.tow.., Tow_len, Start_lat, Start_long, End_lat, End_long, Strata_id)

checks <- rbind(#checktows_BF2018
                #, 
                checktows_BI2019
                #, 
                #checktows_GM2017
                )
checks$Start_lat <- convert.dd.dddd(format="dec.deg", x=checks$Start_lat)
checks$Start_long <- convert.dd.dddd(format="dec.deg", x=checks$Start_long)
checks$End_lat <- convert.dd.dddd(format="dec.deg", x=checks$End_lat)
checks$End_long <- convert.dd.dddd(format="dec.deg", x=checks$End_long)
checks$Start_long <- -checks$Start_long
checks$End_long <- -checks$End_long
checks$dist.calc <- geosphere::distGeo(matrix(c(checks$Start_long, checks$Start_lat), ncol=2), matrix(c(checks$End_long, checks$End_lat), ncol=2))

checks$dir.dist <- checks$dist.calc - checks$Tow_len
mean(abs(checks$dir.dist), na.rm=T)
summary(checks$dir.dist)
ggplot() + geom_histogram(data=checks, aes(dir.dist)) + 
  annotate(x=-200, y=5, geom="text", label="dist.calc shorter\n179 tows < -200", colour="white") +
  annotate(x=200, y=5, geom="text", label="dist.calc longer\n84 tows > 200", colour="white") +
  annotate(geom="segment", x=0, xend=0, y=-Inf, yend=Inf, lty="dashed", colour="white") +
  theme_bw() +
  xlab("Difference between dist.calc and Tow_len (dist.calc - Tow_len)") +
  annotate(x=-600, y=50, geom="text", label="589 tows total", hjust=0, size=5)


#length(flagged[flagged$dir.dist <0,]$Oracle.tow..) # means calculated distance was greater than OLEX distance written (took too long to record coords)
#length(flagged[flagged$dir.dist >0,]$Oracle.tow..) # means calculated distance was less than OLEX distance written (straight line distance on computer is too short)
length(checks[is.na(checks$dir.dist)=="FALSE" & checks$dir.dist > 400,]$Oracle.tow..)
length(checks[is.na(checks$dir.dist)=="FALSE" & checks$dir.dist < -500,]$Oracle.tow..)
View(checks[is.na(checks$dir.dist)=="FALSE" & checks$dir.dist < -500,])
checks[is.na(checks$dir.dist)=="FALSE" & checks$dir.dist > 400,]


## temperature data matching to tows
source("Y:/Inshore scallop/Survey/TemperatureDataScripts/Extract_survey_temperatures_function.r")
survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2019/", cruise = "BI2019", num.temps=4,
                    survey_time = "start", tow_duration=19, fig="pdf", export=T)
survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2019/", cruise = "BF2019", num.temps=4,
                    survey_time = "start", tow_duration=16, fig="pdf", export=T)
# survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2018/", cruise = "GM2018", num.temps=4,
#                     survey_time = "start", tow_duration=14, fig="pdf", export=T)
# survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2018/", cruise = "SFA292018", num.temps=4,
#                     survey_time = "start", tow_duration=14, fig="pdf", export=T)

# In BF2019, Tows 273, 274 and 275 had a reversed temperature profile. The seafloor was WARMER than the surface. For these 3 tows, we have to calculate temperature manually.
# To do this, insert a browser() on line 128 of the function. When it stops at the browser, do the following:
tow273 <- arrange(tempstows[tempstows$Tow==273 & !is.na(tempstows$Tow),], Temperature)
mean(tow273[(length(tow273$Temperature)-3) : length(tow273$Temperature),]$Temperature)
#14.04

tow274 <- arrange(tempstows[tempstows$Tow==274 & !is.na(tempstows$Tow),], Temperature)
mean(tow274[(length(tow274$Temperature)-3) : length(tow274$Temperature),]$Temperature)
#14.21

tow275 <- arrange(tempstows[tempstows$Tow==275 & !is.na(tempstows$Tow),], Temperature)
mean(tow275[(length(tow275$Temperature)-3) : length(tow275$Temperature),]$Temperature)
# 13.86

# Tows in SFA292019 with reversed temp profile:

tow9 <- arrange(tempstows[tempstows$Tow==9 & !is.na(tempstows$Tow),], Temperature)
mean(tow9[(length(tow9$Temperature)-3) : length(tow9$Temperature),]$Temperature)
#10.605
tow32 <- arrange(tempstows[tempstows$Tow==32 & !is.na(tempstows$Tow),], Temperature)
mean(tow32[(length(tow32$Temperature)-3) : length(tow32$Temperature),]$Temperature)
#12.325
tow35 <- arrange(tempstows[tempstows$Tow==35 & !is.na(tempstows$Tow),], Temperature)
mean(tow35[(length(tow35$Temperature)-3) : length(tow35$Temperature),]$Temperature)
#12
tow36 <- arrange(tempstows[tempstows$Tow==36 & !is.na(tempstows$Tow),], Temperature)
mean(tow36[(length(tow36$Temperature)-3) : length(tow36$Temperature),]$Temperature)
#12.2475
tow68 <- arrange(tempstows[tempstows$Tow==68 & !is.na(tempstows$Tow),], Temperature)
mean(tow68[(length(tow68$Temperature)-3) : length(tow68$Temperature),]$Temperature)
#10.29
tow69 <- arrange(tempstows[tempstows$Tow==69 & !is.na(tempstows$Tow),], Temperature)
mean(tow69[(length(tow69$Temperature)-3) : length(tow69$Temperature),]$Temperature)
#11.0375
tow70 <- arrange(tempstows[tempstows$Tow==70 & !is.na(tempstows$Tow),], Temperature)
mean(tow70[(length(tow70$Temperature)-3) : length(tow70$Temperature),]$Temperature)
#10.8825
tow71 <- arrange(tempstows[tempstows$Tow==71 & !is.na(tempstows$Tow),], Temperature)
mean(tow71[(length(tow71$Temperature)-3) : length(tow71$Temperature),]$Temperature)
#11.325
tow72 <- arrange(tempstows[tempstows$Tow==72 & !is.na(tempstows$Tow),], Temperature)
mean(tow72[(length(tow72$Temperature)-3) : length(tow72$Temperature),]$Temperature)
#11.51
tow73 <- arrange(tempstows[tempstows$Tow==73 & !is.na(tempstows$Tow),], Temperature)
mean(tow73[(length(tow73$Temperature)-3) : length(tow73$Temperature),]$Temperature)
#11.6525
tow74 <- arrange(tempstows[tempstows$Tow==74 & !is.na(tempstows$Tow),], Temperature)
mean(tow74[(length(tow74$Temperature)-3) : length(tow74$Temperature),]$Temperature)
#11.4
tow75 <- arrange(tempstows[tempstows$Tow==75 & !is.na(tempstows$Tow),], Temperature)
mean(tow75[(length(tow75$Temperature)-3) : length(tow75$Temperature),]$Temperature)
#11.6
tow89 <- arrange(tempstows[tempstows$Tow==89 & !is.na(tempstows$Tow),], Temperature)
mean(tow89[(length(tow89$Temperature)-3) : length(tow89$Temperature),]$Temperature)
#12.7675
tow90 <- arrange(tempstows[tempstows$Tow==90 & !is.na(tempstows$Tow),], Temperature)
mean(tow90[(length(tow90$Temperature)-3) : length(tow90$Temperature),]$Temperature)
#12.67
tow91 <- arrange(tempstows[tempstows$Tow==91 & !is.na(tempstows$Tow),], Temperature)
mean(tow91[(length(tow91$Temperature)-3) : length(tow91$Temperature),]$Temperature)
#12.5
tow92 <- arrange(tempstows[tempstows$Tow==92 & !is.na(tempstows$Tow),], Temperature)
mean(tow92[(length(tow92$Temperature)-3) : length(tow92$Temperature),]$Temperature)
#12.7375
tow93 <- arrange(tempstows[tempstows$Tow==93 & !is.na(tempstows$Tow),], Temperature)
mean(tow93[(length(tow93$Temperature)-3) : length(tow93$Temperature),]$Temperature)
#12.57
tow94 <- arrange(tempstows[tempstows$Tow==94 & !is.na(tempstows$Tow),], Temperature)
mean(tow94[(length(tow94$Temperature)-3) : length(tow94$Temperature),]$Temperature)
#12.5175
tow95 <- arrange(tempstows[tempstows$Tow==95 & !is.na(tempstows$Tow),], Temperature)
mean(tow95[(length(tow95$Temperature)-3) : length(tow95$Temperature),]$Temperature)
#12.47
tow96 <- arrange(tempstows[tempstows$Tow==96 & !is.na(tempstows$Tow),], Temperature)
mean(tow96[(length(tow96$Temperature)-3) : length(tow96$Temperature),]$Temperature)
#12.56
tow97 <- arrange(tempstows[tempstows$Tow==97 & !is.na(tempstows$Tow),], Temperature)
mean(tow97[(length(tow97$Temperature)-3) : length(tow97$Temperature),]$Temperature)
#12.52
tow98 <- arrange(tempstows[tempstows$Tow==98 & !is.na(tempstows$Tow),], Temperature)
mean(tow98[(length(tow98$Temperature)-3) : length(tow98$Temperature),]$Temperature)
#12.49
tow100 <- arrange(tempstows[tempstows$Tow==100 & !is.na(tempstows$Tow),], Temperature)
mean(tow100[(length(tow100$Temperature)-3) : length(tow100$Temperature),]$Temperature)
#12.95
tow103 <- arrange(tempstows[tempstows$Tow==103 & !is.na(tempstows$Tow),], Temperature)
mean(tow103[(length(tow103$Temperature)-3) : length(tow103$Temperature),]$Temperature)
#12.54
tow104 <- arrange(tempstows[tempstows$Tow==104 & !is.na(tempstows$Tow),], Temperature)
mean(tow104[(length(tow104$Temperature)-3) : length(tow104$Temperature),]$Temperature)
#12.47
tow105 <- arrange(tempstows[tempstows$Tow==105 & !is.na(tempstows$Tow),], Temperature)
mean(tow105[(length(tow105$Temperature)-3) : length(tow105$Temperature),]$Temperature)
#12.5
tow106 <- arrange(tempstows[tempstows$Tow==106 & !is.na(tempstows$Tow),], Temperature)
mean(tow106[(length(tow106$Temperature)-3) : length(tow106$Temperature),]$Temperature)
#12.52
tow107 <- arrange(tempstows[tempstows$Tow==107 & !is.na(tempstows$Tow),], Temperature)
mean(tow107[(length(tow107$Temperature)-3) : length(tow107$Temperature),]$Temperature)
#12.51
tow108 <- arrange(tempstows[tempstows$Tow==108 & !is.na(tempstows$Tow),], Temperature)
mean(tow108[(length(tow108$Temperature)-3) : length(tow108$Temperature),]$Temperature)
#12.39
tow109 <- arrange(tempstows[tempstows$Tow==109 & !is.na(tempstows$Tow),], Temperature)
mean(tow109[(length(tow109$Temperature)-3) : length(tow109$Temperature),]$Temperature)
#12.31
tow111 <- arrange(tempstows[tempstows$Tow==111 & !is.na(tempstows$Tow),], Temperature)
mean(tow111[(length(tow111$Temperature)-3) : length(tow111$Temperature),]$Temperature)
#12.45