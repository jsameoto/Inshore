
######  DATA CHECKS ########

#libraries
require(ggplot2)
require(tidyverse)

#Check tows spatial function:

funcs <- "https://raw.githubusercontent.com/Mar-scal/Inshore/master/Survey/check.tows.spatial.r"
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}
#source(paste0("Y:/Inshore/Survey/", year, "/data entry templates and examples/entry check functions/check.tows.spatial.r"))

#define
direct <- "Y:/Inshore/Survey/"
year <- 2022
CRUISE <- "BI" # "BI", BF", "GM", "SFA29"


# HGTWGT.csv ----------------------------------------------------------------

mwsh <- read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"_HGTWGT.csv"))
str(mwsh)

mwsh$Weight <- as.numeric(as.character(mwsh$Weight))
mwsh$Mycobacteria <- as.factor(mwsh$Mycobacteria)
mwsh$Meat_Colour <- as.factor(mwsh$Meat_Colour)

summary(mwsh)

#Plots height and weight

#all data points
ggplot() + geom_text(data=mwsh, aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow)))

# adjust tow numbers to test it out # if an entire tow is away from the rest, make sure that the height and weight columns weren't flipped!!
#(labels are Tow numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow>1 & mwsh$Tow<50,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow)))

#Plot individual tows (labels are sample numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow==41,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==23,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==29,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==31,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

ggplot() + geom_text(data=mwsh[mwsh$Tow>50 & mwsh$Tow<100,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow))) 

#Plot individual tows (labels are sample numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow==63,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==98,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==80,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==71,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

ggplot() + geom_text(data=mwsh[mwsh$Tow>100 & mwsh$Tow<132,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow))) 

#Plot individual tows (labels are sample numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow==102,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

# bycatch.csv ----------------------------------------------------------------

bycatch <- read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"_bycatch.csv"))

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

#Note - horse mussels are not entered in the bycatch.csv. These are entered in a separate file (so there should *not* be species code 4332 in this file)

ggplot() + geom_point(data=bycatch, aes(as.factor(Sex), Measurement)) + facet_wrap(~Species_code, scales="free")
#bycatch[which(bycatch$Species_code==1191 & bycatch$Tow_num==53 & bycatch$Measurement>100),]$Measurement <-24
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement==30.5),]$Measurement <- 30
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement>30 & bycatch$Tow_num==244),]$Measurement <- 26

#write.csv(bycatch, file="Y:/INSHORE SCALLOP/Survey/2018/data entry templates and examples/entry check functions/BF2018_bycatch_FK.csv")


# dhf.csv ----------------------------------------------------------------

dhf <-  read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"_dhf.csv"))

dhf <- reshape2::melt(dhf, id.vars=c("CRUISE", "TOW", "GEAR", "DEPTH", "c"))
dhf <- dplyr::arrange(dhf, TOW)
dhf$variable <- gsub('X', '', dhf$variable)
dhf$bin <- ifelse(dhf$c %in% c(0,2), dhf$variable, 
                  ifelse(dhf$c %in% c(1,3), as.numeric(dhf$variable)+100, NA))
dhf <- dplyr::arrange(dhf, TOW, GEAR, c)

#Plot to look for outliers:
dhf1 <- dhf[dhf$TOW>0 & dhf$TOW < 25,]
ggplot(dhf1, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf2 <- dhf[dhf$TOW>24 & dhf$TOW < 50,]
#ggplot() + geom_point(data=dhf2, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf2, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf3 <- dhf[dhf$TOW>49 & dhf$TOW < 75,]
#ggplot() + geom_point(data=dhf3, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf3, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf4 <- dhf[dhf$TOW>74 & dhf$TOW < 100,]
#ggplot() + geom_point(data=dhf4, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf4, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf5 <- dhf[dhf$TOW>99 & dhf$TOW < 125,]
#ggplot() + geom_point(data=dhf5, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf5, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf6 <- dhf[dhf$TOW>124 & dhf$TOW < 137,]
#ggplot() + geom_point(data=dhf5, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf6, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

#End for BI

dhf6 <- dhf[dhf$TOW>124 & dhf$TOW < 150,]
#ggplot() + geom_point(data=dhf6, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf6, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf7 <- dhf[dhf$TOW>231 & dhf$TOW < 242,]
#ggplot() + geom_point(data=dhf7, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf7, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>241 & dhf$TOW < 252,]
#ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf8, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf9 <- dhf[dhf$TOW>251 & dhf$TOW < 262,]
#ggplot() + geom_point(data=dhf8, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf9, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf10 <- dhf[dhf$TOW>261 & dhf$TOW < 272,]
ggplot() + geom_point(data=dhf10, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf11 <- dhf[dhf$TOW>271 & dhf$TOW < 282,]
ggplot() + geom_point(data=dhf11, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf12 <- dhf[dhf$TOW>281 & dhf$TOW < 292,]
ggplot() + geom_point(data=dhf12, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf13 <- dhf[dhf$TOW>291 & dhf$TOW < 302,]
ggplot() + geom_point(data=dhf13, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf14 <- dhf[dhf$TOW>301 & dhf$TOW < 312,]
ggplot() + geom_point(data=dhf14, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf15 <- dhf[dhf$TOW>311 & dhf$TOW < 322,]
ggplot() + geom_point(data=dhf15, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf16 <- dhf[dhf$TOW>321 & dhf$TOW < 332,]
ggplot() + geom_point(data=dhf16, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")

dhf17 <- dhf[dhf$TOW>331 & dhf$TOW < 342,]
ggplot() + geom_point(data=dhf17, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")


# horsemussellive.csv---------------------

hm.live <- read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"_horsemussellivefreq.csv"))
num.tows <- read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"tow_CONVERTED.csv"))

#Species Code must be "4332"
table(hm.live$SPECIES.CODE)

hm.live <- hm.live %>% 
    mutate(flag.speciescode = case_when(SPECIES.CODE != 4332 ~ "check",
                            TRUE ~ "ok"))
hm.live %>% filter(flag.speciescode == "check")

#Prorate factor can only be 1 or 2. 
# - 1 if both lined drags fished AND sampled the catch from both these drags (i.e. no prorating) 
# - 2 if only had 1 lined drag fish (i.e. correspond to you having to make NUM_LINED_FREQ = 1 when normally it's 2) OR
   #you had both lined drags fish but subsampled the catch by only counting/measuring the length frequencies from one of the 2 lined drags. 

table(hm.live$PRORATE.FACTOR)
hm.live$PRORATE.FACTOR <- as.numeric(hm.live$PRORATE.FACTOR)

hm.live <- hm.live %>% 
  mutate(flag.proratefac = case_when((PRORATE.FACTOR != 1) & (PRORATE.FACTOR != 2) ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.proratefac == "check")

#For any tows where PRORATE.FACTOR == 2, check
hm.live <- hm.live %>% 
  mutate(flag.factor2 = case_when(PRORATE.FACTOR == as.numeric(2) ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.factor2 == "check") #Ensure this correctly marked as prorated.

#Number of tows should match number of tows from survey
length(hm.live$TOW) == length(num.tows$Oracle.tow..) # same length
table(hm.live$TOW == num.tows$Oracle.tow..) #Same numbers? Should all be TRUE

#There should be x unique tows where x is the number of tows from the survey:
length(unique(hm.live$TOW))
table(duplicated(hm.live$TOW)) #Should all be false


#LIVE.DEAD column should only contain "L"s. D's will be in the horsemusseldead.csv
hm.live <- hm.live %>% 
  mutate(flag.live = case_when(LIVE.DEAD != "L" ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.live == "check")

#Check Cruise - should only be one cruise!
unique(hm.live$CRUISE)
table(hm.live$CRUISE)

#Does the cruise in the data sheet match the cruise you are data checking?
unique(hm.live$CRUISE) == paste0(CRUISE,year)

#All numbers under Bin ID columns should be > 0. Also flag any numbers per bin > 100 (not necessarily incorrect, but will flag these tows to check)
#Less than 0?
hm.live %>% 
  filter_at(vars(5:44), any_vars(. <0))

#Greater than 100?
hm.live %>% 
  filter_at(vars(5:44), any_vars(. >100))

# Horsemusseldead.csv ---------------------

hm.dead <- read.csv(paste0("Y:/Inshore/Survey/", year,"/data entry templates and examples/",CRUISE, year,"/",CRUISE,year,"_horsemusseldeadfreq.csv"))

#Species Code must be "4332"
table(hm.dead$SPECIES.CODE)

hm.dead <- hm.dead %>% 
  mutate(flag.speciescode = case_when(SPECIES.CODE != 4332 ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.speciescode == "check")

#Prorate factor can only be 1 or 2
table(hm.dead$PRORATE.FACTOR)

hm.dead <- hm.dead %>% 
  mutate(flag.proratefac = case_when((PRORATE.FACTOR != 1) & (PRORATE.FACTOR != 2) ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.proratefac == "check")

#For any tows where PRORATE.FACTOR == 2, check
hm.dead <- hm.dead %>% 
  mutate(flag.factor2 = case_when(PRORATE.FACTOR == as.numeric(2) ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.factor2 == "check") #Ensure this correctly marked as prorated.

#Tow numbers should match number of tows from survey
length(hm.dead$TOW) == length(num.tows$Oracle.tow..)# same length
table(hm.dead$TOW == num.tows$Oracle.tow..) #Should all be TRUE

#Number of unique tows should match number of tows from survey:
length(unique(hm.dead$TOW))
table(duplicated(hm.dead$TOW)) #Should all be false


#LIVE.DEAD column should only contain "D"s. 
hm.dead <- hm.dead %>% 
  mutate(flag.dead = case_when(LIVE.DEAD != "D" ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.dead == "check")

#Check Cruise - should only be one cruise!
unique(hm.dead$CRUISE)
table(hm.dead$CRUISE)

#Does the cruise in the data sheet match the cruise you are data checking?
unique(hm.dead$CRUISE) == paste0(CRUISE,year)

#All numbers under Bin ID columns should be > 0. Also flag any values > 50 (not necessarily incorrect, but will flag these tows to check)
#Less than 0?
hm.dead %>% 
  filter_at(vars(5:44), any_vars(. <0))

#Greater than 50?
hm.dead %>% 
  filter_at(vars(5:44), any_vars(. >50))

#Extra horse mussel checks ------------------------------------------------

#Tows where horse mussels were present - live or dead
hm.check.l <- hm.live %>% 
  mutate(totlive = dplyr::select(., X0:X195) %>% rowSums(na.rm = TRUE) %>% round(0))
hm.check.d <- hm.dead %>% 
  mutate(totdead = dplyr::select(., X0:X195) %>% rowSums(na.rm = TRUE) %>% round(0))

hm.check <- data.frame(cbind("TOW" = hm.check.l$TOW, "Total.Live" = hm.check.l$totlive , "TOW.D" = hm.check.d$TOW, "Total.Dead" = hm.check.d$totdead))

hm.check %>% filter(Total.Live > 0 | Total.Dead > 0) 
nrow(hm.check %>% filter(Total.Live > 0 | Total.Dead > 0)) # This number should match the number of records that will be entered into SCBYATCHES 
#associated with code 4332 for that given cruise, and can be used later on as a check to ensure the HM records are being inserted properly.


# SPATIAL CHECKS ----------------------------------------------------------
### tow data spatial checks
### use SCSTRATAINFO to make sure that they don't cross strata
### calculate distances between tow start/end to make sure they are within 1km-ish

# cruise <- "BI2019"
# direct <- "Y:/INSHORE SCALLOP/Survey/"
# desktop="C:/Users/keyserf/Desktop/"

#Check for strange Lat Long outliers

start_long_less6500 <- num.tows %>% filter(Start_long < abs(6500))
min(start_long_less6500$Start_long)
which(start_long_less6500$Start_long == min(start_long_less6500$Start_long)) #Which row is this?

end_long_less6500 <- num.tows %>% filter(End_long < abs(6500))
which(end_long_less6500$End_long == min(end_long_less6500$End_long)) #Which row is this?

end_lat_less4300 <- num.tows %>% filter(End_lat < abs(4300))
min(end_lat_less4300$End_lat) #warning if none.
which(end_lat_less4300$End_lat == min(end_lat_less4300$End_lat)) #Which row is this?

end_lat_more4600 <- num.tows %>% filter(End_lat > abs(4600))
min(end_lat_more4600$End_lat)#warning if none.
which(end_lat_more4600$End_lat == min(end_lat_more4600$End_lat)) #Which row is this?

end_long_less6500 <- num.tows %>% filter(End_long > abs(6500))
min(end_long_less6500$End_long)
which(end_long_less6500$End_long == min(end_long_less6500$End_long)) #Which row is this?


# -----Run check.tows.spatial function --------------------------------------------------------------------

# Produces the following files for review:
# - CRUISE_repeat_check.pdf
# - CRUISE_SPA_check.pdf
# - CRUISE_strata_check
# - CRUISE_flagged_tows.csv
# - Will also return the area and strata objects added within the function for further investigating any flagged tows.

check.tows.spatial(cruise= paste0(CRUISE,year), year=year, direct="Y:/Inshore/Survey/", desktop="NULL", 
                   previouscruisefolder = paste0("data entry templates and examples/",CRUISE,year-1), previouscruisename = paste0(CRUISE,year-1), plot=TRUE, df=TRUE)

#enter flag.tows_CRUISE produced by the check.tows.spatial function. 
flagged.tows <- flagged.tows_BF2021

#Enter the tow number indicated in the flagged_tows file 
flagged.tows[flagged.tows$Oracle.tow..==272,]

#Plot (add more tows if needed)
 ggplot() + 
  geom_polygon(data=area, aes(Longitude, Latitude, group=AREA_ID, fill=Area), colour="black") +
  geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==272,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)#+
 #geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==272,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)


## Need a way to handle overlapping strata! E.g. 31 and 32. THIS WAS ADDRESSED KIND OF...
# ggplot() + geom_polygon(data=strata[strata$STRATA_ID %in% c(31,32),], aes(LONGITUDE, LATITUDE, group=STRATA_ID), fill=NA, colour="black")

test <- subset(double.strata, strata==32)
length(unique(test$Oracle.tow..))

test2<- subset(unique(double.strata[,1:30]), Strata_id==32)
length(unique(test2$Oracle.tow..))

#FOR GM CRUISES ONLY
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


# ----temperature data matching to tows------------------------------------------------------------

source("Y:/Inshore/Survey/TemperatureDataScripts/Extract_survey_temperatures_function.r")
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