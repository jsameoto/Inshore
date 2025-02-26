### 
### Calculate expected surplus production for SFA 29W based on survey data 
### J.Sameoto Feb 2024 
###

library(ggplot2)
library(tidyverse)

### EDITS TO DO - kill off first and then grow up -- more precautionary 

surveyyear <- 2024  #This is the last survey year for which you want to include  - note should match year of cruise below 
cruise <- "SFA292024"  #note should match year for surveyyear set above 
assessmentyear <- 2025 #year in which you are conducting the survey 
path.directory <- "Y:/Inshore/SFA29/"



#### Import data ####
### lbar -- ie commercial shell heights current year (See SHF field) and predicted one year ahead (field SHF.pred )
lbar <- read.csv("Y:/Inshore/SFA29/2025/Assessment/Data/Growth/SFA29.SHobj.2024.csv")
head(lbar)
unique(lbar$size)
lbar <- lbar[,2:dim(lbar)[2]]
head(lbar)
names(lbar)[2] <- "SUBAREA"
## summarize lbar based on subarea by year 
lbar.currentyr <- lbar %>% group_by(SUBAREA, year, size) %>% summarise(lbar = mean(SHF,na.rm = TRUE))
head(lbar.currentyr)

lbar.nextyr <- lbar %>% group_by(SUBAREA, year, size) %>% summarise(lbar.pred = mean(SHF.pred,na.rm = TRUE))
lbar.nextyr$year.pred <- lbar.nextyr$year+1
head(lbar.nextyr)



## stratitied estimates 
numbers <- read.csv("Y:/Inshore/SFA29/2025/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to2024.Numbers.csv")
head(numbers)
unique(numbers$size)
numbers <- numbers[,2:dim(numbers)[2]]
names(numbers)[1] <- "year"
numbers <- numbers %>% filter(size != "prerec")
numbers$size[numbers$size == "rec"] <- "recruit"
numbers$size[numbers$size == "comm"] <- "commercial"
unique(numbers$size)



## areas by subarea 
# The area of each strata in A, c(area of low, area of med+high)
A.areas <- data.frame(area = c(137.5875, 118.125), area.name = c("low","med"))
A.areas

# The area of each strata in B, I think...
B.areas <- data.frame(area =c(248.8925, 244.3425, 52.3925, NA), area.name = c("low","med", "high", "medhigh"))
B.areas
B.areas$area[B.areas$area.name == "medhigh"] <- B.areas$area[B.areas$area.name == "med"] + B.areas$area[B.areas$area.name == "high"]
B.areas

#  the area of each strata in C, c(low, med, high) in km^2
C.areas <- data.frame(area =c(144.9625, 125.1975, 29.2425, NA), area.name = c("low","med", "high","medhigh"))
C.areas
C.areas$area[C.areas$area.name == "medhigh"] <- C.areas$area[C.areas$area.name == "med"] + C.areas$area[C.areas$area.name == "high"]
C.areas

# The area of each strata in D, I think...
D.areas <- data.frame(area =c(133.5575, 142.9425, 51.07, NA), area.name = c("low","med", "high","medhigh"))
D.areas
D.areas$area[D.areas$area.name == "medhigh"] <- D.areas$area[D.areas$area.name == "med"] + D.areas$area[D.areas$area.name == "high"]
D.areas


## weight given lbar ## 
# need current year model object 
load(paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/Growth/SFA29growth",surveyyear,".RData"))
model.object.Y <- get(paste0("MWTSHSFA29.",surveyyear)) #Assign model.object.Y to current year model object - MWTSHSFA29.YYYY
summary(model.object.Y)
#data model ran on 
data.Y <- get(paste0("SFA29detail.",surveyyear))

#prepare data object - log transform depth and height for each year of data 
summary(data.Y)
data.Y <- data.Y[complete.cases(data.Y$HEIGHT),] #remove rows that have no height
data.Y$Log.HEIGHT <- log(data.Y$HEIGHT)
data.Y$Log.DEPTH <- log(abs(data.Y$ADJ_DEPTH)) #take abs to keep value positive
summary(data.Y)

#create same data used to run model on
test.data <- subset(data.Y, YEAR == surveyyear & HEIGHT > 40) #data subsetted as it was modelled


#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/SFA29/SFA29DepthProfile/SFA29_AreaMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
mean.depth
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.29 <- mean.depth[mean.depth$AREA %in% c("SFA29A",
                                                   "SFA29B",
                                                   "SFA29C",
                                                   "SFA29D",
                                                   "SFA29E"),]
dim(mean.depth.29)[1] == 5
unique(mean.depth.29$AREA)
names(mean.depth.29)[1] <- "SUBAREA"
head(mean.depth.29)



## merge depths for each area on to annual data 
dim(lbar.currentyr)
lbar.currentyr <- merge(lbar.currentyr, mean.depth.29, by = "SUBAREA")
head(lbar.currentyr)
dim(lbar.currentyr)

dim(lbar.nextyr)
lbar.nextyr <- merge(lbar.nextyr, mean.depth.29, by = "SUBAREA")
head(lbar.nextyr)
dim(lbar.nextyr)



### THIS IS FOR CURRENT YEAR LBAR (i.e. SHF field)
## prep lbar for being new data for predictions 
lbar.currentyr$Log.HEIGHT.CTR <- log(lbar.currentyr$lbar ) -  mean(test.data$Log.HEIGHT)
lbar.currentyr$Log.DEPTH.CTR <- log(abs(lbar.currentyr$MeanDepth_m)) - mean(test.data$Log.DEPTH)
head(lbar.currentyr)

#predictions for meat weight based on lbar of current year
lbar.currentyr$wgt.g.lbar.yrt <- predict(model.object.Y, newdata =lbar.currentyr, re.form = NA, type = "response") 
head(lbar.currentyr)

lbar.currentyr <- lbar.currentyr %>% select(SUBAREA, year, size, wgt.g.lbar.yrt)
head(lbar.currentyr)

### THIS IS FOR NEXT YEAR LBAR (i.e. SHF.pred field)
## prep lbar for being new data for predictions 
lbar.nextyr$Log.HEIGHT.CTR <- log(lbar.nextyr$lbar.pred ) -  mean(test.data$Log.HEIGHT)
lbar.nextyr$Log.DEPTH.CTR <- log(abs(lbar.nextyr$MeanDepth_m)) - mean(test.data$Log.DEPTH)
head(lbar.nextyr)

#predictions for meat weight based on lbar of current year
lbar.nextyr$wgt.g.lbar.yrt <- predict(model.object.Y, newdata =lbar.nextyr, re.form = NA, type = "response") 
head(lbar.nextyr)

lbar.nextyr <- lbar.nextyr %>% select(SUBAREA, year.pred, size, wgt.g.lbar.yrt)
head(lbar.nextyr)

### Percent change in commercial meat weight based on assumed shell height growth (from VonB) and assuming no change in condition from 2024 to 2025 ####
wgt.lbar.2024 <- lbar.currentyr %>% filter(year == 2024)

wgt.lbar.2025 <- lbar.nextyr %>% filter(year.pred == 2025)
names(wgt.lbar.2025)[4] <- "wgt.g.labr.yrtplust1"

wgt.2024to2025 <- merge(wgt.lbar.2024, wgt.lbar.2025, by = c("SUBAREA", "size"))
wgt.2024to2025$prop.chage <- (wgt.2024to2025$wgt.g.labr.yrtplust1 - wgt.2024to2025$wgt.g.lbar.yrt)/wgt.2024to2025$wgt.g.lbar.yrt
wgt.2024to2025$ratio.prop.change <- wgt.2024to2025$prop.chage+1
wgt.2024to2025

write.csv(wgt.2024to2025, paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/expected.prop.change.by.growthonly.csv"))


## Bump stratified numbers by tow to area of medium and high area 
# scale by q for absolute biomass 
# q from models: 
# B median 0.34
# C median 0.22
# D median 0.34

q <- 0.3
tow.area.km2 <- 800*5.334*1e-6

#Nh 
A.Nh <- A.areas$area[A.areas$area.name == "med"]/tow.area.km2
B.Nh <- B.areas$area[B.areas$area.name == "medhigh"]/tow.area.km2
C.Nh <- C.areas$area[C.areas$area.name == "medhigh"]/tow.area.km2
D.Nh <- D.areas$area[D.areas$area.name == "medhigh"]/tow.area.km2


### A ####

#have biomass for survey years -- now calculate biomass for next year; use predicted lbar weight to get biomass, then kill off with assumed mortality, then add in R biomass after kill some off 

a.numbers <- numbers %>% filter(SUBAREA == "SFA29A")
a.numbers$pop <- a.numbers$yst * A.Nh
#scale by q 
a.numbers$pop <- a.numbers$pop/q

head(a.numbers)
head(lbar.currentyr)
dim(a.numbers)
a.numbers <- merge(a.numbers, lbar.currentyr, by = c("year", "SUBAREA", "size") )
dim(a.numbers)
head(a.numbers)
a.numbers$biomass.kg <- (a.numbers$pop*a.numbers$wgt.g.lbar.yrt)/1000  
head(a.numbers)  
a.numbers$biomass.mt <- a.numbers$biomass.kg/1000
tail(a.numbers)  

# current year biomass estimates 
current.yr.biomass.a <- a.numbers %>% filter(year == 2024)
current.yr.biomass.a

# grow up biomass based on mt wt - SH & vonB expectations 
current.yr.biomass.a
gain.growth.a <- wgt.2024to2025 %>% filter(SUBAREA == "SFA29A" ) %>% select(year, SUBAREA, size, ratio.prop.change)
current.yr.biomass.a <- merge(current.yr.biomass.a, gain.growth.a, by = c("year", "SUBAREA", "size"))
current.yr.biomass.a$biomass.mt.tplus1 <- current.yr.biomass.a$biomass.mt * current.yr.biomass.a$ratio.prop.change

## percent increase in commerical biomass expected from recruitment after growth (before any mortality applied)
(current.yr.biomass.a$biomass.mt.tplus1[current.yr.biomass.a$size == "recruit"] / current.yr.biomass.a$biomass.mt.tplus1[current.yr.biomass.a$size == "commercial"]) * 100
# 0.9983808


## Apply assumed mortality; assume 15% 
m <- 0.15

current.yr.biomass.a$biomass.mt.tplus1minusM <- current.yr.biomass.a$biomass.mt.tplus1 - (current.yr.biomass.a$biomass.mt.tplus1 * m)

# add recruitment to commercial biomass 
current.yr.biomass.a$biomass.mt.tplus1minusM.final[current.yr.biomass.a$size == "commercial"] <- current.yr.biomass.a$biomass.mt.tplus1minusM[current.yr.biomass.a$size == "commercial"] + current.yr.biomass.a$biomass.mt.tplus1minusM[current.yr.biomass.a$size == "recruit"]

current.yr.biomass.a$Biomass.change.mt.24to25 <-  current.yr.biomass.a$biomass.mt.tplus1minusM.final - current.yr.biomass.a$biomass.mt
current.yr.biomass.a
#expect decline of  -7.120376 mt 


#as percent decline from 2024 value  
(current.yr.biomass.a$Biomass.change.mt.24to25/ current.yr.biomass.a$biomass.mt)*100
#-8.340631 % 

#removals 
catch.2025 <- c(0,10,20,30,40,50)
catch.2025/current.yr.biomass.a$biomass.mt.tplus1minusM.final[1]
# 0.0000000 0.1277965 0.2555931 0.3833896 0.5111861 0.6389827


### B ####

#have biomass for survey years -- now calculate biomass for next year; use predicted lbar weight to get biomass, then kill off with assumed mortality, then add in R biomass after kill some off 

b.numbers <- numbers %>% filter(SUBAREA == "SFA29B")
b.numbers$pop <- b.numbers$yst * B.Nh
#scale by q 
b.numbers$pop <- b.numbers$pop/q

head(b.numbers)
head(lbar.currentyr)
dim(b.numbers)
b.numbers <- merge(b.numbers, lbar.currentyr, by = c("year", "SUBAREA", "size") )
dim(b.numbers)
head(b.numbers)
b.numbers$biomass.kg <- (b.numbers$pop*b.numbers$wgt.g.lbar.yrt)/1000  
head(b.numbers)  
b.numbers$biomass.mt <- b.numbers$biomass.kg/1000
tail(b.numbers)  

# current year biomass estimates 
current.yr.biomass.b <- b.numbers %>% filter(year == 2024)
current.yr.biomass.b

# grow up biomass based on mt wt - SH & vonB expectations 
current.yr.biomass.b
gain.growth.b <- wgt.2024to2025 %>% filter(SUBAREA == "SFA29B" ) %>% select(year, SUBAREA, size, ratio.prop.change)
current.yr.biomass.b <- merge(current.yr.biomass.b, gain.growth.b, by = c("year", "SUBAREA", "size"))
current.yr.biomass.b$biomass.mt.tplus1 <- current.yr.biomass.b$biomass.mt * current.yr.biomass.b$ratio.prop.change

## percent increase in commerical biomass expected from recruitment after growth (before any mortality applied)
(current.yr.biomass.b$biomass.mt.tplus1[current.yr.biomass.b$size == "recruit"] / current.yr.biomass.b$biomass.mt.tplus1[current.yr.biomass.b$size == "commercial"]) * 100
# 0.2166036


## Apply assumed mortality; assume 15% 
m <- 0.15

current.yr.biomass.b$biomass.mt.tplus1minusM <- current.yr.biomass.b$biomass.mt.tplus1 - (current.yr.biomass.b$biomass.mt.tplus1 * m)

# add recruitment to commercial biomass 
current.yr.biomass.b$biomass.mt.tplus1minusM.final[current.yr.biomass.b$size == "commercial"] <- current.yr.biomass.b$biomass.mt.tplus1minusM[current.yr.biomass.b$size == "commercial"] + current.yr.biomass.b$biomass.mt.tplus1minusM[current.yr.biomass.b$size == "recruit"]

current.yr.biomass.b$Biomass.change.mt.24to25 <-  current.yr.biomass.b$biomass.mt.tplus1minusM.final - current.yr.biomass.b$biomass.mt
current.yr.biomass.b
#expect decline of    -47.20042 mt 

#as percent decline from 2024 value  
(current.yr.biomass.b$Biomass.change.mt.24to25/ current.yr.biomass.b$biomass.mt)*100
#-5.825991 % 

#removals 
catch.2025 <- c(0,10,20,30,40, 50)
catch.2025/current.yr.biomass.b$biomass.mt.tplus1minusM.final[1]
#  0.00000000 0.01310669 0.02621337 0.03932006 0.05242674 0.06553343





### C ####

#have biomass for survey years -- now calculate biomass for next year; use predicted lbar weight to get biomass, then kill off with assumed mortality, then add in R biomass after kill some off 

c.numbers <- numbers %>% filter(SUBAREA == "SFA29C")
c.numbers$pop <- c.numbers$yst * C.Nh
#scale by q 
c.numbers$pop <- c.numbers$pop/q

head(c.numbers)
head(lbar.currentyr)
dim(c.numbers)
c.numbers <- merge(c.numbers, lbar.currentyr, by = c("year", "SUBAREA", "size") )
dim(c.numbers)
head(c.numbers)
c.numbers$biomass.kg <- (c.numbers$pop*c.numbers$wgt.g.lbar.yrt)/1000  
head(c.numbers)  
c.numbers$biomass.mt <- c.numbers$biomass.kg/1000
tail(c.numbers)  

# current year biomass estimates 
current.yr.biomass.c <- c.numbers %>% filter(year == 2024)

# grow up biomass based on mt wt - SH & vonB expectations 
current.yr.biomass.c
gain.growth.c <- wgt.2024to2025 %>% filter(SUBAREA == "SFA29C" ) %>% select(year, SUBAREA, size, ratio.prop.change)
current.yr.biomass.c <- merge(current.yr.biomass.c, gain.growth.c, by = c("year", "SUBAREA", "size"))
current.yr.biomass.c$biomass.mt.tplus1 <- current.yr.biomass.c$biomass.mt * current.yr.biomass.c$ratio.prop.change

## percent increase in commerical biomass expected from recruitment after growth (before any mortality applied)
(current.yr.biomass.c$biomass.mt.tplus1[current.yr.biomass.c$size == "recruit"] / current.yr.biomass.c$biomass.mt.tplus1[current.yr.biomass.c$size == "commercial"]) * 100
# 0.7761133


## Apply assumed mortality; assume 15% 
m <- 0.15

current.yr.biomass.c$biomass.mt.tplus1minusM <- current.yr.biomass.c$biomass.mt.tplus1 - (current.yr.biomass.c$biomass.mt.tplus1 * m)

# add recruitment to commercial biomass 
current.yr.biomass.c$biomass.mt.tplus1minusM.final[current.yr.biomass.c$size == "commercial"] <- current.yr.biomass.c$biomass.mt.tplus1minusM[current.yr.biomass.c$size == "commercial"] + current.yr.biomass.c$biomass.mt.tplus1minusM[current.yr.biomass.c$size == "recruit"]

current.yr.biomass.c$Biomass.change.mt.24to25 <-  current.yr.biomass.c$biomass.mt.tplus1minusM.final - current.yr.biomass.c$biomass.mt
current.yr.biomass.c
#expect decline of  -8.065164 mt 

#as percent decline from 2024 value  
(current.yr.biomass.c$Biomass.change.mt.24to25/ current.yr.biomass.c$biomass.mt)*100
#-4.936291  % 

#removals 
catch.2025 <- c(0,10,20,30,40, 50)
catch.2025/current.yr.biomass.c$biomass.mt.tplus1minusM.final[1]
#0.00000000 0.06438324 0.12876647 0.19314971 0.25753294 0.32191618





### D ####

#have biomass for survey years -- now calculate biomass for next year; use predicted lbar weight to get biomass, then kill off with assumed mortality, then add in R biomass after kill some off 

d.numbers <- numbers %>% filter(SUBAREA == "SFA29D")
d.numbers$pop <- d.numbers$yst * D.Nh
#scale by q 
d.numbers$pop <- d.numbers$pop/q

head(d.numbers)
head(lbar.currentyr)
dim(d.numbers)
d.numbers <- merge(d.numbers, lbar.currentyr, by = c("year", "SUBAREA", "size") )
dim(d.numbers)
head(d.numbers)
d.numbers$biomass.kg <- (d.numbers$pop*d.numbers$wgt.g.lbar.yrt)/1000  
head(d.numbers)  
d.numbers$biomass.mt <- d.numbers$biomass.kg/1000
tail(d.numbers)  

# current year biomass estimates 
current.yr.biomass.d <- d.numbers %>% filter(year == 2024)

# grow up biomass based on mt wt - SH & vonB expectations 
current.yr.biomass.d
gain.growth.d <- wgt.2024to2025 %>% filter(SUBAREA == "SFA29D" ) %>% select(year, SUBAREA, size, ratio.prop.change)
current.yr.biomass.d <- merge(current.yr.biomass.d, gain.growth.d, by = c("year", "SUBAREA", "size"))
current.yr.biomass.d$biomass.mt.tplus1 <- current.yr.biomass.d$biomass.mt * current.yr.biomass.d$ratio.prop.change

## percent increase in commerical biomass expected from recruitment after growth (before any mortality applied)
(current.yr.biomass.d$biomass.mt.tplus1[current.yr.biomass.d$size == "recruit"] / current.yr.biomass.d$biomass.mt.tplus1[current.yr.biomass.d$size == "commercial"]) * 100
# 0.1398434


## Apply assumed mortality; assume 15% 
m <- 0.15

current.yr.biomass.d$biomass.mt.tplus1minusM <- current.yr.biomass.d$biomass.mt.tplus1 - (current.yr.biomass.d$biomass.mt.tplus1 * m)

# add recruitment to commercial biomass 
current.yr.biomass.d$biomass.mt.tplus1minusM.final[current.yr.biomass.d$size == "commercial"] <- current.yr.biomass.d$biomass.mt.tplus1minusM[current.yr.biomass.d$size == "commercial"] + current.yr.biomass.d$biomass.mt.tplus1minusM[current.yr.biomass.d$size == "recruit"]

current.yr.biomass.d$Biomass.change.mt.24to25 <-  current.yr.biomass.d$biomass.mt.tplus1minusM.final - current.yr.biomass.d$biomass.mt
current.yr.biomass.d
#expect decline of -32.58199 mt 

#as percent decline from 2024 value  
(current.yr.biomass.d$Biomass.change.mt.24to25/ current.yr.biomass.d$biomass.mt)*100
#-9.485427 % 

#removals 
catch.2025 <- c(0,10,20,30,40, 50)
catch.2025/current.yr.biomass.d$biomass.mt.tplus1minusM.final[1]
#0.00000000 0.03216331 0.06432662 0.09648993 0.12865324 0.16081655




##### Relative Exploitation from stratified survey number converted to weights using lbar and avg meat weight, combined with landings #####
# Since survey is right after the fishery 
# Mu = (biomass in year t + catch in year t)/ catch in year t

## import landings by subarea 
landings <- read.csv("Y:/Inshore/SFA29/2025/Assessment/Data/CommercialData/SFA29_totalLandings_YearSubarea.csv")
landings$SUBAREA <- paste0("SFA",landings$Area )
landings$year <- landings$YEAR

options(scipen = 999)

### A ####
head(a.numbers)
a.numbers.comm <- a.numbers %>% filter(size == "commercial")
landings.a <- merge(a.numbers.comm, landings, by = c("SUBAREA", "year") )
head(landings.a )
landings.a$mu <- landings.a$Landingsmt / (landings.a$biomass.mt + landings.a$Landingsmt)
landings.a
landings.a$Subarea <- "Subarea A"

### B ####
head(b.numbers)
b.numbers.comm <- b.numbers %>% filter(size == "commercial")
landings.b <- merge(b.numbers.comm, landings, by = c("SUBAREA", "year") )
head(landings.b )
landings.b$mu <- landings.b$Landingsmt / (landings.b$biomass.mt + landings.b$Landingsmt)
landings.b
landings.b$Subarea <- "Subarea B"


### C ####
head(c.numbers)
c.numbers.comm <- c.numbers %>% filter(size == "commercial")
landings.c <- merge(c.numbers.comm, landings, by = c("SUBAREA", "year") )
head(landings.c )
landings.c$mu <- landings.c$Landingsmt / (landings.c$biomass.mt + landings.c$Landingsmt)
landings.c
landings.c$Subarea <- "Subarea C"



### D ####
head(d.numbers)
d.numbers.comm <- d.numbers %>% filter(size == "commercial")
landings.d <- merge(d.numbers.comm, landings, by = c("SUBAREA", "year") )
head(landings.d )
landings.d$mu <- landings.d$Landingsmt / (landings.d$biomass.mt + landings.d$Landingsmt)
landings.d
landings.d$Subarea <- "Subarea D"


#merge all 
exploitation.atod <- rbind(landings.a, landings.b, landings.c, landings.d)

#LTM 
mu.LTM <- exploitation.atod %>% group_by(Subarea) %>% filter(year < 2024) %>% summarise(LTM = median(mu, na.rm = TRUE))
#Subarea      LTM
#<chr>      <dbl>
#1 Subarea A 0.0255
#2 Subarea B 0.0949
#3 Subarea C 0.175 
#4 Subarea D 0.105 

ggplot(data = exploitation.atod, aes(x = year, y = mu)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Subarea ) + 
  geom_hline(data = mu.LTM, aes(yintercept = LTM), color = "blue", linetype = "dashed") + 
  theme_bw(base_size = 12) + 
  ylab("Relative exploitation (proportion)") + 
  xlab("Year") + 
  xlim(c(2002,2026)) + 
  scale_x_continuous(breaks=seq(2002,2026,by=4))
  
ggsave(filename = "Y:/Inshore/SFA29/2025/Assessment/Figures/relative.exploitation.survey.png" , plot = last_plot(), scale = 2.5, width =8, height = 7, dpi = 300, units = "cm", limitsize = TRUE)





