#2016 Holistic sample data exploration for SFA 29W

#libraries
library(ROracle)
library(sf)
require (tidyverse)
require(lubridate)
library(mapview)
library(lattice)
library(ggcorrplot)
library(GGally)
library(mgcv)
library(lme4)
library(nlme)

#functions
#### Import Mar-scal functions 
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
# Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

source("Z:/Projects/Holistic_sampling_with_HGS/TechReport_SPA3/Pr_Frac_function.R") #Plotting residuals - found on Sky
source("Y:/Admin/Request_and_Review_Tracking/Aquaculture_Reviews/2023/St. Mary's Bay/HighstatLibV13.R")

#credentials
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#Get SFA29W strata from Github:
temp <- tempfile() # Find where tempfiles are stored
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
temp2 <- tempfile()# Figure out what this file was saved as
unzip(zipfile=temp, exdir=temp2) #unzip it

# Now read in the shapefiles
sfa29.poly <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% 
  st_transform(crs = 4326)
sfa29strata <- st_read(paste0(temp2, "/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp"))



#Data Query from ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Db Query:
quer <- paste(
"SELECT cruise, tow_no, strata_id, tow_date, start_lat, start_long, depth, geophys_id, bottom_temp, 0 gear_id, scweightanalysis.shell_no shell_no, sex_id, mat_id,  wet_meat_wgt, wet_tissue_wgt, wet_gonad_wgt, MAX(age_ring) age, tot_hgt height, NULL meat_colour_id, NULL myco_infected ",
"FROM scallsur.sctows, scallsur.scweightanalysis, scallsur.scageanalysis ", 
"WHERE sctows.tow_seq = scweightanalysis.tow_seq ",
"AND scweightanalysis.tow_seq = scageanalysis.tow_seq ",
"AND scweightanalysis.shell_no = scageanalysis.shell_no ",
"AND CRUISE = 'SFA292016' ",
"GROUP BY cruise, tow_no, strata_id, tow_date, start_lat, start_long, depth, geophys_id, bottom_temp, 0, scweightanalysis.shell_no, wet_meat_wgt, wet_tissue_wgt, wet_gonad_wgt, sex_id, mat_id, tot_hgt ", sep="")

ScallopSurv <- dbGetQuery(chan, quer)

# Data exploration --------------------------------------------------------

str(ScallopSurv)
summary(ScallopSurv)
#Note: Tow_no 11 has 10 samples, but shell number 11, no shell 10.
#ScallopSurv |> filter(SHELL_NO == 11)
#ScallopSurv |> filter(TOW_NO == 11)

#Summarize sample ID.
sum.data <- ScallopSurv %>% 
  dplyr::select(TOW_NO, SHELL_NO) %>%
  group_by(TOW_NO) %>% 
  dplyr::summarise(n = n()) 

range(sum.data$n) #number of samples per tow range from 9-10
table(sum.data$n) # 2 tows had 9 samples, and 54 tows had 10 samples
sum(sum.data$n) # total of 558 samples

# Number of samples per tow, strata ID and sex:
table(ScallopSurv$TOW_NO)
table(ScallopSurv$STRATA_ID)
table(ScallopSurv$SEX_ID)
table(ScallopSurv$MAT_ID,ScallopSurv$TOW_DATE)

#What is the range of shell heights sampled?
range(ScallopSurv$HEIGHT) #min 72 mm, max 160 mm.


# Formatting Data -------------------------------------------------------------

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) #Convert to DD

# create character sex_id column "SEX"
ScallopSurv <- ScallopSurv %>% 
  mutate(SEX = case_when(SEX_ID=="0"~ "Unknown",
                         SEX_ID=="1"~ "Male",
                         SEX_ID=="2"~ "Female",
                         TRUE ~ as.character(NA)))

# create character Maturity column
ScallopSurv <- ScallopSurv %>% 
  mutate(MATURITY = case_when(MAT_ID=="1"~ "Immature",
                         MAT_ID=="2"~ "Recovering",
                         MAT_ID=="3"~ "Ripening",
                         MAT_ID=="4"~ "Ripe",
                         MAT_ID=="5"~ "Spawning",
                         MAT_ID=="6"~ "Spent",
                         TRUE ~ as.character(NA)))

#Assign strata name
ScallopSurv <- ScallopSurv %>% 
  mutate(STRATA_ID = case_when(STRATA_ID=="41"~ "A",
                               STRATA_ID=="42"~ "B",
                               STRATA_ID=="43"~ "C",
                               STRATA_ID=="44"~ "D",
                               STRATA_ID=="45"~ "E"))

#simplify names
ScallopSurv <- ScallopSurv |> 
  dplyr::rename(GONAD = WET_GONAD_WGT) |> 
  dplyr::rename(MUSCLE = WET_MEAT_WGT)

#Standardize height, temperature, and depth
ScallopSurv$Log.Height <- log(ScallopSurv$HEIGHT)
ScallopSurv$Log.Height.std <- ScallopSurv$Log.Height - mean(ScallopSurv$Log.Height)

ScallopSurv$Log.Depth <- log(ScallopSurv$DEPTH )
ScallopSurv$Log.Depth.std <- ScallopSurv$Log.Depth - mean(ScallopSurv$Log.Depth)

ScallopSurv$Log.BTemp <- log(ScallopSurv$BOTTOM_TEMP)
ScallopSurv$Log.BTemp.std <- ScallopSurv$Log.BTemp - mean(ScallopSurv$Log.BTemp)

# Convert to Spatial data:
ScallopSurv.sf <- ScallopSurv |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326)
mapview(ScallopSurv.sf, zcol = "STRATA_ID")+
  mapview(sfa29.poly)
#st_write(ScallopSurv.sf, "Z:/Projects/Holistic_sampling_SFA29W/2016/SFA292016_surveytows_shp/SFA292016_towlocations.shp")

# Outliers  --------------------------------------------------------

MyVar <- c("HEIGHT", "GONAD", "DEPTH", "lat", "lon", "MUSCLE", "BOTTOM_TEMP")
Mydotplot(ScallopSurv[, MyVar])
#Gonad outlier >30?

# Colinearity  --------------------------------------------------------

#Function for plotting variables + colinearity
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=1) +
    geom_smooth(color = 'black', method='lm', linewidth=1,...)
  p
}

g <- ggpairs(data = ScallopSurv %>% dplyr::select(all_of(MyVar)),
  lower = list( continuous =  wrap(lowerFn)),
  upper = list(continuous = wrap("cor", size = 4)), progress = F)
g <- g + theme( axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95"))
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)

# Exploratory plots  --------------------------------------------------------
mycols.1 <- c("cyan4","orangered3","seagreen3", "steelblue4", "magenta4")#, "lightgoldenrod2",  "darkseagreen3", "sandybrown" , "peachpuff3", "grey44", "tomato4",  "maroon4", "olivedrab3", "aquamarine3")

p <- pecjector(area = "sfa29",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar=c('tl',0.5,-1,-1)))

p +
  geom_sf(data = ScallopSurv.sf, aes(colour = STRATA_ID, size = 1))+
  scale_colour_manual(values = mycols.1)+
  coord_sf(xlim = c(-66.50,-65.45), ylim = c(43.10,43.80), expand = FALSE)


#Height distribution (were samples randomly selected above 70mm?):
ggplot(ScallopSurv, aes(x=HEIGHT)) +
  geom_histogram(position="dodge",colour = "black")+
  facet_wrap(~TOW_NO)

#Muscle and Sex boxplot - does it vary between sex? Strata?
ggplot() + geom_boxplot(data = ScallopSurv, 
                        aes(y = MUSCLE, x = SEX, fill = SEX))+
  xlab("Sex") + ylab("Muscle weight (g)")+
  scale_fill_manual(values=c("salmon", "lemonchiffon", "grey"))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)
#not really

#Gonad and Sex boxplot - does it vary between sex? Strata?
ggplot() + geom_boxplot(data = ScallopSurv, 
                        aes(y = GONAD, x = SEX, fill = SEX))+
  xlab("Sex") + ylab("Gonad weight (g)")+
  scale_fill_manual(values=c("salmon", "lemonchiffon", "grey"))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)
#no, not really.

#Muscle and Sex boxplot - does it vary between sex? Strata?
ggplot() + geom_boxplot(data = ScallopSurv, 
                        aes(y = MUSCLE, x = SEX, fill = SEX))+
  xlab("Sex") + ylab("Muscle weight (g)")+
  scale_fill_manual(values=c("salmon", "lemonchiffon", "grey"))+
  theme(text = element_text(size=15))+
  facet_wrap(~MATURITY)

#Gonad and Sex boxplot - does it vary between sex? Strata?
ggplot() + geom_boxplot(data = ScallopSurv, 
                        aes(y = GONAD, x = SEX, fill = SEX))+
  xlab("Sex") + ylab("Gonad weight (g)")+
  scale_fill_manual(values=c("salmon", "lemonchiffon", "grey"))+
  theme(text = element_text(size=15))+
  facet_wrap(~MATURITY)

#Muscle Weight and Gonad weight
ggplot() + geom_point(data = ScallopSurv, aes(y = GONAD, x = MUSCLE))+
  xlab("Meat weight") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = GONAD, x = MUSCLE),se = FALSE)

#Gonad Weight and Height by STRATA_ID
ggplot() + geom_point(data = ScallopSurv, aes(y = GONAD, x = HEIGHT))+
  xlab("Shell Height") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = GONAD, x = HEIGHT),se = FALSE)+
  facet_wrap(~STRATA_ID)

#Gonad Weight and Height by tow
#ggplot() + geom_point(data = ScallopSurv, aes(y = GONAD, x = HEIGHT))+
#  xlab("Shell Height") + ylab("Gonad weight (g)")+
#  theme(text = element_text(size=15))+
#  geom_smooth(data = ScallopSurv, aes(y = GONAD, x = HEIGHT),
#              method = "lm",
#              se = FALSE)+
#  facet_wrap(~TOW_NO)

#Meat Weight and Height by STRATA_ID
ggplot() + geom_point(data = ScallopSurv, aes(y = MUSCLE, x = HEIGHT))+
  xlab("Shell Height") + ylab("Muscle weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = MUSCLE, x = HEIGHT), se = FALSE)+
  facet_wrap(~STRATA_ID)

#Gonad Weight and Depth by Strata
ggplot() + geom_point(data = ScallopSurv, aes(x = DEPTH, y = GONAD))+#, colour = STRATA_ID
  xlab("Depth (m)") + ylab("Gonad weight (g)")+
  geom_smooth(data = ScallopSurv, aes(x = DEPTH, y = GONAD),
              method = "lm",
              se = FALSE)+
  #scale_colour_manual(values=c(mycols.1))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)

#Gonad Weight and Bottom_temp by Strata - For D, temp and depth vary more
ggplot() + geom_point(data = ScallopSurv, aes(x = BOTTOM_TEMP, y = GONAD))+#, colour = STRATA_ID
  xlab("Bottom Temp (C)") + ylab("Gonad weight (g)")+
  geom_smooth(data = ScallopSurv, aes(x = BOTTOM_TEMP, y = GONAD),
              method = "lm",
              se = FALSE)+
  #scale_colour_manual(values=c(mycols.1))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)

#Meat Weight and Bottom Temp
ggplot() + geom_point(data = ScallopSurv, aes(x = BOTTOM_TEMP, y = MUSCLE))+#, colour = STRATA_ID
  xlab("Bottom Temp (C)") + ylab("Muscle weight (g)")+
  geom_smooth(data = ScallopSurv, aes(x = BOTTOM_TEMP, y = MUSCLE),
              method = "lm",
              se = FALSE)+
  #scale_colour_manual(values=c(mycols.1))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)

#Meat Weight and Depth
ggplot() + geom_point(data = ScallopSurv, aes(x = DEPTH, y = MUSCLE))+#, colour = STRATA_ID
  xlab("Depth (m)") + ylab("Muscle weight (g)")+
  geom_smooth(data = ScallopSurv, aes(x = DEPTH, y = MUSCLE),
              method = "lm",
              se = FALSE)+
  #scale_colour_manual(values=c(mycols.1))+
  theme(text = element_text(size=15))+
  facet_wrap(~STRATA_ID)

#Meat Weight and Height by Sex
ggplot() + geom_point(data = ScallopSurv, aes(y = MUSCLE, x = HEIGHT))+
  xlab("Shell Height") + ylab("Meat weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = MUSCLE, x = HEIGHT),
              method = "lm",
              se = FALSE)+
  facet_wrap(~SEX)

#Gonad Weight and Height by Sex
ggplot() + geom_point(data = ScallopSurv, aes(y = GONAD, x = HEIGHT))+
  xlab("Shell Height") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = GONAD, x = HEIGHT),
              method = "lm",
              se = FALSE)+
  facet_wrap(~SEX)

#Gonad weight and shell height by trends tow (random intercept, random slope maybe?)
ggplot()+
  geom_point(data = ScallopSurv, 
             aes(y = GONAD, x = HEIGHT),
             shape = 1, 
             size = 1)+
  xlab("Shell Height") + ylab("Gonad weight (g)")+
  theme(text = element_text(size=15))+
  geom_smooth(data = ScallopSurv, aes(y = GONAD, x = HEIGHT, group = TOW_NO), method = "lm",se = FALSE)



