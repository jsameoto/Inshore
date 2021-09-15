
require (lubridate)
require (tidyverse)
require (sf)
library(ROracle)
library(readr)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", "WILSONBR")

#set years
prev.surv.year <- 2019
survey.year <- 2021
area <- "GM" #"GM", "BI", "BF"

path.directory <- "Y:/INSHORE SCALLOP/BoF/"

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#Query for live scallop tow data:
quer1 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)

#Query for repeated tow reference data
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.sccomparisontows s			",
  sep=""
)

#### Import Mar-scal functions 
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

# List of tows that are good options for repeats (i.e. numbers of commercial sized scallops >0 and tow type != 3 (exploratory))  ----------------

#Tow data
ScallopSurv <- dbGetQuery(chan, quer1)
ScallopSurv  <- ScallopSurv[,1:51]

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(rec.com.sum = dplyr::select(., BIN_ID_65:BIN_ID_195) %>% rowSums(na.rm = FALSE) %>% round(0)) %>%  #sum of recruit and commercial sized scallops
  mutate(com.sum = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = FALSE) %>% round(0)) %>%  #sum of commercial sized scallops
  mutate(rec.sum = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = FALSE) %>% round(0)) #sum of recruit sized scallops

table(ScallopSurv$year)
summary(ScallopSurv)

#Filter only data from previous survey year - i.e. containing tows that are going to be selected for repeating in the current year.
repeat.options <- ScallopSurv %>%
  filter(CRUISE == paste0(area, prev.surv.year)) %>% #look at only cruise and year in question
  filter(TOW_TYPE_ID %in% c(1,5)) %>%  #i.e. NOT an exploratory tow (TOW_TYPE_ID == 3)
  filter(com.sum != 0) #filter all tows where the total number of commercial sized scallops are not zero.

summary(repeat.options)

#Tows where there are no commercial sized scallops:
#tows.zero <- ScallopSurv %>% 
#  filter(com.sum == 0) %>% 
  
# Checks - sampling with partial replacement - fails if 0s occur across repeat tows  ------------------------------------------------------

# Repeat Tow information
repeated.tow <- dbGetQuery(chan, quer2)
str(repeated.tow)

repeated.tow <- repeated.tow %>% 
  mutate(year = parse_number(CRUISE)) %>% 
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  filter(CRUISE == paste0(area, survey.year)) # Select current year and reference year

repeated.tow <- merge(ScallopSurv, repeated.tow, by = "ID") 

#What tows have no commercial sized scallops?
repeated.tow %>% 
  filter(com.sum == 0)
