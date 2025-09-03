library(ggplot2)
library(tidyverse)

#### Import Mar-scal functions
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#Histogram - biomass distribution by Habitat

ScallopSurv.kg <- read.csv("Y:/Inshore/SFA29/2025/Assessment/Data/SurveyIndices/SFA29liveweight2024.csv") |> filter(YEAR == 2024) |> arrange(TOW_NO)
head(ScallopSurv.kg)
ScallopSurv.kg <- ScallopSurv.kg %>% dplyr::select(-X) %>%  #removes index column X
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  unite(ID, c("CRUISE", "TOW_NO"), sep = ".", remove = FALSE) %>% 
  mutate(com.bm = dplyr::select(., BIN_ID_100:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - >=100mm; BINS 100 to 195
  mutate(rec.bm = dplyr::select(., BIN_ID_90:BIN_ID_95) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - 90-99; BINS 90 to 95
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_85) %>% rowSums(na.rm = TRUE) %>% round(0)/1000)


sdm <- read.csv("Y:/Inshore/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv") |> filter(CRUISE == "SFA292024") |> arrange(TOW_NO)

dim(sdm)
dim(ScallopSurv.kg)

head(sdm)
sdm <- sdm %>% select(CRUISE, TOW_NO, SDM)
head(ScallopSurv.kg)
ScallopSurv.kg <- merge(ScallopSurv.kg, sdm, by = c("CRUISE", "TOW_NO"))

head(ScallopSurv.kg)

# histogram for SFA29C
pC <- ggplot(ScallopSurv.kg |> filter(STRATA_ID == 43), aes(x=com.bm)) + 
  geom_histogram()+
  facet_wrap(~SDM)
pC



# histogram for SFA29D
pD <- ggplot(ScallopSurv.kg |> filter(STRATA_ID == 44), aes(x=com.bm)) + 
  geom_histogram()+
  facet_wrap(~SDM)
pD


