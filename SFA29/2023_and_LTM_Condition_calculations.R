
library(tidyverse)
assessmentyear <- 2025 #year in which you are conducting the survey 
path.directory <- "Y:/Inshore/SFA29/"
surveyyear <- 2024 

#####Import condition file for SFA29W:######################################################################

SFA29.con.ts <- read.csv(paste0(path.directory, assessmentyear,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))
unique(SFA29.con.ts$STRATA)

#SFA29A median condition for the time series (up to 2022)
SFA29A.2001.2022 <- SFA29.con.ts |> filter(YEAR %in% c(2001:2019, 2021:(surveyyear-1))) |> filter(STRATA == "SFA29A") |>  summarise(median.con = median(CONDITION))
SFA29A.2001.2022 
# 10.4 in 2023
#10.46033 in 2024 

#SFA29B median condition for the time series (up to 2022)
SFA29B.2001.2022 <- SFA29.con.ts |> filter(YEAR %in% c(2001:2019, 2021:(surveyyear-1))) |> filter(STRATA == "SFA29B") |>  summarise(median.con = median(CONDITION))
SFA29B.2001.2022 
# 12.1 in 2023
# 12.15334 in 2024

#SFA29C median condition for the time series (up to 2022)
SFA29C.2001.2022 <- SFA29.con.ts |> filter(YEAR %in% c(2001:2019, 2021:(surveyyear-1))) |> filter(STRATA == "SFA29C") |>  summarise(median.con = median(CONDITION))
SFA29C.2001.2022 
#12.1 in 2023
# 12.2773 in 2024 

#SFA29D median condition for the time series (up to 2022)
SFA29D.2001.2022 <- SFA29.con.ts |> filter(YEAR %in% c(2001:2019, 2021:(surveyyear-1))) |> filter(STRATA == "SFA29D") |>  summarise(median.con = median(CONDITION))
SFA29D.2001.2022 
#12.1 in 2023
#12.1099 in 2024 

#SFA29E median condition for the time series (up to 2022)
SFA29E.2014.2022 <- SFA29.con.ts |> filter(YEAR %in% c(2014:2019, 2021:(surveyyear-1))) |> filter(STRATA == "SFA29E") |>  summarise(median.con = median(CONDITION))
SFA29E.2014.2022 
#10.8 in 2023
# 10.83043 in 2024 


#Compare 2024 to median

#Subarea A - 2023 condition is 38% higher than the LTM 
# 2024 condition is 5% lower than LTM 
((SFA29.con.ts |> filter(YEAR == surveyyear, STRATA == "SFA29A") |> dplyr::select(CONDITION) |> pull())/SFA29A.2001.2022)-1

#Subarea B - 2023 condition is 27% higher than the LTM 
# 2024 condition is 9% lower than LTM 
((SFA29.con.ts |> filter(YEAR == surveyyear, STRATA == "SFA29B") |> dplyr::select(CONDITION) |> pull())/SFA29B.2001.2022)-1

#Subarea C - 2023 condition is 32% higher than the LTM 
# 2024 condition is 5% lower than LTM 
((SFA29.con.ts |> filter(YEAR == surveyyear, STRATA == "SFA29C") |> dplyr::select(CONDITION) |> pull())/SFA29C.2001.2022)-1

#Subarea D - 2023 condition is 27% higher than the LTM 
# condition in 2024 was 9% lower than the LTM 
((SFA29.con.ts |> filter(YEAR == surveyyear, STRATA == "SFA29D") |> dplyr::select(CONDITION) |> pull())/SFA29D.2001.2022)-1

#Subarea E - 2023 condition is 35% higher than the LTM 
# condition in 2024 was 6% lower than the LTM 
((SFA29.con.ts |> filter(YEAR == surveyyear, STRATA == "SFA29E") |> dplyr::select(CONDITION) |> pull())/SFA29E.2014.2022)-1


### Condition for 2023 compared to last year ######
SFA29.con.ts |> filter(YEAR %in% c(2001:2019, 2021:(surveyyear-1)))

#Biggest differene?
SFA29.con.ts.diff.A <- SFA29.con.ts |> filter(STRATA == "SFA29A") |> mutate(diff = CONDITION - lag(CONDITION))
SFA29.con.ts.diff.A |> filter(diff == min(diff, na.rm = T)) 
#YEAR STRATA CONDITION      diff
#2014 SFA29A   8.27237 -2.731779 

SFA29.con.ts.diff.A |> filter(diff == max(diff, na.rm = T))
#YEAR STRATA CONDITION     diff
#2010 SFA29A  11.85988 2.839463 

SFA29.con.ts.diff.B <- SFA29.con.ts |> filter(STRATA == "SFA29B") |> mutate(diff = CONDITION - lag(CONDITION))
SFA29.con.ts.diff.B |> filter(diff == min(diff, na.rm = T)) 
#YEAR STRATA CONDITION     diff
#2014 SFA29B  10.99241 -2.46776
SFA29.con.ts.diff.B |> filter(diff == max(diff, na.rm = T))
#YEAR STRATA CONDITION     diff
#2023 SFA29B   15.4115 3.164393

SFA29.con.ts.diff.C <- SFA29.con.ts |> filter(STRATA == "SFA29C") |> mutate(diff = CONDITION - lag(CONDITION))
SFA29.con.ts.diff.C |> filter(diff == min(diff, na.rm = T)) 
#YEAR STRATA CONDITION     diff
#2014 SFA29C  10.79337 -3.52407
SFA29.con.ts.diff.C |> filter(diff == max(diff, na.rm = T))
#YEAR STRATA CONDITION     diff
#2023 SFA29C  15.94869 3.389707

#YEAR STRATA CONDITION        diff
#2013 SFA29C  14.31744  0.55797000
#2014 SFA29C  10.79337 -3.52407000

1-(10.79337/14.31744) #Decrease by 24%

SFA29.con.ts.diff.D <- SFA29.con.ts |> filter(STRATA == "SFA29D") |> mutate(diff = CONDITION - lag(CONDITION))
SFA29.con.ts.diff.D |> filter(diff == min(diff, na.rm = T)) 
#YEAR STRATA CONDITION     diff
#2014 SFA29D  10.96494 -2.41715
SFA29.con.ts.diff.D |> filter(diff == max(diff, na.rm = T))
#YEAR STRATA CONDITION     diff
#2023 SFA29D  15.36853 3.146495

#############################################################################################

biomass <- read.csv("/Inshore/SFA29/2024/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to2023.Commercial.Weight.csv")

biomass.A <- biomass %>% filter(SUBAREA == "SFA29A", Strata == "med") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)
biomass.B <- biomass %>% filter(SUBAREA == "SFA29B", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)
biomass.C <- biomass %>% filter(SUBAREA == "SFA29C", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)
biomass.D <- biomass %>% filter(SUBAREA == "SFA29D", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)

### Biomass (indices) for 2023 compared to last year ######

biomass.A |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, I))

#YEAR        I
#1 2022 2.310543
#2 2023 4.390289

1-(2.310543/4.390289) #Increased by 47%

biomass.B |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, I))

#YEAR        I
#1 2022 3.819432
#2 2023 4.981909

1-(3.819432/4.981909) #increased by 23%

biomass.C |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, I))

#YEAR        I
#1 2022 4.318221
#2 2023 9.278030

1-(4.318221/9.278030) #increased by 53%

biomass.D |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, I))

#YEAR        I
#1 2022 4.883019
#2 2023 9.182574

1-(4.883019/9.182574) #increased by 47%

#############################################################################################

abundance <- read.csv("/Inshore/SFA29/2024/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to2023.Numbers.csv")

abundance.A <- abundance %>% filter(SUBAREA == "SFA29A", Strata == "med", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)
abundance.B <- abundance %>% filter(SUBAREA == "SFA29B", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)
abundance.C <- abundance %>% filter(SUBAREA == "SFA29C", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)
abundance.D <- abundance %>% filter(SUBAREA == "SFA29D", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)

### Biomass (indices) for 2023 compared to last year ######

abundance.A |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, N))

#YEAR         N
#2022  88.42857
#2023 100.90000

1-(88.42857/100.90000) #Increased by 12%

abundance.B |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, N))

#YEAR        N
#2022 174.5400
#2023 143.4333

1-(174.5400/143.4333) #decreased by 22%

abundance.C |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, N))

#YEAR        N
#2022 210.5750
#2023 283.8375

1-(210.5750/283.8375) #increased by 26%

abundance.D |> filter(YEAR %in% c(2022, 2023)) |> 
  select(c(YEAR, N))

#YEAR        N
#2022 181.0538
#2023 200.7455

1-(181.0538/200.7455) #increased by 9%
