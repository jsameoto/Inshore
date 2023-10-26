###...............................................###
###  Compile Commercial Landings for CSAS Table  ###
###             All BoF Areas                    ###
###       J. Sameoto Nov 2021                    ###
###..............................................###

options(stringsAsFactors=FALSE)
library(tidyverse)
library(openxlsx)
library(stringr)

#DEFINE:
direct <- "Y:/Inshore/BoF"
assessmentyear <- 2023 #year in which you are conducting the assessment 
surveyyear <- 2023  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 

#sources landings data by area 
dat.1A <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1A_TACandLandings_",surveyyear,".xlsx"),sheet = "TACandLandings")
dat.1B <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_",surveyyear,".xlsx"),sheet = "TACandLandings")
dat.3 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA3_TACandLandings_",surveyyear,".xlsx"),sheet = "TACandLandings")
dat.4and5 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA4and5_TACandLandings_",surveyyear,".xlsx"),sheet = "TACandLandings")
dat.6 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA6_TACandLandings_",surveyyear,".xlsx"),sheet = "TACandLandings")

##ADD FSC to the filter for any areas that have FSC rows, if not, they won't be included. We will deal with this further down.
dat.1A$Year[dat.1A$Year == "Full Bay"] <- c("Landings", "FSC")
dat.1B <- dat.1B %>% filter(Year %in% c("Total", "TAC", "FSC"))  
dat.1B$Year[dat.1B$Year == "Total"] <- c("Landings", "FSC")
dat.3 #all rows incl. FSC if added to .xlsx
dat.4and5 #all rows incl. FSC if added to .xlsx
dat.6 <- dat.6 %>% filter(Year %in% c("Total", "TAC", "FSC"))  #FSC in 2023
dat.6$Year[dat.6$Year == "Total"] <-  c("Landings", "FSC")

#flip to long format, create year column, and tidy up 
#.1A. 
dat.1A.long <- as.data.frame(t(dat.1A)) 
dat.1A.long$Year <- row.names.data.frame(dat.1A.long)
names(dat.1A.long) <-  dat.1A.long[1,]
dat.1A.long <- dat.1A.long[-1,]
rownames(dat.1A.long) <- c()
dat.1A.long$Landings <- as.numeric(dat.1A.long$Landings)
dat.1A.long$TAC <- as.numeric(dat.1A.long$TAC)
dat.1A.long$SPA <- "1A"
#If FSC column does not exists, this line adds it with NAs
if(!'FSC' %in% names(dat.1A.long)) dat.1A.long <- dat.1A.long %>% add_column(FSC = NA)
dat.1A.long

#.1B. 
dat.1B.long <- as.data.frame(t(dat.1B)) 
dat.1B.long$Year <- row.names.data.frame(dat.1B.long)
names(dat.1B.long) <-  dat.1B.long[1,]
dat.1B.long <- dat.1B.long[-1,]
rownames(dat.1B.long) <- c()
dat.1B.long$Landings <- as.numeric(dat.1B.long$Landings)
dat.1B.long$TAC <- as.numeric(dat.1B.long$TAC)
dat.1B.long$SPA <- "1B"
#If FSC column does not exists, this line adds it with NAs
if(!'FSC' %in% names(dat.1B.long)) dat.1B.long <- dat.1B.long %>% add_column(FSC = NA)
dat.1B.long

#.3. 
dat.3.long <- as.data.frame(t(dat.3)) 
dat.3.long$Year <- row.names.data.frame(dat.3.long)
names(dat.3.long) <-  dat.3.long[1,]
dat.3.long <- dat.3.long[-1,]
rownames(dat.3.long) <- c()
dat.3.long$Landings <- as.numeric(dat.3.long$Landings)
dat.3.long$TAC <- as.numeric(dat.3.long$TAC)
dat.3.long$SPA <- "3"
#If FSC column does not exists, this line adds it with NAs
if(!'FSC' %in% names(dat.3.long)) dat.3.long <- dat.3.long %>% add_column(FSC = NA)
dat.3.long

#.4and5. 
dat.4and5.long <- as.data.frame(t(dat.4and5)) 
dat.4and5.long$Year <- row.names.data.frame(dat.4and5.long)
names(dat.4and5.long) <-  dat.4and5.long[1,]
dat.4and5.long <- dat.4and5.long[-1,]
rownames(dat.4and5.long) <- c()
dat.4and5.long$SPA4 <- as.numeric(dat.4and5.long$SPA4)
dat.4and5.long$SPA5 <- as.numeric(dat.4and5.long$SPA5)
dat.4and5.long$TAC <- as.numeric(dat.4and5.long$TAC)
dat.4and5.long$Landings <- dat.4and5.long$SPA4 + dat.4and5.long$SPA5
dat.4and5.long$SPA <- "4&5"
dat.4and5.long <- dat.4and5.long %>% dplyr::select(Landings,TAC,Year,SPA) #add FSC here if exists.
if(!'FSC' %in% names(dat.4and5.long)) dat.4and5.long <- dat.4and5.long %>% add_column(FSC = NA)
dat.4and5.long

#.6. 
dat.6.long <- as.data.frame(t(dat.6)) 
dat.6.long$Year <- row.names.data.frame(dat.6.long)
names(dat.6.long) <-  dat.6.long[1,]
dat.6.long <- dat.6.long[-1,]
rownames(dat.6.long) <- c()
dat.6.long$Landings <- as.numeric(dat.6.long$Landings)
dat.6.long$TAC <- as.numeric(dat.6.long$TAC)
dat.6.long$SPA <- "6"
if(!'FSC' %in% names(dat.6.long)) dat.6.long <- dat.6.long %>% add_column(FSC = NA)
dat.6.long <- dat.6.long %>% dplyr::select(Landings,TAC,Year,SPA,FSC) #Re-order columns to match other SPAs
dat.6.long



#merge SPAs 
BoF.landings.TAC <- rbind(dat.1A.long, dat.1B.long, dat.3.long,  dat.4and5.long, dat.6.long)
#massage year such that "season" e.g. 2019/2020 which is from Oct 1 2019 to Sept 30 2020 is listed as the "2020 season"
BoF.landings.TAC$Season <- str_sub(BoF.landings.TAC$Year,-4,-1)

#create total landings field
BoF.landings.TAC$FSC <- as.numeric(BoF.landings.TAC$FSC) #Make FSC column numeric
#Get total landings - FSC + Commercial landings
BoF.landings.TAC <- BoF.landings.TAC |>
  rowwise() |> 
  mutate(Total_Landings = sum(Landings, FSC, na.rm = TRUE))

BoF.landings.TAC$FSC <- as.character(BoF.landings.TAC$FSC) #Make FSC column characters

#edits to FSC column
BoF.landings.TAC <- BoF.landings.TAC |> 
  mutate_if(is.character, ~replace_na(.,"-")) #tidyr::replace_na()

#reorder and subset for export 
BoF.landings.TAC <- BoF.landings.TAC %>% dplyr::select(Season, SPA, TAC, Landings, FSC, Total_Landings) %>% 
  arrange(Season)


#Total each column by season:
#season.tot <- BoF.landings.TAC %>% 
#  group_by(Season) %>% 
#  summarise(TAC = sum(TAC), Landings = sum(Landings), Total_Landings = sum(Total_Landings)) %>% #FSC = sum(FSC)
#  mutate(SPA = "Total") %>% 
#  mutate(FSC = "-") %>% 
#  dplyr::select(Season, SPA, TAC, Landings, FSC, Total_Landings)

#BoF.landings.TAC <- rbind(BoF.landings.TAC, season.tot) %>% 
#  arrange(Season)


write.csv(BoF.landings.TAC, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/BoF_TACandLandings_compiled_",surveyyear,".csv"), row.names = FALSE)



