
library(tidyverse)
library(dplyr)
library(reshape2)
library(cowplot)
library(gridExtra)
library(grid)
library(gridtext)


setwd("Y:/")

surveyyear <- 2024
assessmentyear <- 2025 

# SFA29A ---- 
numbers.A <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))
numbers.A <- numbers.A %>% filter(SUBAREA == "SFA29A", Strata == "med", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)

weight.A <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Commercial.Weight.csv"))
weight.A <- weight.A  %>% filter(SUBAREA == "SFA29A", Strata == "med") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean) 

condition.A <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))
condition.A <- condition.A %>% filter(STRATA == "SFA29A") |> dplyr::select(YEAR, CONDITION)
condition.A
#only since 1997 

#merge
all.dat.A <- as.data.frame(cbind(YEAR = condition.A$YEAR, CONDITION = condition.A$CONDITION, N = numbers.A$N , I = weight.A$I))

#clean up fields 
#all.dat.A <- all.dat.A %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.A$Condition <- all.dat.A$CONDITION/max(all.dat.A$CONDITION, na.rm = TRUE)
all.dat.A$Abundance <- all.dat.A$N/max(all.dat.A$N, na.rm = TRUE)
all.dat.A$Biomass <- all.dat.A$I/max(all.dat.A$I, na.rm = TRUE)
all.dat.A

#long format 
all.dat.A.long <- melt(all.dat.A,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.A.long$value[all.dat.A.long$YEAR == 2020] <- NA

all.dat.A.long

all.dat.A.long$SFA<- "Subarea A"


#condition, biomass, number time series as proportion of the maximum observed 
plot.A <- ggplot(all.dat.A.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index) + 
  ylab("Subarea A") + 
  xlab("") + 
  theme_bw()
plot.A

# SFA29A PLOT FRENCH VERSION

all.dat.A.long.FR <- all.dat.A.long
all.dat.A.long.FR$SPA <- "Sous-zone A"
all.dat.A.long.FR <- all.dat.A.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.A.long.FR$index.FR <- factor(all.dat.A.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.A.FR <- ggplot(all.dat.A.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index.FR) + 
  ylab("Sous-zone A") + 
  xlab("") + 
  theme_bw()
plot.A.FR



# SFA29B ---- 
numbers.B <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))
numbers.B <- numbers.B %>% filter(SUBAREA == "SFA29B", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)

weight.B <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Commercial.Weight.csv"))
weight.B <- weight.B  %>% filter(SUBAREA == "SFA29B", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)

condition.B <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))
condition.B <- condition.B %>% filter(STRATA == "SFA29B") |> dplyr::select(YEAR, CONDITION)
condition.B


#merge
all.dat.B <- as.data.frame(cbind(YEAR = condition.B$YEAR, CONDITION = condition.B$CONDITION, N = numbers.B$N , I = weight.B$I))

#clean up fields 
all.dat.B <- all.dat.B %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.B$Condition <- all.dat.B$CONDITION/max(all.dat.B$CONDITION, na.rm = TRUE)
all.dat.B$Abundance <- all.dat.B$N/max(all.dat.B$N, na.rm = TRUE)
all.dat.B$Biomass <- all.dat.B$I/max(all.dat.B$I, na.rm = TRUE)
all.dat.B

#long format 
all.dat.B.long <- melt(all.dat.B,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.B.long$value[all.dat.B.long$YEAR == 2020] <- NA

all.dat.B.long

all.dat.B.long$SFA<- "Subarea B"


#condition, biomass, number time series as proportion of the maximum observed 
plot.B <- ggplot(all.dat.B.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index) + 
  ylab("Subarea B") + 
  xlab("") + 
  theme_bw()
plot.B

# SFA29B PLOT FRENCH VERSION
all.dat.B.long.FR <- all.dat.B.long
all.dat.B.long.FR$SPA <- "Sous-zone B"
all.dat.B.long.FR <- all.dat.B.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.B.long.FR$index.FR <- factor(all.dat.B.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.B.FR <- ggplot(all.dat.B.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index.FR) + 
  ylab("Sous-zone B") + 
  xlab("") + 
  theme_bw()
plot.B.FR

# SFA29C ---- 
numbers.C <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))
numbers.C <- numbers.C %>% filter(SUBAREA == "SFA29C", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)

weight.C <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Commercial.Weight.csv"))
weight.C <- weight.C  %>% filter(SUBAREA == "SFA29C", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean) #|> distinct(YEAR, .keep_all = TRUE) #NEED TO INVESTIGATE!!!

condition.C <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))
condition.C <- condition.C %>% filter(STRATA == "SFA29C") |> dplyr::select(YEAR, CONDITION)
condition.C


#merge
all.dat.C <- as.data.frame(cbind(YEAR = condition.C$YEAR, CONDITION = condition.C$CONDITION, N = numbers.C$N , I = weight.C$I))

#clean up fields 
all.dat.C <- all.dat.C %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.C$Condition <- all.dat.C$CONDITION/max(all.dat.C$CONDITION, na.rm = TRUE)
all.dat.C$Abundance <- all.dat.C$N/max(all.dat.C$N, na.rm = TRUE)
all.dat.C$Biomass <- all.dat.C$I/max(all.dat.C$I, na.rm = TRUE)
all.dat.C

#long format 
all.dat.C.long <- melt(all.dat.C,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.C.long$value[all.dat.C.long$YEAR == 2020] <- NA

all.dat.C.long

all.dat.C.long$SFA<- "Subarea C"


#condition, biomass, number time series as proportion of the maximum observed 
plot.C <- ggplot(all.dat.C.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index) + 
  ylab("Subarea C") + 
  xlab("") + 
  theme_bw()
plot.C

# SFA29C PLOT FRENCH VERSION
all.dat.C.long.FR <- all.dat.C.long
all.dat.C.long.FR$SPA <- "Sous-zone C"
all.dat.C.long.FR <- all.dat.C.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.C.long.FR$index.FR <- factor(all.dat.C.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.C.FR <- ggplot(all.dat.C.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index.FR) + 
  ylab("Sous-zone C") + 
  xlab("") + 
  theme_bw()
plot.C.FR

# SFA29D ---- 
numbers.D <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))
numbers.D <- numbers.D %>% filter(SUBAREA == "SFA29D", Strata == "high", size == "comm") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(N = Mean)

weight.D <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Commercial.Weight.csv"))
weight.D <- weight.D  %>% filter(SUBAREA == "SFA29D", Strata == "high") |> dplyr::select(YEAR,  Mean) |> dplyr::rename(I = Mean)

condition.D <- read.csv(paste0("/Inshore/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))
condition.D <- condition.D %>% filter(STRATA == "SFA29D") |> dplyr::select(YEAR, CONDITION)
condition.D


#merge
all.dat.D <- as.data.frame(cbind(YEAR = condition.D$YEAR, CONDITION = condition.D$CONDITION, N = numbers.D$N , I = weight.D$I))

#clean up fields 
all.dat.D <- all.dat.D %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.D$Condition <- all.dat.D$CONDITION/max(all.dat.D$CONDITION, na.rm = TRUE)
all.dat.D$Abundance <- all.dat.D$N/max(all.dat.D$N, na.rm = TRUE)
all.dat.D$Biomass <- all.dat.D$I/max(all.dat.D$I, na.rm = TRUE)
all.dat.D

#long format 
all.dat.D.long <- melt(all.dat.D,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.D.long$value[all.dat.D.long$YEAR == 2020] <- NA

all.dat.D.long

all.dat.D.long$SFA<- "Subarea D"


#condition, biomass, number time series as proportion of the maximum observed 
plot.D <- ggplot(all.dat.D.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index) + 
  ylab("Subarea D") + 
  xlab("") + 
  theme_bw()
plot.D

# SFA29D PLOT FRENCH VERSION
all.dat.D.long.FR <- all.dat.D.long
all.dat.D.long.FR$SPA <- "Sous-zone D"
all.dat.D.long.FR <- all.dat.D.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.D.long.FR$index.FR <- factor(all.dat.D.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.D.FR <- ggplot(all.dat.D.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(2001, surveyyear, 5), limits = c(2001,surveyyear))+
  facet_wrap(~index.FR) + 
  ylab("Sous-zone D") + 
  xlab("") + 
  theme_bw()
plot.D.FR

# PLOT ALL ON SAME FIGURE 
plot_grid(plot.A, plot.B, plot.C, plot.D, ncol=1)

#want only single y axis label 
#need to label each row with SPA.. 

####  ENGLISH VERSION   ###
# gridtext
#yleft <- richtext_grob(text = "Proportion of maximum observation", rot=90, gp = gpar(fontsize = 14))
#bottom <-  richtext_grob(text = 'Year', gp = gpar(fontsize = 14))

#p <- list(plot.A, plot.B, plot.C, plot.D)

## Lay out plots
#uni <- grid.arrange(grobs=p, ncol = 1,
#                    left = yleft, bottom = bottom)

## Updated code in 2025 so automatically saved out 
yleft <- grid.text("Proportion of maximum observation",  rot=90, gp=gpar(fontsize=12, col="black"))

bottom <-  richtext_grob(text = 'Year', gp = gpar(fontsize = 14))

p.eng <- list(plot.A, plot.B, plot.C, plot.D)

# Lay out plots
plot.eng <- grid.arrange(grobs=p.eng, ncol = 1,
                         left = yleft, bottom = bottom)

grid.draw(plot.eng)

##Save out 
ggsave(file=paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Figures/ConditionNumberWeight_BySubarea_Proportion.png"), plot.eng, width = 20, height = 20, units = "cm", dpi = 300)





#### FRENCH VERSION ####
plot_grid(plot.A.FR, plot.B.FR, plot.C.FR, plot.D.FR, ncol=1)

#yleft <- richtext_grob(text = "Proportion de l'observation maximale", rot=90, gp = gpar(fontsize = 14))
#bottom <-  richtext_grob(text = paste0('Ann','\U00E9','e'), gp = gpar(fontsize = 14))

#p <- list(plot.A.FR, plot.B.FR, plot.C.FR, plot.D.FR)

## Lay out plots
#uni <- grid.arrange(grobs=p, ncol = 1,
#                    left = yleft, bottom = bottom)

## Updated code in 2025 so automatically saved out 
yleft <- grid.text("Proportion de l'observation maximale",  rot=90, gp=gpar(fontsize=12, col="black"))

bottom <-  richtext_grob(text = 'Année', gp = gpar(fontsize = 14))

p <- list(plot.A.FR, plot.B.FR, plot.C.FR, plot.D.FR)

# Lay out plots
plot.fr <- grid.arrange(grobs=p, ncol = 1,
                        left = yleft, bottom = bottom)
grid.draw(plot.fr)

ggsave(file=paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Figures/ConditionNumberWeight_BySubarea_Proportion_FR.png"), plot.fr, width = 20, height = 20, units = "cm", dpi = 300)


#Save using RStudio GUI 
#size 666 x 868 for document (ConditionNumberWeight_BySubarea.jpeg and ConditionNumberWeight_BySubarea_FR.jpeg)
#size 895 x 763 for presentation (ConditionNumberWeight_BySubarea_forPres.jpeg)


#jpeg(filename="Y:/Inshore/BoF/2023/Assessment/Figures/ConditionNumberWeight_BySPA_BW.png", width = 10, height = 30, units = "px")
#grid.arrange(grobs=p, ncol = 1,
#             left = yleft, bottom = bottom)
#dev.off()


#FOR FRENCH VERSION: From Raphael - Proportion du maximum observC)? Proportion de l'observation maximale, the first one is the more direct translation, and the second is like saying Proportion of the maximum observation.


#ggsave("Y:/Inshore/BoF/2023/Assessment/Figures/ConditionNumberWeight_BySPA.png", width=30, height=20, units="cm", dpi=100) #doesn't work with grid arrange 




##### change in condition as percent #####

YR <- 2024

## Subarea A 
con.yr.t <- condition.A[condition.A$YEAR == YR,]
con.yr.tminus1 <- condition.A[condition.A$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 30.65432 % decline from 2023 to 2024 


## Subarea B 
con.yr.t <- condition.B[condition.B$YEAR == YR,]
con.yr.tminus1 <- condition.B[condition.B$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 28.25511 % decline from 2023 to 2024 

## Subarea C 
con.yr.t <- condition.C[condition.C$YEAR == YR,]
con.yr.tminus1 <- condition.C[condition.C$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 27.1499 % decline from 2023 to 2024 

## Subarea D  
con.yr.t <- condition.D[condition.D$YEAR == YR,]
con.yr.tminus1 <- condition.D[condition.D$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 28.34443 % decline from 2023 to 2024 

## Subarea E  
#con.yr.t <- condition.E[condition.E$YEAR == YR,]
#con.yr.tminus1 <- condition.E[condition.E$YEAR == YR-1,]
#((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100

## From 2023 to 2024 27 to 31% decline in condition 


##### change in biomass index as percent #####

YR <- 2024

## Subarea A 
B.yr.t <- weight.A[weight.A$YEAR == YR,]
B.yr.tminus1 <- weight.A[weight.A$YEAR == YR-1,]
((B.yr.tminus1$I - B.yr.t$I) / (B.yr.tminus1$I))*100
# 74.3809 % decline in biomass in A medium 


## Subarea B
B.yr.t <- weight.B[weight.B$YEAR == YR,]
B.yr.tminus1 <- weight.B[weight.B$YEAR == YR-1,]
((B.yr.tminus1$I - B.yr.t$I) / (B.yr.tminus1$I))*100
# 18.85767 % increase in biomass in B high 

## Subarea C
B.yr.t <- weight.C[weight.C$YEAR == YR,]
B.yr.tminus1 <- weight.C[weight.C$YEAR == YR-1,]
((B.yr.tminus1$I - B.yr.t$I) / (B.yr.tminus1$I))*100
#  90.50055 % decline in biomass in C in high 

## Subarea D 
B.yr.t <- weight.D[weight.D$YEAR == YR,]
B.yr.tminus1 <- weight.D[weight.D$YEAR == YR-1,]
((B.yr.tminus1$I - B.yr.t$I) / (B.yr.tminus1$I))*100
# 64.39385 % decline in biomass in D in high 

## Subarea E  
#B.yr.t <- all.dat.6[all.dat.6$YEAR == YR,]
#B.yr.tminus1 <- all.dat.6[all.dat.6$YEAR == YR-1,]
#((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100

# range in biomass index: 90% decline (C) to 19% increase (B)



##### change in Modelled Biomass as percent #####
## CAN DO WHEN DONE MODELLING 

setwd("Y:/Inshore/BoF/2024/Assessment/Data/Model/")

#current/most recent year 
YR <- 2024

B.1A <- read.csv(paste0("SPA1A/summary stats_1A_",YR,".csv"))
B.1B <- read.csv(paste0("SPA1B/summary stats_1B_",YR,".csv"))
B.3 <- read.csv(paste0("SPA3/summary stats_3_",YR,".csv"))
B.4 <- read.csv(paste0("SPA4/summary stats_4_",YR,".csv"))
B.6 <- read.csv(paste0("SPA6/summary stats_6_",YR,".csv"))



## 1A
B.1A <- B.1A[B.1A$description  == "comm.biomass",]
yr.t <- B.1A[,6]
yr.t
yr.tminus1 <-  B.1A[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
# 44.45334

## 1B
B.1B <- B.1B[B.1B$description  == "comm.biomass",]
yr.t <- B.1B[,6]
yr.t
yr.tminus1 <-  B.1B[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#42.00945


## 3 
B.3 <- B.3[B.3$description  == "comm.biomass",]
yr.t <- B.3[,6]
yr.t
yr.tminus1 <-  B.3[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#32.62144


## 4 
B.4 <- B.4[B.4$description  == "comm.biomass",]
yr.t <- B.4[,6]
yr.t
yr.tminus1 <-  B.4[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#41.86935


## 6 
B.6 <- B.6[B.6$description  == "comm.biomass",]
yr.t <- B.6[,6]
yr.t
yr.tminus1 <-  B.6[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#32.60546


# range in modelled biomass: 33 - 44%





