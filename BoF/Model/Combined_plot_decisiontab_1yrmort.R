#
# PANEL PLOT FOR comparing decision tables using 5 year average natural mortality (default), Or using the current years natural mortality.

#
rm(list=ls(all=T))
options(stringsAsFactors = FALSE)

#PACKAGES:
#required packages and functions:
library(openxlsx)
library(tidyverse)
library(SSModel)

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


#DEFINE:
direct <- "Y:/Inshore/BoF"
assessmentyear <- 2025 #year in which you are conducting the assessment 
surveyyear <- 2025  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019

#Load model indices
raw.dat.1A <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA1A/SPA1A_ModelData_R_2025-10-29.xlsx"),sheet = "AlignedForModel",cols=1:13)

raw.dat.1B <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA1B/SPA1B_ModelData_R_2025-10-30.xlsx"),sheet = "AlignedForModel",cols=1:13)

raw.dat.3 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA3/SPA3_ModelData_R_2025-10-20.xlsx"),sheet = "AlignedForModel",cols=1:13)

raw.dat.4 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA4/SPA4_ModelData_R_2025-10-20.xlsx"),sheet = "AlignedForModel",cols=1:13)

raw.dat.6 <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA6/SPA6_ModelData_R_2025-10-16.xlsx"),sheet = "AlignedForModel",cols=1:13)

SPA6.landings <- read.xlsx(paste0(direct, "/", assessmentyear, "/Assessment/Data/CommercialData/SPA6_TACandLandings_",assessmentyear,".xlsx"))

#Load model file
load(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA1A/SPA1A_Model_2025.RData"))
mod.res.1A <- Spa1A.2025
mod.sum.1A <- read.csv(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA1A/Spa1AModelOutput.csv"))

load(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA1B/SPA1B_Model_2025.RData"))
mod.res.1B <- Spa1B.2025
mod.sum.1B <- read.csv(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA1B/Spa1BModelOutput.csv"))

load(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA3/SPA3_Model_2025.RData"))
mod.res.3 <- Spa3.new.2025
mod.sum.3 <- read.csv(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA3/Spa3ModelOutput.csv"))


load(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA4/SPA4_Model_2025.RData"))
mod.res.4 <- Spa4.2025
mod.sum.4 <- read.csv(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA4/Spa4ModelOutput.csv"))

load(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA6/SPA6_Model_2025.RData"))
mod.res.6 <- Spa6.2025
mod.sum.6 <- read.csv(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA6/Spa6ModelOutput.csv"))

#############################################################################################################
#decision tables for each area

#1A
#Where mortality is the 5 year avg (default):
decision.current.1A <- predict(mod.res.1A, Catch=c(seq(0,680,40)), g.parm=mod.res.1A$data$g[mod.res.1A$data$NY],gr.parm=mod.res.1A$data$gR[mod.res.1A$data$NY])
decision.table.current.1A <- SSModel_predict_summary_median(decision.current.1A, LRP=480, USR=1000, RRP=0.15)
decision.table.current.1A <- decision.table.current.1A$Next.year
decision.table.current.1A <- decision.table.current.1A |> mutate(Table.Type = "Natural mortality is 5 year avg") |> mutate(SPA = "SPA 1A") |> mutate(RRP=0.15)
decision.table.current.1A

#For the 1 year mortality
#Decision tables using m.avg = 1
decision.1yrmort.1A <- predict(mod.res.1A, Catch=c(seq(0,680,40)),g.parm=mod.res.1A$data$g[mod.res.1A$data$NY],gr.parm=mod.res.1A$data$gR[mod.res.1A$data$NY], m.avg = 1)
decision.1yrmort.1A <- SSModel_predict_summary_median(decision.1yrmort.1A, LRP=480, USR=1000, RRP=0.15)
decision.1yrmort.1A <- decision.1yrmort.1A$Next.year
decision.1yrmort.1A <- decision.1yrmort.1A |> mutate(Table.Type = "Natural mortality is current year") |> mutate(SPA = "SPA 1A") |> mutate(RRP=0.15)
decision.1yrmort.1A

#write.csv(decision.1yrmort.1A, "Y:/Inshore/BoF/2025/Assessment/Data/Model/SPA1A/decision_with_1yrmort_2025.csv")

decision.tabs.1A <- rbind(decision.table.current.1A, decision.1yrmort.1A)

#Check the difference in mortality values
Years.1A <- as.data.frame(mod.res.1A$Years)
colnames(Years.1A) <- "Years"

colnames(mod.sum.1A) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
m.rows <- grep("^m\\[", mod.sum.1A$vars)
m <- mod.sum.1A[m.rows, ]
m.1A <- cbind(Years.1A, m$median)
m.1A <- m.1A %>% rename("Median" = "m$median")

m.avg5 <- mean(m.1A %>% filter(Years %in% c(2025,2024,2023,2022,2021)) %>% select(Median) %>% pull())
#.202
m.avg1 <- mean(m.1A %>% filter(Years %in% c(2025)) %>% select(Median) %>% pull())
#0.297

#1B
decision.current.1B <- predict(mod.res.1B, Catch=c(seq(0,825,50)), g.parm=mod.res.1B$data$g[mod.res.1B$data$NY],gr.parm=mod.res.1B$data$gR[mod.res.1B$data$NY])
decision.table.current.1B <- SSModel_predict_summary_median(decision.current.1B, LRP=880, USR=1800, RRP=0.15)
decision.table.current.1B <- decision.table.current.1B$Next.year
decision.table.current.1B <- decision.table.current.1B |> mutate(Table.Type = "Natural mortality is 5 year avg") |> mutate(SPA = "SPA 1B") |> mutate(RRP=0.15)
decision.table.current.1B

#For the 1 year mortality
#Decision tables using m.avg = 1
decision.1yrmort.1B <- predict(mod.res.1B, Catch=c(seq(0,825,50)), g.parm=mod.res.1B$data$g[mod.res.1B$data$NY],gr.parm=mod.res.1B$data$gR[mod.res.1B$data$NY], m.avg = 1)
decision.1yrmort.1B <- SSModel_predict_summary_median(decision.1yrmort.1B, LRP=880, USR=1800, RRP=0.15)
decision.1yrmort.1B <- decision.1yrmort.1B$Next.year
decision.1yrmort.1B <- decision.1yrmort.1B |> mutate(Table.Type = "Natural mortality is current year") |> mutate(SPA = "SPA 1B") |> mutate(RRP=0.15)
decision.1yrmort.1B
#write.csv(decision.1yrmort.1B, "Y:/Inshore/BoF/2025/Assessment/Data/Model/SPA1B/decision_with_1yrmort_2025.csv")

decision.tabs.1B <- rbind(decision.table.current.1B, decision.1yrmort.1B)

#Check the difference in mortality values
Years.1B <- as.data.frame(mod.res.1B$Years)
colnames(Years.1B) <- "Years"

colnames(mod.sum.1B) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
m.rows <- grep("^m\\[", mod.sum.1B$vars)
m <- mod.sum.1B[m.rows, ]
m.1B <- cbind(Years.1B, m$median)
m.1B <- m.1B %>% rename("Median" = "m$median")

m.avg5 <- mean(m.1B %>% filter(Years %in% c(2025,2024,2023,2022,2021)) %>% select(Median) %>% pull())
#.2314
m.avg1 <- mean(m.1B %>% filter(Years %in% c(2025)) %>% select(Median) %>% pull())
#0.342


#3
decision.current.3 <- predict(mod.res.3, Catch=c(seq(0, 320, 20)), g.parm=mod.res.3$data$g[mod.res.3$data$NY],gr.parm=mod.res.3$data$gR[mod.res.3$data$NY])
decision.table.current.3 <- SSModel_predict_summary_median(decision.current.3, LRP=600, USR=1000, RRP=0.15)
decision.table.current.3 <- decision.table.current.3$Next.year
decision.table.current.3 <- decision.table.current.3 |> mutate(Table.Type = "Natural mortality is 5 year avg") |> mutate(SPA = "SPA 3") |> mutate(RRP=0.15)
decision.table.current.3

#For the 1 year mortality
#Decision tables using m.avg = 1
decision.1yrmort.3 <- predict(mod.res.3, Catch=c(seq(0, 320, 20)), g.parm=mod.res.3$data$g[mod.res.3$data$NY],gr.parm=mod.res.3$data$gR[mod.res.3$data$NY], m.avg = 1)
decision.1yrmort.3 <- SSModel_predict_summary_median(decision.1yrmort.3, LRP=600, USR=1000, RRP=0.15)
decision.1yrmort.3 <- decision.1yrmort.3$Next.year
decision.1yrmort.3 <- decision.1yrmort.3 |> mutate(Table.Type = "Natural mortality is current year") |> mutate(SPA = "SPA 3") |> mutate(RRP=0.15)
decision.1yrmort.3
#write.csv(decision.1yrmort.3, "Y:/Inshore/BoF/2025/Assessment/Data/Model/SPA3/decision_with_1yrmort_2025.csv")

decision.tabs.3 <- rbind(decision.table.current.3, decision.1yrmort.3)

#Check the difference in mortality values
Years.3 <- as.data.frame(mod.res.3$Years)
colnames(Years.3) <- "Years"

colnames(mod.sum.3) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
m.rows <- grep("^m\\[", mod.sum.3$vars)
m <- mod.sum.3[m.rows, ]
m.3 <- cbind(Years.3, m$median)
m.3 <- m.3 %>% rename("Median" = "m$median")

m.avg5 <- mean(m.3 %>% filter(Years %in% c(2025,2024,2023,2022,2021)) %>% select(Median) %>% pull())
#0.2632
m.avg1 <- mean(m.3 %>% filter(Years %in% c(2025)) %>% select(Median) %>% pull())
#0.341

#4
decision.current.4 <- predict(mod.res.4, Catch=c(seq(0,420,20)), g.parm=mod.res.4$data$g[mod.res.4$data$NY],gr.parm=mod.res.4$data$gR[mod.res.4$data$NY])
decision.table.current.4 <- SSModel_predict_summary_median(decision.current.4, LRP=530, USR=750, RRP=0.15)
decision.table.current.4 <- decision.table.current.4$Next.year
decision.table.current.4 <- decision.table.current.4 |> mutate(Table.Type = "Natural mortality is 5 year avg") |> mutate(SPA = "SPA 4") |> mutate(RRP=0.15)
decision.table.current.4

#For the 1 year mortality
#Decision tables using m.avg = 1
decision.1yrmort.4 <- predict(mod.res.4, Catch=c(seq(0,420,20)),g.parm=mod.res.4$data$g[mod.res.4$data$NY],gr.parm=mod.res.4$data$gR[mod.res.4$data$NY], m.avg = 1)
decision.1yrmort.4 <- SSModel_predict_summary_median(decision.1yrmort.4, LRP=530, USR=750, RRP=0.15)
decision.1yrmort.4 <- decision.1yrmort.4$Next.year
decision.1yrmort.4 <- decision.1yrmort.4 |> mutate(Table.Type = "Natural mortality is current year") |> mutate(SPA = "SPA 4") |> mutate(RRP=0.15)
decision.1yrmort.4
#write.csv(decision.1yrmort.4, "Y:/Inshore/BoF/2025/Assessment/Data/Model/SPA4/decision_with_1yrmort_2025.csv")

decision.tabs.4 <- rbind(decision.table.current.4, decision.1yrmort.4)

#Check the difference in mortality values
Years.4 <- as.data.frame(mod.res.4$Years)
colnames(Years.4) <- "Years"

colnames(mod.sum.4) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
m.rows <- grep("^m\\[", mod.sum.4$vars)
m <- mod.sum.4[m.rows, ]
m.4 <- cbind(Years.4, m$median)
m.4 <- m.4 %>% rename("Median" = "m$median")

m.avg5 <- mean(m.4 %>% filter(Years %in% c(2025,2024,2023,2022,2021)) %>% select(Median) %>% pull())
#0.1522
m.avg1 <- mean(m.4 %>% filter(Years %in% c(2025)) %>% select(Median) %>% pull())
#0.236

#6
decision.current.6 <- predict(mod.res.6, Catch=c(seq(0, 330, 10)), g.parm=mod.res.6$data$g[mod.res.6$data$NY],gr.parm=mod.res.6$data$gR[mod.res.6$data$NY])
decision.table.current.6 <- SSModel_predict_summary_median(decision.current.6, LRP=236, USR=471, RRP=0.18)
decision.table.current.6 <- decision.table.current.6$Next.year
decision.table.current.6 <- decision.table.current.6 |> mutate(Table.Type = "Natural mortality is 5 year avg") |> mutate(SPA = "SPA 6") |> mutate(RRP=0.18)
decision.table.current.6

#Add column for equivalent catch from total area
prop.catch.in <- round(SPA6.landings %>% filter(Year == "Prop_IN") %>%
                         dplyr::select(paste0(assessmentyear)) %>% pull(),3)
#Add Catch.all column and calculate equivalent catch for total area
decision.table.current.6 <- decision.table.current.6 %>% 
  mutate(Catch.all = round(Catch/prop.catch.in,0))
decision.table.current.6


#For the 1 year mortality
#Decision tables using m.avg = 1
decision.1yrmort.6 <- predict(mod.res.6, Catch=c(seq(0, 330, 10)), g.parm=mod.res.6$data$g[mod.res.6$data$NY],gr.parm=mod.res.6$data$gR[mod.res.6$data$NY], m.avg = 1)
decision.1yrmort.6 <- SSModel_predict_summary_median(decision.1yrmort.6, LRP=236, USR=471, RRP=0.18)
decision.1yrmort.6 <- decision.1yrmort.6$Next.year
decision.1yrmort.6 <- decision.1yrmort.6 |> mutate(Table.Type = "Natural mortality is current year") |> mutate(SPA = "SPA 6") |> mutate(RRP=0.18)
decision.1yrmort.6

#Add column for equivalent catch from total area
prop.catch.in <- round(SPA6.landings %>% filter(Year == "Prop_IN") %>%
                         dplyr::select(paste0(assessmentyear)) %>% pull(),3)
#Add Catch.all column and calculate equivalent catch for total area
decision.1yrmort.6 <- decision.1yrmort.6 %>% 
  mutate(Catch.all = round(Catch/prop.catch.in,0))
decision.1yrmort.6
write.csv(decision.1yrmort.6, "Y:/Inshore/BoF/2025/Assessment/Data/Model/SPA6/decision_with_1yrmort_2025.csv")

decision.tabs.6 <- rbind(decision.table.current.6, decision.1yrmort.6)
#Switch Catch from modelled area to catch all
decision.tabs.6 <- decision.tabs.6 |> 
  mutate(Catch = Catch.all) |> 
  dplyr::select(-Catch.all)
  

#Combine all tables
decision.tabs.all <- rbind(decision.tabs.1A, decision.tabs.1B, decision.tabs.3, decision.tabs.4, decision.tabs.6)

#Check the difference in mortality values
Years.6 <- as.data.frame(mod.res.6$Years)
colnames(Years.6) <- "Years"

colnames(mod.sum.6) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
m.rows <- grep("^m\\[", mod.sum.6$vars)
m <- mod.sum.6[m.rows, ]
m.6 <- cbind(Years.6, m$median)
m.6 <- m.6 %>% rename("Median" = "m$median")

m.avg5 <- mean(m.6 %>% filter(Years %in% c(2025,2024,2023,2022,2021)) %>% select(Median) %>% pull())
#0.1214
m.avg1 <- mean(m.6 %>% filter(Years %in% c(2025)) %>% select(Median) %>% pull())
#0.117


#PLOT Catch and Exploitation:

p <- ggplot() +
  geom_line(data = decision.tabs.all, aes(Catch, Exploit, colour = Table.Type, linetype = Table.Type),linewidth = 1)+
  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
  scale_linetype_manual(values = c("dotted","solid"), name = "")+
  scale_y_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
  xlab("Catch (t)")+ ylab("Exploitation") +
  theme_bw()+
  theme(legend.position = c(.82,.25), legend.text = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
  facet_wrap(~SPA,  nrow = 2, ncol=3, scales = "free")+
  geom_hline(data = decision.tabs.all, aes(yintercept = RRP), linetype = "dashed")
p

#Export plot 
ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/Catchandexp_comparisons_btwn_5yrmort_and_1yr.png"), plot = p, scale = 1.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)

#FRENCH: PLOT Catch and Exploitation:

#decision.tabs.all.FR <- decision.tabs.all |> 
#  mutate(SPA = case_when(SPA == "SPA 1A" ~ "ZPrP 1A",
#                         SPA == "SPA 1B" ~ "ZPrP 1B",
#                         SPA == "SPA 3" ~ "ZPrP 3",
#                         SPA == "SPA 4" ~ "ZPrP 4",
#                         SPA == "SPA 6" ~ "ZPrP 6")) |> 
#  mutate(Table.Type = case_when(Table.Type == "Natural mortality is current year" ~ "NEED FRENCH TRANSLATION",
#                                Table.Type == "Natural mortality is 5 year avg" ~  "NEED FRENCH TRANSLATION"))


#p.FR <- ggplot() +
#  geom_line(data = decision.tabs.all.FR, aes(Catch, Exploit, colour = Table.Type, linetype = Table.Type))+
#  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
#  scale_linetype_manual(values = c("dotted","solid"), name = "")+
#  scale_y_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
#  xlab("Prises (t)")+ ylab("Exploitation") +
#  theme_bw()+
#  theme(legend.position = c(.82,.25), legend.text = element_text(size = 12),
#        axis.text=element_text(size=12),
#        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
#  facet_wrap(~SPA,  nrow = 2, ncol=3, scales = "free")+
#  geom_hline(data = decision.tabs.all.FR, aes(yintercept = RRP), linetype = "dashed")
#p.FR

#Export plot 
#ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/Catchandexp_comparisons_btwn_5yrmort_and_1yr_FR.png"), plot = p.FR, scale = 2.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)


#PLOT Prob above USR and Exploitation:

p <- ggplot() +
  geom_line(data = decision.tabs.all, aes(Exploit, p.USR,  colour = Table.Type, linetype = Table.Type), linewidth = 1)+
  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
  scale_linetype_manual(values = c("dotted","solid"), name = "")+
  scale_x_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
  scale_y_continuous(limits = c(0, 1.0))+
  ylab("Probability above USR")+ xlab("Exploitation") +
  theme_bw()+
  theme(legend.position = c(.82,.25), legend.text = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
  facet_wrap(~SPA,  nrow = 2, ncol=3, scales = "free")+
  geom_vline(data = decision.tabs.all, aes(xintercept = RRP), linetype = "dashed")
p

#Export plot 
ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/USRandexp_comparisons_btwn_5yrmort_and_1yr.png"), plot = p, scale = 2.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)


#PLOT Prob above LRP and Exploitation:
  
  p <- ggplot() +
  geom_line(data = decision.tabs.all, aes(Exploit, p.LRP,  colour = Table.Type, linetype = Table.Type), linewidth = 1)+
  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
  scale_linetype_manual(values = c("dotted","solid"), name = "")+
  scale_x_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
    scale_y_continuous(limits = c(0, 1.0))+
  ylab("Probability above LRP")+ xlab("Exploitation") +
  theme_bw()+
  theme(legend.position = c(.82,.25), legend.text = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
  facet_wrap(~SPA,  nrow = 2, ncol=3, scales = "free")+
  geom_vline(data = decision.tabs.all, aes(xintercept = RRP), linetype = "dashed")
p

#Export plot 
ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/LRPandexp_comparisons_btwn_5yrmort_and_1yr.png"), plot = p, scale = 2.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)