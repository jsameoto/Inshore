surveyyear <- 2021
years <-c(2018:2019, 2021:surveyyear)

livefreq.hm <- readRDS("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/data/Prorated/horsemussellive_prorated.rds") %>% filter(SAMPLE.METHOD != 1)#remove tows where no SHF data was collected.

# add YEAR column to data
livefreq.hm$YEAR <- ifelse(grepl("SFA29", livefreq.hm$CRUISE), as.numeric(substr(livefreq.hm$CRUISE,6,9)), as.numeric(substr(livefreq.hm$CRUISE,3,6)))

table(livefreq.hm$YEAR)

#Shell Length Frequency data set-up and plots are modified from Scallop SHF scripts

# ---- CALCULATE SHF FOR EACH YEAR BY STRATA ----

#1. Saint Mary's Bay
livefreq.SMB <- livefreq.hm %>% 
  filter(STRATA_ID==22) 
SMB.SLFmeans <- sapply(split(livefreq.SMB[c(4:42)], livefreq.SMB$YEAR), function(x){apply(x,2,mean)})
round (SMB.SLFmeans,2)
# matrix to dataframe
SMB.SLFmeans <- data.frame(SMB.SLFmeans)


#2. SFA29
livefreq.SFA29 <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(41:45))
SFA29.SLFmeans <- sapply(split(livefreq.SFA29[c(4:42)], livefreq.SFA29$YEAR), function(x){apply(x,2,mean)})
round (SFA29.SLFmeans,2)
# matrix to dataframe
SFA29.SLFmeans <- data.frame(SFA29.SLFmeans)


#3. SPA6
livefreq.SPA6 <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(30:32))
SPA6.SLFmeans <- sapply(split(livefreq.SPA6[c(4:42)], livefreq.SPA6$YEAR), function(x){apply(x,2,mean)})
round (SPA6.SLFmeans,2)
# matrix to dataframe
SPA6.SLFmeans <- data.frame(SPA6.SLFmeans)


#3. SPA4
livefreq.SPA4 <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(1:20, 47, 48))
SPA4.SLFmeans <- sapply(split(livefreq.SPA4[c(4:42)], livefreq.SPA4$YEAR), function(x){apply(x,2,mean)})
round (SPA4.SLFmeans,2)
# matrix to dataframe
SPA4.SLFmeans <- data.frame(SPA4.SLFmeans)


#5. Upper Bay
livefreq.Upper.Bay <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(35, 49, 50, 51, 52))
Upper.Bay.SLFmeans <- sapply(split(livefreq.Upper.Bay[c(4:42)], livefreq.Upper.Bay$YEAR), function(x){apply(x,2,mean)})
round (Upper.Bay.SLFmeans,2)
# matrix to dataframe
Upper.Bay.SLFmeans <- data.frame(Upper.Bay.SLFmeans)


#6. Inner Bay
livefreq.Inner.Bay <- livefreq.hm %>% 
  filter(STRATA_ID %in% c(54, 37, 38, 53, 55, 56, 39))
Inner.Bay.SLFmeans <- sapply(split(livefreq.Inner.Bay[c(4:42)], livefreq.Inner.Bay$YEAR), function(x){apply(x,2,mean)})
round (Inner.Bay.SLFmeans,2)
# matrix to dataframe
Inner.Bay.SLFmeans <- data.frame(Inner.Bay.SLFmeans)

#---- PLOT SHF FOR EACH YEAR BY STRATA ----

# -- Shell length freq plot for SMB ----------------------------------------------------------------

SMB.SLFmeans.for.plot <- data.frame(bin.label = row.names(SMB.SLFmeans), SMB.SLFmeans)
SMB.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SMB.SLFmeans.for.plot)
SMB.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)




SMB.SLFmeans.for.plot <- pivot_longer(SMB.SLFmeans.for.plot, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SL",
                                         values_drop_na = FALSE)
SMB.SLFmeans.for.plot$year <- as.numeric(SMB.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SMB.SLFmeans.for.plot$SH <- round(SMB.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.SMB.SLF <- ggplot() + geom_col(data = SMB.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SMB.SLF


# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SMB.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SMB.SLF)
dev.off()

# -- Shell length freq plot for SFA29W ----------------------------------------------------------------

SFA29.SLFmeans.for.plot <- data.frame(bin.label = row.names(SFA29.SLFmeans), SFA29.SLFmeans)
SFA29.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SFA29.SLFmeans.for.plot)
SFA29.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)

SFA29.SLFmeans.for.plot <- pivot_longer(SFA29.SLFmeans.for.plot, 
                                      cols = starts_with("X"),
                                      names_to = "year",
                                      names_prefix = "X",
                                      values_to = "SL",
                                      values_drop_na = FALSE)
SFA29.SLFmeans.for.plot$year <- as.numeric(SFA29.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SFA29.SLFmeans.for.plot$SH <- round(SFA29.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.SFA29.SLF <- ggplot() + geom_col(data = SFA29.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SFA29.SLF


# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SFA29.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SFA29.SLF)
dev.off()

# -- Shell length freq plot for SPA6 ----------------------------------------------------------------

SPA6.SLFmeans.for.plot <- data.frame(bin.label = row.names(SPA6.SLFmeans), SPA6.SLFmeans)
SPA6.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SPA6.SLFmeans.for.plot)
SPA6.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)

SPA6.SLFmeans.for.plot <- pivot_longer(SPA6.SLFmeans.for.plot, 
                                        cols = starts_with("X"),
                                        names_to = "year",
                                        names_prefix = "X",
                                        values_to = "SL",
                                        values_drop_na = FALSE)
SPA6.SLFmeans.for.plot$year <- as.numeric(SPA6.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SPA6.SLFmeans.for.plot$SH <- round(SPA6.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.SPA6.SLF <- ggplot() + geom_col(data = SPA6.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SPA6.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA6.SLF)
dev.off()

# -- Shell length freq plot for SPA4 ----------------------------------------------------------------

SPA4.SLFmeans.for.plot <- data.frame(bin.label = row.names(SPA4.SLFmeans), SPA4.SLFmeans)
SPA4.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(SPA4.SLFmeans.for.plot)
SPA4.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)

SPA4.SLFmeans.for.plot <- pivot_longer(SPA4.SLFmeans.for.plot, 
                                       cols = starts_with("X"),
                                       names_to = "year",
                                       names_prefix = "X",
                                       values_to = "SL",
                                       values_drop_na = FALSE)
SPA4.SLFmeans.for.plot$year <- as.numeric(SPA4.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
SPA4.SLFmeans.for.plot$SH <- round(SPA4.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.SPA4.SLF <- ggplot() + geom_col(data = SPA4.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA4.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_SPA4.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA4.SLF)
dev.off()

# -- Shell length freq plot for Upper Bay ----------------------------------------------------------------

Upper.Bay.SLFmeans.for.plot <- data.frame(bin.label = row.names(Upper.Bay.SLFmeans), Upper.Bay.SLFmeans)
Upper.Bay.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(Upper.Bay.SLFmeans.for.plot)
Upper.Bay.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)

Upper.Bay.SLFmeans.for.plot <- pivot_longer(Upper.Bay.SLFmeans.for.plot, 
                                       cols = starts_with("X"),
                                       names_to = "year",
                                       names_prefix = "X",
                                       values_to = "SL",
                                       values_drop_na = FALSE)
Upper.Bay.SLFmeans.for.plot$year <- as.numeric(Upper.Bay.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
Upper.Bay.SLFmeans.for.plot$SH <- round(Upper.Bay.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.UpperBay.SLF <- ggplot() + geom_col(data = Upper.Bay.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.UpperBay.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_UpperBay.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.UpperBay.SLF)
dev.off()

# -- Shell length freq plot for Inner Bay ----------------------------------------------------------------

Inner.Bay.SLFmeans.for.plot <- data.frame(bin.label = row.names(Inner.Bay.SLFmeans), Inner.Bay.SLFmeans)
Inner.Bay.SLFmeans.for.plot$X2020 <- NA # add 2020 column.
head(Inner.Bay.SLFmeans.for.plot)
Inner.Bay.SLFmeans.for.plot$bin.mid.pt <- seq(2.5,195,by=5)

Inner.Bay.SLFmeans.for.plot <- pivot_longer(Inner.Bay.SLFmeans.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SL",
                                            values_drop_na = FALSE)
Inner.Bay.SLFmeans.for.plot$year <- as.numeric(Inner.Bay.SLFmeans.for.plot$year)

#shorten SH data for plot or else get warning when run ggplot 
Inner.Bay.SLFmeans.for.plot$SH <- round(Inner.Bay.SLFmeans.for.plot$SL,3)

ylimits <- c(0,10)
xlimits <- c(0,195)

# plot SHF for 2to8 mile strata
plot.InnerBay.SLF <- ggplot() + geom_col(data = Inner.Bay.SLFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Length (mm)") + 
  scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.UpperBay.SLF

# Save out plot
png(paste0("Z:/Projects/Horse_Mussel/HM_InshoreSurvey/Figures/HM_InnerBay.SLF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.InnerBay.SLF)
dev.off()