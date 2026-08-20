
library(tidyverse)

#Percent changes

condition <- read.csv("Y:/Inshore/Assessment/SFA29/2026/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to2025.csv")



##### change in condition as percent #####

YR <- 2025

## Subarea A
con.yr.t <- condition %>% filter(YEAR == YR & STRATA == "SFA29A") %>% select(CONDITION)
con.yr.tminus1 <- condition %>% filter(YEAR == YR-1 & STRATA == "SFA29A") %>% select(CONDITION)
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
#-5.909154

## Subarea B
con.yr.t <- condition %>% filter(YEAR == YR & STRATA == "SFA29B") %>% select(CONDITION)
con.yr.tminus1 <- condition %>% filter(YEAR == YR-1 & STRATA == "SFA29B") %>% select(CONDITION)
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
#-8.593994

## Subarea C
con.yr.t <- condition %>% filter(YEAR == YR & STRATA == "SFA29C") %>% select(CONDITION)
con.yr.tminus1 <- condition %>% filter(YEAR == YR-1 & STRATA == "SFA29C") %>% select(CONDITION)
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
#-9.775624

## Subarea D
con.yr.t <- condition %>% filter(YEAR == YR & STRATA == "SFA29D") %>% select(CONDITION)
con.yr.tminus1 <- condition %>% filter(YEAR == YR-1 & STRATA == "SFA29D") %>% select(CONDITION)
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# -8.497028

## Subarea E
con.yr.t <- condition %>% filter(YEAR == YR & STRATA == "SFA29E") %>% select(CONDITION)
con.yr.tminus1 <- condition %>% filter(YEAR == YR-1 & STRATA == "SFA29E") %>% select(CONDITION)
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 


##### change in relative exploitation #####

YR <- 2025

rel.exp <- read.csv("Y:/Inshore/Assessment/SFA29/2026/Assessment/Data/SurveyIndices/expected.rel.exploitation.csv")

## Subarea A
rel.exp.yr.t <- rel.exp %>% filter(YEAR == YR & SUBAREA == "SFA29A") %>% select(mu)
rel.exp.yr.tminus1 <- rel.exp %>% filter(YEAR == YR-1 & SUBAREA == "SFA29A") %>% select(mu)
((rel.exp.yr.tminus1$mu - rel.exp.yr.t$mu) / (rel.exp.yr.tminus1$mu))*100
#-49.92

## Subarea B
rel.exp.yr.t <- rel.exp %>% filter(YEAR == YR & SUBAREA == "SFA29B") %>% select(mu)
rel.exp.yr.tminus1 <- rel.exp %>% filter(YEAR == YR-1 & SUBAREA == "SFA29B") %>% select(mu)
((rel.exp.yr.tminus1$mu - rel.exp.yr.t$mu) / (rel.exp.yr.tminus1$mu))*100
#-23.01963

## Subarea C
rel.exp.yr.t <- rel.exp %>% filter(YEAR == YR & SUBAREA == "SFA29C") %>% select(mu)
rel.exp.yr.tminus1 <- rel.exp %>% filter(YEAR == YR-1 & SUBAREA == "SFA29C") %>% select(mu)
((rel.exp.yr.tminus1$mu - rel.exp.yr.t$mu) / (rel.exp.yr.tminus1$mu))*100
#-100

## Subarea D
rel.exp.yr.t <- rel.exp %>% filter(YEAR == YR & SUBAREA == "SFA29D") %>% select(mu)
rel.exp.yr.tminus1 <- rel.exp %>% filter(YEAR == YR-1 & SUBAREA == "SFA29D") %>% select(mu)
((rel.exp.yr.tminus1$mu - rel.exp.yr.t$mu) / (rel.exp.yr.tminus1$mu))*100
# -73.6214
