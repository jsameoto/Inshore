###### SPA4 Model XLSX File Update ######
## AUg 2018 DK made quick edit to switch all the hard coded years to be "Y"

## This file will populate an Excel workbook with survey per tow values, estimates, cvs, and other information needed for the model. 
## The excel workbook is saved here: paste0(direct, Y, "/", Y, " Assessment/SPA", area, "/SPA", area, "_ModelData_R.xlsx")
## This function will require some editing after the 2018 assessment occurs, since Dave has tidied up output files. I suspect that I will be able to remove some of the 
## repetitive "if(area==X)" statements once we have all of the files in a similar format.
## Important notes:
##  - Values are compared to the 2017 model Rdata and replaced with the 2017 model Rdata value if there is a discrepancy or an NA. For this reason
##    there may be slight differences between the values in AlignedForModel (uses 2017 model RData) and AreaEst tabs.
##  - This script only generates the PerTow tab of the excel workbook for Area 6. The values in the other tabs are straight from the 2017 Model Rdata for Area 6.
##  - If running prior to 2018 assessment is complete, you may have issues. Recommend running line-by-line and changing Y back and forth between 2017 and 2018 until it works (or even 2016)
##    due to changes in file outputs over time. 

######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT ######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT ######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT
## HEY FREYA AND DK, AS OF OCT 30, 2018 THIS IS PUTTING THE WRONG YEAR IN FOR THE YEARCATCH COLUMN, WE WANT TO REVISE SO THAT THE YEAR FOR THE CATCH
## IS LABELED AS (YEARSURVEY + 1).  NOTE THE DATA AS LINED UP IS PERFECTLY FINE AND THIS CHANGE HAS NO IMPACT ON THE MODELLING
#  WE JUST NEED TO MAKE SURE THE YEARCATCH IS ID'ED CORRECTLY SO WE DON'T ACCIDENTALLY GET CONFUSED.  THE DATAFILES FOR 2018
#  ASSESSMENTS WERE ALL CORRECTED BY HAND 
######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT ######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT ######ATTENTION!!!  ##### DIE BEACHTUNG ######## AANDACHT

### FK - June 2020. I completed the update above and also made various edits. In 2020 we ran the 2019 assessment, 
######### and updated the growth values for SPAs 1A, 1B and 6 for the entire time series. 
### I am also adding arguments assessmentyear and surveyyear to facilitate the transition to our new folder structure. 
### There is also now a savefile argument that is F by default. This will just have the spreadsheet pop open in excel, without saving it. Great for testing.


### BW - Sept 2025. Updated to add FSC landings to total landings for SPA1B and SPA6

### Enter arguments as follows for 2020 run of 2019 survey data:
### direct = "Y:/INSHORE SCALLOP/BoF/"
### assessmentyear = 2020
### surveyyear = 2019
### area = 3 (alternatively, "1B", "1A", 4, 6)
### LastYearsModelRData  "SPA3_Model_2018" (this is used to check the new values against the past)
### savefile = F


CreateExcelModelFile <- function(direct, assessmentyear, surveyyear, area, LastYearsModelRData, savefile=F) {
  
  require(openxlsx)
  require(dplyr)
  require(tidyr)
  require(compareDF)
  require(ggplot2)
  
  options(scipen=999)

  ##########################################################################
  ######### Set up #########################################################
  ##########################################################################
  
  Y <- surveyyear
  
  #get previous years to fill in the gaps when necessary
  if(assessmentyear<2021) {
    if(area==4) modeloutput <- get(load(file = paste(direct, "/", Y-1, "/", Y-1, " Assessment/SPA4and5/ModelOutput/", LastYearsModelRData, ".RData",sep="")))
    if(!area==4) modeloutput <- get(load(file = paste(direct, "/", Y-1, "/", Y-1, " Assessment/SPA", area, "/ModelOutput/", LastYearsModelRData, ".RData",sep="")))
  }
  if(assessmentyear>2020) modeloutput <- get(load(file = paste(direct, "/", assessmentyear-1, "/Assessment/Data/Model/SPA",area,"/", LastYearsModelRData, ".RData",sep="")))
  
  #get the range of years for this area
  Ys <- min(modeloutput$Years):Y
  
  #set up the dataframe. Area 6 is slightly different from the rest because of the timing of the fishery
  #if(!area==6) AlignedForModel <- data.frame(YearCatch=Ys,YearSurvey=Ys)
  #if(area==6) 
  AlignedForModel <- data.frame(YearCatch=Ys+1,YearSurvey=Ys)
  
  ## Insert new year values if not present:
  if(max(AlignedForModel$YearSurvey) < Y) {
    #if(area==6) 
    AlignedForModel <- rbind(AlignedForModel, 
                             setNames(data.frame(Ys+1, Ys), 
                                      names(AlignedForModel)))
    #if(!area==6) AlignedForModel <- rbind(AlignedForModel, data.frame(YearCatch=Y, YearSurvey=Y))
    
  }
  
  #Must use capital letter for 1A and 1B, and use 4and5 instead of 4
  if(area=="1a") area <- "1A"
  if(area=="1b") area <- "1B"
  if(area==4) area <- "4and5"
  
  # area folder labelling for survey indices and growth
  if(area %in% c("1A", "1B", "4and5")) arealab <- "1A1B4and5"
  if(area %in% c(3, 6)) arealab <- area
  
  
  
  ########################################################################################################################
  ######## TAC and Landings ##############################################################################################
  ########################################################################################################################
  
  ## Start by getting the catch for the years from the TACandlandings spreadsheet. 
  ## This is generally stored in CommercialData
  ## Here, we format it and pull the landings into our dataframes.
  
  # read in files
  if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/CommercialData/SPA", area, "_TACandLandings_",assessmentyear,".xlsx"))) {
    TACandlandings <- read.xlsx(xlsxFile = paste0(direct, "/", assessmentyear, "/Assessment/Data/CommercialData/SPA", area, "_TACandLandings_",assessmentyear,".xlsx"), sheet="TACandLandings")
  }
  
  if(!exists("TACandlandings")) stop(paste0("Could not find TACandlandings in ", direct, "/", assessmentyear, "/Assessment/Data/CommercialData/"))
  
  # switch area 4 back
  if(area=="4and5") area <- 4
  
  # format and rename
  namecols <-  c(TACandlandings[,1])
  TACandlandings <- as.data.frame(t(TACandlandings))
  if(!area=="1A") TACandlandings$Year <- stringr::str_sub(row.names(TACandlandings), -4, -1)
  if(area=="1A") TACandlandings$Year <- as.numeric(stringr::str_sub(row.names(TACandlandings), 1, 4)) + 1
  others <- TACandlandings[is.na(as.numeric(TACandlandings$Year)),] # remove non-data rows
  if(dim(others)[2] > 0) print("check out object called others to see if important data is missing")
  
  TACandlandings <- TACandlandings[!is.na(as.numeric(TACandlandings$Year)),] # remove non-data rows
  TACandlandings <- TACandlandings[,!names(TACandlandings) %in% names(TACandlandings)[which(apply(TACandlandings, 2, function(x) all(is.na(x))))]] # remove empty columns
  
  names(TACandlandings) <- c(namecols[!namecols==""], "Year")
  
  # deal with area-specific quirks:
  if(area %in% c("1A", "3")) names(TACandlandings) <- c("C", "TAC", "YearSurvey")
  if(area =="1B") {
    TACandlandings <- TACandlandings[, c("Full Bay", "Mid Bay", "Upper Bay", "FSC","TAC", "Year")]
    TACandlandings[is.na(TACandlandings)] <- 0 #Takes care of the NAs in FSC
    TACandlandings$C <- as.numeric(as.character(TACandlandings$`Full Bay`)) +  as.numeric(as.character(TACandlandings$`Mid Bay`)) + 
      as.numeric(as.character(TACandlandings$`Upper Bay`)) + as.numeric(as.character(TACandlandings$`FSC`))
    TACandlandings <- TACandlandings[, c("C", "TAC", "Year")]
    names(TACandlandings) <- c("C", "TAC", "YearSurvey") 
  }
  if(area==4){
    TACandlandings <- TACandlandings[, c("SPA4", "TAC", "Year")]
    names(TACandlandings) <- c("C", "TAC", "YearSurvey")
  }
  if(area==6) {
    TACandlandings <- TACandlandings[, c("Catch_IN", "FSC", "TAC", "Year")]
    TACandlandings[is.na(TACandlandings)] <- 0 #Takes care of the NAs in FSC
    TACandlandings$C <- as.numeric(as.character(TACandlandings$Catch_IN)) + as.numeric(as.character(TACandlandings$FSC))
    TACandlandings <- TACandlandings[, c("C", "TAC", "Year")]
    names(TACandlandings) <- c("C", "TAC", "YearCatch")
  }
  
  ## join catch to dataframe
  if(!area==6) TACandlandings$YearSurvey <- as.numeric(as.character(TACandlandings$YearSurvey)) - 1
  AlignedForModel <- plyr::join(AlignedForModel, TACandlandings, type="left")
  AlignedForModel <- subset(AlignedForModel, select=-TAC)
  AlignedForModel$C <- as.character(AlignedForModel$C)
  
  # add some notes
  if(is.na(AlignedForModel$C[AlignedForModel$YearCatch == Y-1])) {
    AlignedForModel$C[AlignedForModel$YearCatch == Y-1] <- "FILL IN UPDATED LANDINGS IN EXCEL"
  }
 # if(!area==6) AlignedForModel$C[AlignedForModel$YearCatch == Y] <- "FILL IN INTERIM TAC OR TAC USED FOR PREDICTION IN EXCEL"
#  if(area==6) 
   AlignedForModel$C[AlignedForModel$YearCatch == Y+1] <- "FILL IN INTERIM TAC OR TAC USED FOR PREDICTION IN EXCEL"
  
  
  ########################################################################################################
  ######### Numbers  #####################################################################################
  ########################################################################################################
  
  ## Starting with numbers, pull from csv. Filenames are slighlty different depending on Area.
  if(area %in% c("1A", "1B", 3)) filename <- paste0("SPA", area, ".population.model.input.",Y,".csv")
  if(area ==4) filename <- paste0("SPA", area, ".Index.Numbers.",Y,".csv")
  if(area == 6) filename <- paste0("SPA", area, ".Index.Numbers.IN.",Y,".csv")

  if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))) {
    Numbers <- read.csv(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))
  }
  
  if(!exists("Numbers")) stop(paste0("Could not find Numbers in ", direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/", arealab))
  
  # special formatting for some
  if(area %in% c(4, 6)) {
  
    # prepare the numbers data for joining. Make it wide format
    if(any(names(Numbers) == "Pop")) names(Numbers)[which(names(Numbers)=="Pop")] <- "N"
    Numbers <- pivot_wider(data = Numbers[,c("Year", "N", "Age")], names_from = Age, values_from=N)
    names(Numbers)[which(names(Numbers) == "Recruit")] <- "NR"
    names(Numbers)[which(names(Numbers) == "Commercial")] <- "N"
  }
  
  # rename
  names(Numbers)[which(names(Numbers) == "Year")] <- "YearSurvey"
  if(any(names(Numbers) == "X")) Numbers <- dplyr::select(Numbers, -X)
  
  # join to dataframe
  if(area %in% c("1A", "1B", 3)) AlignedForModel <- plyr::join(AlignedForModel, Numbers[,c("YearSurvey", "N", "I", "IR", "I.cv", "IR.cv")], type="left") 
  if(area %in% c(4, 6)) AlignedForModel <- plyr::join(AlignedForModel, Numbers[,c("YearSurvey", "N")], type="left") 
  
  
  #########################################################################################################
  #### Weights (only for 1B, 4 and 6, others were done already) ##########################################
  ########################################################################################################
  if(area %in% c(4, 6)) {
    
    if(area == 4) filename <- paste0("SPA", area, ".Index.Weight.",Y,".csv")
    if(area == 6) filename <- paste0("SPA", area, ".Index.Weight.IN.",Y,".csv")
    
    if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))) {
      Weight <- read.csv(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))
    }
    
    if(!exists("Weight") & area %in% c(4, 6)) stop(paste0("Could not find Weight in ", direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/", arealab))
   
    # prepare the weight data for joining. Make it wide format
    if(any(names(Weight)=="Bmass") & !any(names(Weight) == "Biomass_mt")) names(Weight)[which(names(Weight)=="Bmass")] <- "Biomass_mt"
    Weight <- pivot_wider(data = Weight[,c("Year", "Biomass_mt", "cv", "Age")], names_from = Age, values_from=c(Biomass_mt, cv))
    names(Weight)[which(names(Weight) == "Year")] <- "YearSurvey"
    names(Weight)[which(names(Weight) == "Biomass_mt_Recruit")] <- "IR"
    names(Weight)[which(names(Weight) == "Biomass_mt_Commercial")] <- "I"
    names(Weight)[which(names(Weight) == "cv_Recruit")] <- "IR.cv"
    names(Weight)[which(names(Weight) == "cv_Commercial")] <- "I.cv"
    
    # join the weights to the dataframe
    AlignedForModel <- plyr::join(AlignedForModel, Weight, type="left") 
    
  }
  
  
  #########################################################################################################
  ####### Growth ##########################################################################################
  #########################################################################################################

  # read in file
  if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/Growth/SPA", arealab, "/spa", area, ".growthrate.",Y,".csv"))){
    growth <- read.csv(paste0(direct, "/", assessmentyear, "/Assessment/Data/Growth/SPA", arealab, "/spa", area, ".growthrate.",Y,".csv"))
  }
  
  if(!exists("growth")) stop(paste0("Could not find growth files in ", direct, "/", assessmentyear, "/Assessment/Data/Growth/", arealab))
  
  # make it wide format
  growth <- pivot_wider(data = growth[,c("Year", "rate", "Age", "GrowthMethod")], names_from = c(GrowthMethod, Age), values_from=rate)
  
  # rename
  names(growth)[which(names(growth)=="Year")] <- "YearSurvey"
  names(growth)[which(names(growth)=="Actual_Commercial")] <- "g"
  names(growth)[which(names(growth)=="Actual_Recruit")] <- "gR"
    
  # for current year, use the predicted growth
  growth$g[growth$YearSurvey==max(growth$YearSurvey)] <- growth$Predict_Commercial[growth$YearSurvey==max(growth$YearSurvey)]
  growth$gR[growth$YearSurvey==max(growth$YearSurvey)] <- growth$Predict_Recruit[growth$YearSurvey==max(growth$YearSurvey)]

  # join to dataframe
  AlignedForModel <- plyr::join(AlignedForModel, growth[, c("YearSurvey", "g", "gR")], type="left")
  
  
  
  #########################################################################################################
  ####### Clappers ########################################################################################
  #########################################################################################################
  
  # read in files
  
  if(area %in% c("1A", "1B", 3)) filename <- paste0("SPA", area, ".Clappers.N.formodel.", Y, ".csv")
  if(area %in% c(4, 6)) filename <- paste0("SPA", area, ".Index.Clappers.", Y, ".csv")
  
  if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))){
    clappers <- read.csv(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/", filename))
  }
  
  if(!exists("clappers")) stop(paste0("Could not find clappers in ", direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/"))
  
  # special formatting for 4 and 6
  if(area == 4){
    clappers <- pivot_wider(data = clappers[,c("Year", "Age", "Pop")], names_from = c(Age), values_from=Pop)
    clappers <- dplyr::select(clappers, Year, Commercial)
    names(clappers) <- c("Year", "N")
  }
  
  if(area == 6){
    clappers <- pivot_wider(data = clappers[,c("Year", "Age", "VMSSTRATA", "Pop")], names_from = c(VMSSTRATA, Age), values_from=Pop)
    clappers <- dplyr::select(clappers, Year, IN_Commercial)
    names(clappers) <- c("Year", "N")
  }
  
  # rename
  if(any(names(clappers)=="X")) clappers <- dplyr::select(clappers, -X)
  names(clappers)[which(names(clappers)=="Year")] <- "YearSurvey"
  names(clappers)[which(names(clappers)=="N")] <- "clappers"
  
  # Join to dataframe
  AlignedForModel <- plyr::join(AlignedForModel, clappers[, c("YearSurvey", "clappers")], type="left") 
  
  
  
  #########################################################################################################
  ####### Ratio lined to unlined ##########################################################################
  #########################################################################################################
  
  # read in files
  if(file.exists(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/SPA", area, "_ratioLinedtoUnlined", Y, ".csv"))){
    ratio <- read.csv(paste0(direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab, "/SPA", area, "_ratioLinedtoUnlined", Y, ".csv"))
  }
  
  if(!exists("ratio")) stop(paste0("Could not find ratio in ", direct, "/", assessmentyear, "/Assessment/Data/SurveyIndices/SPA", arealab))
  
  # rename
  ratio <- ratio[, c("Year", "ratiolined")]
  names(ratio)[1] <- "YearSurvey"
  
  # Join to dataframe
  AlignedForModel <- plyr::join(AlignedForModel, ratio, type="left")
  
  # # shift the catch year
  # if(!area==6) AlignedForModel$YearCatch <- AlignedForModel$YearCatch + 1 
  
  #browser()
  ############################################################################################
  ########## COMPARE TO LAST YEAR'S MODEL RDATA ##############################################
  ############################################################################################
  
  # prepare last year's data for comparisons
  old <- data.frame(YearSurvey = modeloutput$Years,
                    t(do.call(rbind, modeloutput$data)))
  
  old <- dplyr::select(old, names(dplyr::select(AlignedForModel, -YearCatch)))
  
  # do the comparisons and save out the table
  output <- compare_df(df_new = dplyr::select(AlignedForModel, -YearCatch), df_old=old, group_col = "YearSurvey")
  create_output_table(output, output_type = "xlsx", file_name=paste0(direct, "/", assessmentyear, "/Assessment/Data/Model/SPA", area, "/CompareOutputTable_SPA", area, ".xlsx"))
  
  message(paste0("\nCompareOutputTable written to: ", direct, "/", assessmentyear, "/Assessment/Data/Model/SPA", area, "/CompareOutputTable_SPA", area, ".xlsx"))
  message("\nReview CompareOutputTable. This function will keep the OLD (red) values for all columns and rows except for:\n
          a) Catch (C) for last year\n
          b) Commercial growth (g) for last year\n
          c) Recruit growth (gR) for last year\n
          If you wish to retain NEW (green) values instead, make an edit near in the EDITS section (near line 345) of CreateExcelModelFile_2020.R. Please label it with '# EDIT'.") 
 
  revised <- left_join(dplyr::select(AlignedForModel, YearCatch, YearSurvey), old)
  revised[revised$YearSurvey==Y, c("C", "N", "I", "IR", "I.cv", "IR.cv", "g", "gR", "clappers", "ratiolined")] <- 
    AlignedForModel[AlignedForModel$YearSurvey==Y, c("C", "N", "I", "IR", "I.cv", "IR.cv", "g", "gR", "clappers", "ratiolined")]
#take 2020 values that occur for first time in 2021 - since no survey in 2020 values are interpolated between 2019 and 2021 during 2021 assessment 
  revised[revised$YearSurvey==2020, c("C", "N", "I", "IR", "I.cv", "IR.cv", "g", "gR", "clappers", "ratiolined")] <- 
    AlignedForModel[AlignedForModel$YearSurvey==2020, c("C", "N", "I", "IR", "I.cv", "IR.cv", "g", "gR", "clappers", "ratiolined")]
  
  # special case A) for updating last year's catch
  revised$C[revised$YearSurvey==Y-1] <- as.numeric(AlignedForModel$C[AlignedForModel$YearSurvey==Y-1])
  
  # special case B) for updating last year's predicted growth to actual growth
  revised$g[revised$YearSurvey==Y-1] <- as.numeric(AlignedForModel$g[AlignedForModel$YearSurvey==Y-1])
  
  # special case C) for updating last year's predicted growth to actual growth
  revised$gR[revised$YearSurvey==Y-1] <- as.numeric(AlignedForModel$gR[AlignedForModel$YearSurvey==Y-1])
  
  #browser()
  ################################################ 
  ######## EDITS for special cases go between here.....
  
  ### EDIT SPA 1A
  if(area == "1A"){
    if(Y==2019){
      # EDIT to keep YearSurvey 2019 g and gR values for SPA 1A, 1B and 6
      revised$g <- AlignedForModel$g
      revised$gR <- AlignedForModel$gR
      
      # EDIT to keep new (2020) C's for SPA 1A
      revised$C <- AlignedForModel$C
      
      # EDIT to keep new (2020) I and IR values for 2000 for SPA 1A (NO! Wait for RAP)
      # revised$I[revised$YearSurvey==2000] <- AlignedForModel$I[AlignedForModel$YearSurvey==2000]
      # revised$IR[revised$YearSurvey==2000] <- AlignedForModel$IR[AlignedForModel$YearSurvey==2000]
      
      # EDIT to keep new (2020) I.cv and IR.cv values for SPA 1A
      revised$I.cv <- AlignedForModel$I.cv
      revised$IR.cv <- AlignedForModel$IR.cv
      
      # EDIT to keep new (2020) clappers values for SPA 1A
      revised$clappers <- AlignedForModel$clappers
      
      # EDIT to keep new (2020) ratiolined values for SPA 1A
      revised$ratiolined <- AlignedForModel$ratiolined
    }
  }
  ###
  
  ### EDIT SPA 1B
  if(area == "1B"){
    if(Y==2019){
      # EDIT to keep YearSurvey 2019 g and gR values for SPA 1A, 1B and 6
      revised$g <- AlignedForModel$g
      revised$gR <- AlignedForModel$gR
      
      # EDIT to keep YearSurvey 2016-2018 Ns that were recalculated in assessment year 2020
      revised$N[revised$YearSurvey %in% 2016:2018] <- AlignedForModel$N[AlignedForModel$YearSurvey %in% 2016:2018] 
    
      # EDIT to keep YearSurvey 2018 I, IR, clappers, and ratio
      revised$I[revised$YearSurvey == 2018] <- AlignedForModel$I[AlignedForModel$YearSurvey == 2018] 
      revised$IR[revised$YearSurvey == 2018] <- AlignedForModel$IR[AlignedForModel$YearSurvey == 2018] 
      revised$clappers[revised$YearSurvey == 2018] <- AlignedForModel$clappers[AlignedForModel$YearSurvey == 2018] 
      revised$ratiolined[revised$YearSurvey == 2018] <- AlignedForModel$ratiolined[AlignedForModel$YearSurvey == 2018] 
    }
  }
  ###
  
  ### EDIT SPA 3
  if(area ==3) {
    if(Y==2019){
      # EDIT to keep YearSurvey 2017 g and gR values for SPA 3. Forgot to replace them with actual during the 2018 assessment.
      revised$g[revised$YearSurvey==2017] <- AlignedForModel$g[AlignedForModel$YearSurvey==2017]
      revised$gR[revised$YearSurvey==2017] <- AlignedForModel$gR[AlignedForModel$YearSurvey==2017]
      
      # EDIT to keep the new (2020) gR value for YearSurvey 2014. 
      revised$gR[revised$YearSurvey==2014] <- AlignedForModel$gR[AlignedForModel$YearSurvey==2014]

      # Wait for RAP: EDIT to keep new (2020) C's for SPA 3. A review of landings was conducted in 2019, so we trust these new values over historical. 
      # revised$C <- AlignedForModel$C
      
      # EDIT to keep new (2020) N's, clappers and ratiolined for SPA 3 for YearSurvey 2000 
      revised$N[revised$YearSurvey==2000] <- AlignedForModel$N[AlignedForModel$YearSurvey==2000]
      revised$clappers[revised$YearSurvey==2000] <- AlignedForModel$clappers[AlignedForModel$YearSurvey==2000]
      revised$ratiolined[revised$YearSurvey==2000] <- AlignedForModel$ratiolined[AlignedForModel$YearSurvey==2000]
      
      # EDIT to keep new (2020) N's and clappers for SPA 3 for YearSurvey 2004 
      revised$N[revised$YearSurvey==2004] <- AlignedForModel$N[AlignedForModel$YearSurvey==2004]
      revised$clappers[revised$YearSurvey==2004] <- AlignedForModel$clappers[AlignedForModel$YearSurvey==2004]
    } 
  }
  ###
  
  ### EDIT SPA 4
  if(area == 4) {
    if(Y==2019){
      # Wait for RAP: EDIT to keep new (2020) IR and IR.cv's for SPA 4 for 2000
      # revised$IR[revised$YearSurvey==2000] <- AlignedForModel$IR[AlignedForModel$YearSurvey==2000]
      # revised$IR.cv[revised$YearSurvey==2000] <- AlignedForModel$IR.cv[AlignedForModel$YearSurvey==2000]
      # 
      # Wait for RAP: EDIT to keep new (2020) clappers for SPA 4 for 1988
      # revised$clappers[revised$YearSurvey==1988] <- AlignedForModel$clappers[AlignedForModel$YearSurvey==1988]
      
      # EDIT to keep new (2020) N's and ratiolined for SPA 4
      revised$N <- AlignedForModel$N
      revised$ratiolined <- AlignedForModel$ratiolined
    } 
  }
  ###
  
  ### EDIT SPA 6
  if(area == 6){
    if(Y==2019){
      # EDIT to keep YearSurvey 2019 g and gR values for SPA 1A, 1B and 6
      revised$g <- AlignedForModel$g
      revised$gR <- AlignedForModel$gR
      
      # EDIT to keep YearSurvey 2019 C values for SPA 6
      revised$C <- AlignedForModel$C
    }
  }
  ###
  ######### and here! 
  ##################################################
  
  
  # finally, overwrite AlignedForModel with the revised version
  
  AlignedForModel <- dplyr::select(revised, YearCatch, YearSurvey, C, N, I, IR, I.cv, IR.cv, g, gR, clappers, ratiolined)
  
  
  
  ###################################################################################
  ######### A few diagnostic plots ##################################################
  ###################################################################################
  
  print(ggplot() + geom_text(data=AlignedForModel, aes(N, I, label=YearSurvey)) + theme_bw() +
             ggtitle(paste0("SPA", area)))
  
  print(ggplot() + geom_line(data=AlignedForModel, aes(YearSurvey, I), colour="blue") + 
    geom_line(data=AlignedForModel, aes(YearSurvey, IR/0.1), colour="red") + 
    scale_y_continuous("I", sec.axis = sec_axis(~ . * 0.1, name = "IR")) + 
    theme_bw() +
    ggtitle(paste0("SPA", area)) +
    theme(axis.title.y.left = element_text(colour = "blue"), axis.text.y.left = element_text(colour = "blue"))+
    theme(axis.title.y.right = element_text(colour = "red"), axis.text.y.right = element_text(colour = "red"))
  )
  
  print(ggplot() + geom_line(data=AlignedForModel, aes(YearSurvey, g), colour="blue") + 
    geom_line(data=AlignedForModel, aes(YearSurvey, gR), colour="red") + 
    scale_y_continuous("g", sec.axis = sec_axis(~ ., name = "gR")) + 
    theme_bw() +
    ggtitle(paste0("SPA", area)) +
    theme(axis.title.y.left = element_text(colour = "blue"), axis.text.y.left = element_text(colour = "blue"))+
    theme(axis.title.y.right = element_text(colour = "red"), axis.text.y.right = element_text(colour = "red"))
  )
  
  print(ggplot() + geom_line(data=AlignedForModel, aes(YearSurvey, I), colour="blue") + 
          geom_line(data=AlignedForModel, aes(YearSurvey, N*0.00001), colour="red") + 
          scale_y_continuous("I", sec.axis = sec_axis(~ . / 0.00001, name = "N")) + 
          theme_bw() +
          ggtitle(paste0("SPA", area)) +
          theme(axis.title.y.left = element_text(colour = "blue"), axis.text.y.left = element_text(colour = "blue"))+
          theme(axis.title.y.right = element_text(colour = "red"), axis.text.y.right = element_text(colour = "red"))
  )
  
  message("\nProduced 4 static plots (click arrow in Plots tab to view)")
    
  #####################################################################################
  ############ Add notes ##############################################################
  #####################################################################################
  
  if(area == "1A") AlignedForModel$area_sq_km <- 1931.013
  if(area == "1B") AlignedForModel$area_sq_km <- 2891.282
  if(area == 3) AlignedForModel$area_sq_km <- 1346.000
  if(area == 4) AlignedForModel$area_sq_km <- 670.497
  if(area == 6) AlignedForModel$area_sq_km <- 623.94
  

  if(area==4) area_old <- "4and5"
  if(!area==4) area_old <- area
  
  AlignedForModel$comments <- " "
  AlignedForModel$comments[1] <- "Biomass in term of metric tons: Commercial and Recruit (divided by 1000)"
  AlignedForModel$comments[nrow(AlignedForModel)-1] <- "Growth in previous survey year was updated from predicted growth rates to actual growth rates, since we now have that information available."
  AlignedForModel$comments[nrow(AlignedForModel)] <- "Growth in current survey year are predicted growth rates from year t to t+1 since that's all we have available. These are used only in the prediction evaluation."
  
  if(area==6) {
    AlignedForModel$comments[2] <- "Catch and growth rates need to be aligned lagged by one year"
  }
  
  ################################################################################
  ##### create and save the workbook #############################################
  ################################################################################
  
  Rwb <- createWorkbook(paste0("SPA", area))
  addWorksheet(wb=Rwb, sheetName = "AlignedForModel")
  writeData(Rwb, sheet = 1, x = AlignedForModel)
  
  if(savefile==F) openXL(Rwb)
  if(savefile==T){
    saveWorkbook(Rwb, paste0(direct, "/", assessmentyear, "/Assessment/Data/Model/SPA", area, "/SPA", area, "_ModelData_R_", Sys.Date(), ".xlsx"), overwrite=T)
  }
  
}


 
