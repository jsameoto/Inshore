
require(officer)
require(tidyverse)
require(flextable)
require(huxtable)

#Set year and ensure you have access to ESS. Run full script.

#SCRIPT FORMAT - 
# Title slide
# Overview slide
# Loops through SPA1A, 1B, 3, 4 and 5
# SPA 6 (done seperately because of differences in assessment)
# Supplemetary slides - loops through all SPAs
# Extras - loops through all SPAs

# Define ------------------------------------------------------------------

assessment.year <- 2021
fig.dir <- paste0("Y:/INSHORE SCALLOP/BoF/",assessment.year,"/Assessment/Figures/") #figure directory
areas <- c("SPA1A", "SPA1B", "SPA3", "SPA4and5") #"SPA6")
areas2 <- c("SPA1A", "SPA1B", "SPA3", "SPA4") #Model results Figure directory (SPA4 - The name does not inlcude 5, so need to make seperate object.)

#CHECK!! Hard coded values: 
spa6.USR <- 9.1
spa6.LRP <- 6.2
spa6.removals <- 150 #tonnes of removals used in the Projection 

# Load Functions
funcs <- "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/HarvestScenTabFunc.R"
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# Load model results ------------------------------------------------------

direct <- paste0("Y:/INSHORE SCALLOP/BoF/", assessment.year,"/Assessment/")

loadEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env)
}
SPA1A <- loadEnvironment(paste0(direct,"Data/Model/SPA1A/Model_results_and_diagnostics_", assessment.year, "_1A.RData")) #SPA1A
PA1B <- loadEnvironment(paste0(direct,"Data/Model/SPA1B/Model_results_and_diagnostics_", assessment.year, "_1B.RData")) #SPA1B
SPA3 <- loadEnvironment(paste0(direct,"Data/Model/SPA3/Model_results_and_diagnostics_", assessment.year, "_3.RData")) #SPA3
SPA4 <- loadEnvironment(paste0(direct,"Data/Model/SPA4/Model_results_and_diagnostics_", assessment.year, "_4.RData")) #SPA4
PA5 <-  read.csv(paste0(direct,"Data/SurveyIndices/SPA1A1B4and5/SPA5.Index.Weight2021.csv")) #SPA5
SPA6 <- loadEnvironment(paste0(direct,"Data/Model/SPA6/Model_results_and_diagnostics_", assessment.year, "_6.RData")) #SPA6
PA6.catch <- read.csv(paste0(direct,"Data/CommercialData/CPUE_spa6_combined_",assessment.year+1,".csv")) %>% mutate(year = as.numeric(assessment.year))#SPA6


# build a new ppt from the template
newpres <- read_pptx("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/DO_NOT_EDIT_Template_ISAC_R_ppt.pptx")

# For reference --------
#Don't need to run - but useful to get names of slide layouts and place holder names
# Get names of placeholders for adding text/images/content to. These placeholders are added and named manually in powerpoint - *slide master*

layout_summary(newpres)
main.title.pg <- layout_properties(x = newpres, layout = "Main Title Page", master = "1_Office Theme") %>% dplyr::select(ph_label)
title.content.pg <- layout_properties(x = newpres, layout = "Title and Content", master = "1_Office Theme") %>% dplyr::select(ph_label)
summary.layout.pg <- layout_properties(x = newpres, layout = "Summary", master = "1_Office Theme") %>% dplyr::select(ph_label)
single.fig.pg <- layout_properties(x = newpres, layout = "Single Figure", master = "1_Office Theme") %>% dplyr::select(ph_label)
single.fig.wtext.pg <- layout_properties(x = newpres, layout = "Single Figure wtext", master = "1_Office Theme") %>% dplyr::select(ph_label) #Single Figure wtext 2
single.fig.wtext.2.pg <- layout_properties(x = newpres, layout = "Single Figure wtext 2", master = "1_Office Theme") %>% dplyr::select(ph_label)
dual.fig.pg <- layout_properties(x = newpres, layout = "Dual Figure", master = "1_Office Theme") %>% dplyr::select(ph_label)
table.layout.pg <- layout_properties(x = newpres, layout = "Table", master = "1_Office Theme") %>% dplyr::select(ph_label)
table.2.layout.pg <- layout_properties(x = newpres, layout = "Table_2", master = "1_Office Theme") %>% dplyr::select(ph_label)


#In powerpoint slidemaster view - hit alt F10 to see placeholder names. Rename as required. These will be the placeholder labels i.e. "ph_labels" in the R script.

main.title.pg #title page - with scallop image, and lines
title.content.pg # side by side text box and figure/text box, lower text box (horizontal across slide)
summary.layout.pg #text with bullet points
single.fig.pg #Full slide figure, no text
single.fig.wtext.pg #Full slide figure, with text
single.fig.wtext.2.pg #half slide figure, half text
dual.fig.pg #side by side figures, no text
table.layout.pg #full slide table


# create the presentation -------------------------------------------------

newpres <- newpres %>%
  

# title slide -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("Scallop Production Areas in the Bay of Fundy: Stock Status for ",assessment.year," and Forecast for ",
                         assessment.year +1), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("X December ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(value = "Placopecten magellanicus", ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>% #Pg num placeholder
  ph_with(external_img("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/scallop.png"), location = ph_location_label(ph_label = "Scallop image"), use_loc_size = TRUE) %>%  #This is supposed to be the scallop image.

  
# slide 1 - Outline -------------------------------------------------------------

add_slide(layout="Title and Content", master="1_Office Theme") %>%
  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = "Review by area", location = ph_location_label(ph_label = "Text Placeholder 1"), index=1) %>%
  ph_with(value = unordered_list(
    level_list = c(1, 2, 2, 2, 2, 2),
    str_list = c("Review by area", "SPA 1A", "SPA 1B", "3", "4 & 5", "6")),
    location = ph_location_label(ph_label = "Text Placeholder 1"), level=2, index=2) %>% 
  ph_with(value = paste0("*Note that all landing values and logbook data for ",assessment.year," are preliminary as of October X, ", assessment.year), location = ph_location_label(ph_label = "Text Placeholder 2"), index = 1) %>% 
  ph_with(external_img("Y:/INSHORE SCALLOP/Inshore Scallop Fishing Area Map/InshoreScallopFishingAreas_English_updated2021.png"), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) #Adding an external image - can change height/width, but currently set to match the size of the placeholder in powerpoint.

# Generate slides for SPA1A, 1B, 3, 4and5 with a loop.
  
  for (i in 1:length(areas)) {
    
    if(areas[i] == "SPA1A") area.title <- "SPA 1A"
    if(areas[i] == "SPA1B") area.title <- "SPA 1B"
    if(areas[i] == "SPA3") area.title <- "SPA 3"
    if(areas[i] == "SPA4and5") area.title <- "SPA 4 & 5"
    
    if(areas[i] == "SPA1A") USR <- SPA1A$USR
    if(areas[i] == "SPA1B") USR <- SPA1B$USR
    if(areas[i] == "SPA3") USR <- SPA3$USR
    if(areas[i] == "SPA4and5") USR <- SPA4$USR
    
    if(areas[i] == "SPA1A") LRP <- SPA1A$LRP
    if(areas[i] == "SPA1B") LRP <- SPA1B$LRP
    if(areas[i] == "SPA3") LRP <- SPA3$LRP
    if(areas[i] == "SPA4and5") LRP <- SPA4$LRP
    
    if(areas[i] == "SPA1A") catch.next.year <- SPA1A$catch.next.year
    if(areas[i] == "SPA1B") catch.next.year <- SPA1B$catch.next.year
    if(areas[i] == "SPA3") catch.next.year <- SPA3$catch.next.year
    if(areas[i] == "SPA4and5") catch.next.year <- SPA4$catch.next.year
    
    #Model Figure directory (SPA4 - The name does not inlcude 5, so need to make seperate object.) 
    if(areas2[i] == "SPA1A") areas2.title <- "SPA 1A"
    if(areas2[i] == "SPA1B") areas2.title <- "SPA 1B"
    if(areas2[i] == "SPA3") areas2.title <- "SPA 3"
    if(areas2[i] == "SPA4") areas2.title <- "SPA 4"
    
    if(areas2[i] == "SPA1A") file.suffix <- "1A"
    if(areas2[i] == "SPA1B") file.suffix <- "1B"
    if(areas2[i] == "SPA3") file.suffix <- "3"
    if(areas2[i] == "SPA4") file.suffix <- "4"
    
    newpres <- newpres %>%
      
# slide - Landings Figure (SPA1A, 1B, 3, 4&5) -------------------------------------------------------------
    
    add_slide(layout="Single Figure", master="1_Office Theme") %>%
      ph_with(value = paste0(area.title,": Landings"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
      ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/CommercialData/", areas[i],"_TACandLandings",assessment.year-1,".png"), width = 7.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% #REMOVE THE -1 to make work for 2021

      #paste0(fig.dir,"CommercialData/", areas[i],"_TACandLandings",assessment.year,".png")
      
      
# slide - Modeled Biomass Figure (SPA1A, 1B, 3, 4&5) -------------------------------------------------------------
    
    add_slide(layout="Single Figure wtext", master="1_Office Theme") %>%
      ph_with(value = paste0(areas2.title,": Modeled Biomass"), location = ph_location_label(ph_label = "Title"), index=1) %>% 
      ph_with(external_img(dir(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/Model/",areas2[i],"/"),full.names = TRUE, pattern = "^Model_biomass_figure"), width = 5.21, height = 5.80), location = ph_location(ph_label = "Figure Placeholder"), use_loc_size = FALSE) %>%
      ph_with(value = paste0("USR: ",USR, " t\n LRP: ", LRP," t"), location = ph_location_label(ph_label = "Text Placeholder top right"), index=1) %>%
      ph_with(value = paste0("Projection uses ", catch.next.year," t\n removals (interim TAC)"), location = ph_location_label(ph_label = "Text Placeholder btm right"), index=1)
      
  #ph_with(external_img(dir(paste0(fig.dir,"Model/",areas2[i],"/"),full.names = TRUE, pattern = "^Model_biomass_figure")
      
    
# slide - Harvest Scenerio Table (SPA1A, 1B, 3, 4)-------------------------------------------------------------

#First run the table function
harvest.scen.tab(area = areas2[i], catch.range = catch.range)
    
    newpres <- newpres %>%  
    
    add_slide(layout="Table", master="1_Office Theme") %>%
      ph_with(value = paste0(areas2.title, ": Harvest Scenario table"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
      ph_with(value = paste0("USR is ", USR, " t, LRP is ", LRP, " t"),location = ph_location_label(ph_label = "Text Placeholder"), index = 1) %>%
      ph_with(value = ex.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1)
    
  } #Break loop - move to SPA6 slides

# SPA6 Slides -------------------------------------------------------------

#formatting coloured text
col.prop.norm <- fp_text(color = "black", font.size = 18, font.family = "Century Gothic")
col.prop1 <- fp_text(color = "red3", font.size = 18, font.family = "Century Gothic")
col.prop2<- fp_text(color = "royalblue3", font.size = 18, font.family = "Century Gothic")

par <- block_list(
  fpar(ftext("Inside VMS stratum ", col.prop1), ftext("(red): The survey and analysis for SPA 6 is based on areas defined by VMS fishing patterns from 2002-2014.\n", col.prop.norm)), 
  fpar(ftext("Outside VMS stratum ", col.prop2), ftext("(blue): a combination of the historical survey extent and historical survey index", col.prop.norm)))

#now continue to add slides
newpres <- newpres %>% 
  
# slide -  SPA6 VMS Survey Design  -------------------------------------------------------------

add_slide(layout="Single Figure wtext 2", master="1_Office Theme") %>%
  ph_with(value = paste0("SPA 6: Survey Design"), location = ph_location_label(ph_label = "Title"), index=1) %>%
  ph_with(external_img("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/SPA6_VMS_INOUT.png", width = 5.00, height = 5.25), location = ph_location(ph_label = "Figure Placeholder", left = 1.00, top = 1.00), use_loc_size = FALSE) %>% 
  ph_with(value = par, location = ph_location_label(ph_label = "Text Placeholder right")) %>%
  
# slide - SPA6 Landings Figure -------------------------------------------------------------
  
  add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = paste0("SPA 6: Landings"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/CommercialData/SPA6_TACandLandings",assessment.year-1,".png"), width = 8.00 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>%  #REMOVE THE -1 to make work for 2021 #paste0(fig.dir,"CommercialData/", areas[i],"_TACandLandings",assessment.year,".png")


# slide - SPA6 Stock Status -------------------------------------------------------------  
  
add_slide(layout="Single Figure wtext", master="1_Office Theme") %>%
  ph_with(value = paste0("SPA 6: Stock Status"), location = ph_location_label(ph_label = "Title"), index=1) %>%
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/CommercialData/SPA6_RefPts2020.png"), width = 5.25, height = 5.25), location = ph_location(ph_label = "Figure Placeholder"), use_loc_size = FALSE) %>% 
  ph_with(value = paste0("USR: ",spa6.USR," kg/h\n LRP: ", spa6.LRP," kg/h"), location = ph_location_label(ph_label = "Text Placeholder btm right"), index=1) %>%
  
  # slide - SPA6 Modeled Biomass Figure -------------------------------------------------------------

add_slide(layout="Single Figure wtext", master="1_Office Theme") %>%
  ph_with(value = "SPA 6: Modeled Biomass", location = ph_location_label(ph_label = "Title"), index=1) %>%
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/Model/SPA6/Model_biomass_figure_6.png"), width = 5.21, height = 5.83), location = ph_location(ph_label = "Figure Placeholder"), use_loc_size = FALSE) %>% 
  ph_with(value = paste0("Projection uses ", spa6.removals," t\n removals"), location = ph_location_label(ph_label = "Text Placeholder btm right"), index=1)

    #ph_with(external_img(dir(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/Model/",areas2[i],"/"),full.names = TRUE, pattern = "^Model_biomass_figure")

# slide - SPA6 Harvest Scenerio Table -------------------------------------------------------------

#First run the table function
harvest.scen.tab(area = "SPA6", catch.range = catch.range)

#Now add continue to add slides to presentation:
newpres <- newpres %>%  
  
  add_slide(layout="Table_2", master="1_Office Theme") %>%
  ph_with(value = "SPA 6: Harvest Scenario table", location = ph_location_label(ph_label = "Title")) %>% #This is the title
  ph_with(value = ex.hux, location = ph_location_label(ph_label = "Table Placeholder")) %>% 

# slide - END OF PRESENTATION -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("Scallop Production Areas in the Bay of Fundy: Stock Status for ",assessment.year," and Forecast for ",
                         assessment.year +1), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("X December ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"), location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(external_img("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/scallop.png"), location = ph_location_label(ph_label = "Scallop image"), use_loc_size = TRUE) %>% # Scallop figure
  

# SUPPLEMENTAL SLIDES  -------------------------------------------------------------

# slide - Industry supplemental title -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = "Industry Supplemental", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("X December ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"), location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(external_img("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/scallop.png"), location = ph_location_label(ph_label = "Scallop image"), use_loc_size = TRUE) %>% # Scallop figure
  
  
# slide - Commercial Biomass Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Commercial (>= 80mm) Biomass", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_BoFAll_ComBiomass",assessment.year,".png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) %>% 
  
# slide - Recruit Biomass Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Recruit (65-79 mm) Biomass", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_BoFAll_RecBiomass",assessment.year,".png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) %>% 
  
  
# slide - Pre-recruit Abundance Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Pre-recruit (<65mm) Abundance", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_BoFAll_PrerecDensity",assessment.year,".png"), width = 6, height = 6),location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) 

#Add SPA6 into loop for supplemental slides - Condition plots
areas <- append(areas, "SPA6")
areas2 <- append(areas2, "SPA6")

for (i in 1:length(areas)) {
  
  if(areas[i] == "SPA1A") area.title <- "SPA 1A"
  if(areas[i] == "SPA1B") area.title <- "SPA 1B"
  if(areas[i] == "SPA3") area.title <- "SPA 3"
  if(areas[i] == "SPA4and5") area.title <- "SPA 4 & 5"
  if(areas[i] == "SPA6") area.title <- "SPA 6"
  
  #Model results Figure directory (SPA4 - The name does not inlcude 5, so need to make seperate object.) 
  if(areas2[i] == "SPA1A") areas2.title <- "SPA 1A"
  if(areas2[i] == "SPA1B") areas2.title <- "SPA 1B"
  if(areas2[i] == "SPA3") areas2.title <- "SPA 3"
  if(areas2[i] == "SPA4") areas2.title <- "SPA 4"
  if(areas2[i] == "SPA6") areas2.title <- "SPA 6"
  
  newpres <- newpres %>%

# For each SPA - Commercial Biomass Figure -------------------------------------------------------------
  
  #add_slide(layout="Single Figure", master="1_Office Theme") %>%
  #  ph_with(value = paste0(areas2.title, ": Commercial (>= 80mm) Biomass"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #  ph_with(external_img(paste0(fig.dir,"ContPlot_", areas2[i],"_ComBiomass", assessment.year,".png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) %>% 
  
# For each SPA - Recruit Biomass Figure -------------------------------------------------------------
  
  #add_slide(layout="Single Figure", master="1_Office Theme") %>%
  #  ph_with(value = paste0(areas2.title, ": Pre-recruit (<65mm) Abundance"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #  ph_with(external_img(paste0(fig.dir,"ContPlot_", areas2[i],"_RecBiomass", assessment.year,".png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) %>% 
  
  
# For each SPA - Pre-recruit Abundance Figure -------------------------------------------------------------
  
  #add_slide(layout="Single Figure", master="1_Office Theme") %>%
  #  ph_with(value = paste0(areas2.title, ": Pre-recruit (<65mm) Abundance"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #  ph_with(external_img(paste0(fig.dir,"ContPlot_", areas2[i],"_PreDensity", assessment.year,".png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE) %>% 

# slide - Condition Figure -------------------------------------------------------------
  
  add_slide(layout="Single Figure", master="1_Office Theme") %>%
    ph_with(value = paste0(areas2.title, ": Condition"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
    ph_with(external_img(paste0(fig.dir, areas2[i],"_ConditionTimeSeries.png")), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE)

}

# BREAK - EXTRA SLIDES ----------------------------------------------------

newpres <- newpres %>%

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = "Extra Slides", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("X December ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%
  ph_with(external_img("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/scallop.png"), location = ph_location_label(ph_label = "Scallop image"), use_loc_size = TRUE)# Scallop figure


for (i in 1:length(areas)) {
  
  if(areas[i] == "SPA1A") area.title <- "SPA 1A"
  if(areas[i] == "SPA1B") area.title <- "SPA 1B"
  if(areas[i] == "SPA3") area.title <- "SPA 3"
  if(areas[i] == "SPA4and5") area.title <- "SPA 4 & 5"
  if(areas[i] == "SPA6") area.title <- "SPA 6"
  
  #Model results Figure directory (SPA4 - The name does not inlcude 5, so need to make seperate object.) 
  if(areas2[i] == "SPA1A") areas2.title <- "SPA 1A"
  if(areas2[i] == "SPA1B") areas2.title <- "SPA 1B"
  if(areas2[i] == "SPA3") areas2.title <- "SPA 3"
  if(areas2[i] == "SPA4") areas2.title <- "SPA 4"
  if(areas2[i] == "SPA6") areas2.title <- "SPA 6"
  
  newpres <- newpres %>%


# slide - Commercial Catch Rate Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = paste0(areas2.title,": Commercial Catch Rate"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Figures/CommercialData/", areas2[i],"_CPUE",assessment.year-1,".png"), width = 6.7, height = 8), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% #REMOVE -1 from assessment year
  
# slide - Commercial Fishery Figure -------------------------------------------------------------

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = paste0(area.title,": Commercial Fishery"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/",assessment.year-1,"/Assessment/Figures/CommercialData/", areas[i],"_CPUEgridplot",assessment.year-1,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>%
  ph_with(external_img(paste0("Y:/INSHORE SCALLOP/BoF/",assessment.year-2,"/Assessment/Figures/CommercialData/", areas[i],"_CPUEgridplot",assessment.year-2,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE)
  
}
  
print(newpres, target = paste0("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/ISAC_presentation_Officerbuilt_",assessment.year,".pptx"))  
  
# slide 3 - SPA 1A Summary -------------------------------------------------------------
  
  #add_slide(layout="Summary", master="1_Office Theme") %>%
  #  ph_with(value = "SPA 1A Summary", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #  ph_with(value = unordered_list(
  #    level_list = c(1, 1, 1, 1), #Bullet point levels
  #    str_list = c(paste0("Total landings in the ",assessment.year," fishing year were X t against a TAC of X t (X t before post-quota reconciliation)"), #each bullet pnt
   #                paste0("Commercial catch rate in the ",assessment.year," fishing year was X kg/h, a decrease from ",assessment.year-2," (X kg/h)"), 
   #                paste0("The biomass estimate of recruit scallops in ",assessment.year," was X t, similar to ",assessment.year-2," (X t), and below the long-term (1997-",assessment.year,") median of X t"), 
    #               paste0("Commercial population biomass for ",assessment.year," estimated by the model was X t (meats) which is in the healthy zone"))),
     # location = ph_location_label(ph_label = "Text Placeholder"), index=1) %>% 
  #  ph_with(value = 3, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%  #Pg num placeholder



