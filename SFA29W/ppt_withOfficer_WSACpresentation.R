

###.......................................###
###                                       ###
###       SFA29W WSAC Presentation        ###
###                                       ###
###.......................................###


require(officer)
require(tidyverse)
require(flextable)
require(huxtable)
require(openxlsx)

#Set year and ensure you have access to ESS. Run full script.

#SCRIPT FORMAT - 
# Title slide
# Overview slide
# Fishery Backgroud
# Landings
# Catch Rate
# Logbook figures
# Fishing Positions (VMS)
# Bycatch
# Survey Design
# Habitat Suitability
# Abundance Spatial plots
# Shell heigh frequencies
# Abundance plots
# Condition
# Biomass
# Clappers
# Lobster Bycatch spatial plot
# Grey Meats


# Define ------------------------------------------------------------------

year <- 2022
fig.dir <- paste0("Y:/Inshore/SFA29/",year,"/Assessment/Figures/") #figure directory
subareas <- c("A", "B", "C", "D", "E")
# Load Functions



# Load model results ------------------------------------------------------

direct <- paste0("Y:/Inshore/SFA29/",year,"/Assessment/")

#Read in .RData for each SPA and save within Area object
loadEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env)
}

#SPA1A <- loadEnvironment(paste0(direct,"Data/Model/SPA1A/Model_results_and_diagnostics_", year, "_1A.RData")) #SPA1A
#SPA1A.decision.table <- read.csv(paste0(direct,"Data/Model/SPA1A/decisiontable", year, "_1A.csv"))


# build a new ppt from the template
newpres <- read_pptx("Y:/Inshore/SFA29/2022/Assessment/Documents/Presentations/WSAC/DO_NOT_EDIT_Template_WSAC_R_ppt.pptx")

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
dual.2.fig.pg <- layout_properties(x = newpres, layout = "Dual Figure 2", master = "1_Office Theme") %>% dplyr::select(ph_label)
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
dual.2.fig.pg #side by side figures, with text (SHF plots in extra slides)
table.layout.pg #full slide table


# create the presentation -------------------------------------------------

newpres <- newpres %>%
  
  
  # title slide -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("SFA 29 West Advisory Committee Meeting: Science Advice for ",year), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("XX April ",year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(value = "Placopecten magellanicus", ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>% #Pg num placeholder
  

# slide 1 - Outline -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1, 1, 1,1),
    str_list = c("Background", "Fishery data", "Survey data", "Habitat-based population model", paste0("Stock status and advice for",year))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 

# slide 2 - Multi-Year Science Advice -------------------------------------------------------------
  
add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Multi-Year Science Advice", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 2, 3, 1, 2, 3, 3, 3),
    str_list = c("Full Assessment Year:",
                 "Regional Advisory Process (RAP)",
                 "Assessment or Framework (CSAS Research Document)",
                 "Update Year:",
                 "Annual update to advice",
                 "Annual assessment results (CSAS Science Response)",
                 "Reference points and precautionary approach still apply",
                 "Internal DFO peer-review")),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 


# slide 3 - Landings Time Series -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Landings Time Series", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"CommercialData/SFA29WTACandLandings",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 

  
# slide 4 - YYYY Landings  -------------------------------------------------------------

#ADD TABLE

add_slide(layout="Table", master="1_Office Theme") %>%
  ph_with(value = paste0(year-1, "Landings"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(value = ex.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1)
  

# slide 5 - Catch Rate -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Catch Rate", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"CommercialData/SFA29_CPUEbyfleet_",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
# slide 6- Fishing Positions -------------------------------------------------------------

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = "Fishing Positions: Logbook", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/Inshore/SFA29/",year-1,"/Assessment/Figures/CommercialData/SFA29_CPUEgridplot",year-2,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"CommercialData/SFA29_CPUEgridplot",year-1,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 

#Check if any privacy considerations


# slide 7 - Fishery Bycatch -------------------------------------------------------------

#MIGHT NOT INCLUDE - data not available

#add_slide(layout="Summary", master="1_Office Theme") %>%
#  ph_with(value = "Fishery Bycatch", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
#  ph_with(value = unordered_list(
#    level_list = c(1, 1, 2, 2, 2, 1),
#    str_list = c("Observer coverage of one day per active vessel, monitor fish and invertebrate species. Discard rates reported for all bycatch species",
#                 "In 20XX",
#                 "X active vessels",
#                 "X trips observed corresponding to X days observed",
#                 "No observed trips in Subarea X",
#                 "Discard rates cannot be reported due to Privacy Act considerations")),
#    location = ph_location_label(ph_label = "Text Placeholder 1"), level=2, index=2) %>%
  
  
# slide 8 - Survey: Habitat Suitability -------------------------------------------------------------

#WHERE DO I FIND THIS FIGURE AND TABLE?

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Survey: Habitat Suitability", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(external_img(), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  #Table at bottom - modify/create new layout template
  
  
# slide 9- Pre-recruit Abundance  -------------------------------------------------------------

#Will need to adjust Figure placeholder 1 file path.

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = "Pre-recruit Abundance (<90mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_PreDensity",year-3,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_PreDensity",year-1,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 


# slide 10- Recruit Abundance  -------------------------------------------------------------

#Will need to adjust Figure placeholder 1 file path.

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = "Recruit Abundance (90-99mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_RecDensity",year-3,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_RecDensity",year-1,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 


# slide 11 - Commercial Abundance  -------------------------------------------------------------

#Will need to adjust Figure placeholder 1 file path.

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = "Commercial Abundance (>100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0("Y:/Inshore/SFA29/2020/figures/ContPlot_SFA29_ComDensity",year-3,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_ComDensity",year-1,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) 

#Break for loop

## Loop for each subarea A-E

for (i in 1:length(subareas)) {
  
  if(subareas[i] == "A") area.title <- "SFA29A"
  if(subareas[i] == "B") area.title <- "SFA29B"
  if(subareas[i] == "C") area.title <- "SFA29C"
  if(subareas[i] == "D") area.title <- "SFA29D"
  if(subareas[i] == "E") area.title <- "SFA29E"
  
  newpres <- newpres %>%
    
# slide 12-16 - Shell Height Frequency A-D -------------------------------------------------------------
  
  #SHELL HEIGHT FREQUENCY PNGs DONT HAVE YEAR IN THE FILE NAME - FIX?
  
  add_slide(layout="Single Figure", master="1_Office Theme") %>%
    ph_with(value = paste0(area.title,": Shell Height Frequency"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
    ph_with(external_img(paste0(fig.dir, "Growth/SHF_SFA29", subareas[i],".png"), width = 6.7, height = 8), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE)
  
}

#End loop

newpres <- newpres %>%
  
# slide 17 - Recruit Survey Abundance -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Recruit Survey Abundance (90-99mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Numberspertow.Recruit.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>%
  
  
# slide 18 - Commercial Survey Abundance -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Commercial Survey Abundance (>100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Numberspertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  
# slide 19 - Subarea E -------------------------------------------------------------

#ALL SUBAREAS? JUST E, or NONE?

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Subarea E", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "SFA29E.Numberspertow.Live.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 

# slide 20 - Condition Time series -------------------------------------------------------------

#No YEAR IN FILE NAME - CHANGE?

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Condition Time Series", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "SFA29W.ConditionTimeSeries.png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 


# slide 21 - Commercial Survey Biomass -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Commercial Survey Biomass (>100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Weightpertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 

  

# slide 22 - Commercial Clapper Proportion -------------------------------------------------------------

#CLAPPER PROPORTION FILE NOT SAVED

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Commercial Clapper Proportion (>100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Weightpertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  
# slide 23 - Assessment Model: Habitat Suitability -------------------------------------------------------------

#Need to edit layout for bullet points and figure

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Assessment Model: Habitat Suitability", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1),
    str_list = c("Builds in spatial information on distribution",
                 "Scallop habitat suitability binned into Low [0,0.3), Medium [0.3,0.6), and High [0.6,1.0)")),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>%  
  
# slide 24 - Stock Status -------------------------------------------------------------

#MISSING FIGURE - Biomass density (meats t/km2) by year faceted by area

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Stock Status", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Weightpertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
# slide 25 - Exploitation -------------------------------------------------------------

#MISSING FIGURE - Exploitation by year faceted by area

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Exploitation", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Weightpertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
# slide 26 - Natural Mortality (Instantaneous) -------------------------------------------------------------

#MISSING FIGURE - Natural Mortality by year faceted by area

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "Natural Mortality (Instantaneous)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  #ph_with(external_img(paste0(fig.dir, "SFA29AtoD.Weightpertow.Commercial.",year-1,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  
# slide 27 - Catch Scenerios -------------------------------------------------------------

#Need to edit layout for bullet points and figure

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Catch Scenerios", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1),
    str_list = c("Catch, exploitation, percent change in commercial biomass, and the probability of biomass decline were determined from the model and are presented as catch scenario tables for subareas A-D ",
                 paste0("Catch scenarios for " ,year+1," assume current year (",year,") estimates of condition and use the mean of natural mortality estimates from the last five years (2015 to ",year-1,") within subarea"))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2)  
  
  
# slide Loop for Scenerio Tables A-D  -------------------------------------------------------------

#ADD TABLES (Remove E from loop)

#Break for loop

## Loop for each subarea A-E

for (i in 1:length(subareas)) {
  
  if(subareas[i] == "A") area.title <- "SFA29A"
  if(subareas[i] == "B") area.title <- "SFA29B"
  if(subareas[i] == "C") area.title <- "SFA29C"
  if(subareas[i] == "D") area.title <- "SFA29D"
  if(subareas[i] == "E") area.title <- "SFA29E"
  
  newpres <- newpres %>%

add_slide(layout="Table", master="1_Office Theme") %>%
  ph_with(value = paste0("Subarea",subareas[i], "Catch Scenarios"), location = ph_location_label(ph_label = "Title"), index=1) #This is the title
  #ph_with(value = ex.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1)
  
  
}
  
newpres <- newpres %>%
  
# slide 32 - Subarea E -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Subarea E", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1, 1, 1, 1, 1),
    str_list = c("Not covered by the habitat suitability model",
                 "Only available data to assess this area includes survey data, commercial catch rate, and landings",
                 paste0("Abundance of commercial and recruit sized scallop increased/decreased in ",year-1),
                 "Catch rate cannot be reported (Privacy Act)",
                 paste0("In ",year-1,", commercial fleet landed X.X t against a catch limit of XX t "),
                 "Indications that the commercial abundance is relatively stable at the current level of removals")),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 
  
  
  # End title slide -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("SFA 29 West Advisory Committee Meeting: Science Advice for ",year), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("XX April ",year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(value = "Placopecten magellanicus", ph_location_label(ph_label = "Slide Number Placeholder"), index=1) #Pg num placeholder
  
# End of slides  -------------------------------------------------------------


n_slides <- length(newpres)
for (i_slide in 2:n_slides) {
  newpres <- newpres %>%
    on_slide(index = i_slide) %>%
    ph_with(value = i_slide, location = ph_location_type('sldNum'))
}

print(newpres, target = paste0("Y:/Inshore/SFA29/2022/Assessment/Documents/Presentations/WSAC/DRAFTWSAC_presentation_Officerbuilt_",year,".pptx"))

