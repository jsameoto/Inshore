

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

survey.year <- 2021
assessment.year <- 2022
fig.dir <- paste0("Y:/Inshore/SFA29/",assessment.year,"/Assessment/Figures/") #figure directory
direct <- paste0("Y:/Inshore/SFA29/", assessment.year,"/Assessment/")
subareas <- c("SFA29A", "SFA29B", "SFA29C", "SFA29D")

# Load Functions
funcs <- "https://raw.githubusercontent.com/Mar-scal/Inshore/master/SFA29W/SFA29_HarvestScenTabFunc.R"
dir <- getwd()
for(fun in funcs) 
{
  # temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# Load model results ------------------------------------------------------

#Model results
SFA29A <- read.csv(paste0(direct,"Data/Model/SFA29A/SFA29.A.model.results.summary.", survey.year, ".csv"))
SFA29A.decision.table <- read.csv(paste0(direct,"Data/Model/SFA29A/SFA29.A.mod.Decision.table.", survey.year, ".csv"))

SFA29B <- read.csv(paste0(direct,"Data/Model/SFA29B/SFA29.B.model.results.summary.", survey.year, ".csv"))
SFA29B.decision.table <- read.csv(paste0(direct,"Data/Model/SFA29B/SFA29.B.mod.Decision.table.", survey.year, ".csv"))

SFA29C <- read.csv(paste0(direct,"Data/Model/SFA29C/SFA29.C.model.results.summary.", survey.year, ".csv"))
SFA29C.decision.table <- read.csv(paste0(direct,"Data/Model/SFA29C/SFA29.C.mod.Decision.table.", survey.year, ".csv"))

SFA29D <- read.csv(paste0(direct,"Data/Model/SFA29D/SFA29.D.model.results.summary.", survey.year, ".csv"))
SFA29D.decision.table <- read.csv(paste0(direct,"Data/Model/SFA29D/SFA29.D.mod.Decision.table.", survey.year, ".csv"))

#Fishery data
tac <- read.csv(paste0(direct,"Data/CommercialData/SFA29_TACbyYr.csv"))
landings <- read.csv(paste0(direct,"Data/CommercialData/SFA29_totalLandings_YearFleetFSC.csv"))
#For Appendix
tacland <- read.csv(paste0(direct,"Data/CommercialData/SFA29W_TACandLandingsCompiled_",survey.year,".csv"), fileEncoding = 'UTF-8-BOM') # fileEncoding arg included because Year had a funky hidden character in the front...  ¯\_(???)_/¯
cpue <- read.csv(paste0(direct,"Data/CommercialData/SFA29_CPUE_byYrAreaFleet_",survey.year,".csv"))

#Survey data
survey.ind <- read.csv(paste0(direct,"Data/SurveyIndices/SDM.HighMedLow.2001to",survey.year,".Numbers.csv"))
survey.ind.E <- read.csv(paste0(direct, "Data/SurveyIndices/SubareaE.ExploratoryMeanbyTow.Numbers.",survey.year,".csv"))
condition <- read.csv(paste0(direct, "Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",survey.year,".csv"))



# Start Presentation ------------------------------------------------------

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
  ph_with(value = paste0("SFA 29 West Advisory Committee Meeting: Science Advice for ",assessment.year), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("XX April ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(value = "Placopecten magellanicus", ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>% #Pg num placeholder
  

# slide 1 - Outline -------------------------------------------------------------

add_slide(layout="Summary 2", master="1_Office Theme") %>%
  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1, 1, 1,1),
    str_list = c("Background", "Fishery data", "Survey data", "Habitat-based population model", paste0("Stock status and advice for ",assessment.year))),
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
  ph_with(external_img(paste0(fig.dir,"CommercialData/SFA29WTACandLandings",survey.year,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) 

  #stop for table generation
  
# slide 4 - YYYY Landings  -------------------------------------------------------------

#Make Landings Table

#WILL HAVE TO ADJUST IF SUBAREA A AND E ARE COMBINED!!!!
tacland <- tacland %>% 
  mutate(TAC = replace_na(TAC, "-")) %>% 
  mutate(Landings = replace_na(Landings, "-")) %>% 
  mutate(FSC = replace_na(FSC, "-"))

colnames(tacland) <- c("Year", "Subarea", "TAC (t)", "Landings (t)", "FSC (t)", "Total Landings (t)")
tacland <- tacland %>% filter(Year %in% c(survey.year)) %>% dplyr::select(!Year)

#str(tacland)
tacland.hux <- tacland %>% 
  as_hux() %>% 
  theme_basic() %>% 
  set_tb_padding(0)
row.names(tacland.hux) <- NULL

#huxtable format - function(row#, column#):
tacland.hux <- tacland.hux %>%
  set_bold(1, everywhere) %>% #Bold headers
  set_number_format(everywhere, c(3,5), 1) %>% #defaults to scientific notation - so specify 1 decimal places for Landings.
  set_align(everywhere, 1:2, "left") %>% # year and SPA columns align left
  #set_valign(everywhere, 1, "top") %>% #set vertical alignment for year and SPA columns
  set_align(everywhere, 2:5 , "right") %>% #right align TAC, Landings, FSC and Total Landings columns
  #set_valign(everywhere, 1, "top") %>% #vertical align top for TAC column
  set_valign(everywhere, everywhere, "bottom") %>% #vertical align top for Landings, FSC and Total Landings columns
  #set_align(1,2:5, "centre") %>% #centre TAC, Landings, FSC and Total Landings headers
  set_valign(1,1:5, "top") %>% #centre TAC, Landings, FSC and Total Landings headers
  set_font_size(18) %>% #set font of table
  set_bold(7, everywhere) %>%
  set_number_format(7, 2, 0) %>% #Total rows
  set_col_width(0.05) %>%
  set_col_width(1, 0.03) %>% 
  set_col_width(2, 0.04) %>%
  set_col_width(3, 0.05) %>%
  set_col_width(4, 0.04) %>%
  set_col_width(5, 0.07) %>%
  set_width(2.8) %>% #Set table width
  set_bottom_border(final(1), everywhere) %>%
  set_all_padding(4) %>%
  set_top_border(1:2, everywhere) %>%
  as_flextable() %>% autofit()

#Continue making slides again:

newpres <- newpres %>%
  
  add_slide(layout="Table", master="1_Office Theme") %>%
  ph_with(value = paste0(survey.year, "Landings"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = tacland.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1) %>% 
  
# slide 8 - Survey: Habitat Suitability -------------------------------------------------------------

add_slide(layout="Dual Figure 3", master="1_Office Theme") %>%
  ph_with(value = "Survey: Habitat Suitability", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ScallopSDM_binned_coloured.png"), width = 6.67 , height = 4.83), location = ph_location_label(ph_label = "Figure Placeholder 1")) %>% 
  ph_with(external_img(paste0("Y:/Inshore/SFA29/",assessment.year,"/Assessment/Documents/Presentations/WSAC/SDMpercent_per_area_figure.png"), width = 8.11 , height = 1.43), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  
# slide 11 - Commercial Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Commercial Abundance (>100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_ComDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Commercial.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 

# slide 10- Recruit Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Recruit Abundance (90-99mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_RecDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years figure
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Recruit.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  
# slide 9- Pre-recruit Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Pre-recruit Abundance (<90mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_PreDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years figure
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Prerecruit.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  

# slide 14 - Subarea E -------------------------------------------------------------

#add_slide(layout="Single Figure 3", master="1_Office Theme") %>%
#  ph_with(value = "Subarea E Survey Abundance", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
#  ph_with(external_img(paste0(fig.dir, "SFA29E.Numberspertow.",survey.year,".png"), width = 6.50 , height = 7.2), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  
  # slide 15 - Stock Status -------------------------------------------------------------

add_slide(layout="Single Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Stock Status", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "/Model/Commercial_Biomass_Density",survey.year,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  # slide 16 - Exploitation -------------------------------------------------------------

add_slide(layout="Single Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Exploitation", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "/Model/Model_Exploitation",survey.year,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  # slide 17 - Natural Mortality (Instantaneous) -------------------------------------------------------------

add_slide(layout="Single Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Natural Mortality (Instantaneous)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir, "Model/NatMort_Instantaneous",survey.year,".png"), width = 6.50 , height = 5.73), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% 
  
  
  # slide 18 - Catch Scenerios -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Catch Scenerios", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1),
    str_list = c("Catch, exploitation, percent change in commercial biomass, and the probability of biomass decline were determined from the model and are presented as catch scenario tables for subareas A-D ",
                 paste0("Catch scenarios for " ,assessment.year+1," assume current year (",assessment.year,") estimates of condition and use the mean of natural mortality estimates from the last five years (2015 to ",survey.year,") within subarea"))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2)  

#Break for loop

# slide Loop for Scenerio Tables A-D  -------------------------------------------------------------

for (i in 1:length(subareas)) {
  
  if(subareas[i] == "SFA29A") area.title <- "Subarea A"
  if(subareas[i] == "SFA29B") area.title <- "Subarea B"
  if(subareas[i] == "SFA29C") area.title <- "Subarea C"
  if(subareas[i] == "SFA29D") area.title <- "Subarea D"
  
  if(subareas[i] == "SFA29A") area.caption <- paste0("Catch scenario table for SFA 29W Subarea A to evaluate ",assessment.year, 
                                                " catch levels in terms of expected changes in biomass (%) and probability (Pr.) of increase.") 
  if(subareas[i] == "SFA29B") area.caption <- paste0("Catch scenario table for SFA 29W Subarea B to evaluate ",assessment.year," catch levels in terms of expected changes in biomass (%), probability (Pr.) of increase, and probability of being above the Lower Reference Point (LRP: 1.12 t/km²) and Upper Stock Reference (USR: 2.24 t/km²).") 
  if(subareas[i] == "SFA29C") area.caption <- paste0("Catch scenario table for SFA 29W Subarea C to evaluate ",assessment.year, " catch levels in terms of expected changes in biomass (%), probability (Pr.) of increase, and probability of being above the Lower Reference Point (LRP: 1.41 t/km²) and Upper Stock Reference (USR: 2.82 t/km²).") 
  if(subareas[i] == "SFA29D") area.caption <- paste0("Catch scenario table for SFA 29W Subarea D to evaluate ",assessment.year," catch levels in terms of expected changes in biomass (%), probability (Pr.) of increase, and probability of being above the Lower Reference Point (LRP: 1.3 t/km²) and Upper Stock Reference (USR: 2.6 t/km²).") 
  
#First run the table function
sfa29.harvest.scen.tab(area = subareas[i], catch.range = catch.range, type = "presentation")

#Add slides

newpres <- newpres %>%  
    
    add_slide(layout="Table 2", master="1_Office Theme") %>%
    ph_with(value = paste0(area.title, " Catch Scenarios"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
    ph_with(value = area.caption,location = ph_location_label(ph_label = "Text Placeholder"), index = 1) %>%
    ph_with(value = ex.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1)
  
} #Break loop


newpres <- newpres %>%
  
# slide 32 - Subarea E -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Subarea E", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c( 1, 1, 1, 1),
    str_list = c("Not covered by the habitat suitability model",
                 "Only available data to assess this area includes survey data, commercial catch rate, and landings",
                 paste0("EDIT MANUALLY: In ",survey.year,", no fishing occured "),
                 paste0("Commercial size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "comm") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow, recruit size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "rec") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow"))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 
  
  
  # End title slide -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("SFA 29 West Advisory Committee Meeting: Science Advice for ",assessment.year), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("XX April ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) %>%  #This is the subtite
  ph_with(value = "Placopecten magellanicus", ph_location_label(ph_label = "Slide Number Placeholder"), index=1)  #Pg num placeholder


# Number slides -----------------------------------------------------------

n_slides <- length(newpres)
for (i_slide in 2:n_slides) {
  newpres <- newpres %>%
    on_slide(index = i_slide) %>%
    ph_with(value = i_slide, location = ph_location_type('sldNum'))
}

# Save out ----------------------------------------------------------------

print(newpres, target = paste0("Y:/Inshore/SFA29/2022/Assessment/Documents/Presentations/WSAC/DRAFTWSAC_presentation_Officerbuilt_",assessment.year,".pptx"))

# EXTRA Slides  -------------------------------------------------------------

