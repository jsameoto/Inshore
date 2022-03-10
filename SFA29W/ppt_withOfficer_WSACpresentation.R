

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
# Slide with background as #1.

#add_slide(layout="Summary 2", master="1_Office Theme") %>%
#  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
#  ph_with(value = unordered_list(
#    level_list = c(1, 1, 1, 1,1),
#    str_list = c("Background", "Fishery data", "Survey data", "Habitat-based population model", paste0("Stock status and advice for ",assessment.year))),
#    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 

# slide 1 - Outline -------------------------------------------------------------

add_slide(layout="Summary 2", master="1_Office Theme") %>%
  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c( 1, 1, 1,1),
    str_list = c("Fishery data", "Survey data", "Habitat-based population model", paste0("Stock status and advice for ",assessment.year))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 

# slide 2 - Multi-Year Science Advice -------------------------------------------------------------
  
#add_slide(layout="Summary", master="1_Office Theme") %>%
#  ph_with(value = "Multi-Year Science Advice", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
#  ph_with(value = unordered_list(
#    level_list = c(1, 2, 3, 1, 2, 3, 3, 3),
#    str_list = c("Full Assessment Year:",
#                 "Regional Advisory Process (RAP)",
#                 "Assessment or Framework (CSAS Research Document)",
#                 "Update Year:",
#                 "Annual update to advice",
#                 "Annual assessment results (CSAS Science Response)",
#                 "Reference points and precautionary approach still apply",
#                 "Internal DFO peer-review")),
#    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 


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
  ph_with(value = paste0(survey.year, " Landings"), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = tacland.hux, location = ph_location_label(ph_label = "Table Placeholder"), use_loc_size = TRUE, index = 1) %>% 
  
# slide 8 - Survey: Habitat Suitability -------------------------------------------------------------

add_slide(layout="Dual Figure 3", master="1_Office Theme") %>%
  ph_with(value = "Survey: Habitat Suitability", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ScallopSDM_binned_coloured.png"), width = 6.67 , height = 4.83), location = ph_location_label(ph_label = "Figure Placeholder 1")) %>% 
  ph_with(external_img(paste0("Y:/Inshore/SFA29/",assessment.year,"/Assessment/Documents/Presentations/WSAC/SDMpercent_per_area_figure.png"), width = 8.11 , height = 1.43), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  
# slide 9- Pre-recruit Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Pre-recruit Abundance (<90mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_PreDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years figure
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Prerecruit.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  

# slide 10- Recruit Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Recruit Abundance (90-99mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_RecDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years figure
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Recruit.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  
# slide 11 - Commercial Abundance  -------------------------------------------------------------

add_slide(layout="Dual Figure 2", master="1_Office Theme") %>%
  ph_with(value = "Commercial Abundance (\u2265100mm)", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(external_img(paste0(fig.dir,"ContPlot_SFA29_ComDensity",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 1"), use_loc_size = TRUE) %>% #Previous years commercial cpue
  ph_with(external_img(paste0(fig.dir,"SFA29AtoD.Numberspertow.Commercial.",survey.year,".png"), width = 9, height = 9), location = ph_location_label(ph_label = "Figure Placeholder 2"), use_loc_size = TRUE) %>% 
  
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
                 paste0("Catch scenarios for " ,assessment.year," assume ",survey.year," estimates of growth, recruit abundance, and that natural mortality is the mean over the last 5 years (",survey.year-4," to ",survey.year,") within each subarea"))),
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

#WILL NEED TO EDIT SOME BULLETS MANUALLY EACH YEAR

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Subarea E", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c( 1, 1, 1, 1),
    str_list = c("Not covered by the habitat suitability model",
                 "Data available to assess this area includes survey data, commercial catch rate, and landings",
                 paste0("In ",survey.year,", no fishing occurred "),
                 paste0("In ",survey.year," commercial size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "comm") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow, recruit size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "rec") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow. In ",survey.year-2," commercial size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29E" & size == "comm") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow, recruit size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29E" & size == "rec") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow"))),
    location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 
  

# slide 33 - Conclusions  -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "Conclusions", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1, 1, 1),
    str_list = c(paste0("In ",survey.year,", commercial biomass densities in Subareas B, C, and D are above their respective USRs and are considered to be in the Healthy Zone"),
                 "Indications for Subareas A is that the commercial abundance is relatively stable at the current level of removals",
                 "In Subarea E, indications are that the commercial abundances have increased since 2019",
                 "For all Subareas, biomass declines are predicted, even if no catch is taken in 2022")),
location = ph_location_label(ph_label = "Text Placeholder"), level=2, index=2) %>% 
                 

  # End title slide -------------------------------------------------------------

add_slide(layout="Main Title Page", master="1_Office Theme") %>%
  ph_with(value = paste0("SFA 29 West Advisory Committee Meeting: Science Advice for ",assessment.year), location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value= paste0("XX April ",assessment.year,"\n \n Scallop Unit\n \n Population Ecology Division\n Fisheries and Oceans Canada\n Bedford Institute of Oceanography\n Dartmouth, Nova Scotia, B2Y 4A2"),
          location = ph_location_label(ph_label = "Subtitle"), index=1) #This is the subtite


# Number slides -----------------------------------------------------------

n_slides <- length(newpres)
for (i_slide in 2:n_slides) {
  newpres <- newpres %>%
    on_slide(index = i_slide) %>%
    ph_with(value = i_slide, location = ph_location_type('sldNum'))
}

# Save out ----------------------------------------------------------------

print(newpres, target = paste0("Y:/Inshore/SFA29/2022/Assessment/Documents/Presentations/WSAC/DRAFTWSAC_presentation_Officerbuilt_V3_",assessment.year,".pptx"))


#OPTION TO WRITE OUT NOTES TO WORD DOC

if (FALSE){

# SLIDE NOTES  -------------------------------------------------------------

# create new word document

notes_doc <- read_docx()
styles_info(notes_doc)


notes_doc %>% 
  
# notes for slide 1 - Outline -------------------------------------------------------------
body_add_par("Notes for slide 1 - Outline", style = "heading 1") %>% 
  
body_add_par(paste0("In today's presentation, we will begin by reviewing the fishery data and survey data. Then we will go over the habitat based population model and give an update to the stock status and advice for ",assessment.year)) %>% 
  
# notes for slide 2 - Landings Time Series -------------------------------------------------------------

body_add_par("Notes for slide 2 - Landings Time Series", style = "heading 1") %>% 
  
body_add_par("Here is the time series of the 29W landings, with year on the x-axis and landings in metric tonnes of meat weight on the y-axis.") %>% 
body_add_par("The solid black line is the TAC.") %>% 
body_add_par("From 2001 to 2009 there was a separate TAC for Full Bay and the East of Baccaro fleets, so you can see the landings broken up with Full Bay in white and East of Baccaro in blue.
As of 2010 the TAC was combined so the grey is the landings from both fleets together
The black indicates FSC landings which do not count against the TAC.") %>% 

# notes for slide 3 - Landings -------------------------------------------------------------

body_add_par("Notes for slide 3 - Landings: ADD FSC CATCH IF APPLICABLE AND NOTE ANY CHANGES TO WORDING FOR A AND E IF COMBINED", style = "heading 1") %>% 
  body_add_par("These are the landings from the 2021 fishery broken out by subarea.") %>% 
  body_add_par(paste0("- In Subarea A ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "A") %>%  dplyr::select(Landings) %>% pull(), " t were caught against a subarea catch limit of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "A") %>%  dplyr::select(TAC) %>% pull(), " t")) %>% 
  body_add_par(paste0("- There was no fishing in subarea E, which had a catch limit of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "E") %>%  dplyr::select(TAC) %>% pull(), " t")) %>% 
  body_add_par(paste0("- In Subarea B ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "B") %>%  dplyr::select(Landings) %>% pull(), " t were caught against a subarea catch limit of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "B") %>%  dplyr::select(TAC) %>% pull(), " t")) %>%
  body_add_par(paste0("- In Subarea C ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "C") %>%  dplyr::select(Landings) %>% pull(), " t were caught against a subarea catch limit of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "C") %>%  dplyr::select(TAC) %>% pull(), " t")) %>%
  body_add_par(paste0("- In Subarea D ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "D") %>%  dplyr::select(Landings) %>% pull(), " t were caught against a subarea catch limit of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "D") %>%  dplyr::select(TAC) %>% pull(), " t.")) %>%   body_add_par(paste0("The total commercial landings for 29W was ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "Total") %>%  dplyr::select(Landings) %>% pull(), " against a TAC of ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "Total") %>%  dplyr::select(TAC) %>% pull(), ". The total removals including FSC catch was ", tacland %>% filter(Year %in% c(survey.year) & Subarea == "Total") %>%  dplyr::select(Total.Landings) %>% pull())) %>% 
  body_add_par("Note: Subarea A has a dedicated catch limit of 10 tonnes. Fishing trips could not hail and move to any other area during the same trip when fishing in A.") %>% 

# notes for slide 4 - Habitat Suitability -------------------------------------------------------------
body_add_par("Notes for slide 4 - Habitat Suitability", style = "heading 1") %>% 
body_add_par("This is a map of the scallop habitat suitability for SFA 29W. This map only covers subareas A-D and the survey is designed based on the habitat categories. The suitability index represents the probability of scallop occurrence which ranges from 0 to 1 with values near 0 representing relatively low probability of scallop occurrence and values closer to 1 representing high probability of scallop occurrence. 
Habitat suitability probabilities are binned into 3 categories defined by Low [0, 0.3) represent by blue, Medium [0.3, 0.6) represented by grey, and High [0.6, 1.0) in red.

Looking at the table that corresponds to the square km and percentage of coverage by area, Subarea A has very little high habitat strata and the high is not sampled in the survey so what is presented for area A is low and medium habitat. Subarea D has the highest proportion of medium and high habitat, with 15.6 percent being high. ") %>% 
  
  
# notes for slide 5 - Pre-recruit Abundance  -------------------------------------------------------------

body_add_par("notes for slide 5 - Pre-recruit Abundance: CAN RE-ARRANGE WORDING FOR CURRENT AND PREVIOUS YEARS, ADD INCREASE/DECREASE PHRASING", style = "heading 1") %>% 
body_add_par(paste0("Here we have ",survey.year, " abundance plots of pre-recruit sized scallop which are scallop less than 90 mm in shell height.")) %>% 
  body_add_par("On the left we have the spatial plot showing pre-recruit sized scallop were patchy throughout SFA29 West. The darker the color, the higher the number per tow as indicated by the legends in the upper right corner.") %>% 
  body_add_par("And on the right, is the abundance time series plot showing year on the x axis, and mean number per tow on the y axis, broken out by subarea. The High habitat category is represented by the red lines with squares, the medium - by the grey lines with circles, and the low is represented by the blue lines with triangles. *Note there was no survey in 2020.") %>% 
  
  #Subarea A
  body_add_par(paste0("- Subarea A, in ",survey.year," pre-recruit abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " pre-recruit abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low.")) %>% 
  
#Subarea B
  body_add_par(paste0("- Subarea B, in ",survey.year," pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
#Subarea C
  body_add_par(paste0("- Subarea C, in ",survey.year," pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
               
#Subarea D
  body_add_par(paste0("- Subarea D, in ",survey.year," pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " pre-recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "high" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "med" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "low" & size == "prerec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  
# notes for slide 6 - Recruit Abundance  -------------------------------------------------------------

body_add_par("notes for slide 6 - Recruit Abundance: CAN RE-ARRANGE WORDING FOR CURRENT AND PREVIOUS YEARS, ADD INCREASE/DECREASE PHRASING", style = "heading 1") %>% 
  body_add_par(paste0("Here we have ",survey.year, " abundance plots of Recruit sized scallop which are scallop between 90 to 99 mm in shell height.")) %>% 
  body_add_par("On the left we have the spatial plot showing recruit sized scallop were patchy throughout SFA29 West. The darker the color, the higher the number per tow as indicated by the legends in the upper right corner.") %>% 
  body_add_par("And on the right, is the abundance time series plot showing year on the x axis, and mean number per tow on the y axis, broken out by subarea. The High habitat category is represented by the red lines with squares, the medium - by the grey lines with circles, and the low is represented by the blue lines with triangles. *Note there was no survey in 2020.") %>% 
  
  #Subarea A
  body_add_par(paste0("- Subarea A, in ",survey.year," recruit abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " recruit abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low.")) %>% 
  
  #Subarea B
  body_add_par(paste0("- Subarea B, in ",survey.year," recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  #Subarea C
  body_add_par(paste0("- Subarea C, in ",survey.year," recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  #Subarea D
  body_add_par(paste0("- Subarea D, in ",survey.year," recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " recruit abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "high" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "med" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "low" & size == "rec") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  
  # notes for slide 7 - Commercial Abundance  -------------------------------------------------------------

body_add_par("notes for slide 7 - Commercial Abundance: CAN RE-ARRANGE WORDING FOR CURRENT AND PREVIOUS YEARS, ADD INCREASE/DECREASE PHRASING", style = "heading 1") %>% 
  body_add_par(paste0("Here we have ",survey.year, " abundance plots of Commercial sized scallop which are scallop greater than or equal to 100 mm shell height.")) %>% 
  body_add_par("On the left we have the spatial plot showing commercial sized scallop were patchy throughout SFA29 West. The darker the color, the higher the number per tow as indicated by the legends in the upper right corner.") %>% 
  body_add_par("And on the right, is the abundance time series plot showing year on the x axis, and mean number per tow on the y axis, broken out by subarea. The High habitat category is represented by the red lines with squares, the medium - by the grey lines with circles, and the low is represented by the blue lines with triangles. *Note there was no survey in 2020.") %>% 
  
  #Subarea A
  body_add_par(paste0("- Subarea A, in ",survey.year," commercial abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29A" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " commercial abundances in the Medium habitat category were ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29A" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low.")) %>% 
  
  #Subarea B
  body_add_par(paste0("- Subarea B, in ",survey.year," commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29B" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29B" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  #Subarea C
  body_add_par(paste0("- Subarea C, in ",survey.year," commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29C" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29C" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low")) %>%
  
  #Subarea D
  body_add_par(paste0("- Subarea D, in ",survey.year," commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year & SUBAREA == "SFA29D" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low. In ", survey.year-2, " commercial abundances in the High habitat category were ",format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "high" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), ", ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "med" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in the Medium and ", format(round((survey.ind %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29D" & Strata == "low" & size == "comm") %>% dplyr::select("Mean"))[1,1],1),nsmall = 1), " in Low"))

# notes for slide 8 - Stock Status  -------------------------------------------------------------

bmd.A.med <- format(round((SFA29A %>% filter(Year == survey.year & Habitat == "Med") %>% dplyr::select(BM.dens))[1,1],1), nsmall = 1)
bmd.B.high <- format(round((SFA29B %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(BM.dens))[1,1],1), nsmall = 1)
bmd.C.high <- format(round((SFA29C %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(BM.dens))[1,1],1), nsmall = 1)
bmd.D.high <- format(round((SFA29D %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(BM.dens))[1,1],1), nsmall = 1)

bmd.B.change <- "change"
if(as.numeric(bmd.B.high) > 2.24) { #USR for AREA B
  bmd.B.change <- "above"
}
if(as.numeric(bmd.B.high) < 2.24) { #USR for AREA B
  bmd.B.change <- "below"
}

#Subarea C
bmd.C.change <- "change"
if(as.numeric(bmd.C.high) > 2.82) { #USR for AREA C
  bmd.C.change <- "above"
}
if(as.numeric(bmd.C.high) < 2.82) { #USR for AREA C
  bmd.C.change <- "below"
}

#Subarea D
bmd.D.change <- "change"
if(as.numeric(bmd.D.high) > 2.6) { #USR for AREA D
  bmd.D.change <- "above"
}
if(as.numeric(bmd.D.high) < 2.6) { #USR for AREA D
  bmd.D.change <- "below"
}

notes_doc %>% 

body_add_par("Notes for slide 8 - Stock Status", style = "heading 1") %>% 
  body_add_par("This plot shows the commercial biomass density (tonnes per squared km) in the Medium in A, and High habitat of B, C and D. Black dotted lines represent the USR, and red solid line represents the LRP and the Density associated with Maximum Sustainable Yield (DMSY) is indicated by the dot-dashed (blue) line for Subareas B, C, and D.") %>% 
  
  body_add_par("The LRPs and USRs are 1.12 t/km2 and 2.24 t/km2 Subarea B,1.41 t/km2 and 2.82 t/km2 for Subarea C and 1.3 t/km2 and 2.6 t/km2 for Subarea D, respectively.
 The DMSY values are 3.75, 4.68, 4.32 t/km2 for Subareas B, C, and D, respectively.") %>% 
  
  body_add_par(paste0("In ", survey.year, " commercial biomass denisty in")) %>% 
  body_add_par(paste0("Subarea A was ",bmd.A.med, " t/km2 in the Medium habitat.")) %>%
  body_add_par(paste0("In Subarea B, ", survey.year, " commercial biomass density was ", bmd.B.high," t/km2 in the High habitat category and ", bmd.B.change," the USR of 2.24 t/km2.")) %>% 
  body_add_par(paste0("In Subarea C, ", survey.year, " commercial biomass density was ", bmd.C.high," t/km2 in the High habitat category and ", bmd.C.change," the USR of 2.82 t/km2.")) %>%
  body_add_par(paste0("In Subarea D, ", survey.year, " commercial biomass density was ", bmd.D.high," t/km2 in the High habitat category and ", bmd.D.change," the USR of 2.6 t/km2."))
  
# notes for slide 9 - Exploitation  -------------------------------------------------------------

exploit.A.med <- format(round((SFA29A %>% filter(Year == survey.year & Habitat == "Med") %>% dplyr::select(mu))[1,1],2), nsmall = 2)
exploit.B.high <- format(round((SFA29B %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(mu))[1,1],2), nsmall = 2)
exploit.C.high <- format(round((SFA29C %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(mu))[1,1],2), nsmall = 2)
exploit.D.high <- format(round((SFA29D %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(mu))[1,1],2), nsmall = 2)

notes_doc %>% 
                 
body_add_par("Notes for slide 9 - Exploitation", style = "heading 1") %>% 
  body_add_par("Here we have model estimates of exploitation for Medium habitat category in Subarea A and High habitat for Subareas B, C and D. Exploitation is the removals relative to the biomass.") %>%
  body_add_par(paste0("In Subarea A, in ", survey.year, " the exploitation was ", exploit.A.med, " in the Medium habitat category")) %>% 
  body_add_par(paste0("In Subarea B, in ", survey.year, " the exploitation was ", exploit.B.high, " in the High habitat category")) %>% 
  body_add_par(paste0("In Subarea C, in ", survey.year, " the exploitation was ", exploit.C.high, " in the High habitat category")) %>% 
  body_add_par(paste0("In Subarea D, in ", survey.year, " the exploitation was ", exploit.D.high, " in the High habitat category"))
  
# notes for slide 10 - Natural Mortality  -------------------------------------------------------------
  
natmort.A.med <- format(round((SFA29A %>% filter(Year == survey.year & Habitat == "Med") %>% dplyr::select(nat.m))[1,1],2), nsmall = 2)
natmort.B.high <- format(round((SFA29B %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(nat.m))[1,1],2), nsmall = 2)
natmort.C.high <- format(round((SFA29C %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(nat.m))[1,1],2), nsmall = 2)
natmort.D.high <- format(round((SFA29D %>% filter(Year == survey.year & Habitat == "High") %>% dplyr::select(nat.m))[1,1],2), nsmall = 2)

notes_doc %>% 

body_add_par("notes for slide 10 - Natural Mortality - EDIT ABOVE/BELOW LONG TERM MEDIANS IF NEEDED", style = "heading 1") %>% 
  body_add_par(paste0("This is a plot showing modeled instantaneous natural mortality for Medium habitat category in Subarea A and High habitat for Subareas B, C and D. The 5-year (",survey.year-4,"-",survey.year,") mean natural mortality is indicated by the dashed black line and the long-term median (2001-",survey.year-1,") is indicated by the solid black line.")) %>% 
  body_add_par(paste0("In ", survey.year," in Subareas A and B, the instantaneous natural mortality model estimates were above their respective long-term medians (2001-",survey.year-1,").")) %>% 
  body_add_par(paste0("In ", survey.year," in Subareas C and D, the instantaneous natural mortality model estimates were below their respective long-term medians (2001-",survey.year-1,").")) %>% 
  
  body_add_par(paste0("In Subarea A, in ",survey.year,", natural mortality was ",natmort.A.med, " in the Medium habitat category.")) %>% 
  body_add_par(paste0("In Subarea B, in ",survey.year,", natural mortality was ",natmort.B.high, " in the High habitat category.")) %>% 
  body_add_par(paste0("In Subarea C, in ",survey.year,", natural mortality was ",natmort.C.high, " in the High habitat category.")) %>% 
  body_add_par(paste0("In Subarea D, in ",survey.year,", natural mortality was ",natmort.D.high, " in the High habitat category.")) %>%
  
# notes for slide 11 - Catch Senarios  -------------------------------------------------------------

body_add_par("notes for slide 11 - Catch Scenarios", style = "heading 1") %>%
  body_add_par(paste0("Catch, exploitation, percent change in commercial biomass, and the probability of biomass decline were determined from the model and are presented as catch scenario tables for subareas A-D. Catch scenarios for ", assessment.year," assume ", survey.year," estimates of growth, recruit abundance, and that natural mortality is the mean over the last 5 years (",survey.year-4,"-",survey.year,") within each subarea.")) %>% 
  
  # notes for slide 12 - Subarea A Catch Senarios  -------------------------------------------------------------

body_add_par("notes for slide 12 - Subarea A Catch Scenario", style = "heading 1") %>%
  body_add_par("We will now go through the catch scenarios for each modeled subarea. For an example on how to interpret the tables, we will use row 4. An example of a catch scenario for Subarea A is as follows:") %>% 

  body_add_par(paste0("a total Subarea catch of ", format(SFA29A.decision.table[4,2], nsmall = 1)," t corresponds to an exploitation of ", format(SFA29A.decision.table[4,1], nsmall = 2)," in the Medium habitat category, this is projected to result in a ", format(abs(SFA29A.decision.table[4,4]), nsmall = 1),"% biomass decrease in the Medium habitat category, and the probability of a biomass increase in the Medium habitat category is ", format(SFA29A.decision.table[4,5], nsmall = 2),". This is associated with a predicted ", format(abs(SFA29A.decision.table[4,6]), nsmall = 1), "% biomass decline in all of Subarea A; the associated probability of biomass increase for all of Subarea A is ", format(SFA29A.decision.table[4,7], nsmall = 2),".")) %>% 
  
# notes for slide 13 - Subarea B Catch Senarios  -------------------------------------------------------------
body_add_par("notes for slide 13 - Subarea B Catch Scenario", style = "heading 1") %>%
  body_add_par(" An example of a catch scenario for Subarea B is as follows:") %>% 
  body_add_par(paste0("a total Subarea catch of ", format(SFA29B.decision.table[4,2], nsmall = 1)," t corresponds to an exploitation of ", format(SFA29B.decision.table[4,1], nsmall = 2)," in the High habitat category, this is projected to result in a ", format(abs(SFA29B.decision.table[4,4]), nsmall = 1),"% biomass decrease in the High habitat category, and the probability of a biomass increase in the High habitat category is ", format(SFA29B.decision.table[4,5], nsmall = 2),". This is associated with a predicted ", format(abs(SFA29B.decision.table[4,6]), nsmall = 1), "% biomass decline in all of Subarea B; the associated probability of biomass increase for all of Subarea B is ", format(SFA29B.decision.table[4,7], nsmall = 2),". After ", format(SFA29B.decision.table[4,2], nsmall = 1)," t of catch is removed, the probability of being above the LRP is ", format(SFA29B.decision.table[4,8], nsmall = 2),", and the probability of being above the USR is ", format(SFA29B.decision.table[4,9], nsmall = 2),".")) %>% 

  # notes for slide 14 - Subarea C Catch Senarios  -------------------------------------------------------------
body_add_par("notes for slide 14 - Subarea C Catch Scenario", style = "heading 1") %>%
  body_add_par(" An example of a catch scenario for Subarea C is as follows:") %>% 
body_add_par(paste0("a total Subarea catch of ", format(SFA29C.decision.table[4,2], nsmall = 1)," t corresponds to an exploitation of ", format(SFA29C.decision.table[4,1], nsmall = 2)," in the High habitat category, this is projected to result in a ", format(abs(SFA29C.decision.table[4,4]), nsmall = 1),"% biomass decrease in the High habitat category, and the probability of a biomass increase in the High habitat category is ", format(SFA29C.decision.table[4,5], nsmall = 2),". This is associated with a predicted ", format(abs(SFA29C.decision.table[4,6]), nsmall = 1), "% biomass decline in all of Subarea C; the associated probability of biomass increase for all of Subarea C is ", format(SFA29C.decision.table[4,7], nsmall = 2),". After ", format(SFA29C.decision.table[4,2], nsmall = 1)," t of catch is removed, the probability of being above the LRP is ", format(SFA29C.decision.table[4,8], nsmall = 2),", and the probability of being above the USR is ", format(SFA29C.decision.table[4,9], nsmall = 2),".")) %>% 

  # notes for slide 15 - Subarea D Catch Senarios  -------------------------------------------------------------
body_add_par("notes for slide 15 - Subarea D Catch Scenario", style = "heading 1") %>%
  body_add_par(" An example of a catch scenario for Subarea D is as follows:") %>% 
  body_add_par(paste0("a total Subarea catch of ", format(SFA29D.decision.table[4,2], nsmall = 1)," t corresponds to an exploitation of ", format(SFA29D.decision.table[4,1], nsmall = 2)," in the High habitat category, this is projected to result in a ", format(abs(SFA29D.decision.table[4,4]), nsmall = 1),"% biomass decrease in the High habitat category, and the probability of a biomass increase in the High habitat category is ", format(SFA29D.decision.table[4,5], nsmall = 2),". This is associated with a predicted ", format(abs(SFA29D.decision.table[4,6]), nsmall = 1), "% biomass decline in all of Subarea D; the associated probability of biomass increase for all of Subarea D is ", format(SFA29D.decision.table[4,7], nsmall = 2),". After ", format(SFA29D.decision.table[4,2], nsmall = 1)," t of catch is removed, the probability of being above the LRP is ", format(SFA29D.decision.table[4,8], nsmall = 2),", and the probability of being above the USR is ", format(SFA29D.decision.table[4,9], nsmall = 2),".")) %>% 

  # notes for slide 16 - Subarea E   -------------------------------------------------------------
body_add_par("notes for slide 16 - Subarea E - EDIT IF FISHING/NO FISHING OCCURRED", style = "heading 1") %>%
  body_add_par(paste0("Subarea E is not covered by the habitat suitability model. Data available to assess this area includes survey data, commercial catch rate, and landings. In ", survey.year, " no fishing occurred.")) %>%
  body_add_par(paste0("In ", survey.year, " commercial size abundances were ",format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "comm") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow, recruit size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year & SUBAREA == "SFA29E" & size == "rec") %>% dplyr::select("yst"))[1,1],1),nsmall = 1),". In ", survey.year-2, " commercial size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29E" & size == "comm") %>% dplyr::select("yst"))[1,1],1),nsmall = 1), " per tow, recruit size abundances were ", format(round((survey.ind.E %>% filter(YEAR == survey.year-2 & SUBAREA == "SFA29E" & size == "rec") %>% dplyr::select("yst"))[1,1],1),nsmall = 1)," per tow.")) %>% 
  
  
# notes for slide 17 - Conclusions  -------------------------------------------------------------
body_add_par("notes for  slide 17 - Conclusions - SEE UPDATE DOCUMENT FOR CONCLUSION NOTES", style = "heading 1")

  
  
print(notes_doc, target = "Y:/Inshore/SFA29/2022/Assessment/Documents/Presentations/WSAC/WSAC_presentation_notes.docx")

}