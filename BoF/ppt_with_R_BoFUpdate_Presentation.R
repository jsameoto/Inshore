
require(officer)
require(tidyverse)
require(flextable)

# Define ------------------------------------------------------------------

assessment.year <- 2021
fig.dir <- paste0("Y:/INSHORE SCALLOP/BoF/",assessment.year,"/Assessment/Figures/") #figure directory

# Load model results ------------------------------------------------------

#loadEnvironment <- function(RData, env = new.env()){
#  load(RData, env)
#  return(env)
#}
#SPA1A <- loadEnvironment(paste0(direct,"Data/Model/SPA1A/Model_results_and_diagnostics_", year, "_1A.RData")) #SPA1A
#PA1B <- loadEnvironment(paste0(direct,"Data/Model/SPA1B/Model_results_and_diagnostics_", year, "_1B.RData")) #SPA1B
#SPA3 <- loadEnvironment(paste0(direct,"Data/Model/SPA3/Model_results_and_diagnostics_", year, "_3.RData")) #SPA3
#SPA4 <- loadEnvironment(paste0(direct,"Data/Model/SPA4/Model_results_and_diagnostics_", year, "_4.RData")) #SPA4
#PA5 <-  read.csv(paste0(direct,"Data/SurveyIndices/SPA1A1B4and5/SPA5.Index.Weight2021.csv")) #SPA5
#SPA6 <- loadEnvironment(paste0(direct,"Data/Model/SPA6/Model_results_and_diagnostics_", year, "_6.RData")) #SPA6
#PA6.catch <- read.csv(paste0(direct,"Data/CommercialData/CPUE_spa6_combined_",year+1,".csv")) %>% mutate(year = as.numeric(year))#SPA6


# create an annotated base ppt with all layouts
#annotate_base(path = "Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/DO_NOT_EDIT_Template_InshoreUpdate_R_ppt.pptx", output_file = "Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/annotated_layout.pptx")

# build a new ppt from the template
newpres <- read_pptx("Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/DO_NOT_EDIT_Template_InshoreUpdate_R_ppt.pptx")

# For reference --------
#Don't need to run - but useful to get names of slide layouts and place holder names
# Get names of placeholders for adding text/images/content to. These placeholders are added and named manually in powerpoint - *slide master*

layout_summary(newpres)
main.title.pg <- layout_properties(x = newpres, layout = "Main Title Page", master = "1_Office Theme") %>% dplyr::select(ph_label)
title.content.pg <- layout_properties(x = newpres, layout = "Title and Content", master = "1_Office Theme") %>% dplyr::select(ph_label)
summary.layout.pg <- layout_properties(x = newpres, layout = "Summary", master = "1_Office Theme") %>% dplyr::select(ph_label)
single.fig.pg <- layout_properties(x = newpres, layout = "Single Figure", master = "1_Office Theme") %>% dplyr::select(ph_label)
dual.fig.pg <- layout_properties(x = newpres, layout = "Dual Figure", master = "1_Office Theme") %>% dplyr::select(ph_label)
table.layout.pg <- layout_properties(x = newpres, layout = "Table", master = "1_Office Theme") %>% dplyr::select(ph_label)


#In powerpoint slidemaster view - hit alt F10 to see placeholder names. Rename as required. These will be the placeholder labels i.e. "ph_labels" in the R script.

main.title.pg #title page - with scallop image, and lines
title.content.pg # side by side text box and figure/text box, lower text box (horizontal across slide)
summary.layout.pg #text with bullet points
single.fig.pg #Full slide figure, no text
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

  
# slide 2 - Outline -------------------------------------------------------------

add_slide(layout="Title and Content", master="1_Office Theme") %>%
  ph_with(value = "Outline", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = "Review by area", location = ph_location_label(ph_label = "Text Placeholder 1"), index=1) %>%
  ph_with(value = unordered_list(
    level_list = c(1, 2, 2, 2, 2, 2),
    str_list = c("Review by area", "SPA 1A", "SPA 1B", "3", "4 & 5", "6")),
    location = ph_location_label(ph_label = "Text Placeholder 1"), level=2, index=2) %>% 
  ph_with(value = paste0("*Note that all landing values and logbook data for ",assessment.year," are preliminary as of October X, ", assessment.year), location = ph_location_label(ph_label = "Text Placeholder 2"), index = 1) %>% 
  ph_with(value = 2, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%  #Pg num placeholder
  ph_with(external_img("Y:/INSHORE SCALLOP/Inshore Scallop Fishing Area Map/InshoreScallopFishingAreas_English_updated2021.png"), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>% #Adding an external image - can change height/width, but currently set to match the size of the placeholder in powerpoint.

  
# slide 3 - SPA 1A Summary -------------------------------------------------------------

add_slide(layout="Summary", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A Summary", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = unordered_list(
    level_list = c(1, 1, 1, 1), #Bullet point levels
    str_list = c(paste0("Total landings in the ",assessment.year," fishing year were X t against a TAC of X t (X t before post-quota reconciliation)"), #each bullet pnt
                 paste0("Commercial catch rate in the ",assessment.year," fishing year was X kg/h, a decrease from ",assessment.year-2," (X kg/h)"), 
                 paste0("The biomass estimate of recruit scallops in ",assessment.year," was X t, similar to ",assessment.year-2," (X t), and below the long-term (1997-",assessment.year,") median of X t"), 
                 paste0("Commercial population biomass for ",assessment.year," estimated by the model was X t (meats) which is in the healthy zone"))),
    location = ph_location_label(ph_label = "Text Placeholder"), index=1) %>% 
  ph_with(value = 3, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%  #Pg num placeholder

  
# slide 4 - SPA 1A Landings Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A: Landings", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = 4, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%   #Pg num placeholder
  
# slide 5 - SPA 1A Commercial Catch Rate Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A: Commercial Catch Rate", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = 5, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%   #Pg num placeholder 
  
  
# slide 6 - SPA 1A Commercial Fishery Figure -------------------------------------------------------------

add_slide(layout="Dual Figure", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A: Commercial Fishery", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = 6, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%   #Pg num placeholder 
  
# slide 7 - SPA 1A Condition Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A: Condition", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = 7, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%   #Pg num placeholder
  ph_with(external_img(paste0(fig.dir,"SPA1A_ConditionTimeSeries.png")), location = ph_location_label(ph_label = "Figure Placeholder"), use_loc_size = TRUE) %>%
  
# slide 8 - SPA 1A Pre-recruit Abundance Figure -------------------------------------------------------------

add_slide(layout="Single Figure", master="1_Office Theme") %>%
  ph_with(value = "SPA 1A: Pre-recruit (<65mm) Abundance", location = ph_location_label(ph_label = "Title"), index=1) %>% #This is the title
  ph_with(value = 8, ph_location_label(ph_label = "Slide Number Placeholder"), index=1) %>%   #Pg num placeholder
  ph_with(external_img(paste0(fig.dir,"ContPlot_SPA1A_PreDensity2021.png"), width = 6, height = 6), location = ph_location(ph_label = "Figure Placeholder", left = 1.75, top = 0.82), use_loc_size = FALSE)

##Before running - enusre the powerpoint is not open - Rstudio crashes if you try to write over it when it is open.               
print(newpres, target = "Y:/INSHORE SCALLOP/BoF/2021/Assessment/Documents/Presentations/Example_Officerbuilt_InshoreUpdate_presentation.pptx")   

