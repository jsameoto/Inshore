#This function is to produce the Harvest Scenerio Tables for the Inshore Scallop ISAC presentation.
#Note that tables for SPA1A, 1B, 3 and 4 are formatted the same, but the table for SPA6 is formatted differently.

#FORMATING MAY NEED ADJUSTING - SEE https://hughjonesd.github.io/huxtable/reference/index.html for table package references.
#In particular: Table width and height (see example)
# set_width(1.5) %>%
# set_height(.62) 

#And catch.range may need adjusting for each area depending on how many scenerios want to be displayed.

#Arguments:
# area - SPA#
# catch range - define range of values shown within the table - varies for each area

#Function to produce harvest scenario tables for each area (uses packages huxtable and flextable)

harvest.scen.tab = function(area = area, catch.range = catch.range)
  
{
  require(huxtable) || stop("Install huxtable")
  require(flextable) || stop("Install flextable")
  require(dplyr) || stop("Install dplyr")
  
  if(area == "SPA1A") {
    catch.range <- c(280, 300, 320, 340, 360, 380, 400)
    decision.table <- SPA1A$decision.table
    table.caption <- paste0("Table 1. Harvest scenario table for SPA 1A to evaluate ", year,"/", year+1, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA1A$USR, " t), and above the Lower Reference Point (LRP; ", SPA1A$LRP, " t). Potential catches (t) in ", year,"/", year+1," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }
  
  if(area == "SPA1B") {
    catch.range <- c(300, 325, 350, 375, 400, 425, 450, 475)
    decision.table <- SPA1B$decision.table
    table.caption <- paste0("Table 2. Harvest scenario table for SPA 1B to evaluate ", year,"/", year+1, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA1B$USR, " t), and above the Lower Reference Point (LRP; ", SPA1B$LRP, " t). Potential catches (t) in ", year,"/", year+1," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }
  
  if(area == "SPA3") {
    catch.range <- c(100, 120, 140, 160, 180, 200, 220, 240, 260)
    decision.table <- SPA3$decision.table
    table.caption <- paste0("Table 3. Harvest scenario table for SPA 3 to evaluate ", year,"/", year+1, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA3$USR, " t), and above the Lower Reference Point (LRP; ", SPA3$LRP, " t). Potential catches (t) in ", year,"/", year+1," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }
  
  if(area == "SPA4") {
    catch.range <- c(100, 120, 140, 160, 180, 200, 220)
    decision.table <- SPA4$decision.table
    table.caption <- paste0("Table 4. Harvest scenario table for SPA 4 to evaluate ", year,"/", year+1, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA4$USR, " t), and above the Lower Reference Point (LRP; ", SPA4$LRP, " t). Potential catches (t) in ", year,"/", year+1," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }
  
  if(area == "SPA6") {
    catch.range <- c(100, 120, 140, 160, 180, 200, 220)
    decision.table <- SPA6$decision.table
    table.caption <- paste0("Table 5. Harvest scenario table for the SPA 6 modelled area to evaluate ", year-1,"/", year, " fishing season catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), and probability (Pr) of commercial biomass increase.")
  }
  
  #FOR SPA1A, 1B, 3, 4 create table this way:
  if(area %in% c("SPA1A", "SPA1B", "SPA3", "SPA4")) {
    
    ex.table <<- decision.table[["Next.year"]][,c("Catch", "Exploit", "B.change", "pB0", "p.LRP", "p.USR")] #Catch scenarios
    interim.table <- decision.table[["Interim.RRP"]][,c("Catch", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6")] #potential catch
    ex.table <<- merge(ex.table, interim.table, by = "Catch") #merge data together for formating into table
    
    #Set range of catch for Harvest scenario table
    ex.table <<- dplyr::filter(ex.table, Catch %in% catch.range)
    ex.table <<- ex.table %>% mutate(across(where(is.numeric), ~ round(., 2))) %>%  #All columns 2 decimal places (Catch should be whole number)
      mutate(B.change = round(B.change, 0)) %>% #% change no decimal places
      mutate(p.LRP = as.character(p.LRP)) %>% 
      mutate(p.USR = as.character(p.USR)) %>% 
      mutate(p.LRP = replace(p.LRP, p.LRP == "1", ">0.99")) %>%  #if Prob > LRP is 1.00 change to >0.99.
      mutate(p.USR = replace(p.USR, p.USR == "1", ">0.99")) #if Prob > USR is 1.00 change to >0.99.
    str(ex.table)
    row.names(ex.table) <<- NULL
    #names(ex.table) <- c("Catch (t)", "\U1D486", "%\n Change", "Pr\n Increase", "Pr\n >\n LRP", "Pr\n >\n USR")
    
    #exploitation table - as huxtable
    ex.hux <<- ex.table %>% 
      as_hux() %>% 
      theme_basic() %>% 
      set_tb_padding(0)
    
    #huxtable format - function(row#, column#):
    ex.hux <<- ex.hux %>%
      set_bold(1, 7:12) %>% #Bold probability exploitation headers (0.1, 0.2, 0.3 etc.)
      insert_row("", "","", "","", "", "Potential Catch (t)","", "", "", "", "", after = 0) %>%
      set_italic(1, 7) %>% 
      merge_cells(1, 7:12) %>% #Merging right and left side of table (Potential catch (t) header) "\U1D486"
      insert_row("Catch \n(t)", "\U1D486", "%\n Change", "Pr\n Increase", "Pr\n >\n LRP", "Pr\n >\n USR", "Probability Exploitation > 0.15","", "","", "", "", after = 0) %>% #new row at top
      set_bold(1, 7) %>% #Bold "Probability exploitation > 0.15"
      merge_cells(1, 7:12) %>% #Merging right and left side of table (Prob exploitation header)
      insert_row(paste0(year,"/", year+1, " Fishing Season"),"" ,"", "","", "", paste0(year+1,"/", year+2, " Fishing Season"),"", "","", "", "", after = 0) %>% #Table header - ADJUST YEARS!
      set_number_format(1, c(1,7), 0) %>% #defaults to scientific notation - so specify no decimal places for year.
      merge_cells(1, 1:6) %>% #Merging top row left section (YYYY/YYYY Fishing Season header)
      merge_cells(1, 7:12) %>% #Merging top row right section (YYYY/YYYY Fishing Season header)
      merge_cells(2:4, 1) %>%
      merge_cells(2:4, 2) %>%
      merge_cells(2:4, 3) %>%
      merge_cells(2:4, 4) %>%
      merge_cells(2:4, 5) %>%
      merge_cells(2:4, 6) %>%  #Merging left cell data headers (i.e. Catch, e, % Change etc.)
      set_number_format(everywhere, c(2,4,6), 2) %>%
      set_bottom_border(final(1), everywhere) %>% #bottom border on last row (final), all columns
      set_right_border(everywhere, 6) %>%
      set_font_size(14) %>%
      set_top_border(1:5, everywhere) %>% #eg. top border across rows 1 and 2 and all (everywhere) columns.
      set_align("center") %>% #Horizontal alignment of text
      set_valign("middle") %>% #Vertical alignment of text
      #set_font("arial") %>%
      set_row_height(0.06) %>% 
      set_col_width(0.0785) %>% #May need to be adjusted depending on LRP and USR (i.e. if need to accomodate for > symbol)
      set_col_width(4, 0.1) %>% #make Pr Increase column slightly wider
      set_col_width(3, 0.1) %>% #make % Change column slightly wider
      set_width(1.5) %>%
      set_height(.62) %>% 
      set_all_padding(0.09) %>%
      as_flextable()
      #flextable::set_caption(table.caption, autonum = FALSE)
    
  }
  
  if(area == "SPA6") { #If area is SPA6 - make the table this way:
    ex.table <<- decision.table[["Next.year"]][,c("Catch", "Exploit", "B.change", "pB0")]
    #Set range of catch for Harvest scenario table
    ex.table <<- dplyr::filter(ex.table, Catch %in% catch.range)
    ex.table <<- ex.table %>% mutate(across(where(is.numeric), ~ round(., 2))) %>%  #All columns 2 decimal places (Catch should be whole number)
      mutate(B.change = round(B.change, 0))#% change no decimal places
    
    str(ex.table)
    row.names(ex.table) <<- NULL
    names(ex.table) <<- c("Catch (t)", "\U1D486", "%\n Change", "Pr\n Increase")
    
    ex.hux <<- ex.table %>% 
      as_hux() %>% 
      theme_basic() %>% 
      set_tb_padding(0)
    
    #hextable format - function(row#s, column#s):
    ex.hux <<- ex.hux %>%
      #insert_row("Catch \n(t)", "\U1D486", "%\n Change", "Pr\n Increase", after = 0) %>% #new row at top
      insert_row(paste0(year,"/", year+1, " Fishing Season"),"" ,"", "", after = 0) %>% #Table header
      set_number_format(1, c(1,4), 0) %>% #defaults to scientific notation - so specify no decimal places for year.
      set_number_format(everywhere, c(2,4), 2) %>%
      set_bold(2, 1:4, FALSE) %>% #Bold Catch (t), e, % Change, Pr Increase
      merge_cells(1, 1:4) %>% #Merging top row left section (YYYY/YYYY Fishing Season header)
      set_top_border(1:2, everywhere) %>%
      set_bottom_border(final(1), everywhere) %>%
      set_font_size(14) %>% 
      set_align("center") %>% #Horizontal alignment of text
      set_valign("middle") %>% #Vertical alignment of text
      #set_font("arial") %>%
      set_row_height(0.07) %>% 
      set_col_width(0.0785) %>% #May need to be adjusted depending on LRP and USR (i.e. if need to accomodate for > symbol)
      set_col_width(4, 0.1) %>% #make Pr Increase column slightly wider
      set_col_width(3, 0.1) %>% #make % Change column slightly wider
      set_width(1.7) %>%
      set_height(.60) %>%
      set_all_padding(0.09) %>%
      as_flextable()# %>% 
      #flextable::set_caption(table.caption, autonum = NULL)
  }
  
}

#test <- harvest.scen.tab(area = "SPA1B", catch.range = catch.range)
#ex.hux
#ex.table