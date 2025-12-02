#This function is to produce the Harvest Scenario Tables for the Inshore Scallop ISAC presentation.
#Note that tables for SPA1A, 1B, 3 and 4 are formatted the same, but the table for SPA6 is formatted differently.

#FORMATING MAY NEED ADJUSTING - SEE https://hughjonesd.github.io/huxtable/reference/index.html for table package references.
#In particular: Table width and height (see example)
# set_width(1.5) %>%
# set_height(.62)

#And catch.range may need adjusting for each area depending on how many scenarios want to be displayed.

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
    catch.range <- c(50,75,100,125,150,175, 200, 225, 250, 275)
    decision.table <- SPA1A.decision.table
    table.caption <- paste0("Table 1. Harvest scenario table for SPA 1A to evaluate ", year,"--", year+1-2000, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA1A$USR, " t), and above the Lower Reference Point (LRP; ", SPA1A$LRP, " t). Potential catches (t) in ", year,"--", year+1-2000," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }

  if(area == "SPA1B") {
    catch.range <- c(125,150,175,200,225,250,275,300,325,350)
    decision.table <- SPA1B.decision.table
    table.caption <- paste0("Table 2. Harvest scenario table for SPA 1B to evaluate ", year,"--", year+1-2000, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA1B$USR, " t), and above the Lower Reference Point (LRP; ", SPA1B$LRP, " t). Potential catches (t) in ", year,"--", year+1-2000," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }

  if(area == "SPA3") {
    catch.range <- c(50,60,70,80,90,100,110,120,130,140)
    decision.table <- SPA3.decision.table
    table.caption <- paste0("Table 3. Harvest scenario table for SPA 3 to evaluate ", year,"--", year+1-2000, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA3$USR, " t), and above the Lower Reference Point (LRP; ", SPA3$LRP, " t). Potential catches (t) in ", year,"--", year+1-2000," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }

  if(area == "SPA4") {
    catch.range <- c(50,60,70,80,90,100,110,120,130,140)
    decision.table <- SPA4.decision.table
    table.caption <- paste0("Table 4. Harvest scenario table for SPA 4 to evaluate ", year,"--", year+1-2000, " catch levels in terms of resulting exploitation (e), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase, probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA4$USR, " t), and above the Lower Reference Point (LRP; ", SPA4$LRP, " t). Potential catches (t) in ", year,"--", year+1-2000," are evaluated in terms of the posterior probability of exceeding exploitation rate of 0.15.")
  }

  if(area == "SPA6") {
    catch.range <- c(30,45,60,75,90,105,120,135)
    decision.table <- SPA6.decision.table
    table.caption <- paste0("Table 5. Harvest scenario table for the SPA 6 modelled area to evaluate ", year,"--", year+1-2000, " catch levels in terms of resulting exploitation (\U1D452), expected changes in commercial biomass (%), probability (Pr) of commercial biomass increase. The probability that after removal the stock will be above the Upper Stock Reference (USR; ", SPA6$USR, " t), and above the Lower Reference Point (LRP; ", SPA6$LRP, " t); grey shaded columns. Corresponding catch levels for the whole area of SPA 6 are conditional on the proportion of catch from the modeled area staying the same in ", year+1," as in ", year," (", round((SPA6.landings %>% filter(Year == "Prop_IN") %>% dplyr::select(paste0(year)) %>% pull()*100),0),"%).")
  }

  #FOR SPA1A, 1B, 3, 4 create table this way:
  if(area %in% c("SPA1A", "SPA1B", "SPA3", "SPA4")) {

    #Set range of catch for Harvest scenario table
    ex.table <<- decision.table
    ex.table <<- dplyr::filter(ex.table, Next.year.Catch %in% catch.range)
    ex.table <<- ex.table |> #%>% mutate(across(where(is.numeric), ~ round(., 2))) %>%  #All columns 2 decimal places (Catch should be whole number)
      mutate(Next.year.B.change = round(Next.year.B.change, 0)) %>% #% change no. decimal places
      mutate(Next.year.Exploit = format(round(Next.year.Exploit, 2), nsmall = 2)) %>% #% change no. decimal places
      mutate(Next.year.pB0 = format(round(Next.year.pB0, 2), nsmall = 2)) %>% #% change no. decimal places
      mutate(Next.year.p.LRP = as.character(Next.year.p.LRP)) %>%
      mutate(Next.year.p.USR = as.character(Next.year.p.USR)) %>%
      mutate(Next.year.p.LRP = substr(as.character(Next.year.p.LRP), 1, 4)) |>
      mutate(Next.year.p.USR = substr(as.character(Next.year.p.USR), 1, 4)) |>
      mutate(Next.year.p.LRP = replace(Next.year.p.LRP, Next.year.p.LRP == "1", "> 0.99")) %>%  #if Prob > LRP is 1.00 change to >0.99.
      mutate(Next.year.p.USR = replace(Next.year.p.USR, Next.year.p.USR == "1", "> 0.99"))  #if Prob > USR is 1.00 change to >0.99.
    ex.table <<- ex.table %>% dplyr::select(-Interim.RRP.Catch) #remove duplicated catch column
    #rownames(ex.table) <- NULL
    names(ex.table) <<- c("Catch (t)", "\U1D486", "%\n Change", "Pr\n Increase", "Pr\n >\n LRP", "Pr\n >\n USR", "0.1", "0.2","0.3", "0.4", "0.5", "0.6")

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
      insert_row(paste0(year,"--", year+1-2000, " Fishing Season"),"" ,"", "","", "", paste0(year+1,"--", year+2-2000, " Fishing Season"),"", "","", "", "", after = 0) %>% #Table header - ADJUST YEARS!
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

    #Set range of catch for Harvest scenario table

    ex.table <<- decision.table
    ex.table <<- dplyr::filter(ex.table, Next.year.Catch %in% catch.range)

    ex.table <<- ex.table %>% #mutate(across(where(is.numeric), ~ round(., 2), nsmall = 2)) %>%  #All columns 2 decimal places (Catch should be whole number)
      mutate(Next.year.B.change = round(Next.year.B.change, 0)) %>% #% change no decimal places
      mutate(Next.year.Exploit = format(round(Next.year.Exploit, 2), nsmall = 2)) %>% #% change no. decimal places
      mutate(Next.year.pB0 = format(round(Next.year.pB0, 2), nsmall = 2)) %>% #% change no. decimal places
      mutate(Next.year.p.LRP = as.character(Next.year.p.LRP)) %>%
      mutate(Next.year.p.USR = as.character(Next.year.p.USR)) %>%
      mutate(Next.year.p.LRP = substr(as.character(Next.year.p.LRP), 1, 4)) |>
      mutate(Next.year.p.USR = substr(as.character(Next.year.p.USR), 1, 4)) |>
      mutate(Next.year.p.LRP = replace(Next.year.p.LRP, Next.year.p.LRP == "1", "> 0.99")) %>%  #if Prob > LRP is 1.00 change to >0.99.
      mutate(Next.year.p.USR = replace(Next.year.p.USR, Next.year.p.USR == "1", "> 0.99")) #if Prob > USR is 1.00 change to >0.99. %>%

    ex.table <<- ex.table %>% dplyr::select(., Next.year.Catch:Next.year.Catch.all) #Select columns for table

    #rownames(ex.table) <- NULL
    names(ex.table) <<- c("Catch (t)", "\U1D452", "%\n Change", "Pr\n Increase", "Pr\n >\n LRP", "Pr\n >\n USR", " Catch (t) ")

    #ex.table <- ex.table[,c(1:4)]

    ex.hux <<- ex.table %>%
      as_hux() %>%
      theme_basic() %>%
      set_tb_padding(0)


    #huxtable format - function(row#, column#):
    ex.hux <<- ex.hux %>%
      #set_bold(1, 7:12) %>% #Bold probability exploitation headers (0.1, 0.2, 0.3 etc.)
      #insert_row("", "","", "","", "", "Potential Catch (t)","", "", "", "", "", after = 0) %>%
      insert_row("Modelled Area", "", "", "", "", "", "Whole Area", after = 0) %>% #new row at top
      set_italic(1, everywhere) %>%
      merge_cells(1, 1:6) %>% #Merging Modeled Area columns
      #set_bold(1, 7) %>% #Bold "Probability exploitation > 0.15"
      insert_row(paste0(year,"--", year+1-2000, " Fishing Season"),"" ,"", "","", "","", after = 0) %>% #Table header - ADJUST YEARS!
      merge_cells(1, everywhere) %>% #Merging right and left side of table (Modeled/Whole area header)
      set_number_format(1, c(1,7), 0) %>% #defaults to scientific notation - so specify no decimal places for year.
      set_number_format(everywhere, c(2,4,6), 2) %>%
      set_top_border(1:2, everywhere) %>% #eg. top border across rows 1 and 2 and all (everywhere) columns.
      set_bottom_border(2, everywhere) %>% #bottom border on Area row, all columns
      set_bottom_border(final(1), everywhere) %>% #bottom border on last row (final), all columns
      set_right_border(everywhere, 6) %>%
      set_background_color(everywhere, 5:6, "grey90") %>%
      set_font_size(14) %>%
      set_align("center") %>% #Horizontal alignment of text
      set_valign("middle") %>% #Vertical alignment of text
      #set_font("arial") %>%
      set_row_height(0.07) %>%
      set_col_width(0.0785) %>% #May need to be adjusted depending on LRP and USR (i.e. if need to accommodate for > symbol)
      set_col_width(4, 0.1) %>% #make Pr Increase column slightly wider
      set_col_width(3, 0.1) %>% #make % Change column slightly wider
      set_col_width(7, 0.17) %>% #make Catch column slightly wider
      set_width(1.7) %>%
      set_all_padding(0.09) %>%
      as_flextable()
    #flextable::set_caption(table.caption, autonum = FALSE)
  }

}

#test <- harvest.scen.tab(area = "SPA6", catch.range = catch.range)
#ex.hux
#ex.table
