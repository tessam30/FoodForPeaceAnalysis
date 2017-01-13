# Food for Peace Procurement Data Analysis -----------------------------------------
#
# 02_summaryStats.R
#
# Script to produce summary tables for FFP
# 
# Data are from the FFP
#
# Tim Essam, tessam@usaid.gov 
#
# Copyright 2016 by Tim Essam via MIT License
#
# -------------------------------------------------------------------------

# Uses the ffp data frame created in 01_wrangle_ffp
# TODO: add in source directory

  #For ETHIOPIA Comms piece
  ffp %>% 
  filter(titleII == 1 & Rec.Ctry.Name == "ETHIOPIA") %>% 
  group_by(FiscalYear) %>% 
  summarise(totalValue = sum(`Net value`, na.rm = TRUE)) %>%
  mutate(allValue = sum(totalValue)) %>% 
  spread(FiscalYear, totalValue) %>% 
  select(-allValue, everything())%>% 
  arrange(-allValue) %>% 
  kable(format.args = list(big.mark = ","), digits = 0) 
  
ffp %>% 
  filter(titleII == 1) %>% 
  group_by(Rec.Ctry.Name) %>% 
  summarise(totalValue = sum(`Net value`, na.rm = TRUE)) %>%
  mutate(allValue = sum(totalValue)) %>% 
  arrange(-totalValue) 


  
# Create a function to produce a table to answer the question. 
# only current input is the group by variables
 
 tot_MT = function(data, ...) {
      data %>% 
        group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
        summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
        mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
        spread(FiscalYear, MT) %>% 
        select(-totMT, everything()) %>% 
        arrange(-totMT)
 }
 
 

 
 
 # Formats the dataframe to a markdown table. d controls the number of decimals
 fmt = function(data, d = 0) {
   data %>% 
   kable(format.args = list(big.mark = ","), digits = d) 
 }
 
  # this chunk ungroups, slices and formats the output into a markdown table
  # the main inputs x = begining range of slice, y = end range of slice
  grpfmt = function(data, beg = 1, end = 25) {
    data %>% 
      ungroup() %>% 
      glimpse() %>% 
      slice(beg:end) 
  }
  
  
   # Create tables for FFP report
  tot_MT(ffp, functArea, FiscalYear) %>% 
   fmt()
  
  # By Category Description
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(., `Category Description`, FiscalYear) %>% 
   fmt() 
  
  # By vendor name -- only printing top 25 results
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(.,  `Vendor Name`, FiscalYear) %>% 
    grpfmt() %>% fmt()
    
   # By vendor place location -- only printing top 25 results
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(.,  Vend.Pl.Name, FiscalYear) %>% 
    grpfmt() %>% fmt()
  
  # By load port/terminal --> quite a few NAs in the Load Port Terminal Name
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(., `LoadPort Term.Name`, FiscalYear) %>% 
    grpfmt(., beg=1, end =100) %>%  fmt()

# -------------------------------------------------------------------------------
  
  tot_MTQ = function(data, ...) {
    data %>% 
      group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
      summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
      mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
      spread(FiscalQtr, MT) %>% 
      select(-totMT, everything()) %>% 
      arrange(-totMT)
  }
  
  # CMetric Tons purchased each quarter
  tot_MTQ(ffp, functArea, FiscalQtr) %>% 
    fmt()
  
  # What does this look like by Fiscal Quarter and Fiscal Year (sorted by month)
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(., FiscalQtr, FiscalYear) %>% 
    fmt()
  
  # For each of the commodities
  # By Category Description
  ffp %>% filter(titleII == 1) %>% 
    tot_MTQ(., `Category Description`, FiscalQtr) %>% 
    fmt() 
  
  # By vendor name -- only printing top 25 results
  ffp %>% filter(titleII == 1) %>% 
    tot_MTQ(.,  `Vendor Name`, FiscalQtr) %>% 
    grpfmt() %>% fmt()
  
  # By vendor place location -- only printing top 25 results
  ffp %>% filter(titleII == 1) %>% 
    tot_MTQ(.,  Vend.Pl.Name, FiscalQtr) %>% 
    grpfmt() %>%  fmt()
  
  # By load port/terminal --> quite a few NAs in the Load Port Terminal Name
  ffp %>% filter(titleII == 1) %>% 
    tot_MTQ(., `LoadPort Term.Name`, FiscalQtr) %>% 
    grpfmt() %>%  fmt()

# ---------------------------------------------  
# Questions 3 - Recut everything by each Month
  tot_MTM = function(data, ...) {
    data %>% 
      group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
      summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
      mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
      spread(CreationMonth, MT) %>% 
      select(-totMT, everything()) %>% 
      arrange(-totMT)
  }
  

  # CMetric Tons purchased each month
  tot_MTM(ffp, functArea, CreationMonth) %>% 
        fmt()
  
 # What does this look like by Creation month and Fiscal Year (sorted by month)
   ffp %>% filter(titleII == 1) %>% 
    tot_MT(., CreationMonth, FiscalYear) %>% 
    kable(format.args = list(big.mark = ","), digits = 0)
   
 # For each of the commodities
   # By Category Description
   ffp %>% filter(titleII == 1) %>% 
     tot_MTM(., `Category Description`, CreationMonth) %>% 
     fmt() 
  
   # By vendor name -- only printing top 25 results
   ffp %>% filter(titleII == 1) %>% 
     tot_MTM(.,  `Vendor Name`, CreationMonth) %>% 
     grpfmt() %>% fmt()
   
   # By vendor place location -- only printing top 25 results
   ffp %>% filter(titleII == 1) %>% 
     tot_MTM(.,  Vend.Pl.Name, CreationMonth) %>% 
     grpfmt() %>%  fmt()
   
   # By load port/terminal --> quite a few NAs in the Load Port Terminal Name
   ffp %>% filter(titleII == 1) %>% 
     tot_MTM(., `LoadPort Term.Name`, CreationMonth) %>% 
     grpfmt() %>%  fmt()

    
# ---------------------------------------------------------------------------------
# Questions 4 - April 2011 and FY 2016
   tot_MTF = function(data, ...) {
     data %>% filter(durationFlag == 1) %>% 
       group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
       summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
       mutate(share =  MT / sum(MT, na.rm = TRUE)) %>% 
       #spread(CreationMonth, MT) %>% 
       select(-MT, -share, everything()) %>% 
       arrange(-MT, -share)
   }
 
   fmt2 = function(data, ...) {
     data %>% 
       kable(format.args = list(big.mark = ","), digits = c(0, 0, 3)) 
   } 
     
# By functional category
 tot_MTF(ffp, functArea) %>% 
   fmt2()
 
 # By Category Description
 ffp %>% filter(titleII == 1) %>% 
   tot_MTF(., `Category Description`) %>% 
   fmt2() 
 
 # By vendor name -- only printing top 25 results
 ffp %>% filter(titleII == 1) %>% 
   tot_MTF(.,  `Vendor Name`) %>% 
   grpfmt() %>% fmt2()
 
 # By vendor place location -- only printing top 25 results
 ffp %>% filter(titleII == 1) %>% 
   tot_MTF(.,  Vend.Pl.Name) %>% 
   grpfmt() %>%  fmt2()
 
 # By load port/terminal --> quite a few NAs in the Load Port Terminal Name
 ffp %>% filter(titleII == 1) %>% 
   tot_MTF(., `LoadPort Term.Name`) %>% 
   grpfmt() %>%  fmt2()
 

 
 
 # -------------------------------------------------------------------------
# Question 5. What was the total average commodity cost per metric ton?
 # All Commodities
 
 # Chunk to summarize, spread, select, arrange, and format
  test <- function(data, col_name1, ...){
    # Convert character vector to list of symbols
      dots <- lapply(col_name1, as.symbol)
      data %>% 
      group_by_(.dots = dots)) %>% 
      mutate(avp = mean(`Total Intl Comm Pric`, na.rm = TRUE)) %>% 
      ungroup() %>% 
      
      # Need to pass a new set of .dots through group_by_
      dots2 <- lapply()
      
  }

 

 
 
 
  ffp %>% group_by(functArea) %>% 
   mutate(avePrice_fa = mean(`Total Intl Comm Pric`, na.rm = TRUE),
          count = n()) %>% 
   group_by(functArea, FiscalYear, avePrice_fa, count) %>% 
   summarize(avePrice_FY = mean(`Total Intl Comm Pric`, na.rm = TRUE)) %>% 
   spread(FiscalYear, avePrice_FY) %>% 
   select(-avePrice_fa, -count, everything()) %>%
   arrange(desc(count, avePrice_fa)) %>% 
   fmt()
 
 
 #Only tittle II purchases
 ffp %>% 
   filter(titleII == 1) %>% 
   group_by(`Category Description`) %>% 
   mutate(avePrice_cd = mean(`Total Intl Comm Pric`, na.rm = TRUE),
          count = n()
          ) %>% 
   group_by(FiscalYear, `Category Description`, avePrice_cd, count) %>% 
   summarize(avePrice_FY = mean(`Total Intl Comm Pric`, na.rm = TRUE))%>% 
   spread(FiscalYear, avePrice_FY)  %>% 
   select(-avePrice_cd, -count, everything()) %>%
   arrange(desc(count), desc(avePrice_cd)) %>% 
   fmt()
 
 # By vendor name, keeping only top 25
 ffp %>% 
   filter(titleII == 1) %>% 
   group_by(`Vendor Name`) %>% 
   mutate(avePrice_cd = mean(`Total Intl Comm Pric`, na.rm = TRUE),
          count = n()) %>% 
   group_by(FiscalYear, `Vendor Name`, avePrice_cd, count) %>% 
   summarize(avePrice_FY = mean(`Total Intl Comm Pric`, na.rm = TRUE))%>% 
   spread(FiscalYear, avePrice_FY)  %>% 
   select(-avePrice_cd, -count, everything()) %>%
   arrange(desc(count, avePrice_cd)) %>% 
   grpfmt(beg = 1, end = 50) %>% 
   fmt2()
 
 ffp %>% 
   filter(titleII == 1) %>% 
   group_by(Vend.Pl.Name) %>% 
   mutate(avePrice_cd = mean(`Total Intl Comm Pric`, na.rm = TRUE)) %>% 
   group_by(FiscalYear, Vend.Pl.Name, avePrice_cd) %>% 
   summarize(avePrice_FY = mean(`Total Intl Comm Pric`, na.rm = TRUE))%>% 
   spread(FiscalYear, avePrice_FY)  %>% 
   select(-avePrice_cd, everything()) %>%
   arrange(desc(avePrice_cd)) %>% 
   grpfmt(beg = 1, end = 50) %>% 
   fmt2()
 
 ffp %>% 
   filter(titleII == 1) %>% 
   group_by(`LoadPort Term.Name`) %>% 
   mutate(avePrice_cd = mean(`Total Intl Comm Pric`, na.rm = TRUE)) %>% 
   group_by(FiscalYear, `LoadPort Term.Name`, avePrice_cd) %>% 
   summarize(avePrice_FY = mean(`Total Intl Comm Pric`, na.rm = TRUE),
             count = n()
             )%>% 
   spread(FiscalYear, avePrice_FY)  %>% 
   select(-avePrice_cd, everything()) %>%
   arrange(desc(avePrice_cd)) %>% 
   grpfmt(beg = 1, end = 25) %>% 
   fmt2()

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 # test <- function(data, col_name, ...) {
 #   dots <- lapply(col_name, as.symbol)
 #   
 #   dots2 <- lapply(c(""))
 #   
 #   data %>% 
 #   filter(titleII == 1) %>% 
 #  
 #    group_by_(.dots = dots) %>% 
 #     mutate(avePrice_cd = mean(`Total Intl Comm Pric`, na.rm = TRUE)) %>% 
 # 
 #   
 # 
 
 
 
 
 
 
 
    
  
  

 
 


 
