# Food for Peace Procurement Data Analysis -----------------------------------------
#
# 01_wrangle_ffp.r Combine FFP data into a single data frame
#
# Script to combine excel files into a single dataframe
# 
# Data are from the FFP
#
# Tim Essam, tessam@usaid.gov 
#
# Copyright 2016 by Tim Essam via MIT License
#
# -------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(knitr)
library(forcats)
library(scales)
library(formattable)


setwd("C:/Users/t/Documents/FoodForPeace/Datain/")

read_excel_allsheets <- function(filename) {

    sheets <- readxl::excel_sheets(filename) 
    
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = TRUE))
    names(x) <- sheets
    return(x)
    
  }

# Execute the function to aggregate all the spreadsheets into a list
  FFP_list <- read_excel_allsheets("PO Line Item Report FY 11 throuigh F16.xlsx")
  summary(FFP_list)


# Combine all spreadsheets together, but don't do the 6th one as it has a different length per above
  df_main <- (bind_rows(FFP_list[-6]))
  df_sub <- (bind_rows(FFP_list[6]))

# Pull out column names the overlap and then append dataframes together, keeping only overlap
  cols <- intersect(colnames(df_main), colnames(df_sub))
  df_ffp <- rbind(df_main[,cols], df_sub[,cols])

# Convert all the appropriate chr to numeric
  cols.conv <- c("Transaction Number", "Item Number", "Bid Number", "Vendor",
                "Vendor Plant", "Recipient Ctry", "Related ShipTo", "Itm.ShipTo",
                "Item Goods Recp.", "Frght.Forwarder",
                "Sales Doc.", "Sales Doc.Itm.",
                "External Req.","External Req. Item",
                "Reference PO Number", "Reference PO Item Number",
                "Vessel ID","Product Name")

  df_ffp[cols.conv] <- sapply(df_ffp[cols.conv], as.numeric)
  sapply(df_ffp[cols.conv], class)
  
# Remove rows that are missing transaction numbers -- these are necessary to be a valid entry  
  ffp = df_ffp %>%  
    
    # Remove empty rows at the end 
    filter(!is.na(`Transaction Number`)) %>% 
    
    # Create basic date information for the creation date
    # TODO: Verify this is the correct variable to be using for this task. 
    mutate(CreationMonth = month(`Creation Date`),
           
           CreationDay = wday(`Creation Date`, label=TRUE)
           )
  
# Dates -- Fix the dates to include fiscal years and quarters.
#  fy.tmp <- seq( as.POSIXct('2011-10-01'), length = 6, by = 'year')
  
  # Create Fiscal year dummies and Fiscal Quarter dummies
  ffp = ffp  %>% 
    
    # Fiscal year variables to flag transactions occuring during USAID fiscal calendar
    mutate(FiscalYear = ifelse(`Creation Date` < "2011-10-01", 2011, 
                               ifelse(`Creation Date` >= "2011-10-01" & `Creation Date` < "2012-10-01", 2012,
                                      ifelse(`Creation Date` >= "2012-10-01" & `Creation Date` < "2013-10-01", 2013,
                                             ifelse(`Creation Date`  >= "2013-10-01" & `Creation Date` < "2014-10-01", 2014,
                                                    ifelse(`Creation Date`  >= "2014-10-01" & `Creation Date` < "2015-10-01", 2015,
                                                           ifelse(`Creation Date`  >= "2015-10-01" & `Creation Date` < "2016-10-01", 2016, 0)))))),
           # Create fiscal quarters to use as filters
           FiscalQtr = case_when(ffp$CreationMonth %in% 10:12 ~ 1,
                                 ffp$CreationMonth %in% 1:3 ~ 2,
                                 ffp$CreationMonth %in% 4:6 ~ 3,
                                 ffp$CreationMonth %in% 7:10 ~ 4,
                                 TRUE ~ NA_real_),
           deliveryTime = as.period(`End Delivery Date` - `Start Delivery Date`, units = "days"),   
           durationFlag = ifelse(`Creation Date` >= "2012-04-01" & `Creation Date` < "2016-10-01", 1, 0),
           
           # Create binary variables for rapid filtering
           titleII = ifelse(grepl("480-TITLE_II", `Functional Area`), 1, 0),
           bulkProduct = ifelse(grepl("BULK", `Product Short Text`), 1, 0),
           bulkCategory = ifelse(grepl("BULK", `Category Description`), 1, 0),
           CreationMonth = factor(CreationMonth, levels=1:12, labels=month.name)
           
           ) %>% 
    rename(functArea = `Functional Area`)

             
# write a cut of data to .csv
  write.csv(df_ffp, "ffp_procurement.csv")
  
# TODO: GeoCode  
  
  
  
  
  
  
  
  
  
  
  
  

# TODO:
# Create a function for Fiscal Year related questions to produce a table to answer the question. 
   tot_MT = function(data, ...) {
    data %>% 
      group_by_(.dots = lazyeval::lazy_dots(...)) %>% 
      summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
      mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
      spread(FiscalYear, MT) %>% 
      select(-totMT, everything()) %>% 
      arrange(-totMT) %>% 
      kable(format.args = list(big.mark = ","), digits = 0)  
  }
  
  tot_MT(ffp, functArea, FiscalYear)
  
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(., `Category Description`, FiscalYear)
 
  ffp %>% filter(titleII == 1) %>% 
    tot_MT(.,  Vend.Pl.Name, FiscalYear)
  
  
  
# First, consider the basic tabulation of the functional areas by fiscal year  
  ffp %>% 
    count(functArea, FiscalYear) %>% 
    arrange(-n, FiscalYear) %>% 
    spread(FiscalYear, n) %>% 
    kable()
  
# 1.	How many total metric tons were purchased:
 fmt <- format.args = list(big.mark = ",")
  
  ffp %>% filter(functArea != "NA") %>% 
    group_by(functArea, FiscalYear ) %>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
              count = n()) %>% 
    mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
    #arrange(desc(MT), functArea) %>% 
    select(-count) %>% 
    spread(FiscalYear, MT) %>% 
    arrange(-totMT) %>% 
    
    # Move totMT to the end of the table
    select(-totMT, everything())  %>% 
    kable(format.args = list(big.mark = ","), digits = 0)
  
  # Foreach commodity 
  ffp %>% filter(titleII == 1) %>%  
    group_by(`Category Description`, FiscalYear) %>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`))%>% 
    mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
    spread(FiscalYear, MT) %>% 
    select(-totMT, everything()) %>% 
    arrange(-totMT) %>% 
    kable(format.args = list(big.mark = ","), digits = 0)
 
  # by each vendor plant name
  ffp %>% filter(titleII == 1) %>% 
    group_by(Vend.Pl.Name, FiscalYear)%>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`)) %>% 
    mutate(totMT = sum(MT, na.rm = TRUE)) %>% 
    spread(FiscalYear, MT) %>% 
    select(-totMT, everything()) %>% 
    arrange(-totMT) %>% 
    kable(format.args = list(big.mark = ","), digits = 0)   

  
  
  
  
  
  
  
  
  
# 2. By fiscal year  
  ffp %>%  filter(`Functional Area` != "NA") %>% 
    group_by(`Functional Area`, FiscalYear) %>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
            count = n()) %>% 
    mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
    arrange(desc(totMT)) %>% 
    select(-count) %>% 
    spread(FiscalYear, MT) %>% 
    
    # Move totMT to the end of the table
    select(-totMT, everything()) %>% 
    kable()


  # b.	for each of the commodities (sort commodities by functional area)
  ffp %>%  filter(`Functional Area` != "NA") %>% 
    group_by(`Functional Area`, FiscalYear) %>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
              count = n()) %>% 
    mutate(totMT = sum(MT, na.rm=TRUE),
           maxMT = max(totMT)) %>% 
    arrange(desc(totMT)) %>% 
    ggplot(., aes(MT, fct_reorder(`Functional Area`, MT), label = comma_format()(round(MT)))) +
    geom_point(size = 7, color = "grey50") +
    geom_segment(aes(x = 0, y = `Functional Area`, xend = MT, 
                     yend = `Functional Area`), color = "grey50", size = 1.25) +
    facet_wrap(~FiscalYear) +
    geom_text(inherit.aes = TRUE, hjust = -0.25, color = "grey50")+
    labs(
      title = "Nearly 7 Million tonnes of food aid were purchased under 480 Title II",
      #subtitle = "Two seaters (sports cars) are an exception because of their light weight",
      caption = "Data Office of Food For Peace"
      ) + 
    theme(legend.position="none") + scale_x_continuous(labels = comma, limits = c(0, 2.25e6))
  
  
  # #ggplot(., aes(fct_reorder(`Functional Area`, MT), MT)) + 
  # geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
  # geom_text(aes(label = comma_format()(round(MT)), y = MT-(0.05*MT), size = 5)) +
  # labs(
  #   title = "Nearly 7 Million tonnes of food aid were purchased under 480 Title II",
  #   #subtitle = "Two seaters (sports cars) are an exception because of their light weight",
  #   caption = "Data Office of Food For Peace"
  # ) + theme(legend.position="none")




# b.	for each of the commodities (sort commodities by functional area)
  ffp %>%  filter(`functArea` != "NA") %>% 
    group_by(`Functional Area`, `Category Description`, FiscalYear) %>% 
    summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
              count = n()) %>% 
    mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
    arrange(desc(totMT)) %>% 
    select(-count) %>% 
    spread(FiscalYear, MT) %>% 
    select(-totMT, everything()) %>% 
    kable()
  

ffp %>% group_by(`Functional Area`, `Category Description`) %>% 
  #filter(!is.na(`Functional Area`)) %>% 
  filter(`Functional Area` == "480-TITLE_II") %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), na.rm = TRUE, 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE),
         share = MT / totMT,
         sortvar = fct_reorder(`Category Description`, MT)) %>% 
  arrange(desc(MT, count, totMT)) %>% 
  ggplot(., aes(MT, sortvar)) + 
  geom_point(size = 5, color = "grey50")+
  geom_segment(aes(x = 0, y = `Category Description`, 
                   xend = MT, 
                   yend = `Category Description`), 
                   color = "grey50", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Bulk grain products constitute the largest volumne of 480 Title II procurements",
    #subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data Office of Food For Peace"
  ) #+
  #geom_text(aes(label = comma_format()(round(MT)), x = MT+(0.1*MT), size = 5))
  
  
  
  
  geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
  geom_text(aes(label = comma_format()(round(MT)), y = MT-(0.05*MT), size = 5))


c.	for all packaged commodities



d.	for all bulk commodities

#e.	for each vendor
df_ffp %>%   filter(`Functional Area` == "480-TITLE_II") %>% 
  group_by(`Vendor Name`, `Category Description`) %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
  arrange(desc(MT, count, totMT)) %>%
  ggplot(., aes(fct_reorder(`Vendor Name`, totMT), MT)) +
  geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
 geom_text(aes(label = comma_format()(round(MT)), y = MT-(0.05*MT), size = 3))



f.	by vendor plant location



g.	by load port/terminal




  
