# Food for Peace Procurement Data Analysis -----------------------------------------
#
# 01_FFP_combine: calculate improved latrine percentages
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

  df_ffp[cols.conv] <- sapply(df_ffp[cols.conv],as.numeric)
  sapply(df_ffp[cols.conv], class)
  
# Remove rows that are missing transaction numbers -- these are necessary to be a valid entry  
  df_ffp = df_ffp %>%  filter(!is.na(`Transaction Number`))
  
# Drop empty rows from datafram

# write a cut of data to .csv
  write.csv(df_ffp, "ffp_procurement.csv")


# Answer a few question
# 1.	How many total metric tons were purchased
df_ffp %>%  filter(`Functional Area` != "NA") %>% 
  group_by(`Functional Area`) %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
  arrange(desc(totMT)) %>% 
  #kable()
  ggplot(., aes(MT, fct_reorder(`Functional Area`, MT), label = comma_format()(round(MT)))) +
  geom_point(size = 7, color = "grey50") +
  geom_segment(aes(x = 0, y = `Functional Area`, xend = MT, 
                   yend = `Functional Area`), color = "grey50", size = 1.25)+
  geom_text(inherit.aes = TRUE, hjust = -0.25, color = "grey50") +
  labs(
    title = "Nearly 7 Million tonnes of food aid were purchased under 480 Title II",
    #subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data Office of Food For Peace"
  ) + theme(legend.position="none") + scale_x_continuous(labels = comma, limits = c(0, 8e6))
  
  
  # #ggplot(., aes(fct_reorder(`Functional Area`, MT), MT)) + 
  # geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
  # geom_text(aes(label = comma_format()(round(MT)), y = MT-(0.05*MT), size = 5)) +
  # labs(
  #   title = "Nearly 7 Million tonnes of food aid were purchased under 480 Title II",
  #   #subtitle = "Two seaters (sports cars) are an exception because of their light weight",
  #   caption = "Data Office of Food For Peace"
  # ) + theme(legend.position="none")




# b.	for each of the commodities (sort commodities by functional area)

df_ffp %>% group_by(`Functional Area`, `Category Description`) %>% 
  #filter(!is.na(`Functional Area`)) %>% 
  filter(`Functional Area` == "480-TITLE_II") %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), na.rm = TRUE, 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE),
         share = MT / totMT,
         sortvar = fct_reorder(`Category Description`, MT)) %>% 
  arrange(desc(MT, count, totMT)) %>% 


%>% 
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
  )
  #+
  #geom_text(aes(label = comma_format()(round(MT)), x = MT+(0.1*MT), size = 5))
  
  
  
  
  geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
  geom_text(aes(label = comma_format()(round(MT)), y = MT-(0.05*MT), size = 5))


# c.	for all packaged commodities
# d.	for all bulk commodities

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



# f.	by vendor plant location



# g.	by load port/terminal




  
