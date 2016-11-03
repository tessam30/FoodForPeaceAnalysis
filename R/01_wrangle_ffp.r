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

# Create a few date variables from POSIX dates



# Answer a few question
# 1.	How many total metric tons were purchased
df_ffp %>%  group_by(`Functional Area`) %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
  arrange(desc(totMT)) %>% 
  kable()

# b.	for each of the commodities
df_ffp %>%  group_by(`Functional Area`, `Category Description`) %>% 
  filter(!is.na(`Functional Area`)) %>% 
  summarise(MT = sum(`Quantity in Net Metric Tons MT`), 
            count = n()) %>% 
  mutate(totMT = sum(MT, na.rm=TRUE)) %>% 
  arrange(desc(MT, count, totMT)) %>% 
  kable()





c.	for all packaged commodities
d.	for all bulk commodities
e.	for each vendor
f.	by vendor plant location
g.	by load port/terminal




  
