#=============================================================================================================#
# Script created by Tim Essam @ USAID GeoCenter
# Script created in version R 3.3.0 
# This script is for analyzing Food for Peace Procurement Data
#=============================================================================================================#

# Import/Load packages ----------------------------------------------------
pkgs = c('dplyr', 'haven', 'tidyr', 'ggplot2', 'readxl', 'foreign', 'data.table', 'stringr', 
         'lubridate','xtable', 'scales', 'zoo', 'knitr', 'formattable')

# Check if packages are installed
alreadyInstalled = installed.packages()[, "Package"]

toInstall = pkgs[!pkgs %in% alreadyInstalled]

# Install anything that isn't already installed.
if (length(toInstall > 0)) {
  print(paste0("Installing these packages: ", toInstall))
  
  install.packages(toInstall)
}

# Load packages
for (i in seq_along(pkgs)) {
  library(pkgs[i], character.only = TRUE, quietly = quiet)
}


setwd("~/Github/FoodForPeaceAnalysis/Datain")

# Read in food for peace procurement data
df <- tbl_df(read_excel("ffp_procurement.xlsx"))

df <- rename(df, date = `PO Date`, FY = `Fiscal Year`, mt = `Metric Tons`,
                value = `PO Value`, salesid = `Sales Order`, 
                country = `Recipient Country`, dest = `Ship to`,
                ip = `Sold To Party`, commodity = Material, 
                prog = Program)
head(df)
glimpse(df)  


# Fix inconsistent country names for cleaner faceting
df$country[df$country == "SOUTH SUDAN"] <- "REPUBLIC OF SOUTH SUDAN"

kable(df %>% group_by(country) %>%
        summarise(count = n()) %>% 
        arrange(-count))



# Summary table of materials
mat_df <- df %>% group_by(commodity) %>%
  summarise(count = n(), totValue = sum(value)) %>% 
  arrange(-totValue, -count)
kable(mat_df, digits=0)

formattable(mat_df, list(
  totValue =  normalize_bar("pink", 0.1),
  count = color_tile("white", "orange")))


# Summary table of types of commodities in major groups by benefitting countries and total value
mat_df2 <- df %>% group_by(commodity, country)%>% 
  summarise(count = n(), totValue = sum(value), totMin = min(value), totMax = max(value)) %>% 
  arrange(-totValue, -count)
kable(mat_df2, format.args = list(big.mark = ","), digits = 0)

formattable(mat_df2, list(
  totValue =  normalize_bar("yellow", 0.4)))

# Create a month variable in the original dataframe
df$month = month(df$date) 
df$year = year(df$date)
df$tt <- as.yearmon(paste(df$year, df$month, sep = "-"))


# What countries received the most total value in Title II transfers?
df2 <- df %>% filter(country != "NA") %>%
  group_by(country, tt, month) %>% 
  summarise(totValue = sum(value)/1000000, n = n()) %>% 
  arrange(-totValue)
kable(df2, digits = 0)

# Relevel countries
df2$country <- factor(df2$country, levels = unique(as.character(df2$country)))
df2 <- transform(df2, country = reorder(country, -totValue))

df2$tt <- as.Date(df2$tt)
df2$month <- as.factor(df2$month)

ggplot(df2, aes(x = tt, y = totValue, fill = factor(month))) + scale_color_brewer(palette = "Set3") +
  geom_bar(stat = "identity") + 
  facet_wrap(~country) + theme_bw() + scale_x_date(labels=date_format("%Y-%m-%d"))


# Eastern AFrica?
ggplot(filter(df2, country %in% c("ETHIOPIA", "SUDAN", "KENYA", "MALAWI", "KENYA", "REPUBLIC OF SOUTH SUDAN0",
                                  "CONGO-DEM. REPUB.", "TANZANIA", "CONGO-REPUB. OF", " C. AFRICAN REP.",
                                  "BURUNDI", "ZIMBABWE", "RWANDA", "SOUTH SUDAN")), 
       aes(x = tt, y = totValue, fill = factor(month))) + 
  scale_color_brewer(palette = "Set3") +
  geom_bar(stat = "identity") + 
  facet_wrap(~country) + theme_bw() + scale_x_date(labels=date_format("%Y-%m-%d"))


# Basic function to create tiny multiples by different countries

myplot <- function(cname, t) {
    ggplot(filter(df, country == cname), aes(x = FY, y = value)) + 
    geom_bar(stat = "identity") + facet_wrap(~commodity)
    
  }

myplot("NIGER")





