# Plotting FFP data

# Import/Load packages ----------------------------------------------------
library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)       # kable : prettier data.frame output
library(haven)       # read in stata data (.dta)
library(cowplot)     # combine ggplots into a single graphic
library(extrafont)

# import fonts for segoe ui
font_import(pattern="[S/s]egoe")


# Pull in data from stata file
setwd("~/GitHub/FoodForPeaceAnalysis")
d <- read_dta("Dataout/ffp_procurement.dta")


# Create a basic heatmap of the frequency of purchase by the categories; Need to manipulate the dates a bit
d <- group_by(d, category) %>% mutate(totCount = n())
d <- group_by(d, moYear) %>% mutate(monthlyCount = n())
d$category <- as.factor(d$category)
d$category <- factor(d$category, levels = d$category[order(d$totCount)])
d$sortValue <- d$category
d$sortValue <- factor(d$sortValue, levels = d$sortValue[order(d$totCatValue)])
d$sortValue2 <- 


d_freq = d %>% group_by(category, moYear, totCount, monthlyCount, sortValue) %>% 
  summarise(count = n(), value = sum(povalue)/1000000, mean_value = mean(po_unitcost)) %>% 
  arrange(category, -count)

# Order the commodities by total count variable for graphing
key.events <- data.frame(date=as.Date(c("2011-10-15","2012-10-15","2013-10-15",
                                        "2014-10-15", "2015-10-15", "2016-10-15")))
p2 <- ggplot(d_freq, aes(moYear, category, fill = count)) +  
  geom_tile(colour = 'gray80',size = 0.25, stat = "identity") +
  geom_text(aes(y = category, x = moYear, label = round(count, 0)), size = 2, colour = "gray40") +
  scale_fill_distiller(palette="YlGnBu", trans = "reverse", na.value = "gray50") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")+
  geom_vline(data=key.events, colour = "gray100", size = 0.25, linetype = "dashed",
             aes(xintercept=as.integer(date))) + 
    #labs(title = "Peas are the most commonly procured commodity") +
  theme_fivethirtyeight(base_size = 8) 
p2

p1 <- d_freq %>% group_by(moYear) %>% summarise(count = mean(monthlyCount)) %>% 
  ggplot(aes(moYear, y = count)) + geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
  geom_vline(data=key.events, colour = "gray80", size = 0.25, linetype = "dashed",
             aes(xintercept=as.integer(date))) +
  theme_fivethirtyeight(base_size = 8)
p1

plotfreq <- plot_grid(p1, p2, align = "v", ncol = 1, rel_heights=c(1,3))  
plotfreq
ggsave("~/GitHub/FoodForPeaceAnalysis/Graphics/freq.pdf", width = 11, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# Focus on the unit cost average per commodity   
p4 <- ggplot(d_freq, aes(moYear, sortValue, fill = value)) + 
  geom_tile(colour = 'white', size = 0.25, stat = "identity") +
  scale_fill_distiller(palette="YlGn", trans = "reverse")  + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")+
  geom_vline(data=key.events, colour = "gray80", size = 1, linetype = "dashed",
             aes(xintercept=as.integer(date))) + 
  geom_text(aes(y = category, x = moYear, label = round(value, 2)), size = 1.5, colour = "gray40") +
  theme_fivethirtyeight() 
p4

# Melt data down into 2 columns, one for value and one for date
p3 <- d_freq %>% group_by(moYear) %>% 
  summarise(totValue = sum(value)) %>% 
  ggplot(aes(moYear, totValue)) + geom_bar(stat = "identity") +
  #scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
  xlab("")+
  geom_vline(data=key.events, colour = "gray80", size = 1, linetype = "dashed",
             aes(xintercept=as.integer(date))) + 
  theme_fivethirtyeight() 
p3
    
plotvalue <- plot_grid(p3, p4, align = "v", ncol = 1, rel_heights=c(1,3))
plotvalue
ggsave("~/GitHub/FoodForPeaceAnalysis/Graphics/values.pdf", width = 11, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# One final plot of the average unit cost of commodities; Separate colors for Ready to use and fortified foods
p5 <- ggplot(d_freq, aes(moYear, sortValue, fill = mean_value)) + 
  geom_tile(colour = 'white', size = 0.25, stat = "identity") +
  scale_fill_distiller(palette="YlGn", trans = "reverse")  + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")+
  geom_vline(data=key.events, colour = "gray80", size = 1, linetype = "dashed",
             aes(xintercept=as.integer(date))) + 
  geom_text(aes(y = category, x = moYear, label = round(mean_value, 2)), size = 1.5, colour = "gray40") +
  theme_fivethirtyeight() 
p5





 