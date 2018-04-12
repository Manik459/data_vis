library(quantmod)
library(googleVis)
library(dplyr)
library(leaflet)


data <- read.csv("gapminderDataFiveYear.csv")
#scatter plot
data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
data_2007$size <- data_2007$pop
colors1 <- c('#F73A18', '#18F7A3','#1972A4', '#FF7070', '#3318F7')

years <- unique(data$year)
years
data_2002 <- data[which(data$year == 2002),]
data_2002 <- data_2002[order(data_2002$continent, data_2002$country),]
data_2002$size <- data_2002$pop
colors2 <- c('#7318F7', '#18F7A3','#F71826', '#FF7070', '#3318F7')