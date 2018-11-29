####  Title:  Box and Whiskers Plot
####  Description:  Plots particulate matter summary using box and whiskeras plot
####  Develloper:   
####                Juan Villegas. Khalifa Univeristy 2018
####  Version:      1.0.0.0
####  Date:         11/29/2018
####  License:      CC 2018

####################### SETUP ####
#Set Work Environment: Make sure to set that with the dataset of interest.
fileName <-"PM2.5 UAE.csv"
strLocation <- "Dubai Ras Al Khor"
strFilePrefix <- "AllBox"
strDateInterval <- "1 days"

# Load packages
#library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

fileDir = paste0(getwd(),"/Datasets/",fileName)
#Usefull variables
dust_event_t <- 1000    #treshold to identify something as a dust event and filter out the data

####################### IMPORT ALL PM CONCENTRATIONS DATASET ####

dataset <- read.csv(fileDir)             #Read the file with the concentrations of pollutants

keeps <- c("Measurement", "Expo2020","Dubai.MoCCE","Ras.al.Khor","AD.MoCCE","Sharjah")                   #Set variables to keep to build an array with the PM2.5 values
dataset <- dataset[keeps]           #Drop all the other variables
str(dataset)                             #Displays the dataset properties

boxplot(dataset$Expo2020, dataset$Dubai.MoCCE, dataset$Ras.al.Khor, dataset$AD.MoCCE,dataset$Sharjah,
        main = "UAE PM2.5 field campaign",
        at = c(1,2,3,4,5),
        names = c("Dubai Expo2020", "Dubai MoCCAE", "Dubai Ras al Khor", "Abu Dhabi MoCCAE", "Sharjah"),
        las = 1,
        col = c("blue","green", "orange", "purple" ,"red"),
        border = "black",
        horizontal = TRUE,
        notch = FALSE
)

dataset2 <- gather(dataset, "Location", "Concentration",2:6) 

ggplot(dataset2, 
        aes(Location, Concentration)) + 
        geom_boxplot() +
        ylab(expression(paste(PM[2.5], " (µg/",m^3, ") monitored (24h)")))+
        theme(axis.title.x=element_blank(),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="plain")) +
        theme(axis.title.y = element_text(face="plain", colour="black", size=14),
          axis.text.y  = element_text(angle=0, vjust=0, size=14, colour = "black")) +
        geom_hline(yintercept = 50, size = 1, linetype = "dashed")



#######################   PM VALUES PLOT ####
str("Generating Time Series Plots")  
plot1 <- ggplot(mass_dataset, aes(as.POSIXct(Date), PM2.5)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 1) +
  geom_point(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 4) +
  ylab(expression(paste(PM[2.5], "( µg/",m^3, ")", " monitored (24h)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="plain")) +
  theme(axis.title.y = element_text(face="plain", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=-0.9, size=16, colour = "black")) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed")+
  ggtitle(paste("Aerosol Particulate Matter(",strLocation,")")) + 
  theme(plot.title = element_text(lineheight=.8, face="plain", size = 16, hjust = 0.5)) +
  scale_x_datetime(breaks = date_breaks(strDateInterval), labels = date_format("%d %b."))+
  ylim(0, 300)
plot1

outputPath <- paste0(outputDir,strFilePrefix,"_TS.png")
png(outputPath,
    width = 1700, height = 900, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot1)
dev.off()
