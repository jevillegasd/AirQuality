####  Title:  TimeSeries Plots
####  Description:  Plots particulate matter Time Series datasets.
####  Develloper:   
####                Juan Villegas. Khalifa Univeristy 2018
####  Version:      1.0.0.0
####  Date:         11/25/2018
####  License:      CC 2018

####################### SETUP ####
#Set Work Environment: Make sure to set that with the dataset of interest.
fileName <-"PM_RasAlKhor.csv"
strLocation <- "Dubai Ras Al Khor"
strFilePrefix <- "RasAlKhor"
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

conc_dataset <- read.csv(fileDir)             #Read the file with the concentrations of pollutants
n_pollutants <- ncol(conc_dataset)-3          #Saves the number of pollutants

keeps <- c("Date", "PM2.5")                   #Set variables to keep to build an array with the PM2.5 values
mass_dataset <- conc_dataset[keeps]           #Drop all the other variables
mass_dataset <- mass_dataset %>%   mutate(Date = mdy(Date))  #Format Dates in the new dataset
mass_dataset$Date <- as.Date(mass_dataset$Date)
str(mass_dataset)                             #Displays the dataset properties

conc_dataset <- conc_dataset[which(conc_dataset$PM2.5<dust_event_t),]   #Filter any high level of measurments (dust events)
conc_dataset <- gather(conc_dataset, "Pollutant", "Concentration",4:(n_pollutants+3))  #Gathers all the data in a column array
colnames(conc_dataset) <- c("Filter","Date", "PM2.5","Pollutant", "Concentration")
keeps <- c("Date", "PM2.5", "Pollutant", "Concentration" )      #Set variables to keep
conc_dataset<-conc_dataset[keeps]                               #Drop the Filter NUmbering Column
conc_dataset <- conc_dataset %>%   mutate(Date = mdy(Date))     #Read string as Dates
conc_dataset$Date <- as.POSIXct(conc_dataset$Date)              #Change to a Date format with timezone control


str(conc_dataset)                                               #Displays the dataset properties

####################### PLOTTING ####

if (!(file.exists("Plots")))                  #Create a folder to store the files
  dir.create(file.path(getwd(),"Plots"))
outputDir <- paste0(getwd(),"/Plots/")

Dates <- mass_dataset$Date
rangDate <- range(Dates, finite= TRUE) 

#######################   PM VALUES PLOT ####
str("Generating Time Series Plots")  
plot1 <- ggplot(mass_dataset, aes(as.POSIXct(Date), PM2.5)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 1) +
  geom_point(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 4) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " monitored (24h)"))) + 
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

# Include ouliers by removing the limits on the yscale
plot2 <- ggplot(mass_dataset, aes(as.POSIXct(Date), PM2.5)) +
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 1) +
  geom_point(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 4) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " monitored (24h)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="plain")) +
  theme(axis.title.y = element_text(face="plain", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=-0.9, size=16, colour = "black")) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed")+
  ggtitle(paste("Aerosol Particulate Matter(",strLocation,")")) + 
  theme(plot.title = element_text(lineheight=.8, face="plain", size = 16, hjust = 0.5)) +
  scale_x_datetime(breaks = date_breaks(strDateInterval), labels = date_format("%d %b."))
plot2

outputPath <- paste0(outputDir,strFilePrefix,"_TSo.png")
png(outputPath,
    width = 1700, height = 900, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot2)
dev.off()

