####  Title:  Stacked Plots
####  Description:  Plots for reports on Particulate Matter on aerosols, it generates three png with plots, one
####                containing the stacked visualization of the pollutants in air and two plotting only the 
####                PM2.5 measuremnts. The two plots might differ if the samples include anormally high values.
####  Develloper:   Federico K., Khalifa Univeristy 2018
####                Juan Villegas. Khalifa Univeristy 2018
####  Version:      1.1.0
####  Date:         11/13/2018
####  License:      CC 2018

####################### SETUP ####
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

#Set Work Environment: Make sure to set that with the dataset of interest.
fileName <-"PM_EXPO2020.csv"
fileDir <- paste0(getwd(),"/Plotting/",fileName)

#Usefull variables
dust_event_t <- 1000    #treshold to identify something as a dust event and filter out the data

####################### IMPORT POLLUTANT CONCENTRATION DATASET ####

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
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=14, colour = "black", face="plain")) +
  theme(axis.title.y = element_text(face="plain", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed")+
  scale_x_datetime(breaks = date_breaks("1 weeks"), labels = date_format("%d %b."))+
  ylim(0, 500)
plot1

fileDir <- paste0(outputDir,"timeSeries.png")
png(fileDir,
    width = 1700, height = 1050, units = "px", pointsize = 30,
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
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=14, colour = "black", face="plain")) +
  theme(axis.title.y = element_text(face="plain", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed")+
  scale_x_datetime(breaks = date_breaks("1 weeks"), labels = date_format("%d %b."))
plot2

fileDir <- paste0(outputDir,"timeSeries_ouliers.png")
png(fileDir,
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot2)
dev.off()

#######################   STACKED PLOTs ####
# Colours follow alphabetic order of the name of the pollutants
# Filter out some irrelevant chemical elements
str("Generating Stacked Plots")  
plot3 <- ggplot(data = conc_dataset, 
                aes(Date, Concentration, fill = Pollutant)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_line(aes(Date, PM2.5), col="red", size = 2, linetype="twodash") +
  geom_point(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="black", size = 3) +
  scale_fill_manual(values=c("#ffc0cb", "#ccac00", "#b7b7b7", "#ff7f7f", "#9d6060", "#b2b2ff",
                             "#008000", "#ff0000", "#99e0e0", "#0000ff", "#ffff00", "#ffffcc",
                             "#000000", "#731d1d", "#808080", "#ffa500", "#f2f2e5",
                             "#edd4d4")) +
  guides(fill = guide_legend(override.aes = list(shape=NA))) +
  theme(legend.text = element_text(colour="black", size = 12, face = "plain")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=16,face="plain", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"),size=16)) + 
  theme(axis.title.y = element_text(face="plain", colour="black", size=16),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0.5, size=16)) +
  theme(axis.text.x  = element_text(angle=90, vjust=-0.8, hjust=1, size=16)) +
  ggtitle("Chemical composition of fine Particulate Matter (EXPO2020 Dubai)") + 
  theme(plot.title = element_text(lineheight=.8, face="plain", size = 16)) +
  ylim(0, 500) +
  scale_x_datetime(breaks = date_breaks("1 weeks"), labels = date_format("%d %b."))
plot3

fileDir <- paste0(outputDir,"stackedPlot.png")
png(fileDir,
    width = 1700, height = 900, units = "px", pointsize = 13,
    bg = "white", res = 150)
print(plot3)
dev.off()

#### END ####
str("Finished")  
rm(list=ls())
