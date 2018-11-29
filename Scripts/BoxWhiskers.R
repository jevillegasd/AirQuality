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
strFilePrefix <- "PM2.5"
fileDir = paste0(getwd(),"/Datasets/",fileName)

####################### IMPORT ALL PM CONCENTRATIONS DATASET ####

dataset <- read.csv(fileDir)             #Read the file with the concentrations of pollutants

keeps <- c("Measurement", "Expo2020","Dubai.MoCCE","Ras.al.Khor","AD.MoCCE","Sharjah")                   #Set variables to keep to build an array with the PM2.5 values
dataset <- dataset[keeps]           #Drop all the other variables
str(dataset)                             #Displays the dataset properties

dataset2 <- gather(dataset, "Location", "Concentration",2:6) 

####################### PLOTTING ####

if (!(file.exists("Plots")))                  #Create a folder to store the files
  dir.create(file.path(getwd(),"Plots"))
outputDir <- paste0(getwd(),"/Plots/")

plot1 <-ggplot(dataset2, 
        aes(Location, Concentration)) + 
        geom_boxplot() +
        ylab(expression(paste(PM[2.5], " (µg/",m^3, ") monitored (24h)")))+
        theme(axis.title.x=element_blank(),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="plain")) +
        theme(axis.title.y = element_text(face="plain", colour="black", size=14),
          axis.text.y  = element_text(angle=0, vjust=0, size=14, colour = "black")) +
        geom_hline(yintercept = 50, size = 1, linetype = "dashed")

outputPath <- paste0(outputDir,strFilePrefix,"_Box.png")
png(outputPath,
    width = 1700, height = 900, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot1)
dev.off()
