
library(tidyverse)
library(ggplot2) 
library(cowplot)

# salmon recovery

data<-read.csv("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Salmon/Big Bar Rockslide /prep REPORT post WORK/EXCEL and CSV files/KK_recovery_analysis_june30.csv")

# calculate % of studies that have some info on recovery 
discmain <- data[!(data$Recovery_fatigue==0),]
discuss <- data[(data$Recovery_fatigue=="discuss"),]
main <- data[(data$Recovery_fatigue=="main"),]

n<-nrow(data)
nrow(main)/n
nrow(discuss)/n
nrow(discmain)/n

nrow(main)/nrow(discmain)
nrow(discuss)/nrow(discmain) 
  
nR<-nrow(main)

nrow(data[!(data$blood==0),])/nR
nrow(data[!(data$recovery_ratio.repeat_swim==0),])/nR
nrow(data[!(data$fatigue_swim==0),])/nR
nrow(data[!(data$metabolism==0),])/nR
nrow(data[!(data$cardiac_phys==0),])/nR
nrow(data[!(data$gross_energy_muscleworkup==0),])/nR





