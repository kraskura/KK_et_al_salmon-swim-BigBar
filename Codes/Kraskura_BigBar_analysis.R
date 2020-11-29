

# import used libraries 
library(tidyverse)
library(reshape2)
if (!require("magrittr")) install.packages("magrittr")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(plyr)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)
library(MuMIn)
library(mgcv)
 
setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Salmon/Big Bar Rockslide /FINAL FILES - prep REPORT /EXCEL and CSV files/")

data<-read.csv("Kraskura_BigBarSlide_salmonid_swimming_ANALYSIS.csv")
data<-data[,-c(1,49)]
# rename column names: 
names(data) <- c("Species_latin", "Origin", "Key_origin", "Year",
                 "N_morphometrics", "Size_MIN_kg", "Size_MAX_kg", "Size_MEAN_kg",
                 "Size_error_kg", "UNIT_Size_error_kg", "Length_MIN_cm", "Length_MAX_cm",
                 "Length_MEAN_cm", "Length_error", "UNIT_length_error", "Length_UNIT",
                 "Sex_F_M", "Temp_test_mean", "SW_FW", "Test_performance",
                 "Indiv_group", "swim_speed_MIN_BL_s", "swim_speed_MAX_BL_s", "swim_speed_MEAN_BL_s", 
                 "swim_speed_error_BL_s", "UNIT_swim_error_BLs","Duration_swim", "swim_speed_MIN_cm_s",
                 "swim_speed_MAX_cm_s", "swim_speed_MEAN_cm_s", "swim_speed_error_cm_s", "UNIT_swim_error_cm_s",
                 "Water_flows_cm_s", "Water_flows_m_3_s", "Swim_Conditions", "Fish_Conditions",
                 "GSI_MEAN", "Gonad_g", "Blood", "Mortality",
                 "Surgery", "Recovery", "Tracking", "Data_source", "Reference_number_1", "FishID", "Fish_Condition0")


# standardize all na's, empty cells, 
data <- data %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(na_if,"na") 

# standardize all error (se, SE, sd, SD) formatting
data$UNIT_Size_error_kg<-plyr::revalue(data$UNIT_Size_error_kg, c("se"="SE", "sd"="SD"))
data$UNIT_length_error<-plyr::revalue(data$UNIT_length_error, c("se"="SE", "sd"="SD"))
data$UNIT_swim_error_BLs<-plyr::revalue(data$UNIT_swim_error_BLs, c("se"="SE", "sd"="SD"))
data$UNIT_swim_error_cm_s<-plyr::revalue(data$UNIT_swim_error_cm_s, c("se"="SE", "sd"="SD"))

data[c(887,888),]  # these are two combined Salmonid species O nerka and O kisutch
data$Species_latin<-as.character(data$Species_latin)
data[which(data$Species_latin=="Oncorhynchus gorbuscha "),"Species_latin"]<-"Oncorhynchus gorbuscha"
data[which(data$Species_latin=="Onchorynchus tshawytscha"),"Species_latin"]<-"Oncorhynchus tshawytscha"
data[which(data$Species_latin=="Oncorhynchus gardneri"),"Species_latin"]<-"Oncorhynchus mykiss"
data[c(887,888),"Species_latin"]<-"Oncorhynchus spp."
data$Species_latin<-as.factor(as.character(data$Species_latin))


# group two density measures in just density
data$Fish_Conditions<-as.character(data$Fish_Conditions)
data[which(data$Fish_Conditions=="high density, exercise trained"),"Fish_Conditions"]<-"density exercise trained"
data[which(data$Fish_Conditions=="low density, exercise trained"),"Fish_Conditions"]<-"density exercise trained"
data[which(data$Fish_Conditions=="high density"),"Fish_Conditions"]<-"density"
data[which(data$Fish_Conditions=="low density"),"Fish_Conditions"]<-"density"

data$Fish_Conditions<-as.factor(as.character(data$Fish_Conditions))

# make all differnet jumps together in one column
# make all ucrits (repeat test as well as EMG Ucrits under one - Ucrit)

# one fast start study by Harper et al [ref 67]: the acceleration data are not included. 
data<-data[-which(data$swim_speed_MEAN_cm_s > 2000),] # lines 1114, 1115

# grouping Test performance in broader categories
data$Test_performance2<-NA

for (i in 1:nrow(data)){
  if (grepl("Jump", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Jump"
  }
  if (grepl("Ucrit", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Ucrit"
  }
  if (grepl("Swim", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Swim"
  }
  if (grepl("Umax", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Umax"
  }
  if (grepl("Field", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Field"
  }
  if (grepl("Acceleration", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"Acceleration"
  }
  if (grepl("TTF", as.character(data$Test_performance[i]), perl = TRUE)){
    data$Test_performance2[i]<-"TTF"
  }
  if (is.na(data$Test_performance2[i])){
    data$Test_performance2[i]<-"other"
    print("Category \"other\" performance noted")
  }
}

# grouping swim performance in broader categories
data$Swim_Conditions2<-NA

for (i in 1:nrow(data)){
  if (grepl("Fall", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Fall"
  }
  if (grepl("Fraser", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Fraser"
  }
  if (grepl("Dam", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Dam"
  }
 
  if (grepl("Toyohira", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Toyohira"
  }
  if (grepl("Shibetsu", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Shibetsu"
  }
  if (grepl("Field", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Field"
  }
  if (grepl("Klickitat", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    data$Swim_Conditions2[i]<-"Klickitat"
  }
  if (is.na(data$Swim_Conditions2[i]) & !is.na(data$Swim_Conditions[i])){
    data$Swim_Conditions2[i]<-"other"
    print("Category: other")
  }
}


# summarize all of this by the study, give refs, all levels (bof category --> smallest level)
split_swimcond<-split(data, data$Swim_Conditions2)
# select needed columns

sum_swimcond<-as.data.frame(matrix(nrow=0, ncol=3))
colnames(sum_swimcond)<-c("mainCateg", "subCat", "ref")
for (i in 1:length(split_swimcond)){
  swimcond<-as.data.frame(split_swimcond[i])
  addDF<-unique(swimcond[, c(49,35, 45)] )
  colnames(addDF)<-c("mainCateg", "subCat", "ref")
  sum_swimcond<-rbind(sum_swimcond,addDF)
  colnames(sum_swimcond)<-c("mainCateg", "subCat", "ref")
}
  

# summarize all of this by the study, give refs, all levels (bof category --> smallest level)
split_fishcond<-split(data, data$Fish_Conditions)
# select needed columns

sum_fishcond<-as.data.frame(matrix(nrow=0, ncol=3))
colnames(sum_fishcond)<-c("mainCateg", "subCat", "ref")
for (i in 1:length(split_fishcond)){
  fishcond<-as.data.frame(split_fishcond[i])
  addDF<-unique(fishcond[, c(36,47, 45)] )
  colnames(addDF)<-c("mainCateg", "subCat", "ref")
  sum_fishcond<-rbind(sum_fishcond,addDF)
  colnames(sum_fishcond)<-c("mainCateg", "subCat", "ref")
}

split_swimtest<-split(data, data$Test_performance)

sum_swimtest<-as.data.frame(matrix(nrow=0, ncol=3))
colnames(sum_swimtest)<-c("mainCateg", "subCat", "ref")
for (i in 1:length(split_swimtest)){
  swimtest<-as.data.frame(split_swimtest[i])
  addDF<-unique(swimtest[, c(48,20, 45)] )
  colnames(addDF)<-c("mainCateg", "subCat", "ref")
  sum_swimtest<-rbind(sum_swimtest,addDF)
  colnames(sum_swimtest)<-c("mainCateg", "subCat", "ref")
}

sum_surgery<-as.data.frame(matrix(nrow=0, ncol=2))
colnames(sum_surgery)<-c("mainCateg", "ref")
sum_surgery<-unique(data[, c(41, 45)] )
sum_surgery[order(sum_surgery$Surgery, na.last = TRUE),]
  

# data summaries: ----------- 
nrow(data)
names(data)
cols = c(5:9, 11:14, 18, 22:25, 27:31, 33:34, 37:38)
cols_fact = c(1:4,10,15:17,19:21, 32, 35:36, 39:49)
data[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
summary(data[,cols])
data[,cols_fact] %<>% lapply(function(x) as.factor(as.character(x)))
summary(data[,cols_fact])

data_sum <- as.data.frame(do.call(cbind, lapply(data[,cols], summary)))
data_sum$summary<-rownames(data_sum)
rownames(data_sum)<-1:nrow(data_sum)

# reorganize the data wide to long format
data_sum2<-gather(data_sum,  measurement, summary_metric, N_morphometrics:Gonad_g, factor_key=TRUE)
data_sum2<-dcast(data_sum2, measurement ~ summary, value.var="summary_metric")

# save the new summary file with all numeric variables
write.csv(file="./Exported data/Kraskura_summary_Allnumeric.csv", data_sum2, row.names=FALSE) 

data_split_test<-split(data, data$Test_performance2)

# all but Atlatic salmon and trout; North America
target1<-data[-c(which(data$Species_latin == "Salmo salar" | data$Species_latin == "Oncorhynchus mykiss")),]
length(unique(target1$Reference_number_1)) # 52 studies, 1165 dp
nrow(target1)

# Japan 
target2<-data[c(which(grepl("Shibetsu", target1$Origin)), which(grepl("Toyohira", target1$Origin))), ] # 2
unique(target2$Reference_number_1) # 5 studies, 42 dp
nrow(target2)


# Atlantic and mykiss
length(which(data$Species_latin == "Salmo salar")) #158
length(which(data$Species_latin == "Oncorhynchus mykiss")) #136

data_sum000<-data %>% 
  group_by(Key_origin) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s))


# summary by differnet test performances
data_sum374<-data %>% 
  group_by(Test_performance2) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s))

#datapoints cm - INDIVIDUAL vs GROUP - for size
data_sum3dp<-data %>% 
  group_by(Test_performance2,Indiv_group) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s), 
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg))

#datapoints cm - INDIVIDUAL vs GROUP for cm/s
data_sum3dpCM<- data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
  group_by(Test_performance2,Indiv_group) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s), 
                    min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s))

#datapoints BL - INDIVIDUAL vs GROUP for BL/s
data_sum3dpBL<- data[!is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Test_performance2, Indiv_group) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_BL_s),
                    min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s))





#datapoints  - SEX for size
data_sum7dpsex<-data %>% 
  group_by(Test_performance2,Sex_F_M) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s), 
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg))

#datapoints cm - SEX for cm/s
data_sum7dpCMsex<- data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
  group_by(Test_performance2,Sex_F_M) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s), 
                    min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s))

#datapoints BL - SEX for BL/s 
data_sum7dpBLsex<- data[!is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Test_performance2, Sex_F_M) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_BL_s),
                    min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s))

as.data.frame(data_sum374) # reported 

as.data.frame(data_sum3dp) # reported 
as.data.frame(data_sum3dpCM)# reported 
as.data.frame(data_sum3dpBL)# reported 
as.data.frame(data_sum7dpsex)# reported 
as.data.frame(data_sum7dpCMsex)# reported 
as.data.frame(data_sum7dpBLsex)# reported 

data_sum4<-data %>% 
  group_by(Test_performance2, Surgery) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)),n_data=length(swim_speed_MEAN_cm_s))
# data_perTest_sum<-as.data.frame(matrix(ncol=3, nrow=0))
data_sum4dp<-data %>% 
  group_by(Test_performance2, Surgery) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s),
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg))
#datapoints SURGERY - for cm/s
data_sum4dpCM<-data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
  group_by(Test_performance2, Surgery) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_cm_s), 
                    min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s))

#datapoints BL - SURGERY for BL/s
data_sum4dpBL<-data[!is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Test_performance2, Surgery) %>% 
  dplyr:::summarize(n_studies=length(unique(Reference_number_1)), n_data=length(swim_speed_MEAN_BL_s), 
                    min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s))



as.data.frame(data_sum4dp) # reported
as.data.frame(data_sum4dpBL) # reported
as.data.frame(data_sum4dpCM) # reported
# as.data.frame(data_sum4)


# Fish condition
data_sum8<-data[which(!is.na(data$swim_speed_error_BL_s) | !is.na(data$swim_speed_error_cm_s)) , ] %>% 
  group_by(Fish_Conditions) %>% 
  dplyr:::summarize(n_studies_ref=length(unique(Reference_number_1)), 
                    n_dp=length(swim_speed_MEAN_cm_s))
# Fish condition
data_sum9<-data[which(!is.na(data$swim_speed_error_BL_s) | !is.na(data$swim_speed_error_cm_s)) , ] %>% 
  group_by(Swim_Conditions2) %>% 
  dplyr:::summarize(n_studies_ref=length(unique(Reference_number_1)), 
                    n_dp=length(swim_speed_MEAN_cm_s))


# by species
data_sum5<-as.data.frame(data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                    studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints
data_sum5dp<-as.data.frame(data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(n_data=length(swim_speed_MEAN_cm_s),
                     studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints BL
data_sum5dpBL<-as.data.frame(data[!is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                    studies = paste(unique(Reference_number_1), collapse = ', ')))
data_sum5bothswim<-as.data.frame(data[c(!is.na(data$swim_speed_MEAN_BL_s) & !is.na(data$swim_speed_MEAN_cm_s)),] %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(n_data=length(swim_speed_MEAN_BL_s),
                     studies = paste(unique(Reference_number_1), collapse = ', ')))

# save for prospectus

# recovery studies, data points
data_sum6<-data[c(!is.na(data$swim_speed_MEAN_cm_s) & !data$Recovery=="n"),] %>% 
  group_by(Test_performance2, Recovery) %>% 
  dplyr:::summarize( n_studies_rec=length(unique(Reference_number_1)))
#datapoints
data_sum6dp<-data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
  group_by(Recovery) %>% 
  dplyr:::summarize(n_data=length(swim_speed_MEAN_cm_s))
#datapoints BL
data_sum6dpBL<-data[!is.na(data$swim_speed_MEAN_BL_s),]  %>% 
  group_by(Recovery) %>% 
  dplyr:::summarize(n_data=length(swim_speed_MEAN_BL_s))


# max speed
swim_max<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(max_max= max(swim_speed_MAX_cm_s, na.rm=TRUE), mean_max= max(swim_speed_MEAN_cm_s, na.rm=TRUE))
swim_maxBL<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(max_max= max(swim_speed_MAX_BL_s, na.rm=TRUE), mean_max= max(swim_speed_MEAN_BL_s, na.rm=TRUE))


### by species using MIN and MAX to get the full range 
# by species - USING MEANS ONLY
data_sum_species_test_MINMAX<-data %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedCM = min(swim_speed_MIN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MAX_cm_s, na.rm = TRUE), 
                    min_swim_speedBL = min(swim_speed_MIN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MAX_BL_s, na.rm = TRUE), 
                    min_LengthCM = min(Length_MIN_cm, na.rm = TRUE), max_LengthCM = max(Length_MAX_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MIN_kg, na.rm = TRUE), max_Sizekg = max(Size_MAX_kg, na.rm = TRUE))

data_sum_species_MINMAX<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(min_swim_speedCM = min(swim_speed_MIN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MAX_cm_s, na.rm = TRUE), 
                    min_swim_speedBL = min(swim_speed_MIN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MAX_BL_s, na.rm = TRUE), 
                    min_LengthCM = min(Length_MIN_cm, na.rm = TRUE), max_LengthCM = max(Length_MAX_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MIN_kg, na.rm = TRUE), max_Sizekg = max(Size_MAX_kg, na.rm = TRUE))


# by species - USING MEANS ONLY
data_sum_species_test<-data %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s), 
                    min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s), 
                    
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg),
                    n_studies_total=length(unique(Reference_number_1)))

data_sum_species<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarize(min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s), 
                    min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s), 
                    
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg),
                    n_studies_total=length(unique(Reference_number_1)))



# write.csv(file="./Exported data/Kraskura_summary_species_test.csv", data_sum_species, row.names=FALSE) 
# write.csv(file="./Exported data/Kraskura_summary_species_test_2.csv", data_sum_species_test, row.names=FALSE) 
as.data.frame(data_sum_species) # reported 
as.data.frame(data_sum_species_test) # reported 
as.data.frame(data_sum_species_MINMAX)
as.data.frame(data_sum_species_test_MINMAX)



## what is the top 10% speed distribution of each species? 


# estimate either missing bw or TL based - values from Fishbase.org -------
# units must be in cm and g
# Fishbase table in: W = a × L^b
# the intercept a conversion:  conversion from (cm, kg) to (cm, g) as follows: 
# a'(cm, g) = a (cm, kg)*1000

# set b=3 and calculate the missing pieces

# log10(weight) = log10(a) + b*log10(length)

# pink salmon: 
pink_a = 0.01175 #(0.00648 - 0.02132)
pink_b = 3.08 #(2.93 - 3.23)
coho_a = 0.01120
coho_b = 3.000
sockeye_a1 = 0.01555
sockeye_a2 = 0.01922
sockeye_a = mean(c(sockeye_a1, sockeye_a2))
sockeye_b = 3.000
chum_a = 0.01413 #(0.00734 - 0.02718)
chum_b = 3.09 #(2.92 - 3.26)
atlantic_a = 0.01047 #(0.00847 - 0.01294)
atlantic_b = 3.02 #(2.97 - 3.07)
chinook_a = 0.01333
chinook_b = 3.0
masu_a = 0.00933 # (0.00563 - 0.01547), 
masu_b = 3.02 # (2.88 - 3.16)
trout_a = 0.00955 #(0.00738 - 0.01236)
trout_b = 3.03 # (2.98 - 3.08)

# data[1,c("Species_latin_name", "Size_MEAN_kg", "Length_MEAN_cm")]
data$Length_cm_estimated<-NA
data$Size_kg_estimated<-NA
data$species_b<-NA
data$species_a<-NA

# formula to estimate mass
# log10(weight) = log10(a) + b*log10(length)

# formula to estimate lenght
# log10(length) = (log10(weight) - log10(a)) / b

for (i in 1:nrow(data)){
  if (is.na(data[i,c("Species_latin")])){next}
  
  if (data[i,c("Species_latin")]=="Oncorhynchus mykiss"){ # trout
    data$species_a[i] <- trout_a
    data$species_b[i] <- trout_b
    
    if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
       data$Size_kg_estimated[i] <- 10^(log10(trout_a) + (trout_b * log10(data$Length_MEAN_cm[i])))/1000
    }
    if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
      data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(trout_a)) / trout_b)
    }
  }
  
  if (data[i,c("Species_latin")]=="Oncorhynchus masou"){ # masu
    data$species_a[i] <- masu_a
    data$species_b[i] <- masu_b
    
    if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
       data$Size_kg_estimated[i] <- 10^(log10(masu_a) + (masu_b * log10(data$Length_MEAN_cm[i])))/1000
    }
    if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
      data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(masu_a)) / masu_b)
    }
  }
  
  if (data[i,c("Species_latin")]=="Oncorhynchus kisutch"){ # coho
    data$species_a[i] <- coho_a
    data$species_b[i] <- coho_b
    
    if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
       data$Size_kg_estimated[i] <- 10^(log10(coho_a) + (coho_b * log10(data$Length_MEAN_cm[i])))/1000
    }
    if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
      data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(coho_a)) / coho_b)
    }
  }
  if (data[i,c("Species_latin")]=="Oncorhynchus keta"){ # chum
    data$species_a[i] <- chum_a
    data$species_b[i] <- chum_b
    
    if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(chum_a) + (chum_b * log10(data$Length_MEAN_cm[i])))/1000
    }
    if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(chum_a)) / chum_b)
    }
  }
  
  if (data[i,c("Species_latin")]=="Oncorhynchus gorbuscha"){ # pink
    data$species_a[i] <- pink_a
    data$species_b[i] <- pink_b
    
    if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
      data$Size_kg_estimated[i] <- 10^(log10(pink_a) + (pink_b * log10(data$Length_MEAN_cm[i])))/1000
    }
    if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
      data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(pink_a)) / pink_b)
    }
  }
  
   if (data[i,c("Species_latin")]=="Oncorhynchus nerka"){ # sockeye
    data$species_a[i] <- sockeye_a
    data$species_b[i] <- sockeye_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(sockeye_a) + (sockeye_b * log10(data$Length_MEAN_cm[i])))/1000
       }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(sockeye_a)) / sockeye_b)
      }
      
   }
  
   if (data[i,c("Species_latin")]=="Salmo salar"){ # atlantic
    data$species_a[i] <- atlantic_a
    data$species_b[i] <- atlantic_b
    
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(atlantic_a) + (atlantic_b * log10(data$Length_MEAN_cm[i])))/1000
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(atlantic_a)) / atlantic_b)
      }
    
   }
  
  if (data[i,c("Species_latin")]=="Oncorhynchus tshawytscha"){ # chinook
    data$species_a[i] <- chinook_a
    data$species_b[i] <- chinook_b
    
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(chinook_a) + (chinook_b * log10(data$Length_MEAN_cm[i])))/1000
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(chinook_a)) / chinook_b)
      }
  }

  if (is.na(data$Length_cm_estimated[i])){
      data$Length_cm_estimated[i] <- data$Length_MEAN_cm [i]
  }
  if (is.na(data$Size_kg_estimated[i])){
      data$Size_kg_estimated[i] <- data$Size_MEAN_kg [i]
  }
  
}

data$Size_kg_value <- NA
data$Size_kg_value[which(data$Size_MEAN_kg==data$Size_kg_estimated & !is.na(data$Size_MEAN_kg))] <- "reported"
data$Size_kg_value[which(!is.na(data$Size_kg_estimated) & is.na(data$Size_MEAN_kg))]<- "estimated"
# 
data$Length_cm_value <- NA
data$Length_cm_value[which(data$Length_MEAN_cm==data$Length_cm_estimated & !is.na(data$Length_MEAN_cm))] <- "reported"
data$Length_cm_value[which(!is.na(data$Length_cm_estimated) & is.na(data$Length_MEAN_cm))]<- "estimated"
# 
data[,c("Species_latin", "Size_MEAN_kg", "Length_MEAN_cm","Size_kg_estimated", "Length_cm_estimated", "Size_kg_value", "Length_cm_value")]


# how many studies? 
length(unique(data$Reference_number)) 
# what is the year range that these data cover?

# how many tracking studies? 
tracking<-data[c(data$Tracking=="y"), ]
lab<-data[c(data$Tracking=="n"), ]
length(unique(tracking$Reference_number)) # 24
length(unique(lab$Reference_number)) # 71

Fieldswim<-data[c(data$Test_performance=="Field"), ]
Labswim<-data[!c(data$Test_performance=="Field"), ]
length(unique(Fieldswim$Reference_number)) # 24
length(unique(Labswim$Reference_number)) # 71

data[which(data$Test_performance!="Field" & data$Tracking=="y"), "Reference_number_1"] # ones that do not match are acceletaration, but still from teh field tracking. Refs 47, 56
data[which(data$Test_performance=="Field" & data$Tracking=="n"), "Reference_number_1"] # the one that does not match is sweim study right in the field, fish swum in near dam, but not tagged/tracked ref 53

# What is the absolute temperature range? 
# obtain all categorical variables 
unique(data$Test_performance)
unique(data$Swim_Conditions)
unique(data$Fish_Conditions)
unique(data$Surgery)


# different Fish conditions 
unique(data$Condition_category)
unique(as.factor(data$Test_performance2))
summary(data$Condition_category)
summary(as.factor(data$Test_performance2))
summary(as.factor(data$Test_performance))


# freshawter vs saltwater adults:
fresh<-data[c(data$SW_FW=="FW"),]
salt<-data[!c(data$SW_FW=="FW"),] # this includes brackish

male<-data[which(data$Sex_F_M=="M"),]
female<-data[which(data$Sex_F_M=="F"),]
mixedsex<-data[which(data$Sex_F_M=="mixed"),]

sum_male<-male %>% 
  group_by(Test_performance2) %>% 
  dplyr:::summarize(min_speed = min(swim_speed_MEAN_cm_s, na.rm=TRUE), max_speed = max(swim_speed_MEAN_cm_s, na.rm=TRUE), min_Relspeed = min(swim_speed_MEAN_BL_s, na.rm=TRUE), max_Relspeed = max(swim_speed_MEAN_BL_s, na.rm=TRUE))

sum_female<-female %>% 
  group_by(Test_performance2) %>% 
  dplyr:::summarize(min_speed = min(swim_speed_MEAN_cm_s, na.rm=TRUE), max_speed = max(swim_speed_MEAN_cm_s, na.rm=TRUE), min_Relspeed = min(swim_speed_MEAN_BL_s, na.rm=TRUE), max_Relspeed = max(swim_speed_MEAN_BL_s, na.rm=TRUE))

# estimate fishes swimming capacity from BL/s to cm/s
data$fish_swim_cm_s_ESTIMATED<-data$swim_speed_MEAN_BL_s * data$Length_MEAN_cm # all original data from the paper
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s)),]) # added 290 data points using this analysis
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="indiv"),]) #  analysis # 209 from individual
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="group"),]) #  analysis # 81 from group :( - not good estimates

data$fish_swim_cm_s_ESTIMATED_2<-data$swim_speed_MEAN_BL_s * data$Length_cm_estimated # estimated length using fishbase relationships
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED_2) & is.na(data$swim_speed_MEAN_cm_s)),]) 
data[,c("fish_swim_cm_s_ESTIMATED","fish_swim_cm_s_ESTIMATED_2", "swim_speed_MEAN_cm_s", "Length_MEAN_cm","Length_cm_estimated", "Length_cm_value")] # compare how different these are

# --- Figures------

# formatting: 
display.brewer.pal(n = 9, name = "Paired") # use for species
display.brewer.all()


# fgure legends: Symbol's shape, colors, fill
# fig v1: all black - just overall trends - NOT neccesarry? 

# fig v2: color - species
# scale_colour_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=brewer.pal(n = 9, name = "Paired"), labels=c("Oncorhynchus spp.", "O. gardnieri", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))+

# fig v3: color - temperature gradient 
  # scale_colour_gradient(low = "blue", high = "red")+
# colour=Temp_test_mean

# fig v4: shape - sex; open circles females, closed ciicles males 
  # scale_shape_manual(breaks = c("F", "M", "mixed"), values=c(21, 19, 13))+
  # pch=Sex_F_M

# Always facet by test  
#### Plots: trends by year PUBLISHED ---------
year_trend<-ggplot(data = data, aes(y=swim_speed_MEAN_cm_s, x=Species_latin, fill=as.numeric(as.character(Year))))+
  # geom_point(position=position_jitter(width=0.2), alpha=1, colour="grey3", pch=21, size=2)+
  geom_boxplot(fill = "white", alpha=0.9, outlier.shape=NA) +
  geom_point(position=position_jitter(), alpha=0.7, pch=21, size=2 )+
  scale_fill_gradient(low = "black", high = "white")+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(year_trend, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
year_trend<-year_trend +theme (axis.text.x = element_text(angle=45, hjust=1), legend.title = element_blank())

png("../DATA and STATS analyses/Swim_year_cm_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange( year_trend)
dev.off()

year_trend3<-ggplot(data = data[!duplicated(data[,c('Reference_number_1', 'Species_latin')]),], aes(as.numeric(as.character(Year)), linetype=Species_latin))+
  # geom_point(position=position_jitter(width=0.2), alpha=1, colour="grey3", pch=21, size=2)+
 geom_histogram( aes(as.numeric(as.character(Year)),  fill=Test_performance2), alpha=0.5, colour="white", binwidth = 1, inherit.aes=FALSE)+
  geom_freqpoly(binwidth = 5, lwd=1)

ggformat(year_trend3, title = "", y_title = "N studies", x_title = "Year")
year_trend3<-year_trend3 +theme (axis.text.x = element_text(angle=45, hjust=1), legend.title = element_blank(), legend.key.width = unit(2, "cm"))

year_trend2<-ggplot(data = data, aes(as.numeric(as.character(Year)), linetype=Species_latin))+
  # geom_point(position=position_jitter(width=0.2), alpha=1, colour="grey3", pch=21, size=2)+
 geom_histogram( aes(as.numeric(as.character(Year)),  fill=Test_performance2), alpha=0.5, colour="white", binwidth = 1, inherit.aes=FALSE)+
  geom_freqpoly(binwidth = 5, lwd=1)

ggformat(year_trend2, title = "", y_title = "N datapoints", x_title = "Year")
year_trend2<-year_trend2 +theme (axis.text.x = element_text(angle=45, hjust=1), legend.title = element_blank(), legend.key.width = unit(2, "cm"))


png("../DATA and STATS analyses/Swim_year2_cm_bySPECIES.png", res = 200, width = 12, height = 12, units="in")
  grid.arrange( year_trend2, year_trend3, ncol=1, nrow=2)
dev.off()

# Plots:  categorical fish condition ----------
# colors: 
brewer.pal(n=8, name = "Accent")
# diet "dodgerblue"
# exercise trained "#386CB0"
# fallback "red"
# infection "#D9D9D9"
# mature  "#FFFFB3"
# pass "#7FC97F"
# prior anesthetic   "#A65628"
# spawned "#FB8072"
# toxicant "grey"
# unhealthy "black"
# high density exercis trained  "#386CB0"
# high density "white"
# low density exercise trained "#386CB0"
# low density "white"


 c("dodgerblue", "#386CB0", "red", "#D9D9D9","#FFFFB3","#7FC97F", "#A65628", "#FB8072", "grey", "black")


fish_cond<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=swim_speed_MEAN_cm_s, x=Species_latin, fill=Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(), alpha=0.7, size=2, pch=21)+
  scale_fill_manual(values = c("white", "#386CB0", "dodgerblue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(fish_cond, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
fish_cond<-fish_cond + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

### ESTIMATED SWWIM SPEEDS
# fish_cond<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=fish_swim_cm_s_ESTIMATED_2, x=Species_latin, fill=Fish_Conditions))+
#   geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) +
#   geom_point(position=position_jitterdodge(), alpha=0.9, size=3)+
#    scale_shape_manual(values=c(21,22), breaks=c("reported", "estimated"))+
#   scale_fill_manual(values = c("dodgerblue", "#386CB0", "red", "#D9D9D9","#FFFFB3","#7FC97F", "#A65628", "#FB8072", "grey", "darkgrey"))+
#   scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
# ggformat(fish_cond, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
# fish_cond<-fish_cond + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())


png("../DATA and STATS analyses/Swim_FishCond_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange(fish_cond)
dev.off()

fish_condBL<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=swim_speed_MEAN_BL_s, x=Species_latin, fill=Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  scale_fill_manual(values = c("white", "#386CB0", "dodgerblue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(fish_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "Species")
fish_condBL<-fish_condBL + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/SwimBL_FishCond_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange(fish_condBL)
dev.off()


# Plots:  swim condition  ------

# colors for Swim conditions
# Dam  
# Fall
# Fraser   
# Klickitat
# other
# Shibetsu
# Toyohira


swim_cond<-ggplot(data = data[!is.na(data$Swim_Conditions2),], aes(y=swim_speed_MEAN_cm_s, x=Species_latin, fill=Swim_Conditions2))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  scale_fill_manual(values = brewer.pal(n=8, name = "Set1"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(swim_cond, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
swim_cond<-swim_cond + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/Swim_SwimCond_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange( swim_cond)
dev.off()


swim_condBL<-ggplot(data = data[!is.na(data$Swim_Conditions2),], aes(y=swim_speed_MEAN_BL_s, x=Species_latin, fill=Swim_Conditions2))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  scale_fill_manual(values = brewer.pal(n=8, name = "Set1"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(swim_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "Species")
swim_condBL<-swim_condBL + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/SwimBL_SwimCond_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange( swim_condBL)
dev.off()


## Plots:  all test by test performance:
swim_test<-ggplot(data = data[!is.na(data$Test_performance),], aes(y=swim_speed_MEAN_cm_s, x=Species_latin, fill=Test_performance2))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  # scale_shape_manual(values=c(21, 24))+
  scale_fill_manual(values = brewer.pal(n=8, name = "Set3"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(swim_test, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
swim_test<-swim_test + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/Swim_Test_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange( swim_test)
dev.off()


swim_testBL<-ggplot(data = data[!is.na(data$Test_performance),], aes(y=swim_speed_MEAN_BL_s, x=Species_latin, fill=Test_performance2))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  # scale_shape_manual(values=c(21, 24))+
  scale_fill_manual(values = brewer.pal(n=8, name = "Set3"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(swim_testBL, title = "", y_title = "Swim speed (BL/s)", x_title = "Species")
swim_testBL<-swim_testBL + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/SwimBL_Test_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange( swim_testBL)
dev.off()

### Plots:  recovery specific ----------
recov_swim<-ggplot(data = data[which(data$Test_performance2=="Ucrit"),], aes(y=swim_speed_MEAN_cm_s, x=Species_latin, fill=Test_performance))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  # scale_shape_manual(values=c(21, 24))+
  scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(recov_swim, title = "", y_title = "Swim speed (cm/s)", x_title = "Species")
recov_swim<-recov_swim + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/Recov_Test_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange(recov_swim)
dev.off()

recov_swimBL<-ggplot(data = data[which(data$Test_performance2=="Ucrit"),], aes(y=swim_speed_MEAN_BL_s, x=Species_latin, fill=Test_performance))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)+
  # scale_shape_manual(values=c(21, 24))+
  scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) 
ggformat(recov_swimBL, title = "", y_title = "Swim speed (BL/s)", x_title = "Species")
recov_swimBL<-recov_swimBL + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

png("../DATA and STATS analyses/RecovBL_Test_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
  grid.arrange(recov_swimBL)
dev.off()


# Plots: temperature -----

# "Oncorhynchus"        "#A6CEE3"
# "Oncorhynchus mykiss"  "#1F78B4" 
# "Oncorhynchus gorbuscha" "#B2DF8A" 
# "Oncorhynchus keta"     "#33A02C"
# "Oncorhynchus kisutch"   "#FB9A99" 
# "Oncorhynchus nerka"   "#E31A1C"
# "Oncorhynchus tshawytscha"  "#FDBF6F" 
# "Oncorhynchus masou"  "#FF7F00"
# "Salmo salar"  "#CAB2D6"
c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6")


temp1<-ggplot(data=data, aes(Temp_test_mean, swim_speed_MEAN_cm_s, fill=Species_latin,
                              # colour=Temp_test_mean,
                              # shape=Sex_F_M,
                              label=Reference_number_1))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) +
  geom_point(size=2, alpha=0.7, colour="black", pch=21)
 ggformat(temp1, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Temperature (deg. C)", title ="")
temp1<-temp1 +theme (legend.title = element_blank())

temp1BL<-ggplot(data=data, aes(Temp_test_mean, swim_speed_MEAN_BL_s, fill=Species_latin,
                              # colour=Temp_test_mean,
                              # shape=Sex_F_M,
                              label=Reference_number_1))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=brewer.pal(n = 9, name = "Paired"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) + geom_point(size=2, alpha=0.7, pch=21, colour="black")
 ggformat(temp1BL, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Temperature (deg. C)", title ="")
temp1BL<-temp1BL + theme (legend.title = element_blank())


png("../DATA and STATS analyses/Temp_swim_bySPECIES.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange( temp1)
dev.off()
 
png("../DATA and STATS analyses/Temp_swimBL_bySPECIES.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange( temp1BL)
dev.off()
 


# extra datasets that will be useful
#### Plots: size ----
p.size<-ggplot(data=data, aes(x=Size_MEAN_kg, y=Length_MEAN_cm,
                              fill=Species_latin
                              # colour=Temp_test_mean,
                              # shape=Length_cm_value,
                              ))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))+
  # scale_colour_gradient(low = "blue", high = "red")+
  # scale_shape_manual(breaks = c("F", "M", "mixed"), values=c(21, 19, 13))+
  # geom_errorbar(aes(ymin=swim_speed_MEAN_cm_s-swim_speed_error_cm_s,
  #                   ymax=swim_speed_MEAN_cm_s+swim_speed_error_cm_s))+
  # geom_errorbarh(aes(xmin=Length_cm_estimated-Length_error,
  #                   xmax=Length_cm_estimated+Length_error))+
  # geom_point(size=3)+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                    xmax=Size_MEAN_kg+Size_error_kg))+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error))+
  geom_point(size=2.5, alpha=0.6, pch=21)
  # scale_shape_manual(values=c(21,23))+
  # geom_abline(aes(slope=0, intercept = 50),  lty="dashed")
  # facet_grid(Test_performance2~.)
 ggformat(p.size, print=TRUE, y_title = "Body length (length, cm)", x_title = "Body size (weight, kg)", title ="")
p.size<-p.size+theme(legend.position="top", legend.title = element_blank())

# p.size
png("../DATA and STATS analyses/Weight-length_bySPECIES.png", res = 200, width = 6, height = 6, units="in")
  grid.arrange( p.size)
dev.off()




# Plots:  sex specific: --------

# png("./Figures/Weight-length_byTEMP.png", res = 200, width = 6, height = 10, units="in")
#   grid.arrange( p.size)
# dev.off()
# png("./Figures/Weight-length_bySEX.png", res = 200, width = 6, height = 10, units="in")
#   grid.arrange( p.size)
# dev.off()
# sex and temp 
sex1<-ggplot(data=data, aes( y=swim_speed_MEAN_cm_s, x=Sex_F_M, fill=Test_performance2))+
 # scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) +
  scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)
 ggformat(sex1, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Sex", title ="")
sex1<-sex1 +theme (legend.title = element_blank())

png("../DATA and STATS analyses/Sex_swim_byTEST.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange(sex1)
dev.off()

sex1BL<-ggplot(data=data, aes( y=swim_speed_MEAN_BL_s, x=Sex_F_M, fill=Test_performance2))+
 # scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) +
  scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)
 ggformat(sex1BL, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Sex", title ="")
sex1BL<-sex1BL +theme (legend.title = element_blank())

png("../DATA and STATS analyses/Sex_swimBL_byTEST.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange(sex1BL)
dev.off()



sex2<-ggplot(data=data, aes( y=swim_speed_MEAN_cm_s, x=Sex_F_M, fill=Species_latin))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) +
  # scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)
 ggformat(sex2, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Sex", title ="")
sex2<-sex2 +theme (legend.title = element_blank())

png("../DATA and STATS analyses/Sex_swim_bySPECIES.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange(sex2)
dev.off()

sex2BL<-ggplot(data=data, aes( y=swim_speed_MEAN_BL_s, x=Sex_F_M, fill=Species_latin))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")) +
  # scale_fill_manual(values = brewer.pal(n=8, name = "Dark2"))+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  geom_point(position=position_jitterdodge(), alpha=0.7, pch=21, size=2)
 ggformat(sex2BL, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Sex", title ="")
sex2BL<-sex2BL +theme (legend.title = element_blank())

png("../DATA and STATS analyses/Sex_swimBL_bySPECIES.png", res = 200, width = 6, height = 4.5, units="in")
  grid.arrange(sex2BL)
dev.off()


#### Plots: swimspeed size---- 


p1.cm<-ggplot(data=data[!is.na(data$swim_speed_MEAN_cm_s),], aes(y=swim_speed_MEAN_cm_s, x=Length_MEAN_cm,
                                                                 fill=Species_latin,
                                                                 # colour=Temp_test_mean,
                                                                 # shape=Sex_F_M,
                                                                 label=Reference_number_1))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))+
  # scale_colour_gradient(low = "blue", high = "red")+
  # scale_shape_manual(breaks = c("F", "M", "mixed"), values=c(21, 19, 13))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_cm_s-swim_speed_error_cm_s,
                    ymax=swim_speed_MEAN_cm_s+swim_speed_error_cm_s))+
  geom_errorbarh(aes(xmin=Length_MEAN_cm-Length_error,
                    xmax=Length_MEAN_cm+Length_error))+
  geom_point(size=3, alpha=0.8, pch=21)+
  # geom_text(angle=45, check_overlap = T)+
  facet_grid(Test_performance2~., scales="free")
ggformat(p1.cm, print=TRUE, y_title = "Swim speed (mean, cm/s)", x_title = "Body size (length, cm)", title ="")

p1.cm <- p1.cm+theme(legend.position = "top", legend.title = element_blank())
# p1.cm <- p1.cm+geom_point(data=data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s)),], aes(y=fish_swim_cm_s_ESTIMATED, x=Length_cm_estimated, colour=Species_latin), alpha=0.5, size=3)
# p1.cm <- p1.cm+geom_text(data=data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s)),], aes(label=Reference_number_1), colour="red", pch=17, alpha=0.5)
# p1.cm

# sex specific:
png("../DATA and STATS analyses/Size_swim_bySPECIES.png", res = 200, width = 6, height = 10, units="in")
  grid.arrange( p1.cm)
dev.off()
# png("./Figures/Size_swim_byTEMP.png", res = 200, width = 6, height = 10, units="in")
#   grid.arrange( p1.cm)
# dev.off()
# png("./Figures/Size_swim_bySEX.png", res = 200, width = 6, height = 10, units="in")
#   grid.arrange( p1.cm)
# dev.off()
# reference number plot
# png("./Figures/Size_swim_byREFnumb2.png", res = 200, width = 6, height = 10, units="in")
#   grid.arrange( p1.cm)
# dev.off()


## all the same as above, just in BL/s
p1.BL<-ggplot(data=data[!is.na(data$swim_speed_MEAN_BL_s),], aes(y=swim_speed_MEAN_BL_s, x=Length_MEAN_cm,
                                                                 fill=Species_latin,
                                                                 # colour=Temp_test_mean,
                                                                 # shape=Sex_F_M,
                                                                 label=Reference_number_1))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_speed_error_BL_s,
                    ymax=swim_speed_MEAN_BL_s+swim_speed_error_BL_s))+
  geom_errorbarh(aes(xmin=Length_MEAN_cm-Length_error,
                    xmax=Length_MEAN_cm+Length_error))+
  geom_point(size=3, alpha=0.8, pch=21)+
  # geom_text(angle=45, check_overlap = T)+
  facet_grid(Test_performance2~., scales="free")
ggformat(p1.BL, print=TRUE, y_title = "Swim speed (mean, BL/s)", x_title = "Body size (length, cm)", title ="")
p1.BL <- p1.BL+theme(legend.position = "top", legend.title = element_blank())

# p1.BL <- p1.BL+geom_text(data=data[c(!is.na(data$fish_swim_BL_s_ESTIMATED) & is.na(data$swim_speed_MEAN_BL_s)),], aes(label=Reference_number_1), colour="red", pch=17, alpha=0.5)
# p1.BL

# sex specific:
png("../DATA and STATS analyses/Size_swimBL_bySPECIES.png", res = 200, width = 6, height = 10, units="in")
  grid.arrange( p1.BL)
dev.off()
# png("./Figures/Size_swimBL_byTEMP.png", res = 200, width = 6, height = 10, units="in")


data$Surgery<-as.character(data$Surgery)
data[is.na(data$Surgery),"Surgery"] <- "-"
data$Surgery<-as.factor(data$Surgery)

# acceleration "orange"
# acoustic "darkgreen"
# EMG "red"
# Radio "blue"
# undefined "black"
# Canulation "magenta"
# canulation and floProbe "pink"
# FlowProbe "blueviolet"
# ligation "cyan3"

##### histograms speed surgery, sex faceted--------
# acceleration "orange"
# acoustic "darkgreen"
# EMG "red"
# Radio "blue"
# undefined "black"
# Canulation "magenta"
# canulation and floProbe "pink"
# FlowProbe "blueviolet"
# ligation "cyan3"
# magnetic ring "grey"
# heart perfusion in situ "brown"

# acceleration "orange"
# acoustic "darkgreen"
# EMG "red"
# Radio "blue"
# undefined "black"
# Canulation "magenta"
# canulation and floProbe "pink"
# FlowProbe "blueviolet"
# ligation "cyan3"
# magnetic ring "grey"
# Optode implant "white"

# Field only 
hist1<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s) & data$Test_performance=="Field"),], x = "swim_speed_MEAN_cm_s",
   bins = 30,
   # add = "mean",
   rug = TRUE,
   fill = "Surgery",
   facet.by= "Sex_F_M",
   position="stack",
   palette = c( "black", "red", "blue"),
   # title= "Field studies",
   ylab="Count",
   legend.title = "",
   legend = "right",
   xlab="Swim speed (cm/s)")  # EMG, radio, NA
hist1

hist1.bl<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s) & data$Test_performance=="Field"),], x = "swim_speed_MEAN_BL_s", bins = 30,
   # add = "mean",
   rug = TRUE,
   fill = "Surgery",
   facet.by= "Sex_F_M",
   position="stack",
   palette = c("black", "orange", "darkgreen", "red", "grey", "blue"),
   # title= "Field studies",
   ylab="Count",
   legend.title = "",
     legend = "right",
   xlab="Swim speed (BL/s)") # --, acceleration, # acoustic, EMG, MagneticRing, Radio
hist1.bl


# png("./Figures/Swim_hist_Field_bySURGERY.png", res = 200, width = 10, height = 9, units="in")
#   grid.arrange( hist1, hist1.bl, ncol=1, nrow=2)
# dev.off()


# # Non field only 
hist1.lab<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s) & data$Test_performance!="Field"),], x = "swim_speed_MEAN_cm_s",
   bins = 30,
   # add = "mean",
   rug = TRUE,
   fill = "Surgery",
   facet.by= "Sex_F_M",
   position="stack",
   palette = c("black", "orange", "magenta", "pink", "red", "blueviolet", "cyan3", "blue"),
   # title= "NON-field studies",
   ylab="Count",
  legend.title = "",
    legend = "right",
   xlab="Swim speed (cm/s)")  #  - , Acceler, Canul, Canul+flow, EMG, flow, ligation, radio
hist1.lab
# 
# 
hist1.bl.lab<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s) & data$Test_performance!="Field"),], x = "swim_speed_MEAN_BL_s", bins = 30,
   # add = "mean",
   rug = TRUE,
   fill = "Surgery",
   facet.by= "Sex_F_M",
   position="stack",
   palette = c("black", "orange", "magenta", "pink", "red", "blueviolet", "cyan3", "white"),
   # title= "NON-field studies",
   ylab="Count",
   legend.title = "",
     legend = "right",
   xlab="Swim speed (BL/s)")
hist1.bl.lab


png("../DATA and STATS analyses/Swim_hist_Lab_bySURGERY.png", res = 200, width = 12, height = 8, units="in")
  grid.arrange( hist1.lab, hist1.bl.lab,  ncol=1, nrow=2)
dev.off()

png("../DATA and STATS analyses/Swim_hist_Field_bySURGERY.png", res = 200, width = 12, height = 8, units="in")
  grid.arrange(  hist1, hist1.bl, ncol=1, nrow=2)
dev.off()
###  histograms speed surgery - all ------

 # Field only 
hist1.A<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s) & data$Test_performance=="Field"),], x = "swim_speed_MEAN_cm_s",
   bins = 60,
   # add = "mean",
   rug = TRUE,
   # colour="black",
   fill = "Surgery",
   position="stack",
   palette = c( "black", "red", "blue"),
   ylab="Count",
   # legend.title = "",
   legend="none",
   xlab="Swim speed (cm/s)")  # EMG, radio, NA
hist1.A


hist1.bl.A<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s) & data$Test_performance=="Field"),], x = "swim_speed_MEAN_BL_s", bins = 60,
   # add = "mean",
   rug = TRUE,
   # colour="black",
   fill = "Surgery",
   position="stack",
   palette = c("black", "orange", "darkgreen", "red", "grey", "blue"),
   # title= "Field studies",
   ylab="Count",
   # legend.title = "",
   legend="none",
   xlab="Swim speed (BL/s)") # --, acceleration, # acoustic, EMG, MagneticRing, Radio
hist1.bl.A


# Non field only 
hist1.lab.A<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s) & data$Test_performance!="Field"),], x = "swim_speed_MEAN_cm_s",
   bins = 60,
   # add = "mean",
   rug = TRUE,
   fill = "Surgery",
   # facet.by= "Sex_F_M",
   position="stack",
   palette = c("black", "orange", "magenta", "pink", "red", "blueviolet", "cyan3", "blue"),
   # colour="black",
   legend="none",
   # legend.title = "",
   # title= "NON-field studies",
   ylab="Count",
   xlab="Swim speed (cm/s)")  #  - , Acceler, Canul, Canul+flow, EMG, flow, ligation, radio
hist1.lab.A

# fix colors
# acceleration "orange"
# acoustic "darkgreen"
# EMG "red"
# Radio "blue"
# undefined "black"
# Canulation "magenta"
# canulation and floProbe "pink"
# FlowProbe "blueviolet"
# ligation "cyan3"
# magnetic ring "grey"
# heart perfusion in situ "brown"
# Optode implant "white"
hist1.bl.lab.A<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s) & data$Test_performance!="Field"),], x = "swim_speed_MEAN_BL_s", bins = 60,
   # add = "mean",
   rug = TRUE,
   # colour="black",
   fill = "Surgery",
   # facet.by= "Sex_F_M",
   position="stack",
   # legend.title = "",
   legend="none",
   palette = c("black", "orange", "magenta", "pink", "red", "blueviolet",  "cyan3", "white"),
   # title= "NON-field studies",
   ylab="Count",
   xlab="Swim speed (BL/s)") 
hist1.bl.lab.A


png("../DATA and STATS analyses/Swim_hist_bySurgery_all-nolegend.png", res = 200, width = 12, height = 12, units="in")
  grid.arrange(hist1.lab.A, hist1.bl.lab.A, hist1.A, hist1.bl.A, ncol=2, nrow=2)
dev.off()
  
  
  
### histograms speed by test ------------

# breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#A6CEE3" ,"#1F78B4", "#B2DF8A", "#33A02C" ,"#FB9A99" "#E31A1C" ,"#FDBF6F", "#FF7F00", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. gardnieri", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar")

# "Oncorhynchus"        "#A6CEE3"
# "Oncorhynchus mykiss"  "#1F78B4" 
# "Oncorhynchus gorbuscha" "#B2DF8A" 
# "Oncorhynchus keta"     "#33A02C"
# "Oncorhynchus kisutch"   "#FB9A99" 
# "Oncorhynchus nerka"   "#E31A1C"
# "Oncorhynchus tshawytscha"  "#FDBF6F" 
# "Oncorhynchus masou"  "#FF7F00"
# "Salmo salar"  "#CAB2D6"

# "O. gardnieri"
# "O. gorbuscha"
# "O. keta"
# "O. kisutch"
# "O. nerka"
# "O. tshawytscha"
# "O. masou"
# "Salmo salar"

# "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6"


# # all tests but the field 
# hist2<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s)),], x = "swim_speed_MEAN_cm_s",
#    bins = 30,
#    add = "mean",
#    rug = TRUE,
#    facet.by = "Test_performance2",
#    position = "stack",
#    fill = "azure3",
#    colour="black",
#    # title= "All studies",
#    ylab="Count",
#    xlab="Swim speed (cm/s)")
# hist2
# 
# hist2.BL<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s)),], x = "swim_speed_MEAN_BL_s",
#    bins = 30,
#    add = "mean",
#    rug = TRUE,
#    facet.by = "Test_performance2",
#    position = "stack",
#    fill = "azure3",
#    colour="black",
#    # title= "All studies",
#    ylab="Count",
#    xlab="Swim speed (BL/s)") 
# hist2.BL
# 
# png("../DATA and STATS analyses/Swim_hist_testAllCM_studies.png", res = 200, width = 10, height = 9, units="in")
#   grid.arrange(hist2) # in cm 
# dev.off()
# png("../DATA and STATS analyses/Swim_hist_testAllBL_studies.png", res = 200, width = 9, height = 9, units="in")
#   grid.arrange(hist2.BL) # in BL
# dev.off()
# species on data
# c("Oncorhynchus", "Oncorhynchus gardnieri", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha","Salmo salar"), values=c("#FF0000FF","#FFBF00FF", "#80FF00FF", "#00FF40FF", "#00FFFFFF", "#0040FFFF", "#8000FFFF", "#FF00BFFF")
hist3.BL<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_BL_s)),], x = "swim_speed_MEAN_BL_s", bins = 30,
   # add = "mean",
   rug = TRUE,
   facet.by = "Test_performance2",
   position = "stack",
   fill = "Species_latin",
   # colour = "black",
   # title= "All studies",
   palette = c(  "#B2DF8A","#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C", "#FDBF6F", "#CAB2D6"),
   ylab="Count",
   legend="right",
   legend.title = "",
   xlab="Swim speed (BL/s)") 
hist3.BL

# "Oncorhynchus mykiss"  "#1F78B4" 
# "Oncorhynchus gorbuscha" "#B2DF8A" 
# "Oncorhynchus keta"     "#33A02C"
# "Oncorhynchus kisutch"   "#FB9A99" 
# "Oncorhynchus nerka"   "#E31A1C"
# "Oncorhynchus tshawytscha"  "#FDBF6F" 
# "Oncorhynchus masou"  "#FF7F00"
# "Salmo salar"  "#CAB2D6"
hist3<- gghistogram(data = data[c(!is.na(data$swim_speed_MEAN_cm_s)),], x = "swim_speed_MEAN_cm_s", bins = 30,
   # add = "mean",
   rug = TRUE,
   facet.by = "Test_performance2",
   position = "stack",
   fill = "Species_latin",
   palette = c(  "#B2DF8A","#33A02C", "#FB9A99" , "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"),
   # title= "All studies",
   ylab="Count",
   legend = "right",
   legend.title = "",
   xlab="Swim speed (cm/s)") 
hist3

png("../DATA and STATS analyses/Swim_hist_bySPECIESandTest.png", res = 200, width = 10, height = 6, units="in")
  grid.arrange(hist3)
dev.off()
png("../DATA and STATS analyses/Swim_hist_BL_bySPECIESandTest.png", res = 200, width = 8, height = 6, units="in")
  grid.arrange(hist3.BL)
dev.off()

# temp of X axis?
hist4<- gghistogram(data = data, x = "Temp_test_mean", bins = 30,
   # add = "mean",
   rug = TRUE,
   facet.by = c("Test_performance2", "Sex_F_M"),
   position = "stack",
   fill = "Species_latin",
  palette = c(  "#B2DF8A","#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"),
   # title= "All studies - all data points",
   ylab="Count",
   legend.title = "",
   xlab="Mean test temperature (deg C)") 
hist4

png("../DATA and STATS analyses/Temp_hist_bySPECIESbySexbyTemo.png", res = 200, width = 10, height = 10, units="in")
  grid.arrange(hist4)
dev.off()


# size
hist5<- gghistogram(data = data[c(!is.na(data$Length_MEAN_cm)),], x = "Length_MEAN_cm", bins = 30,
   # add = "mean",
   rug = TRUE,
   position = "stack",
   fill = "Species_latin",
   # title= "All studies",
    palette = c(  "#B2DF8A","#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#FDBF6F", "#CAB2D6"),
   ylab="Count",
   legend="right",
   legend.title = "",
   facet.by = c("Sex_F_M"),
   xlab="Fish size (cm)") 
hist5

png("../DATA and STATS analyses/Size_hist_bySPECIESbySEX.png", res = 200, width = 10, height = 5, units="in")
  grid.arrange(hist5)
dev.off()




### Stats -----------
# anovas / mixed models

### All parameters:

## Fish Condition cannot be included in mixed model because limited data - overcomplicated model
### This would be the full model
fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Swim_Conditions2 + Fish_Conditions + Surgery +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE)

## FULL MODELS
fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
fit_BL<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")


fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
fit_BL<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")

# no inter size X temp
fit_CM_nINT<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
fit_BL_nINT<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")

# - fit sea water - it had teh lowest size effect -- GOOD proceed, lower BIC | and its the same effect on swim speed when using BL s and cm s 
fit_CM_nINT.2<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
fit_BL_nINT.2<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + Test_performance2 + Indiv_group + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")


# - indiv group - it had teh lowest size effect, likely still overfitting-- GOOD proceed, lower BIC | and its the same effect on swim speed when usingBL s and cm s 
fit_CM_nINT.3<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + Test_performance2 + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
fit_BL_nINT.3<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + Test_performance2 + Species_latin +
               Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")

model.sel(fit_CM, fit_CM_nINT, fit_CM_nINT.2, fit_CM_nINT.3, rank = "BIC")
model.sel(fit_BL, fit_BL_nINT,fit_BL_nINT.2,fit_BL_nINT.3, rank = "BIC")
# anova(fit_CM_nINT.2)
# anova(fit_BL_nINT.2)

anova(fit_CM_nINT.3)
anova(fit_BL_nINT.3)
summary(fit_CM_nINT.3)
summary(fit_BL_nINT.3)

# cm/s
emm_sp_CM = emmeans(fit_CM_nINT.3, specs = pairwise ~ Species_latin)
emm_sp_CM$emmeans
emm_sp_CM$contrasts

emm_test_CM = emmeans(fit_CM_nINT.3, specs = pairwise ~ Test_performance2)
emm_test_CM$emmeans
emm_test_CM$contrasts

emm_sex_CM = emmeans(fit_CM_nINT.3, specs = pairwise ~ Sex_F_M)
emm_sex_CM$emmeans
emm_sex_CM$contrasts

# cm/s
emm_sp_BL = emmeans(fit_BL_nINT.3, specs = pairwise ~ Species_latin)
emm_sp_BL$emmeans
emm_sp_BL$contrasts

emm_test_BL = emmeans(fit_BL_nINT.3, specs = pairwise ~ Test_performance2)
emm_test_BL$emmeans
emm_test_BL$contrasts

emm_sex_BL = emmeans(fit_BL_nINT.3, specs = pairwise ~ Sex_F_M)
emm_sex_BL$emmeans
emm_sex_BL$contrasts


emm2 = emmeans(fit_sex, specs = pairwise ~ Species_latin, type="response")

# Leave this as a final model  -- anova(fit_CM_nINT.3) ; anova(fit_BL_nINT.3)



# Swim Condition
# fit_SwimCondREML<-lmer(swim_speed_MEAN_cm_s ~ Swim_Conditions2 + Species_latin + (1|Reference_number_1), data=data, REML=T)
# fit_SwimCond<-lmer(swim_speed_MEAN_cm_s ~ Swim_Conditions2 + (1|Reference_number_1) + (1|FishID), data=data, REML=F)
# summary(fit_SwimCond)
# 
# anova(fit_SwimCond)
# ranova(fit_SwimCond)
# 
# 
# # Fish Condition 
# fit_FishCond<-lmer(swim_speed_MEAN_cm_s ~ Fish_Conditions + Species_latin + (1|Reference_number_1) + (1|FishID), data=data, REML=F)
# summary(fit_FishCond)
# plot(fit_FishCond) # diagnostic plots
# anova(fit_FishCond)
# 
# # Surgery
# fit_Surgery<-lmer(swim_speed_MEAN_cm_s ~ Surgery + Species_latin + (1|Reference_number_1) + (1|FishID), data=data, REML=F)
# summary(fit_Surgery)
# plot(fit_Surgery) # diagnostic plots
# anova(fit_Surgery)


## size non linera model 
# Fishbase table in: W = a × L^b
WL_data<-data[c(which(!is.na(data$Length_MEAN_cm) & !is.na(data$Size_MEAN_kg))),]
pwr.eq <- function(a, b, L) {a * (L^b)}
pwr.eq(0.01, 3, 50)  
WL_results <- nls(Size_MEAN_kg*1000 ~ pwr.eq(a, b, Length_MEAN_cm), data =WL_data, start = list(a = 0.01, b = 3), trace = T)


predicted<-pwr.eq(as.numeric(coeffs(WL_results)[1]), as.numeric(coeffs(WL_results)[2]), seq(20,85, 1))
WL_dummy<-as.data.frame(matrix(ncol=2, nrow=(length(seq(20,85, 1)))))
WL_dummy[,1]<- predicted
WL_dummy[,2] <- seq(20,85, 1)
WL_dummy$Species_latin<-"Onchorynchus nerka"
names(WL_dummy)<-c("kg", "cm", "Species_latin")
WL_dummy<-as.data.frame(WL_dummy)

p.size<-ggplot(data=WL_data, aes(x=Size_MEAN_kg, y=Length_MEAN_cm,
                              fill=Species_latin
                              ))+
 scale_fill_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6"), labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                    xmax=Size_MEAN_kg+Size_error_kg), alpha=0.5)+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error), alpha=0.5)+
  geom_point(size=2.5, alpha=0.5, pch=21)+
  geom_line(data=WL_dummy, aes(y=cm, x=kg/1000), color="black", lty=1 )

 ggformat(p.size, print=TRUE, y_title = "Body length (length, cm)", x_title = "Body size (weight, kg)", title ="")
p.size<-p.size+theme(legend.position="top", legend.title = element_blank())

summary(WL_results)


# p.size
png("../DATA and STATS analyses/Weight-length_bySPECIES.png", res = 200, width = 6, height = 6, units="in")
  grid.arrange( p.size)
dev.off()



# Temp non linear trends
# using swim speed in BL/s
# using swim speed in cm/s
## simple Orodnary regression 
data_t<-data[!is.na(data$Temp_test_mean),]
temp_lm<-lm(swim_speed_MEAN_cm_s ~ poly(Temp_test_mean, degree=3), data=data_t)
predict(temp_lm, newdata =  data_t)

plot(data_t$Temp_test_mean, data_t$swim_speed_MEAN_cm_s, ylim=c(0,400))
points(data_t$Temp_test_mean, predict(temp_lm, newdata =  data_t), col="red", cex=1)

# gam
z <- gam(swim_speed_MEAN_cm_s ~ s(Temp_test_mean), data = data, family=gaussian(link = "identity"), method = "GCV.Cp")
summary(z)   # regression coefficients
fitted(z)    # predicted (fitted) values on original scale
predict(z)   # predicted values on the logit (or log, etc) scale
plot(z)      # the spline on the logit (or log, etc) scale
plot(residuals(z, type = "response"))  # residuals
plot(z, se = TRUE, seWithMean=TRUE) # plus or minus 2 standard error
plot(z, se = 1, seWithMean=TRUE)    # plus or minus 1 standard error

# mixed model with poly term
data_tCM<-data[c(!is.na(data$Temp_test_mean) & !is.na(data$swim_speed_MEAN_cm_s)),]
data_tBL<-data[c(!is.na(data$Temp_test_mean) & !is.na(data$swim_speed_MEAN_BL_s)),]

fit_temp3<-lmer(swim_speed_MEAN_cm_s ~ poly(Temp_test_mean, degree=3) + Sex_F_M + Test_performance2 + Species_latin + Size_MEAN_kg + (1|Reference_number_1) + (1|FishID), data=data_tCM, REML=F)
fit_temp2<-lmer(swim_speed_MEAN_cm_s ~ poly(Temp_test_mean, degree=2) + Sex_F_M + Test_performance2 + Species_latin + Size_MEAN_kg + (1|Reference_number_1) + (1|FishID), data=data_tCM, REML=F)
fit_temp0<-lmer(swim_speed_MEAN_cm_s ~ Temp_test_mean + Sex_F_M + Test_performance2 + Size_MEAN_kg + Species_latin + (1|Reference_number_1) + (1|FishID), data=data_tCM, REML=F)

fit_temp3BL<-lmer(swim_speed_MEAN_BL_s ~ poly(Temp_test_mean, degree=3) + Sex_F_M + Test_performance2 + Species_latin + Size_MEAN_kg + (1|Reference_number_1) + (1|FishID), data=data_tBL, REML=F)
fit_temp2BL<-lmer(swim_speed_MEAN_BL_s ~ poly(Temp_test_mean, degree=2) + Sex_F_M + Test_performance2 + Species_latin + Size_MEAN_kg + (1|Reference_number_1) + (1|FishID), data=data_tBL, REML=F)
fit_temp0BL<-lmer(swim_speed_MEAN_BL_s ~ Temp_test_mean + Sex_F_M + Test_performance2 + Species_latin+ Size_MEAN_kg + (1|Reference_number_1) + (1|FishID), data=data_tBL, REML=F)

model.sel(fit_temp3, fit_temp2, fit_temp0, rank = "BIC")
model.sel(fit_temp3BL, fit_temp2BL, fit_temp0BL, rank = "BIC")

plot(data_tCM$Temp_test_mean, data_tCM$swim_speed_MEAN_cm_s, ylim=c(0,400))
points(data_tCM$Temp_test_mean, predict(fit_temp3, newdata=data_tCM), col="red", cex=1)

plot_model(fit_temp3, type = "pred", terms="Temp_test_mean [all]")
plot_model(fit_temp2, type = "pred", terms="Temp_test_mean [all]")
plot_model(fit_temp0, type = "pred", terms="Temp_test_mean [all]")

poly.plot<-sjp.poly(data_tCM$swim_speed_MEAN_cm_s, data_tCM$Temp_test_mean, 1:4, show.scatter = FALSE, show.loess = TRUE)
ggformat(poly.plot, title = "", y_title = "Swim speed (cm/s)", x_title = "Temperature")
# poly.plot<- poly.plot + 
  # ylim(0,400)+
  # geom_point(data=data, aes(Temp_test_mean, swim_speed_MEAN_cm_s, colour=Species_latin), size=2, alpha=0.3, colour="black", pch=21) +
  # theme (legend.title = element_blank())+
  
poly.plot
png("../DATA and STATS analyses/Temp_POLY-PLOT.png", res = 200, width = 6.6, height = 4.5, units="in")
  grid.arrange(poly.plot)
dev.off()

# poly.plot2<-
poly.plot2<-poly.plot +  
  geom_point(data=data, aes(Temp_test_mean, swim_speed_MEAN_cm_s, colour=Species_latin), size=2, alpha=0.3,  pch=19)+
  theme (legend.title = element_blank())+
   ylim(0,400)+
  scale_colour_manual(breaks = c("Oncorhynchus", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"), values=c("#B2DF8A" ,"#33A02C", "#FB9A99" ,"#FF7F00", "#1F78B4","#E31A1C" , "#A6CEE3", "#FDBF6F", "#CAB2D6", "red",  "blue", "green3", "purple"))
poly.plot2


png("../DATA and STATS analyses/Temp_POLY-PLOT.png", res = 200, width = 6.6, height = 4.5, units="in")
  grid.arrange(poly.plot2)
dev.off()




## Ucrit only the repeat 
ucrit<-data[(data$Test_performance2=="Ucrit"),]

ucrit80percent<-ucrit %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(mean_ucrit90Perc= mean(swim_speed_MEAN_cm_s, na.rm=TRUE)*0.80)

# when anaerobic starts (Geist 2003)
mean(ucrit$swim_speed_MEAN_cm_s, na.rm=TRUE)*0.80

fit_ucritCM<-lmer(swim_speed_MEAN_cm_s ~ Test_performance + Sex_F_M + Species_latin + Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=ucrit, REML=FALSE)
fit_ucritBL<-lmer(swim_speed_MEAN_BL_s ~ Test_performance + Sex_F_M + Species_latin + Size_MEAN_kg + Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=ucrit, REML=FALSE)
summary(fit_ucritCM)
summary(fit_ucritBL)

anova(fit_ucritCM)
anova(fit_ucritBL)
## Summary including coefficient table with p-values for t-statistics using
## Satterthwaite's method for denominator degrees of freedom:
emm_sp_ucritCM = emmeans(fit_ucritCM, specs = pairwise ~ Test_performance)
emm_sp_ucritCM$emmeans
emm_sp_ucritCM$contrasts


emm_sp_ucritBL = emmeans(fit_ucritBL, specs = pairwise ~ Test_performance)
emm_sp_ucritBL$emmeans
emm_sp_ucritBL$contrasts

# sex
emm_sp_ucritBL = emmeans(fit_ucritBL, specs = pairwise ~ Sex_F_M)
emm_sp_ucritBL$emmeans
emm_sp_ucritBL$contrasts


# subgrouping and explorng --------- 
# passage success
# pass<-data[which(data$Condition_category=="successful"),]
# no_pass<-data[which(data$Condition_category=="unsuccessful"),]
# 
# nrow(pass[!is.na(pass$swim_speed_MEAN_cm_s),])
# p15.cm<-ggplot(data = pass[!is.na(pass$swim_speed_MEAN_cm_s),], aes(swim_speed_MEAN_cm_s, colour=Species_latin, fill=Species_latin)) +
#    geom_histogram(binwidth = 10, alpha=0.5, position = 'stack')+
#   annotate(geom = "text", y = 3.1, x=200, label = "paste(Successful~fish,~N~\" = 16 \")", parse=TRUE)
# # facet_grid(Species_latin~.)
# ggformat(p15.cm, print=TRUE, y_title = "Frequency", x_title = "Swim speed (cm/s)", title ="")
# p15.cm<-p15.cm+theme(legend.position="top", legend.title = element_blank())
# 
# nrow(no_pass[!is.na(no_pass$swim_speed_MEAN_cm_s),])
# p16.cm<-ggplot(data = no_pass[!is.na(no_pass$swim_speed_MEAN_cm_s),], aes(swim_speed_MEAN_cm_s, colour=Species_latin, fill=Species_latin)) +
#    geom_histogram(binwidth = 10, alpha=0.5, position = 'stack')+
#   annotate(geom = "text", y = 2.9, x=200, label = "paste(Unsuccessful~fish,~N~\" = 16 \")", parse=TRUE)
#    # facet_grid(Species_latin~.)
# ggformat(p16.cm, print=TRUE, y_title = "Frequency", x_title = "Swim speed (cm/s)", title ="")
# p16.cm<-p16.cm+theme(legend.position="top", legend.title = element_blank())
# 
# nrow(pass[!is.na(pass$swim_speed_MEAN_BL_s),])
# p15.bl<-ggplot(data = pass[!is.na(pass$swim_speed_MEAN_BL_s),], aes(swim_speed_MEAN_BL_s, colour=Species_latin, fill=Species_latin)) +
#    geom_histogram(binwidth = 0.5, alpha=0.5, position = 'stack')+
#   annotate(geom = "text", y = 4, x=5, label = "paste(Successful~fish,~N~\" = 24 \")", parse=TRUE)
# # facet_grid(Species_latin~.)
# ggformat(p15.bl, print=TRUE, y_title = "Frequency", x_title = "Swim speed (BL/s)", title ="")
# p15.bl<-p15.bl+theme(legend.position="top", legend.title = element_blank())
# 
# nrow(no_pass[!is.na(no_pass$swim_speed_MEAN_BL_s),])
# p16.bl<-ggplot(data = no_pass[!is.na(no_pass$swim_speed_MEAN_BL_s),], aes(swim_speed_MEAN_BL_s, colour=Species_latin, fill=Species_latin)) +
#    geom_histogram(binwidth = 0.5, alpha=0.5, position = 'stack')+
#   annotate(geom = "text", y = 10, x=7, label = "paste(Unsuccessful~fish,~N~\" = 25 \")", parse=TRUE)
#    # facet_grid(Species_latin~.)
# ggformat(p16.bl, print=TRUE, y_title = "Frequency", x_title = "Swim speed (BL/s)", title ="")
# p16.bl<-p16.bl+theme(legend.position="top", legend.title = element_blank())
# 
# 
# png("Plot_passageSuccess.png", res = 200, width = 10, height = 10, units="in")
#   grid.arrange(p15.cm, p16.cm, p15.bl, p16.bl,ncol=2, nrow=2)
# dev.off()

# png("Plot_NOTpassingfishSwim.png", res = 200, width = 5, height = 5, units="in")
#   grid.arrange(p16)
# dev.off()


## ARCHIVE -----------
# add error bars
# p1.bl<-ggplot(data=data.BLswim, aes(y=swim_speed_MEAN_BL_s, x=Length_cm_estimated, colour=Species_latin, pch=Indiv_group))+
#   scale_shape_manual(values=c(19, 21))+
#   geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_speed_error_BL_s,
#                     ymax=swim_speed_MEAN_BL_s+swim_speed_error_BL_s))+
#   geom_errorbarh(aes(xmin=Length_cm_estimated-Length_error,
#                     xmax=Length_cm_estimated+Length_error))+
#   geom_point(size=3)+
#   facet_grid(Test_performance2~., scales="free")
# ggformat(p1.bl, print=TRUE, y_title = "Swim speed (mean, BL/s)", x_title = "Body size (length, cm)", title ="")
# p1.bl<-p1.bl+theme(legend.position = "none")
# 
# p2.bl<-ggplot(data=data[!is.na(data$swim_speed_MEAN_BL_s),], aes(y=swim_speed_MEAN_BL_s, x=Size_kg_estimated, colour=Species_latin, pch=Indiv_group))+
#   geom_point(size=3)+
#   scale_shape_manual(values=c(19, 21))+
#   geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_speed_error_BL_s,
#                     ymax=swim_speed_MEAN_BL_s+swim_speed_error_BL_s))+
#   geom_errorbarh(aes(xmin=Size_kg_estimated-Size_error_kg,
#                     xmax=Size_kg_estimated+Size_error_kg))+
#   facet_grid(Test_performance2~.)
# ggformat(p2.bl, print=TRUE, y_title = "Swim speed (mean, BL/s)", x_title = "Body size (weight, kg)", title ="")


# 
# p2.cm<-ggplot(data=data[!is.na(data$swim_speed_MEAN_cm_s),], aes(y=swim_speed_MEAN_cm_s, x=Size_kg_estimated, colour=Species_latin, pch=Indiv_group))+
#   geom_point(size=3)+
#   scale_shape_manual(values=c(19, 21))+
#   geom_errorbar(aes(ymin=swim_speed_MEAN_cm_s-swim_speed_error_cm_s,
#                     ymax=swim_speed_MEAN_cm_s+swim_speed_error_cm_s))+
#   geom_errorbarh(aes(xmin=Size_kg_estimated-Size_error_kg,
#                     xmax=Size_kg_estimated+Size_error_kg))+
#   facet_grid(Test_performance2~.)
# ggformat(p2.cm, print=TRUE, y_title = "Swim speed (mean, BL/s)", x_title = "Body size (weight, kg)", title ="")
# 


# p4<-ggplot(data = data[!is.na(data$swim_speed_MEAN_cm_s),], aes(swim_speed_MEAN_cm_s, fill=Surgery)) +
#    # geom_histogram(binwidth = 10, alpha=0.5, colour="black",  position = 'identity')+
#    geom_dotplot(stackgroups = TRUE, binwidth = 5, method="histodot", dotsize=2, stackratio = 0.9)+
#    # facet_grid(Species_latin~., scales="free")
#    # theme(legend.position="top")
# ggformat(p4, print=TRUE, y_title = "Density", x_title = "Swim speed (cm/s)", title ="")
# # p4<-p4+theme(legend.position="top", legend.title = element_blank())
# p4

