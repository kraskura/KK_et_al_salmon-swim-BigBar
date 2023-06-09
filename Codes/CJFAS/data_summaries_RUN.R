# (feb 28 2023)
# 0. source data and code: ---------
source("./Codes/get_dataset.R")

# 1. import and wrangle the data -------------
data.all<-get.adult.salmonid.swim.data(
  data.file = "./Data/Files/Kraskura_salmonSwim_analysis_feb2023.csv")

# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data<-as.data.frame(unclass(data), stringsAsFactors = TRUE)

data.ttf<-data[c(data$Test_performance2=="TTF"),]
data.NO.tff<-data[!c(data$Test_performance2=="TTF" | -c(data$Species_latin=="Oncorhynchus spp.") ),]

data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]
data.cm<-data[!is.na(data$SWIM_cms),]
dataF<-as.data.frame(data.all[7])
dataLab<-as.data.frame(data.all[8])

# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))

# # to do it for some names in a vector named 'col_names'
# col_names <- c("Mortality", "Surgery", "Swim_Conditions2","Species_latin", "Blood","Recovery", "Fish_Conditions")
# data[col_names] <- lapply(data[col_names] , factor)

# *******************************************
# how many are estimated? what is estimated:

length(data$SWIM_cms_source[which(data$SWIM_cms_source=="estimated")]) # 748 (mar 2023)
length(data$SWIM_cms_source[which(data$SWIM_cms_source=="reported")]) # 2723 (may 2023)

data$length_speed_source<-paste("FL=", data$Length_cm_value_source, "-Speed=", data$SWIM_cms_source,sep="")

## 1.1. size and swim estimations ------------------------
est<-data[which(data$Length_cm_value_source == "estimated" &
                  is.na(data$Length_MEAN_cm) & 
                  is.na(data$swim_speed_MEAN_cm_s) &  # <<<<<< 
                  !is.na(data$SWIM_cms)), 
          c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

# length reported, swim speed estimated
est2<-data[which(data$Length_cm_value_source == "reported" &
                  !is.na(data$Length_MEAN_cm) & 
                  is.na(data$swim_speed_MEAN_cm_s) & # <<<<<< 
                  !is.na(data$SWIM_cms)), 
          c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

# swim speed reported, length either way
rep<-data[which(c(data$Length_cm_value_source == "reported" | data$Length_cm_value_source == "estimated") &
                  !is.na(data$swim_speed_MEAN_cm_s)& # <<<<<< 
                  !is.na(data$SWIM_cms)) , 
                c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm","swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

# swim speed reported, length not reported at all
rep2<-data[which(c(is.na(data$Length_cm_value_source)) &
                  !is.na(data$swim_speed_MEAN_cm_s)& # <<<<<< 
                  !is.na(data$SWIM_cms)) , 
          c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm","swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

# rep2<-data[which(is.na(data$Length_cm_value_source) &
#                                    is.na(data$swim_speed_MEAN_cm_s)& 
#                                    !is.na(data$SWIM_cms)), 
#                            c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

total<-data[which(!is.na(data$SWIM_cms)), 
     c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

# sanity check
nrow(total) == nrow(est)+nrow(est2)+nrow(rep)+nrow(rep2)


# 2. mini stats and numbers ------
data.cm.ucrit<-data.cm[data.cm$Test_performance2=="Ucrit" | data.cm$Test_performance2=="Umax",]
data.cm.field<-data.cm[data.cm$Test_performance2=="Field" | data.cm$Test_performance2=="Fishway",]
data.cm.field.fishway<-data.cm[data.cm$Test_performance2=="Fishway",]

length(levels(factor(data.cm.ucrit[, "Reference_number_1"]))) # 71 
length(levels(factor(data.cm.field[, "Reference_number_1"]))) # 24

# Ucrit Umax data stats 
length(levels(factor(data.cm.ucrit$Species_latin))) # 8 
nrow(data.cm.ucrit[!is.na(data.cm.ucrit$SWIM_cms),]) # 1246
summary(data.cm.ucrit$Temp_test_mean)
summary(data.cm.ucrit$Length_MEAN_cm)

summary(data.cm.ucrit[!is.na(data.cm.ucrit$SWIM_cms), "SWIM_cms"]) 
summary(data.cm.ucrit[!is.na(data.cm.ucrit$SWIM_cms), "swim_speed_MEAN_BL_s"]) 

# summary Jump studies 
data.jump<-data.cm[data.cm$Test_performance2=="Jump",]
summary(data.jump$SWIM_cms)

# what is the year range that these data cover?
summary(as.numeric(as.character(data$Year_published))) # 1956 - 2022

# N studies between 2000 and 2010?
length(levels(factor(data[c(as.numeric(as.character(data$Year_published)) >= 2000 &
               as.numeric(as.character(data$Year_published)) < 2011), "Reference_number_1"])))
# N studies between 2011 and now 
length(levels(factor(data[c(as.numeric(as.character(data$Year_published)) >= 2011), "Reference_number_1"])))


# 1. Atlantic and mykiss
length(which(data$Species_latin == "Salmo salar")) # 423 (feb 28 2023)
length(which(data$Species_latin == "Oncorhynchus mykiss")) # 324 (feb 28 2023)
# 2. N studies: # 103 (may 2023)
length(unique(data$Reference_number_1)) 
# 3. N data points 
nrow(data) # 3593 (may 2023)

# max size all fish 
summary(data$LENGTH_cm)

# get reference list to report 
data.UniqueRef<-data[!duplicated(data$Reference_number_1),c("Reference_number_1", "Reference_2")]
# paste(data.UniqueRef$Reference_number_1, ": ", data.UniqueRef$Reference_2, sep = "")

# reported % vals > 2 BL 
nrow(data.BL[!is.na(data.BL$swim_speed_MEAN_BL_s) & data.BL$swim_speed_MEAN_BL_s > 2, ])/ nrow(data.BL[!is.na(data.BL$swim_speed_MEAN_BL_s), ])

# reported: but XX %  (XX / YY) of compiled Ucrit swim performance measures of sockeye salmon
nrow(data.cm[c(!is.na(data.cm$SWIM_cms) & data.cm$SWIM_cms < 200 & data.cm$Species_latin == "Oncorhynchus nerka") , ])/
  nrow(data.cm[c(!is.na(data.cm$SWIM_cms) &  data.cm$Species_latin == "Oncorhynchus nerka") , ])

# reported female male and no NAs:
summary(factor(dataLab$Sex_F_M))
summary(dataLab[dataLab$Sex_F_M == "F","SWIM_cms"], na.rm = T)
summary(dataLab[dataLab$Sex_F_M == "M","SWIM_cms"], na.rm = T)

summary(factor(dataF$Sex_F_M))
summary(dataF[dataF$Sex_F_M == "F","SWIM_cms"], na.rm = T)
summary(dataF[dataF$Sex_F_M == "M","SWIM_cms"], na.rm = T)

# how many tracking studies? 
tracking<-data[c(data$Tracking=="1"), ]
lab<-data[c(data$Tracking=="0"), ]
length(unique(tracking$Reference_number)) # 24
length(unique(lab$Reference_number)) # 74

data[which(data$Test_performance!="Field" & data$Tracking=="1"), ] # Geist et al - EMG tag calibr
data[which(data$Test_performance=="Field" & data$Tracking=="0"), ] # Swim study right in the field, fish swum in near dam, but not tagged/tracked ref 53, Weaver study

# What is the absolute temperature range? 
# obtain all categorical variables 
unique(data$Test_performance)
unique(data$Swim_Conditions)
unique(data$Fish_Conditions)
unique(data$Surgery)

# different Fish conditions 
# 
# unique(data$Condition_category)
# summary(data$Condition_category)

unique(as.factor(data$Test_performance2))
summary(as.factor(data$Test_performance2))
summary(as.factor(data$Test_performance))


# reported: N from Umax, N from Ucrit
nrow(data.cm[(data.cm$Species_latin == "Oncorhynchus gorbuscha" & data.cm$Test_performance2 == "Umax" & !is.na(data.cm$Temp_test_mean)),]) # 132 (feb 28 2023)
nrow(data.cm[(data.cm$Species_latin == "Oncorhynchus gorbuscha" & data.cm$Test_performance2 == "Ucrit" & !is.na(data.cm$Temp_test_mean)),]) # 59 (feb 28 2023)
nrow(data.cm[(data.cm$Species_latin == "Oncorhynchus gorbuscha" & !is.na(data.cm$Temp_test_mean)),]) # 215 (feb 28 2023)

# n populations sockeye
summary(factor(data[(data$Species_latin == "Oncorhynchus nerka"),"population"]))

# 3. data estimate summaries  ----------
# ****************************************************

## 3.1. Recovery: in-text summary stats ---------
data_sum6<-as.data.frame(data[c(!is.na(data$SWIM_cms) & !data$Recovery=="0"),] %>% 
                           group_by(Test_performance2, Recovery) %>% 
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints
data_sum6dp<-as.data.frame(data[!is.na(data$SWIM_cms),] %>% 
                             group_by(Recovery) %>% 
                             dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                               studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints BL
data_sum6<-as.data.frame(data  %>% 
                               group_by(Recovery) %>% 
                               dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                                 studies = paste(unique(Reference_number_1), collapse = ', ')))

# ***********************************************************
## 3.2. Species: in-text & figure panels summary stats  ---- 
swim_max<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(mean_max= max(SWIM_cms, na.rm=TRUE), n = sum(!is.na(SWIM_cms)))

swim_maxBL<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(mean_max= max(swim_speed_MEAN_BL_s, na.rm=TRUE), n =sum(!is.na(swim_speed_MEAN_BL_s)))


# by species using MIN and MAX to get the full range 
# by species - USING MEANS ONLY

data_sum_species_MINMAX<-data %>%
  group_by(Species_latin, Test_performance3) %>%
  dplyr:::summarize(min_swim_speedCM = min(SWIM_cms, na.rm = TRUE),
                    max_swim_speedCM = max(SWIM_cms, na.rm = TRUE),
                    mean_swim_speedCM = mean(SWIM_cms, na.rm = TRUE),
                    min_swim_speedBL = min(swim_speed_MIN_BL_s, na.rm = TRUE),
                    max_swim_speedBL = max(swim_speed_MAX_BL_s, na.rm = TRUE),
                    mean_swim_speedBL = mean(swim_speed_MAX_BL_s, na.rm = TRUE),
                    min_LengthCM = min(LENGTH_cm, na.rm = TRUE),
                    max_LengthCM = max(LENGTH_cm, na.rm = TRUE),
                    mean_LengthCM = mean(LENGTH_cm, na.rm = TRUE),
                    min_Sizekg = min(Size_MIN_kg, na.rm = TRUE),
                    max_Sizekg = max(Size_MAX_kg, na.rm = TRUE),
                    mean_Sizekg = mean(Size_MAX_kg, na.rm = TRUE),
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE), 
                    count_temp = sum(!is.na(Temp_test_mean)), 
                    count_Lenght = sum(!is.na(LENGTH_cm)), 
                    count_swim = sum(!is.na(SWIM_cms))) %>% 
  as.data.frame()

data_sum_species_Fig6 <- data %>%
  group_by(Species_latin) %>%
  dplyr:::summarize(min_LengthCM = min(LENGTH_cm, na.rm = TRUE),
                    max_LengthCM = max(LENGTH_cm, na.rm = TRUE),
                    mean_LengthCM = mean(LENGTH_cm, na.rm = TRUE),
                    range_LengthCM = max_LengthCM - min_LengthCM,
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE), 
                    count_temp = sum(!is.na(Temp_test_mean)), 
                    count_Lenght = sum(!is.na(LENGTH_cm))) %>% 
  as.data.frame()

## TABLE A3 -----
# datapoints SWIM cm/.s estimated, or reported, usen in all figs etc
data_sum5_ALL<-as.data.frame(data[!is.na(data$SWIM_cms),] %>% 
                           group_by(Species_latin) %>% 
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', '))) 
#datapoints cm/s reported by author
data_sum5<-as.data.frame(data %>%
                           filter(!is.na(swim_speed_MEAN_cm_s)) %>%
                           group_by(Species_latin) %>%
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', '))) 

#datapoints BL reported by author 
data_sum5dpBL<-as.data.frame(data %>%
                            filter(!is.na(swim_speed_MEAN_BL_s)) %>%  
                            group_by(Species_latin) %>% 
                            dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                              studies = paste(unique(Reference_number_1), collapse = ', '))) 

data_sum5bothswim<-as.data.frame(data[c(!is.na(data$swim_speed_MEAN_BL_s) & !is.na(data$swim_speed_MEAN_cm_s)),] %>% 
                                   group_by(Species_latin) %>% 
                                   dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                                     studies = paste(unique(Reference_number_1), collapse = ', ')))


data_sum5_ALL$swim_units<-"CM_S_ALL"
data_sum5$swim_units<-"cm.s.reported"
data_sum5dpBL$swim_units<-"BL.s"
data_sum5bothswim$swim_units<-"BL.s_AND_cm.s"
refs_n<-rbind(data_sum5_ALL, data_sum5, data_sum5dpBL, data_sum5bothswim)

# merged_dsum5<-merge(merge(data_sum5dpBL,data_sum5,  by = "Species_latin", all.x = T),data_sum5bothswim,  by = "Species_latin", all.x = T)
# save for MS
write.csv(file="./ms_exports/Tables/Table_A3.csv", refs_n, row.names=FALSE)





## TABLE 1: cm/s by species, by field/fishway/ucri/umax-------------------

data_sum_species_test.cm<-data.cm %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedCM = min(SWIM_cms, na.rm = TRUE),
                    max_swim_speedCM = max(SWIM_cms, na.rm = TRUE), 
                    mean_swim_speedCM = mean(SWIM_cms, na.rm = TRUE), 
                    count_swim = sum(!is.na(SWIM_cms)),
                    min_LengthCM = min(LENGTH_cm, na.rm = TRUE),
                    max_LengthCM = max(LENGTH_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(LENGTH_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE),
                    max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  write.csv(file="./ms_exports/Tables/Table_1.csv", row.names=FALSE)


## TABLE S4: BL/s by species, by field/fishway/ucri/umax-------------------
data_sum_species_test.BL<-data.BL %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE),
                    max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE),
                    count_swimBL = sum(!is.na(swim_speed_MEAN_BL_s)),
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE),
                    max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE),
                    max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  write.csv(file="./ms_exports/Tables/Table_S4.csv", row.names=FALSE)

# data_sum_species<-data %>% 
#   group_by(Species_latin) %>% 
#   dplyr:::summarize(min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
#                     mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s), 
#                     min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
#                     mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s), 
#                     
#                     min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
#                     mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), n_LengthCM = length(Length_MEAN_cm),
#                     min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
#                     mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), n_Sizekg = length(Size_MEAN_kg),
#                     n_studies_total=length(unique(Reference_number_1)))


# as.data.frame(data_sum_species_test.BL) # reported 
# as.data.frame(data_sum_species_test.cm) # reported 


## TABLE A5  ---------
data_sumSwimCond_cms<-as.data.frame(data %>% 
                           group_by(Swim_Conditions2, Swim_Conditions) %>% 
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', '))) %>% 
  write.csv(file = "./ms_exports/Tables/Table_A5.csv")

## TABLE A4  ---------
data_sumTestCond_cms<-as.data.frame(data %>% 
                                 group_by(Test_performance2, Test_performance) %>% 
                                 dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                                   studies = paste(unique(Reference_number_1), collapse = ', '))) %>% 
  write.csv(file = "./ms_exports/Tables/Table_A4.csv")

## TABLE A7  ---------
data_sumSurg<-as.data.frame(data %>% 
                                 group_by(Surgery) %>% 
                                 dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                                   studies = paste(unique(Reference_number_1), collapse = ', '))) %>% 
  write.csv(file = "./ms_exports/Tables/Table_A7.csv")

## TABLE A6  ---------
data_sumFishCond<-as.data.frame(data %>% 
                                 group_by(Fish_Conditions) %>% 
                                 dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(SWIM_cms),") ", sep = ''),
                                                   studies = paste(unique(Reference_number_1), collapse = ', '))) %>% 
  write.csv(file = "./ms_exports/Tables/Table_A6.csv")


## TABLE S1 ------------
# by Fish condition, man subgroup - USING MEANS ONLY
data_sum_cond.cm<-data.cm %>% 
  group_by(Fish_Conditions) %>% 
  dplyr:::summarize(min_swim_speedCM = min(SWIM_cms, na.rm = TRUE),
                    max_swim_speedCM = max(SWIM_cms, na.rm = TRUE), 
                    mean_swim_speedCM = mean(SWIM_cms, na.rm = TRUE), 
                    count_swim = sum(!is.na(SWIM_cms)),
                    min_LengthCM = min(LENGTH_cm, na.rm = TRUE),
                    max_LengthCM = max(LENGTH_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(LENGTH_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE),
                    max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE))%>% 
  as.data.frame() %>% 
  write.csv(file="./ms_exports/Tables/Table_S1.csv", row.names=FALSE)

## TABLE S2 ------------
data_sum_cond.BL<-data.BL %>% 
  group_by(Fish_Conditions) %>% 
  dplyr:::summarize(min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE),
                    max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE),
                    count_swimBL = sum(!is.na(swim_speed_MEAN_BL_s)),
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE),
                    max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE),
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE),
                    max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE),
                    max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  write.csv(file="./ms_exports/Tables/Table_S2.csv", row.names=FALSE)


# Recovery stats from the qualitative perspective -------

# References -------
length(unique(data$Reference_2))
length(unique(data$Reference_number_1))
write.csv(file="./ms_exports/Tables/Appendix_B.csv", data[!duplicated(data$Reference_number_1), c("Reference_number_1", "Test_performance2", "Reference_2")], row.names=FALSE)

