
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))

# view(data)
# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])

# overall data stats:
# 1. Atlantic and mykiss
length(which(data$Species_latin == "Salmo salar")) # 253 (feb 11, 2022)
length(which(data$Species_latin == "Oncorhynchus mykiss")) #134 (feb 11 2022)
# 2. N studies:
length(unique(data$Reference_number_1))
nrow(data)


# datasets --------
# data<-data[!c(data$Test_performance2=="TTF"),]
# data.ttf<-data[c(data$Test_performance2=="TTF"),]

data$Species_latin<-as.factor(data$Species_latin)
data$Sex_F_M<-as.factor(data$Sex_F_M)
data$Temp_test_mean<-round(data$Temp_test_mean) # round temp to cluster per degree

# to do it for some names in a vector named 'col_names'
col_names <- c("Mortality", "Surgery", "Swim_Conditions2","Species_latin", "Blood","Recovery", "Fish_Conditions")
data[col_names] <- lapply(data[col_names] , factor)

# *******************************************
# how many are estimated? what is estimated:

length(data$SWIM_cms_source[which(data$SWIM_cms_source=="estimated")])
length(data$SWIM_cms_source[which(data$SWIM_cms_source=="reported")])

data$length_speed_source<-paste("CM=", data$Length_cm_value_source, "-CM.S=", data$SWIM_cms_source,sep="")

# length estimated, swim speed estimated ------------------------
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

nrow(total) == nrow(est)+nrow(est2)+nrow(rep)+nrow(rep2)


# data estiamte summaries ***************************************************
# # estimate fishes swimming capacity from BL/s to cm/s
# data$fish_swim_cm_s_ESTIMATED<-data$swim_speed_MEAN_BL_s * data$Length_MEAN_cm # all original data from the paper
# nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s)),]) # added 290 data points using this analysis
# nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="indiv"),]) #  analysis # 209 from individual
# nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="group"),]) #  analysis # 81 from group :( - not good estimates
# 
# data$fish_swim_cm_s_ESTIMATED_2<-data$swim_speed_MEAN_BL_s * data$Length_cm_estimated # estimated length using fishbase relationships
# nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED_2) & is.na(data$swim_speed_MEAN_cm_s)),]) 
# data[,c("fish_swim_cm_s_ESTIMATED","fish_swim_cm_s_ESTIMATED_2", "swim_speed_MEAN_cm_s", "Length_MEAN_cm","Length_cm_estimated", "Length_cm_value")] # compare how different these are
# 
# data[,c("Species_latin", "Size_MEAN_kg", "Length_MEAN_cm","Size_kg_estimated", "Length_cm_estimated", "Size_kg_value", "Length_cm_value")]

# how many studies? 
length(unique(data$Reference_number)) 
# what is the year range that these data cover?

# how many tracking studies? 
tracking<-data[c(data$Tracking=="1"), ]
lab<-data[c(data$Tracking=="0"), ]
length(unique(tracking$Reference_number)) # 24
length(unique(lab$Reference_number)) # 71

data[which(data$Test_performance!="Field" & data$Tracking=="1"), ] # Geist et al - EMG tag calibr
data[which(data$Test_performance=="Field" & data$Tracking=="0"), ] # Swim study right in the field, fish swum in near dam, but not tagged/tracked ref 53, Weaver study

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





# data estimate summaries  ----------
# ****************************************************
# by species
data.cm<-data[!is.na(data$swim_speed_MEAN_cm_s),]
data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]

data_sum5<-as.data.frame(data.cm %>% 
                           group_by(Species_latin) %>% 
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints BL
data_sum5dpBL<-as.data.frame(data.BL %>% 
                               group_by(Species_latin) %>% 
                               dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                                 studies = paste(unique(Reference_number_1), collapse = ', ')))

data_sum5bothswim<-as.data.frame(data[c(!is.na(data$swim_speed_MEAN_BL_s) & !is.na(data$swim_speed_MEAN_cm_s)),] %>% 
                                   group_by(Species_latin) %>% 
                                   dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                                     studies = paste(unique(Reference_number_1), collapse = ', ')))
data_sum5$swim_units<-"cm.s"
data_sum5dpBL$swim_units<-"BL.s"
data_sum5bothswim$swim_units<-"BL.s_cm.s"
rbind(data_sum5, data_sum5bothswim, data_sum5dpBL)
merged_dsum5<-merge(merge(data_sum5dpBL,data_sum5,  by = "Species_latin", all.x = T),data_sum5bothswim,  by = "Species_latin", all.x = T)
# save for MS
write.csv(file="/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/SUPL_table1_summary.csv", merged_dsum5, row.names=FALSE)


# recovery studies, data points 
# the info here has been updated in late june 2020, went through all studies to see which studies report address recovery 
# ***
data_sum6<-as.data.frame(data[c(!is.na(data$swim_speed_MEAN_cm_s) & !data$Recovery=="0"),] %>% 
                           group_by(Test_performance2, Recovery) %>% 
                           dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                             studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints
data_sum6dp<-as.data.frame(data[!is.na(data$swim_speed_MEAN_cm_s),] %>% 
                             group_by(Recovery) %>% 
                             dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                               studies = paste(unique(Reference_number_1), collapse = ', ')))
#datapoints BL
data_sum6<-as.data.frame(data  %>% 
                               group_by(Recovery) %>% 
                               dplyr:::summarize(n_studies_ref_dp=paste(length(unique(Reference_number_1)), " (", length(swim_speed_MEAN_cm_s),") ", sep = ''),
                                                 studies = paste(unique(Reference_number_1), collapse = ', ')))


# max speed table species ---- 
swim_max<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(mean_max= max(swim_speed_MEAN_cm_s, na.rm=TRUE))
swim_maxBL<-data %>% 
  group_by(Species_latin) %>% 
  dplyr:::summarise(mean_max= max(swim_speed_MEAN_BL_s, na.rm=TRUE))


### by species using MIN and MAX to get the full range 
# by species - USING MEANS ONLY
# data_sum_species_test_MINMAX<-data %>% 
#   group_by(Species_latin, Test_performance2) %>% 
#   dplyr:::summarize(min_swim_speedCM = min(swim_speed_MIN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MAX_cm_s, na.rm = TRUE), 
#                     min_swim_speedBL = min(swim_speed_MIN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MAX_BL_s, na.rm = TRUE), 
#                     min_LengthCM = min(Length_MIN_cm, na.rm = TRUE), max_LengthCM = max(Length_MAX_cm, na.rm = TRUE), 
#                     min_Sizekg = min(Size_MIN_kg, na.rm = TRUE), max_Sizekg = max(Size_MAX_kg, na.rm = TRUE))
# 
# data_sum_species_MINMAX<-data %>% 
#   group_by(Species_latin) %>% 
#   dplyr:::summarize(min_swim_speedCM = min(swim_speed_MIN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MAX_cm_s, na.rm = TRUE), 
#                     min_swim_speedBL = min(swim_speed_MIN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MAX_BL_s, na.rm = TRUE), 
#                     min_LengthCM = min(Length_MIN_cm, na.rm = TRUE), max_LengthCM = max(Length_MAX_cm, na.rm = TRUE), 
#                     min_Sizekg = min(Size_MIN_kg, na.rm = TRUE), max_Sizekg = max(Size_MAX_kg, na.rm = TRUE))


# by species - USING MEANS ONLY
data_sum_species_test.cm<-data.cm %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedCM = min(swim_speed_MEAN_cm_s, na.rm = TRUE), max_swim_speedCM = max(swim_speed_MEAN_cm_s, na.rm = TRUE), 
                    mean_swim_speedCM = mean(swim_speed_MEAN_cm_s, na.rm = TRUE), n_swim_speedCM = length(swim_speed_MEAN_cm_s),
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE), max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE))

# add temperature

data_sum_species_test.BL<-data.BL %>% 
  group_by(Species_latin, Test_performance2) %>% 
  dplyr:::summarize(min_swim_speedBL = min(swim_speed_MEAN_BL_s, na.rm = TRUE), max_swim_speedBL = max(swim_speed_MEAN_BL_s, na.rm = TRUE), 
                    mean_swim_speedBL = mean(swim_speed_MEAN_BL_s, na.rm = TRUE), n_swim_speedBL = length(swim_speed_MEAN_BL_s), 
                    min_LengthCM = min(Length_MEAN_cm, na.rm = TRUE), max_LengthCM = max(Length_MEAN_cm, na.rm = TRUE), 
                    mean_LengthCM = mean(Length_MEAN_cm, na.rm = TRUE), 
                    min_Sizekg = min(Size_MEAN_kg, na.rm = TRUE), max_Sizekg = max(Size_MEAN_kg, na.rm = TRUE), 
                    mean_Sizekg = mean(Size_MEAN_kg, na.rm = TRUE), 
                    min_Temp = min(Temp_test_mean, na.rm = TRUE), max_Temp = max(Temp_test_mean, na.rm = TRUE), 
                    mean_Temp = mean(Temp_test_mean, na.rm = TRUE))


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


as.data.frame(data_sum_species_test.BL) # reported 
as.data.frame(data_sum_species_test.cm) # reported 
write.csv(file="/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/Table2_swim_BLsummary.csv",
          data_sum_species_test.BL, row.names=FALSE)
write.csv(file="/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/Table2_swim_cmsummary.csv",
          data_sum_species_test.cm, row.names=FALSE)


# 












# Additional fish condition datasets -----------


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

# ***********************************************************

