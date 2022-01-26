
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

# view(data)
data<-as.data.frame(data.all[1])

# *******************************************
# how many are estimated? what is estimated:

length(data$SWIM_cms_source[which(data$SWIM_cms_source=="estimated")])
length(data$SWIM_cms_source[which(data$SWIM_cms_source=="reported")])

data$length_speed_source<-paste("CM=", data$Length_cm_value_source, "-CM.S=", data$SWIM_cms_source,sep="")

# length estimated, swim speed estimated
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

# rep2<-data[which(is.na(data$Length_cm_value_source) &
#                                    is.na(data$swim_speed_MEAN_cm_s)& 
#                                    !is.na(data$SWIM_cms)), 
#                            c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

total<-data[which(!is.na(data$SWIM_cms)), 
     c("SWIM_cms", "Size_MEAN_kg", "Length_MEAN_cm", "LENGTH_cm", "swim_speed_MEAN_BL_s", "swim_speed_MEAN_cm_s", "Length_cm_value_source" )]

nrow(total) == nrow(est)+nrow(est2)+nrow(rep)

# *******************************************





# studies 89 and 9 are too much off in swim estimate performace
# data.test[c(data.test$Reference_number_1==89 | data.test$Reference_number_1==9), ]
ggplot(data[c(data$Reference_number_1==89 | data$Reference_number_1==9), ],
       aes(y = SWIM_cms-swim_speed_MEAN_cm_s, x = LENGTH_cm, label =Reference_number_1 , color = Temp_test_mean))+
  geom_point()

ggplot(data[which(!is.na(data$SWIM_cms)),])+
  geom_histogram(mapping = aes(SWIM_cms, fill =length_speed_source ))+
  # geom_histogram(mapping = aes(SWIM_cms-swim_speed_MEAN_cm_s), fill = "green3")+
  facet_grid(.~length_speed_source)+
  theme(legend.position = "top")

plot_si0<-ggplot(data[which(!is.na(data$SWIM_cms)),])+
  geom_point(mapping = aes(y = SWIM_cms, x = LENGTH_cm, fill = length_speed_source), pch=21)+
  # geom_histogram(mapping = aes(SWIM_cms-swim_speed_MEAN_cm_s), fill = "green3")+
  facet_grid(.~length_speed_source)+
  geom_text(mapping = aes(y = SWIM_cms, x = LENGTH_cm, label = Reference_number_1), check_overlap = TRUE, size=1)
ggformat(plot=plot_si0, y_title ="Swim speed, cm/s", x_title = "Body length, cm")
plot_si0<-plot_si0+theme(legend.position = "top")
plot_si0
  
plot_si1<-ggplot(data, aes(y = SWIM_cms, x = swim_speed_MEAN_cm_s, color = Temp_test_mean))+
  geom_point()+
  # facet_grid(.~Length_cm_value_source)+
  geom_abline(slope = 1, intercept = 0)+
  xlim(0,350)+
  ylim(0,350)
ggformat(plot=plot_si1, y_title ="Swim speed (est), cm/s", x_title = "Swim speed (measured), cm/s")

plot_si2<-ggplot(data[which(!is.na(data$SWIM_cms)),], aes(y = SWIM_cms, x = swim_speed_MEAN_BL_s, color = Temp_test_mean))+
  geom_point()+
  facet_grid(.~Length_cm_value_source)+
  geom_abline(slope = 50, intercept = 0)
ggformat(plot=plot_si2, y_title ="Swim speed (est), cm/s", x_title = "Swim speed BL/s")


ggplot(data)+
  geom_point(mapping = aes(y = SWIM_cms, x = LENGTH_cm, size = N_morphometrics, alpha = N_swim_speed))+
  geom_point(mapping = aes(y = swim_speed_MEAN_cm_s, x = LENGTH_cm, size = N_morphometrics, alpha = N_swim_speed), color = "blue")+
  facet_grid(.~Length_cm_value_source)

# data[which(!is.na(data$Size_kg_estimated) & is.na(data$Size_MEAN_kg)),]

# my_data <- escalc(mi = swim_speed_MEAN_cm_s, sdi =swim_speed_error_cm_s, 
#                   ni = N_morphometrics,
#                   data = data.test, measure = "MD")


# data estiamte summaries ***************************************************
# estimate fishes swimming capacity from BL/s to cm/s
data$fish_swim_cm_s_ESTIMATED<-data$swim_speed_MEAN_BL_s * data$Length_MEAN_cm # all original data from the paper
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s)),]) # added 290 data points using this analysis
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="indiv"),]) #  analysis # 209 from individual
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED) & is.na(data$swim_speed_MEAN_cm_s) & data$Indiv_group=="group"),]) #  analysis # 81 from group :( - not good estimates

data$fish_swim_cm_s_ESTIMATED_2<-data$swim_speed_MEAN_BL_s * data$Length_cm_estimated # estimated length using fishbase relationships
nrow(data[c(!is.na(data$fish_swim_cm_s_ESTIMATED_2) & is.na(data$swim_speed_MEAN_cm_s)),]) 
data[,c("fish_swim_cm_s_ESTIMATED","fish_swim_cm_s_ESTIMATED_2", "swim_speed_MEAN_cm_s", "Length_MEAN_cm","Length_cm_estimated", "Length_cm_value")] # compare how different these are

data[,c("Species_latin", "Size_MEAN_kg", "Length_MEAN_cm","Size_kg_estimated", "Length_cm_estimated", "Size_kg_value", "Length_cm_value")]

# how many studies? 
length(unique(data$Reference_number)) 
# what is the year range that these data cover?

# how many tracking studies? 
tracking<-data[c(data$Tracking=="y"), ]
lab<-data[c(data$Tracking=="n"), ]
length(unique(tracking$Reference_number)) # 24
length(unique(lab$Reference_number)) # 71

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
# data estiamte summaries ****************************************************

sum_male<-male %>% 
  group_by(Test_performance2) %>% 
  dplyr:::summarize(min_speed = min(swim_speed_MEAN_cm_s, na.rm=TRUE), max_speed = max(swim_speed_MEAN_cm_s, na.rm=TRUE), min_Relspeed = min(swim_speed_MEAN_BL_s, na.rm=TRUE), max_Relspeed = max(swim_speed_MEAN_BL_s, na.rm=TRUE))

sum_female<-female %>% 
  group_by(Test_performance2) %>% 
  dplyr:::summarize(min_speed = min(swim_speed_MEAN_cm_s, na.rm=TRUE), max_speed = max(swim_speed_MEAN_cm_s, na.rm=TRUE), min_Relspeed = min(swim_speed_MEAN_BL_s, na.rm=TRUE), max_Relspeed = max(swim_speed_MEAN_BL_s, na.rm=TRUE))



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

