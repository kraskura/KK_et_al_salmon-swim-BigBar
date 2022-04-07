
# Tasks, brainstorming:
  # EJE: what happens when you only present data from fish tested in their optimal temperature range? 
  # EJE: need to be careful when pooling the data 
  # DAP: for those projects that swim fish at different temperatures you could set the highest performance as 1 or 100%, and scale all the rest of that value (most of the noise we are seeing probably comes from different experimental approachs/tests etc) 
  # DAP: could also report that X% of experiments indicated a clear size effect 
# *********************************

# Import libraries, source data. -----------
library(ggsci)
library(ggridges)
library(cowplot)
library(tidyverse)
library(dplyr)
library(broom)
detach(package:plyr)
library(merTools) # for CI of lmer models 



source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

# view(data)
# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data.ttf<-data[c(data$Test_performance2=="TTF"),]

# datasets --------
data<-data[!c(data$Test_performance2=="TTF" | -c(data$Species_latin=="Oncorhynchus spp.") ),]
data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]
data.cm<-data[!is.na(data$SWIM_cms),]

# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))
dataF<-as.data.frame(data.all[7])

# estimated anaerobic > 2 BL/s
data.cm$anaerob<-0
for(i in 1:nrow(data.cm)){
  if(!is.na(data.cm$LENGTH_cm[i])){
    if((data.cm$SWIM_cms[i] / data.cm$LENGTH_cm[i]) >= 2){
      data.cm$anaerob[i]<- 1
      print((data.cm$SWIM_cms[i] / data.cm$LENGTH_cm[i]))
    }
  }
}

data.BL$anaerob<-0
data.BL$anaerob<-0
data.BL[c(data.BL$swim_speed_MEAN_BL_s>=2),"anaerob"] <- 1
data.cm$anaerob<-as.factor(data.cm$anaerob)
data.BL$anaerob<-as.factor(data.BL$anaerob)

data.BL.pink<-data.BL[data.BL$Species_latin == "Oncorhynchus gorbuscha",]
data.cm.pink<-data.cm[data.cm$Species_latin == "Oncorhynchus gorbuscha",]

data.BL.soc<-data.BL[data.BL$Species_latin == "Oncorhynchus nerka",]
data.cm.soc<-data.cm[data.cm$Species_latin == "Oncorhynchus nerka",]

data.BL.coho<-data.BL[data.BL$Species_latin == "Oncorhynchus kisutch",]
data.cm.coho<-data.cm[data.cm$Species_latin == "Oncorhynchus kisutch",]

data.BL.sal<-data.BL[data.BL$Species_latin == "Salmo salar",]
data.cm.sal<-data.cm[data.cm$Species_latin == "Salmo salar",]


# reported only cms
data.cm.rep<-data.cm[data.cm$SWIM_cms_source == "reported",]
data.cm.rep$xi<-1:nrow(data.cm.rep)
data.cm.rep<-data.cm.rep %>% 
  mutate(xi = reorder(xi, -SWIM_cms))
data.cm.rep$xi<-as.numeric(data.cm.rep$xi)
data.cm.rep$Species_latin2<-as.character(data.cm.rep$Species_latin)
data.cm.rep$Species_latin2[which(data.cm.rep$Species_latin == "Oncorhynchus masou" | data.cm.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.cm.rep$Species_latin2<-factor(data.cm.rep$Species_latin2)

data.BL.rep<-data.BL[!is.na(data.BL$swim_speed_MEAN_BL_s),]
data.BL.rep$xi<-1:nrow(data.BL.rep)
data.BL.rep<-data.BL.rep %>% 
  mutate(xi = reorder(xi, -swim_speed_MEAN_BL_s))
data.BL.rep$xi<-as.numeric(data.BL.rep$xi)
data.BL.rep$Species_latin2<-as.character(data.BL.rep$Species_latin)
data.BL.rep$Species_latin2[which(data.BL.rep$Species_latin == "Oncorhynchus masou" | data.BL.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.BL.rep$Species_latin2<-factor(data.BL.rep$Species_latin2)
data.cm$Reference_number_1<-factor(data.cm$Reference_number_1)


# normalize by species, test performance, and reference number
data.cm$test_sp_ref<-factor(paste(data.cm$Species_latin, data.cm$Test_performance2,data.cm$Reference_number_1, sep="-"))
data.cm.split.sp.t.st<-split(data.cm, data.cm$test_sp_ref)
length(data.cm.split.sp.t.st) # 106

for(i in 1:length(data.cm.split.sp.t.st)){
  data.cm0<-as.data.frame(data.cm.split.sp.t.st[[i]])
  colnames(data.cm0)<-colnames(data.cm)
  
  data.cm0.100<-max(data.cm0$SWIM_cms)
  data.cm0.0<-0
  data.cm0$SWIM_cms.prop0<-(data.cm0$SWIM_cms / ((data.cm0.100 - data.cm0.0)/100))
  
  data.cm0.100<-max(data.cm0$SWIM_cms)
  data.cm0.min<-min(data.cm0$SWIM_cms)
  data.cm0$SWIM_cms.propMIN<-(data.cm0$SWIM_cms - data.cm0.min) / ((data.cm0.100 - data.cm0.min)) * 100 
  
  if(i ==1) {
    data.cmTNorm<-data.cm0
  }else{
    data.cmTNorm<-rbind(data.cmTNorm, data.cm0)
  }
}

# BL
data.BL$test_sp_ref<-factor(paste(data.BL$Species_latin, data.BL$Test_performance2,data.BL$Reference_number_1, sep="-"))
data.BL.split.sp.t.st<-split(data.BL, data.BL$test_sp_ref)
length(data.BL.split.sp.t.st) # 106

for(i in 1:length(data.BL.split.sp.t.st)){
  data.BL0<-as.data.frame(data.BL.split.sp.t.st[[i]])
  colnames(data.BL0)<-colnames(data.BL)
  
  data.BL0.100<-max(data.BL0$swim_speed_MEAN_BL_s)
  data.BL0.0<-0
  data.BL0$swim_speed_MEAN_BL_s.prop0<-(data.BL0$swim_speed_MEAN_BL_s / ((data.BL0.100 - data.BL0.0)/100))
  
  data.BL0.100<-max(data.BL0$swim_speed_MEAN_BL_s)
  data.BL0.min<-min(data.BL0$swim_speed_MEAN_BL_s)
  data.BL0$swim_speed_MEAN_BL_s.propMIN<-(data.BL0$swim_speed_MEAN_BL_s - data.BL0.min) / ((data.BL0.100 - data.BL0.min)) * 100 
  
  if(i ==1) {
    data.BLTNorm<-data.BL0
  }else{
    data.BLTNorm<-rbind(data.BLTNorm, data.BL0)
  }
}

data.cmTNorm
data.BLTNorm

# now use only studies that have swum fish across at least 3 C differential temperatures
# and three swim performance tests:
# 1) Ucrit, 2) Umax, 3) Field
d.cm.t<-data.cmTNorm %>% 
  dplyr:::group_by(Reference_number_1, Test_performance2, Species_latin) %>% 
  summarize(delta_temp = max(Temp_test_mean, na.rm = T) - min(Temp_test_mean, na.rm = T), n = length(Temp_test_mean)) %>% 
  as.data.frame()
# view(d.cm.t )

d.cm.t<-d.cm.t[d.cm.t$Test_performance2 == "Ucrit" , ]
d.cm.t.use<-d.cm.t[c(d.cm.t$delta_temp >= 3), ]
data.cmTNorm.use<-data.cmTNorm[0,]

data.cmTNorm$useTNorm<-0
for(i in 1:nrow(data.cmTNorm)){
  if(as.character(data.cmTNorm$Reference_number_1[i]) =="3"|
     # as.character(data.cmTNorm$Reference_number_1[i]) =="5"| # field 
     as.character(data.cmTNorm$Reference_number_1[i]) =="9"| 
     as.character(data.cmTNorm$Reference_number_1[i]) =="12"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="13"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="14"|
     # as.character(data.cmTNorm$Reference_number_1[i]) =="24"| # umax
     as.character(data.cmTNorm$Reference_number_1[i]) =="26"|
     # as.character(data.cmTNorm$Reference_number_1[i]) =="30"| # field
     as.character(data.cmTNorm$Reference_number_1[i]) =="33"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="37"|
     # as.character(data.cmTNorm$Reference_number_1[i]) =="45"| # umax
     as.character(data.cmTNorm$Reference_number_1[i]) =="57"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="68"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="69"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="76"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="85"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="87"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="89"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="92"|
     as.character(data.cmTNorm$Reference_number_1[i]) =="96"){
    data.cmTNorm$useTNorm[i]<-1
  }
}

d.BL.t<-data.BLTNorm %>% 
  dplyr:::group_by(Reference_number_1, Test_performance2, Species_latin) %>% 
  summarize(delta_temp = max(Temp_test_mean, na.rm = T) - min(Temp_test_mean, na.rm = T), n = length(Temp_test_mean)) %>% 
  as.data.frame()
# view(d.BL.t )

d.BL.t<-d.BL.t[d.BL.t$Test_performance2 == "Ucrit" , ]
d.BL.t.use<-d.BL.t[c(d.BL.t$delta_temp >= 3), ]
data.BLTNorm.use<-data.BLTNorm[0,]

data.BLTNorm$useTNorm<-0
for(i in 1:nrow(data.BLTNorm)){
  if(as.character(data.BLTNorm$Reference_number_1[i]) =="3"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="9"| 
     as.character(data.BLTNorm$Reference_number_1[i]) =="12"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="13"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="14"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="26"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="68"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="89"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="92"|
     as.character(data.BLTNorm$Reference_number_1[i]) =="96"){
    data.BLTNorm$useTNorm[i]<-1
  }
}

# length(unique(data.cmTNorm$useTNorm))
# unique(d.cm.t.use)
data.BLTNorm.use<-data.BLTNorm[data.BLTNorm$useTNorm==1,]
data.cmTNorm.use<-data.cmTNorm[data.cmTNorm$useTNorm==1,]










# SIZE: stats, modelling ------------
# the regression datasets:
data.cm.ucrit<-data.cm[data.cm$Test_performance2=="Ucrit" | data.cm$Test_performance2=="Umax",]
data.cm.field<-data.cm[data.cm$Test_performance2=="Field",]
data.cm.field.l<-data.cm[data.cm$Test_performance2=="Field" & data.cm$SWIM_cms < 250,]
data.cm.field.h<-data.cm[data.cm$Test_performance2=="Field" & data.cm$SWIM_cms >= 250,]
data.cm.jump<-data.cm[data.cm$Test_performance2=="Jump",]

size.sum.field<-data.cm.field %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))
size.sum.ucrit<-data.cm.ucrit %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

size.ucrit<-data.cm %>%
  dplyr::group_by(Species_latin, Test_performance2) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))


# *********** START ***************************************
# mixed models, species random eintercept
# ******************************************************
mod.size.ucrit<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.ucrit, na.action = na.exclude)
mod.size.field<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field, na.action = na.exclude)
mod.size.field.l<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.l, na.action = na.exclude)
mod.size.field.h<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.h, na.action = na.exclude)

# fail to converge or all singular fits  ---> 
# mod.size.ucrit.sp<-lmer(SWIM_cms~ LENGTH_cm + (LENGTH_cm|Species_latin), data.cm.ucirt)
# mod.size.field.sp<-lmer(SWIM_cms~ LENGTH_cm + (LENGTH_cm|Species_latin), data.cm.field)
# mod.size.field.l.sp<-lmer(SWIM_cms~ LENGTH_cm + (LENGTH_cm|Species_latin), data.cm.field.l)
# mod.size.field.h.sp<-lmer(SWIM_cms~ LENGTH_cm + (LENGTH_cm|Species_latin), data.cm.field.h)

mod.size.jump<-lm(SWIM_cms~ LENGTH_cm , data.cm.jump)

# ******************************************************
# *********** END *************************************





# *********** START ***************************************
# Field all swim speeds
# ******************************************************
data.cm.field.sp<-data.cm.field[!(data.cm.field$Species_latin == "Oncorhynchus masou" |
                               data.cm.field$Species_latin == "Salmo salar" |
                                 data.cm.field$Species_latin == "Oncorhynchus kisutch" ),]
data.cm.field.sp$Species_latin<-factor(data.cm.field.sp$Species_latin)

# species specific fits, CI of models, resid plots
# 1. pink : "Oncorhynchus gorbuscha"
mod.size.field.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),])
n<-nrow(data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),])
CI.pink<-confint(mod.size.field.pink)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.size.field.pink)
data.pred.CI.field.pink<-as.data.frame(predict(mod.size.field.pink, interval = "confidence")) 


# 2. sockeye: Oncorhynchus nerka
mod.size.field.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),])
n<-nrow(data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),])
CI.soc<-confint(mod.size.field.soc)
# plot(mod.size.field.soc)
data.pred.CI.field.soc<-as.data.frame(predict(mod.size.field.soc, interval = "confidence")) 


# 3. trouts: Oncorhynchus mykiss
mod.size.field.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),])
n<-nrow(data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),])
CI.trouts<-confint(mod.size.field.trouts)
# plot(mod.size.field.trouts)
data.pred.CI.field.trouts<-as.data.frame(predict(mod.size.field.trouts, interval = "confidence")) 


# 4. chinook: Oncorhynchus tshawytscha
mod.size.field.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),])
n<-nrow(data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),])
CI.chin<-confint(mod.size.field.chin)
# plot(mod.size.field.chin) # bad
data.pred.CI.field.chin<-as.data.frame(predict(mod.size.field.chin, interval = "confidence")) 


# 5. chum: Oncorhynchus keta
mod.size.field.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),])
n<-nrow(data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),])
CI.chum<-confint(mod.size.field.chum)
# plot(mod.size.field.chum)
data.pred.CI.field.chum<-as.data.frame(predict(mod.size.field.chum, interval = "confidence")) 


# summary of all fits
data.cm.field.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()
# ******************************************************
# *********** END *************************************






# *********** START *******************************************
# only low swim speeds (comparable with Ucrit, < 250 cm/s)
# ******************************************************

data.cm.field.l.sp<-data.cm.field.l[!(data.cm.field.l$Species_latin == "Oncorhynchus masou" |
                                    data.cm.field.l$Species_latin == "Salmo salar" |
                                    data.cm.field.l$Species_latin == "Oncorhynchus kisutch" ),]
data.cm.field.l.sp$Species_latin<-factor(data.cm.field.l.sp$Species_latin)

# No species specific fits, just not that many data points, low size range, not reliable
data.cm.field.l.sp %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))

# for summary of all fits 
data.cm.field.l.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()

# ******************************************************
# *********** END **************************************







# ***********START *************************************
# Ucrit species specific swim size relationships
# ******************************************************
data.cm.ucrit.sp<-data.cm.ucrit[!(data.cm.ucrit$Species_latin == "Oncorhynchus masou"),]
data.cm.ucrit.sp$Species_latin<-factor(data.cm.ucrit.sp$Species_latin)

# species specific fits, CI of models, resid plots
# 1. pink : "Oncorhynchus gorbuscha"
mod.size.ucrit.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus gorbuscha"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus gorbuscha"),])
CI.ucritpink<-confint(mod.size.ucrit.pink)
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.size.ucrit.pink)
data.pred.CI.ucrit.pink<-as.data.frame(predict(mod.size.ucrit.pink, interval = "confidence")) 


# 2. sockeye: Oncorhynchus nerka
mod.size.ucrit.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus nerka"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus nerka"),])
CI.ucritsoc<-confint(mod.size.ucrit.soc)
# plot(mod.size.ucrit.soc)
data.pred.CI.ucrit.soc<-as.data.frame(predict(mod.size.ucrit.soc, interval = "confidence")) 


# 3. trouts: Oncorhynchus mykiss
mod.size.ucrit.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus mykiss"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus mykiss"),])
CI.ucrittrouts<-confint(mod.size.ucrit.trouts)
# plot(mod.size.ucrit.trouts)
data.pred.CI.ucrit.trouts<-as.data.frame(predict(mod.size.ucrit.trouts, interval = "confidence")) 

# 4. chinook: Oncorhynchus tshawytscha
mod.size.ucrit.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus tshawytscha"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus tshawytscha"),])
CI.ucritchin<-confint(mod.size.ucrit.chin)
# plot(mod.size.ucrit.chin) 
data.pred.CI.ucrit.chin<-as.data.frame(predict(mod.size.ucrit.chin, interval = "confidence")) 

# 5. chum: Oncorhynchus keta
mod.size.ucrit.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus keta"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus keta"),])
CI.ucritchum<-confint(mod.size.ucrit.chum)
# plot(mod.size.ucrit.chum)
data.pred.CI.ucrit.chum<-as.data.frame(predict(mod.size.ucrit.chum, interval = "confidence")) 

# 6. Atlantics: Salmo salar
mod.size.ucrit.salar<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Salmo salar"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Salmo salar"),])
CI.ucritsalar<-confint(mod.size.ucrit.salar)
# plot(mod.size.ucrit.salar)
data.pred.CI.ucrit.salar<-as.data.frame(predict(mod.size.ucrit.salar, interval = "confidence")) 


# 7. coho : "Oncorhynchus kisutch"
mod.size.ucrit.coho<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus kisutch"),])
n<-nrow(data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus kisutch"),])
CI.ucritcoho<-confint(mod.size.ucrit.coho)
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.size.ucrit.coho)
data.pred.CI.ucrit.coho<-as.data.frame(predict(mod.size.ucrit.coho, interval = "confidence")) 

# summary of all fits 
data.cm.ucrit.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()

# ******************************************************
# *********** END **************************************






# TEMPERATURE: stats, modelling ------------

data.cmTNorm.use<-data.cmTNorm.use[!is.na(data.cmTNorm.use$Temp_test_mean),]
data.cmT.use<-data.cm[c(!is.na(data.cm$Temp_test_mean) & c(data.cm$Test_performance2 == "Ucrit" | data.cm$Test_performance2 == "Umax")),] # for all swim data together
data.cmT.use<- data.cmT.use[!data.cmT.use$Species_latin == "Oncorhynchus masou",]# masu salmon one datapoint to use, no curves. 

# *********** START ***************************************
# mixed models, species random eintercept
# ******************************************************
data.cmTNorm.use.ucrit<-data.cmTNorm.use[data.cmTNorm.use$Test_performance2 == "Ucrit",]
mod.size.ucrit<-lmer(SWIM_cms.prop0~ poly(Temp_test_mean,2) + (1|Species_latin), data.cmTNorm.use.ucrit)

# all test data as recorded:
mod.size.ucritI<-lmer(SWIM_cms~ poly(Temp_test_mean,2) + (1|Species_latin), data.cmT.use)


temp.sum<-data.cmTNorm.use.ucrit %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), mintemp = min(Temp_test_mean), maxtemp = max(Temp_test_mean), n_studies = length(unique(Reference_number_1)))

temp.sumI<-data.cmT.use %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), mintemp = min(Temp_test_mean), maxtemp = max(Temp_test_mean), n_studies = length(unique(Reference_number_1)))
# ******************************************************
# *********** END *************************************



# *********** START ***************************************
# Field all swim speeds
# ******************************************************
data.cmTNorm.use.ucrit.sp$Species_latin<-factor(data.cmTNorm.use.ucrit.sp$Species_latin)
data.cmTNorm.use.ucrit.sp$Species_latin<-factor(data.cmTNorm.use.ucrit.sp$Species_latin)

# species specific fits, CI of models, resid plots
# 1. pink : "Oncorhynchus gorbuscha"
mod.pink<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus gorbuscha"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus gorbuscha"),])
CI.temp.pink<-confint(mod.pink)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.pink)
data.pred.CI.temp.pink<-as.data.frame(predict(mod.pink, interval = "confidence")) 


# 2. sockeye: Oncorhynchus nerka
mod.soc<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus nerka"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus nerka"),])
CI.temp.soc<-confint(mod.soc)
plot(mod.soc)
data.pred.CI.temp.soc<-as.data.frame(predict(mod.soc, interval = "confidence")) 


# 3. trouts: Oncorhynchus mykiss
mod.trouts<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus mykiss"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus mykiss"),])
CI.temp.trouts<-confint(mod.trouts)
# plot(mod.trouts)
data.pred.CI.temp.trouts<-as.data.frame(predict(mod.trouts, interval = "confidence")) 

# 4. chinook: Oncorhynchus tshawytscha
mod.chin<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus tshawytscha"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus tshawytscha"),])
CI.temp.chin<-confint(mod.chin)
# plot(mod.chin) 
data.pred.CI.temp.chin<-as.data.frame(predict(mod.chin, interval = "confidence")) 

# 5. coho: Oncorhynchus kisutch
mod.coho<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus kisutch"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus kisutch"),])
CI.temp.coho<-confint(mod.coho)
# plot(mod.coho) 
data.pred.CI.temp.coho<-as.data.frame(predict(mod.coho, interval = "confidence")) 

# 6. Atlantic: Salmo salar
mod.salar<-lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), na.action=na.omit, data = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Salmo salar"),])
n<-nrow(data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Salmo salar"),])
CI.temp.salar<-confint(mod.salar)
# plot(mod.salar) 
data.pred.CI.temp.salar<-as.data.frame(predict(mod.salar, interval = "confidence")) 





### ****** ALL DATA AS RECORDED, not normalized:
# species specific fits, CI of models, resid plots
# 1. pink : "Oncorhynchus gorbuscha"
modI.pink<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus gorbuscha"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus gorbuscha"),])
CI.tempI.pink<-confint(modI.pink)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(modI.pink)
data.pred.CI.tempI.pink<-as.data.frame(predict(modI.pink, interval = "confidence")) 


# 2. sockeye: Oncorhynchus nerka
modI.soc<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus nerka"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus nerka"),])
CI.tempI.soc<-confint(modI.soc)
plot(modI.soc)
data.pred.CI.tempI.soc<-as.data.frame(predict(modI.soc, interval = "confidence")) 


# 3. trouts: Oncorhynchus mykiss
modI.trouts<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus mykiss"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus mykiss"),])
CI.tempI.trouts<-confint(modI.trouts)
# plot(modI.trouts)
data.pred.CI.tempI.trouts<-as.data.frame(predict(modI.trouts, interval = "confidence")) 

# 4. chinook: Oncorhynchus tshawytscha
modI.chin<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus tshawytscha"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus tshawytscha"),])
CI.tempI.chin<-confint(modI.chin)
# plot(modI.chin) 
data.pred.CI.tempI.chin<-as.data.frame(predict(modI.chin, interval = "confidence")) 

# 5. coho: Oncorhynchus kisutch
modI.coho<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus kisutch"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus kisutch"),])
CI.tempI.coho<-confint(modI.coho)
# plot(modI.coho) 
data.pred.CI.tempI.coho<-as.data.frame(predict(modI.coho, interval = "confidence")) 

# 6. Atlantic: Salmo salar
modI.salar<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Salmo salar"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Salmo salar"),])
CI.tempI.salar<-confint(modI.salar)
# plot(modI.salar) 
data.pred.CI.tempI.salar<-as.data.frame(predict(modI.salar, interval = "confidence")) 

# 7 Chum 
modI.chum<-lm(SWIM_cms~ poly(Temp_test_mean,2), na.action=na.exclude, data = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus keta"),])
n<-nrow(data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus keta"),])
CI.tempI.chum<-confint(modI.chum)
# plot(modI.salar) 
data.pred.CI.tempI.chum<-as.data.frame(predict(modI.chum, interval = "confidence")) 
### ****** ALL DATA AS RECORDED, not normalized:





# summary of all fits
data.cmTNorm.use.ucrit %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms.prop0~ poly(Temp_test_mean,2), ., na.action=na.exclude))) %>% 
  ungroup()

data.cmT.use %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ poly(Temp_test_mean,2), ., na.action=na.exclude))) %>% 
  ungroup()
# ******************************************************
# *********** END *************************************


species.temp.plots<-function(model, dd, species, temp.sum = temp.sum, dataset = "normalized"){
  
    data.pred.CI<-predict(model, interval = "confidence") 
    dd$pred.mod<-data.pred.CI[,1]
    dd$pred.modCI.h<-data.pred.CI[,2]
    dd$pred.modCI.l<-data.pred.CI[,3]
    n.stud<-temp.sum[temp.sum$Species_latin == species,"n_studies"]
    n<-temp.sum[temp.sum$Species_latin == species,"n"]
    label.plot<-paste("n = ", n, " (", n.stud,")", sep="")
    
    if(dataset == "normalized"){
      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms.prop0, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point(size = 2, alpha=1)+
        # facet_wrap(.~Species_latin)+
        geom_text(mapping = aes( x = 12, y = 5), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(aes(ymin = pred.modCI.l,
                        ymax = pred.modCI.h), alpha = 0.3)+
        geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
        ylim(0, 120)+
        scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
      ggformat(plot.sp.t, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
      plot.sp.t<-plot.sp.t+theme(legend.position = "none")
      # p2.cmNorm0
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""),dd, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""))
      
      ggsave(filename = paste("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Temp_normalized/Sp_", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
      
    }
   
    if(dataset == "full"){
      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point(size = 2, alpha=1)+
        # facet_wrap(.~Species_latin)+
        geom_text(mapping = aes( x = 12, y = 15), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(aes(ymin = pred.modCI.l,
                        ymax = pred.modCI.h), alpha = 0.3)+
        geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
        ylim(0, 250)+
        scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
      ggformat(plot.sp.t, print=F, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
      plot.sp.t<-plot.sp.t+theme(legend.position = "none")
      # p2.cmNorm0
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""),dd, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""))
      
      ggsave(filename = paste("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Temp_normalized/Sp_FULL_", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
    }
    
    
   
}

species.size.plots<-function(model, dd, species, sum.file, test="field"){
  
  data.pred.CI<-predict(model, interval = "confidence") 
  dd$pred.mod<-data.pred.CI[,1]
  dd$pred.modCI.h<-data.pred.CI[,2]
  dd$pred.modCI.l<-data.pred.CI[,3]
  n.stud<-sum.file[sum.file$Species_latin == species,"n_studies"]
  n<-sum.file[sum.file$Species_latin == species,"n"]
  label.plot<-paste("n = ", n, " (", n.stud,")", sep="")

  
  plot.sp.s<-ggplot(data=dd, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
    scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                       labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                       values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
    scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                      labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                      values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
    geom_point(size = 2, alpha=1)+
    # facet_wrap(.~Species_latin)+
    scale_shape_manual(values = c(21,23))+
    geom_ribbon(aes(ymin = pred.modCI.l,
                    ymax = pred.modCI.h), alpha = 0.3)+
    geom_line(aes(y=pred.mod, x=LENGTH_cm), color = "black")+
    scale_x_continuous(limits = c(30, 100), breaks = c(30, 40, 50, 60, 70, 80, 90))
    if(test=="field"){
      plot.sp.s<- plot.sp.s+ geom_text(mapping = aes( x = 45, y = 865), label = label.plot, color = "black", size=4.5)+
        geom_text(mapping = aes( x = 45, y = 965), label = "Field", color = "black", size=4.5)+
        ylim(0, 1000)
    }
    if(test=="ucrit"){
      plot.sp.s<- plot.sp.s+geom_text(mapping = aes( x = 45, y = 225), label = label.plot, color = "black", size=4.5)+
        geom_text(mapping = aes( x = 45, y = 256), label = "Ucrit", color = "black", size=4.5)+
        ylim(0, 270)
    }
    ggformat(plot.sp.s, print=F, y_title = "Swim Speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 12)
  plot.sp.s<-plot.sp.s+theme(legend.position = "none")
  plot.sp.s
  
  if(test=="field"){
    assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Fdd",sep=""),dd, envir = .GlobalEnv)
    message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Fdd",sep=""))
  } 
  if(test=="ucrit"){
    assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Udd",sep=""),dd, envir = .GlobalEnv)
    message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Udd",sep=""))
  }
  
  ggsave(filename = paste("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Size_byTest/Sp_", species,"-", test,".png", sep=""),
         plot = plot.sp.s, width = 3, height = 3)
}

species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),],
                   model=mod.size.field.pink,
                   species="Oncorhynchus gorbuscha",
                   sum.file=size.sum.field)
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),],
                   model=mod.size.field.chin,
                   species="Oncorhynchus tshawytscha",
                   sum.file=size.sum.field)
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),],
                   model=mod.size.field.soc,
                   species="Oncorhynchus nerka",
                   sum.file=size.sum.field)
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),],
                   model=mod.size.field.trouts,
                   species="Oncorhynchus mykiss",
                   sum.file=size.sum.field)
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),],
                   model=mod.size.field.chum,
                   species="Oncorhynchus keta",
                   sum.file=size.sum.field)


species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus gorbuscha"),],
                   model=mod.size.ucrit.pink,
                   species="Oncorhynchus gorbuscha",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus tshawytscha"),],
                   model=mod.size.ucrit.chin,
                   species="Oncorhynchus tshawytscha",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus nerka"),],
                   model=mod.size.ucrit.soc,
                   species="Oncorhynchus nerka",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus mykiss"),],
                   model=mod.size.ucrit.trouts,
                   species="Oncorhynchus mykiss",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus keta"),],
                   model=mod.size.ucrit.chum,
                   species="Oncorhynchus keta",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Salmo salar"),],
                   model=mod.size.ucrit.salar,
                   species="Salmo salar",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")
species.size.plots(dd=data.cm.ucrit[(data.cm.ucrit$Species_latin == "Oncorhynchus kisutch"),],
                   model=mod.size.ucrit.coho,
                   species="Oncorhynchus kisutch",
                   sum.file=size.sum.ucrit, 
                   test = "ucrit")

species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus gorbuscha"),],
                   model = mod.pink,
                   species = "Oncorhynchus gorbuscha",temp.sum = temp.sum, dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus nerka"),],
                   model = mod.soc,
                   species = "Oncorhynchus nerka",temp.sum = temp.sum, dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus kisutch"),],
                   model = mod.coho,
                   species = "Oncorhynchus kisutch",temp.sum = temp.sum, dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Salmo salar"),],
                   model = mod.salar,
                   species = "Salmo salar",temp.sum = temp.sum, dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus mykiss"),],
                   model = mod.trouts,
                   species = "Oncorhynchus mykiss",temp.sum = temp.sum, dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use.ucrit[(data.cmTNorm.use.ucrit$Species_latin == "Oncorhynchus tshawytscha"),],
                   model = mod.chin,
                   species = "Oncorhynchus tshawytscha",temp.sum = temp.sum, dataset = "normalized")


species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus gorbuscha"),],
                   model = modI.pink,
                   species = "Oncorhynchus gorbuscha", 
                   temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus nerka"),],
                   model = modI.soc,
                   species = "Oncorhynchus nerka", temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus kisutch"),],
                   model = modI.coho,
                   species = "Oncorhynchus kisutch", temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Salmo salar"),],
                   model = modI.salar,
                   species = "Salmo salar",temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus mykiss"),],
                   model = modI.trouts,
                   species = "Oncorhynchus mykiss", temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus tshawytscha"),],
                   model = modI.chin,
                   species = "Oncorhynchus tshawytscha", temp.sum = temp.sumI, 
                   dataset = "full")
species.temp.plots(dd = data.cmT.use[(data.cmT.use$Species_latin == "Oncorhynchus keta"),],
                   model = modI.chum,
                   species = "Oncorhynchus keta", temp.sum = temp.sumI, 
                   dataset = "full")





# DATASETS for plotting ------------

dd_Temp<-rbind(Oncorhynchusgorbuscha_Tdd,
      Oncorhynchuskisutch_Tdd,
      Oncorhynchusmykiss_Tdd, Oncorhynchusnerka_Tdd,
      Oncorhynchustshawytscha_Tdd, Salmosalar_Tdd)

dd_TempI<-rbind(Oncorhynchusgorbuscha_Tdd_full,
               Oncorhynchuskisutch_Tdd_full,
               Oncorhynchusmykiss_Tdd_full, Oncorhynchusnerka_Tdd_full,
               Oncorhynchustshawytscha_Tdd_full, Salmosalar_Tdd_full)

dd_sizeUcrit<-rbind(Oncorhynchusgorbuscha_Udd, Oncorhynchuskisutch_Udd,
      Oncorhynchuskisutch_Udd,
      Oncorhynchusmykiss_Udd, Oncorhynchusnerka_Udd,
      Oncorhynchustshawytscha_Udd, Salmosalar_Udd)

dd_sizeField<-rbind(
      Oncorhynchusgorbuscha_Fdd,
      Oncorhynchusketa_Fdd,
      Oncorhynchusmykiss_Fdd, Oncorhynchusnerka_Fdd,
      Oncorhynchustshawytscha_Fdd)

# preds <- predictInterval(mod.size.ucrit, newdata = data.cm.ucrit, n.sims = 999)
data.cm.ucrit$fit.mod.ALL2 <- predict(mod.size.ucrit)
data.cm.ucrit$fit.mod.ALL<-preds[,1]
data.cm.ucrit$fit.mod.ALL.CI.l<-preds[,3]
data.cm.ucrit$fit.mod.ALL.CI.h<-preds[,2]
# warnings()

# predsF <- predictInterval(mod.size.field, newdata = data.cm.field, n.sims = 999 )
data.cm.field$fit.mod.ALL2 <- predict(mod.size.field)
# data.cm.field$fit.mod.ALL<-predsF[,1] # ran ef
# data.cm.field$fit.mod.ALL.CI.l<-predsF[,3]
# data.cm.field$fit.mod.ALL.CI.h<-predsF[,2]
# warnings()

# predsFl <- predictInterval(mod.size.field.l, newdata = data.cm.field.l, n.sims = 999)
data.cm.field.l$fit.mod.ALL2 <- predict(mod.size.field.l)
# data.cm.field.l$fit.mod.ALL<-predsFl[,1]
# data.cm.field.l$fit.mod.ALL.CI.l<-predsFl[,3]
# data.cm.field.l$fit.mod.ALL.CI.h<-predsFl[,2]

# predsFh <- predictInterval(mod.size.field.h, newdata = data.cm.field.h, n.sims = 999)
data.cm.field.h$fit.mod.ALL2 <- predict(mod.size.field.h)
# data.cm.field.h$fit.mod.ALL<-predsFh[,1]
# data.cm.field.h$fit.mod.ALL.CI.l<-predsFh[,3]
# data.cm.field.h$fit.mod.ALL.CI.h<-predsFh[,2]

# SIZE: figures ------------

# p2.cmNorm0
p1.cm<-ggplot(data=data.cm.ucrit, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                                                      colour=Species_latin,
                                                                      shape=SWIM_cms_source,
                                                                      label=Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2, alpha=0.6)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2, alpha=0.4)+
  geom_point(size=2, alpha=0.4)+
  geom_hline(yintercept = 250, color = "grey", size=0.3, lty=2)+
  scale_shape_manual(values = c(21,23))+
  geom_text(mapping = aes( x =31, y = 230), label = "Ucrit & Umax", color = "black", size=4.5)+
  # geom_ribbon(aes(ymin = fit.mod.ALL.CI.l,
  #                 ymax = fit.mod.ALL.CI.h), alpha = 0.3)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6)+
  xlim(15, 100)+
  ylim(0, 260)
ggformat(p1.cm, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm<-p1.cm+theme(legend.position = "none")
p1.cm

p1.cmF<-ggplot(data=data.cm.field, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                      colour=Species_latin,
                                      shape=SWIM_cms_source,
                                      label=Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2, alpha=0.6)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2, alpha=0.4)+
  geom_point(size=2, alpha=0.4)+
  scale_shape_manual(values = c(21,23))+
  geom_text(mapping = aes( x =32, y = 870), label = "Field", color = "black", size=4.5)+
  # geom_ribbon(aes(ymin = fit.mod.ALL.CI.l,
  #                 ymax = fit.mod.ALL.CI.h), alpha = 0.3)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2, data = data.cm.field.h)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6, data = data.cm.field.h)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2, data = data.cm.field.l)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6, data = data.cm.field.l)+
  geom_hline(yintercept = 250, color = "grey", size=0.3, lty=2)+
  xlim(25, 100)+
  ylim(0,1000)
ggformat(p1.cmF, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cmF<-p1.cmF+theme(legend.position = "none")
p1.cmF



# 
# 
# p1.cmJUMP<-ggplot(data=data.cm[data.cm$Test_performance2=="Jump",], aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
#                                                                           colour=Species_latin,
#                                                                           shape=SWIM_cms_source,
#                                                                           label=Reference_number_1))+
#   scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
#                      labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
#                      values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
#   scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
#                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
#                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
#   geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
#                     ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
#   geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
#                      xmax=LENGTH_cm+Length_error), size =0.2)+
#   geom_point(size=2, alpha=0.7)+
#   scale_shape_manual(values = c(21,23))+
#   facet_wrap(~Test_performance2)+     
#   ylim(0, 1000)+
#   xlim(25, 100)+
#   geom_abline(intercept =coef(mod.size.jump)[1], slope =coef(mod.size.jump)[2], color = "black")
# # stat_smooth(method = "lm", color = "black", aes(shape = NULL, colour = NULL, fill = NULL, group =NULL))
# ggformat(p1.cmJUMP, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
# p1.cmJUMP<-p1.cmJUMP+theme(legend.position = "none")
# p1.cmJUMP


# TEMPERATURE: figures ----------------------

p2.PREDFIT <- ggplot(data= dd_Temp, aes(y=pred.mod, x=Temp_test_mean, fill=Species_latin,
                                                                                              colour=Species_latin,
                                                                                              shape=SWIM_cms_source,
                                                                                              label=Reference_number_1,
                                                                                              group = Species_latin))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  # geom_ribbon(aes(ymin = pred.modCI.l,
  #                 ymax = pred.modCI.h), alpha = 0.3)+
  geom_line(aes(y=pred.mod, x=Temp_test_mean), size=2)+ 
  geom_text(mapping = aes( x =21, y = 8), label = "Ucrit only", color = "black", size=4.5)+
  geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black", size=0.3)+  
  scale_shape_manual(values = c(21,23))+
  ylim(0, 125)
ggformat(p2.PREDFIT, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="")
p2.PREDFIT<-p2.PREDFIT+theme(legend.position = "none")
p2.PREDFIT

p2.PREDFIT_full <- ggplot(data= dd_TempI, aes(y=pred.mod, x=Temp_test_mean, fill=Species_latin,
                                        colour=Species_latin,
                                        shape=SWIM_cms_source,
                                        label=Reference_number_1,
                                        group = Species_latin))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  # geom_ribbon(aes(ymin = pred.modCI.l,
  #                 ymax = pred.modCI.h), alpha = 0.3)+
  geom_line(aes(y=pred.mod, x=Temp_test_mean), size=2)+ 
  geom_text(mapping = aes( x =21, y = 8), label = "Ucrit & Umax", color = "black", size=4.5)+
  # geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black", size=0.3)+  
  scale_shape_manual(values = c(21,23))+
  ylim(0, 250)
ggformat(p2.PREDFIT_full, print=F, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.PREDFIT_full<-p2.PREDFIT_full+theme(legend.position = "none")
p2.PREDFIT_full




ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigTEMP_NORM.png",
       plot = p2.PREDFIT, width = 4, height = 4,units = "in")
ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigTEMP_FULL.png",
       plot = p2.PREDFIT_full, width = 4, height = 4,units = "in")


cowplot::plot_grid(p1.cm, p1.cmF,
                   nrow = 1, ncol =2, align = "hv",
                   labels = "AUTO",
                   label_x = c(0.24, 0.24),
                   label_y = c(0.9)) %>%
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigSIZE_NORM.png",
         width = 8, height = 4,units = "in")


















# misc, additional, not used ---------------
# The other scaling technique - use scaled using min and max in each study 
# figures
p2.cmNormMIN<-ggplot(data= data.cmTNorm.use[data.cmTNorm.use$Test_performance2=="Ucrit" ,], aes(y=SWIM_cms.propMIN, x=Temp_test_mean, fill=Species_latin,
                                                                                                colour=Species_latin,
                                                                                                shape=SWIM_cms_source,
                                                                                                label=Reference_number_1,
                                                                                                group = Species_latin))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_point(size = 2, alpha=1)+
  facet_wrap(.~Species_latin)+
  scale_shape_manual(values = c(21,23))+
  stat_smooth(se=F, method = "lm", formula = y ~ x + I(x^2), size = 1, color = "black")+
  # geom_point(data.cmTNorm.use[c(data.cmTNorm.use$anaerob==1  & data.cmTNorm.use$Test_performance2=="Ucrit"),], mapping = aes(y=SWIM_cms.propMIN, x=Temp_test_mean, fill=Species_latin), size = 3, colour="black", alpha=0.7)
  ylim(0, 110)
ggformat(p2.cmNormMIN, print=F, y_title = "Normalized Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.cmNormMIN<-p2.cmNormMIN+theme(legend.position = "none")
p2.cmNormMIN




# figures - ALL togehter
p2.cmNormMIN<-ggplot(data= data.cmTNorm.use[data.cmTNorm.use$Test_performance2=="Ucrit" ,], aes(y=SWIM_cms.propMIN, x=Temp_test_mean, fill=Species_latin,
                                                                                                colour=Species_latin,
                                                                                                shape=SWIM_cms_source,
                                                                                                label=Reference_number_1,
                                                                                                group = Species_latin))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  # geom_point(size = 2, alpha=0.5)+
  # facet_wrap(.~Species_latin)+
  scale_shape_manual(values = c(21,23))+
  stat_smooth(se=F, method = "lm", formula = y ~ x + I(x^2), size = 1)+
  # geom_point(data.cmTNorm.use[c(data.cmTNorm.use$anaerob==1  & data.cmTNorm.use$Test_performance2=="Ucrit"),], mapping = aes(y=SWIM_cms.propMIN, x=Temp_test_mean, fill=Species_latin), size = 3, colour="black", alpha=0.7)
  ylim(0, 110)
ggformat(p2.cmNormMIN, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="")
p2.cmNormMIN<-p2.cmNormMIN+theme(legend.position = "none")
p2.cmNormMIN





# 
# cowplot::plot_grid(p1.cm, p1.BL, 
#                    nrow = 2, ncol =1, align = "hv", 
#                    labels = "AUTO", 
#                    label_x = c(0.2, 0.2),
#                    label_y = c(0.9, 0.9)) %>% 
#   ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1AB_size_speed.png",
#          width = 5, height = 10,units = "in")
# 
# cowplot::plot_grid(p1.cm.soc, p1.cm.pink,  p1.cm.coho,
#                    nrow = 3, ncol =1, align = "hv", 
#                    labels = "auto", 
#                    label_x = c(0.24, 0.24, 0.24),
#                    label_y = c(0.86, 0.86, 0.86)) %>% 
#   ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1abc_size_speed.png",
#          width = 3.2, height = 8,units = "in")
# 
# cowplot::plot_grid(p1.BL.soc, p1.BL.pink,  p1.BL.coho,
#                    nrow = 3, ncol =1, align = "hv", 
#                    labels = "auto", 
#                    label_x = c(0.2, 0.2, 0.2),
#                    label_y = c(0.86, 0.86, 0.86)) %>% 
#   ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1abc_sizeBL_speed.png",
#          width = 3, height = 8,units = "in")
# 









