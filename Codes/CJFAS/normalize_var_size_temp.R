
# Tasks, brainstorming:
  # EJE: what happens when you only present data from fish tested in their optimal temperature range? 
  # EJE: need to be careful when pooling the data 
  # DAP: for those projects that swim fish at different temperatures you could set the highest performance as 1 or 100%, and scale all the rest of that value (most of the noise we are seeing probably comes from different experimental approachs/tests etc) 
  # DAP: could also report that X% of experiments indicated a clear size effect 
# *********************************

# Import libraries, source data. -----------
library(lme4)
library(ggsci)
library(ggridges)
library(cowplot)
library(tidyverse)
library(broom)
# detach(name = plyr)
library(merTools) # for CI of lmer models 
library(dplyr)



source("/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
source("/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Codes/CJFAS/temp_and_size_species_mod_fnxns.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Data/Files/Kraskura_salmonSwim_analysis_jan2022.csv")

# available datasets: data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim

# view(data)
# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data.ttf<-data[c(data$Test_performance2=="TTF"),]

# 1. Import and organize data sets --------
# data<-data[!c(data$Test_performance2=="TTF" | -c(data$Species_latin=="Oncorhynchus spp.") ),] # two data points off from oncorhynchus species. 
data<-data[!c(data$Test_performance2=="TTF"),]
data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]
data.cm<-data[!is.na(data$SWIM_cms),]
data.cm$Reference_number_1<-factor(data.cm$Reference_number_1)

# field data 
dataF<-as.data.frame(data.all[7])
dataLab<-as.data.frame(data.all[8])
dataLab.use<-dataLab[!c(is.na(dataLab$Temp_test_mean) | is.na(dataLab$SWIM_cms)),] # exclude NAs from SWIM_cms (n=2), and those without temperature

# sanity check everything is subset correctly 
nrow(dataF) + nrow(dataLab) == nrow(data) + nrow(data.ttf)

# 2. Standardize & normalize data -------  
# author reported data: cm/s
data.cm.rep<-data.cm[data.cm$SWIM_cms_source == "reported",] 
data.cm.rep$xi<-1:nrow(data.cm.rep) 
data.cm.rep<-data.cm.rep %>% 
  mutate(xi = reorder(xi, -SWIM_cms)) # order the data 
data.cm.rep$xi<-as.numeric(data.cm.rep$xi)
data.cm.rep$Species_latin2<-as.character(data.cm.rep$Species_latin)
data.cm.rep$Species_latin2[(data.cm.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.cm.rep$Species_latin2<-factor(data.cm.rep$Species_latin2)

# author reported data: BL/s
data.BL.rep<-data.BL[!is.na(data.BL$swim_speed_MEAN_BL_s),]
data.BL.rep$xi<-1:nrow(data.BL.rep)
data.BL.rep<-data.BL.rep %>% 
  mutate(xi = reorder(xi, -swim_speed_MEAN_BL_s))
data.BL.rep$xi<-as.numeric(data.BL.rep$xi)
data.BL.rep$Species_latin2<-as.character(data.BL.rep$Species_latin)
data.BL.rep$Species_latin2[(data.BL.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.BL.rep$Species_latin2<-factor(data.BL.rep$Species_latin2)

# **************** Normalize data **************************
## normalize by species, test performance, and reference number --------
# cm/s
data.cm$test_sp_ref<-factor(paste(data.cm$Species_latin, data.cm$Test_performance2,data.cm$Reference_number_1, sep="-"))
data.cm.split.sp.t.st<-split(data.cm, data.cm$test_sp_ref)
length(data.cm.split.sp.t.st) # 114 

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

# BL/s
data.BL$test_sp_ref<-factor(paste(data.BL$Species_latin, data.BL$Test_performance2,data.BL$Reference_number_1, sep="-"))
data.BL.split.sp.t.st<-split(data.BL, data.BL$test_sp_ref)
length(data.BL.split.sp.t.st) # 77

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

# use only studies that have swum fish across at least 3 C differential temperatures
# and any of the lab tests (not field)
d.cm.t<-data.cmTNorm %>% 
  dplyr:::group_by(Reference_number_1, Test_performance2, Species_latin) %>% 
  summarize(delta_temp = max(Temp_test_mean, na.rm = T) - min(Temp_test_mean, na.rm = T), n = length(Temp_test_mean)) %>% 
  as.data.frame()
# view(d.cm.t )

d.cm.t<-d.cm.t[!d.cm.t$Test_performance2 == "Field", ]
d.cm.t.use<-d.cm.t[c(d.cm.t$delta_temp >= 3), ]
data.cmTNorm.use<-data.cmTNorm[0,]

data.cmTNorm$useTNorm<-0
for(i in 1:nrow(data.cmTNorm)){
  if(any(d.cm.t.use$Reference_number_1 == as.character(data.cmTNorm$Reference_number_1[i]))){
    data.cmTNorm$useTNorm[i]<-1
  }
}

d.BL.t<-data.BLTNorm %>% 
  dplyr:::group_by(Reference_number_1, Test_performance2, Species_latin) %>% 
  summarize(delta_temp = max(Temp_test_mean, na.rm = T) - min(Temp_test_mean, na.rm = T), n = length(Temp_test_mean)) %>% 
  as.data.frame()
# view(d.BL.t )

d.BL.t<-d.BL.t[!d.BL.t$Test_performance2 == "Field" , ]
d.BL.t.use<-d.BL.t[c(d.BL.t$delta_temp >= 3), ]
data.BLTNorm.use<-data.BLTNorm[0,]

data.BLTNorm$useTNorm<-0
for(i in 1:nrow(data.BLTNorm)){
  if(any(d.BL.t.use$Reference_number_1 == as.character(data.BLTNorm$Reference_number_1[i]))){
    data.BLTNorm$useTNorm[i]<-1
  }
}

# length(unique(data.cmTNorm$useTNorm))
# unique(d.cm.t.use)
data.BLTNorm.use<-data.BLTNorm[data.BLTNorm$useTNorm==1,]
data.cmTNorm.use<-data.cmTNorm[data.cmTNorm$useTNorm==1,]

# Normalized data summary 
temp.sum<-data.cmTNorm.use %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), mintemp = min(Temp_test_mean, na.rm = TRUE), maxtemp = max(Temp_test_mean, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

data.cmTNorm.use %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = T) , ., na.action=na.exclude))) %>% 
  ungroup()

# all studies for normalized data are from tunnel swim tests, Ucrit and Umax
# END: Normalize data **************************


# ******************************************************
# 3. Temperature models------------
## 3.1. mixed model: lmer w/ species random intercept ------
# dataLab.use[dataLab.use$Species_latin == "Oncorhynchus masou",] # n = 1; checking

mod.size.tunnelTNorm<-lmer(SWIM_cms.prop0~ poly(Temp_test_mean , 2, raw = TRUE) + (1|Species_latin), data.cmTNorm.use) # ucrit & Umax
summary(mod.size.tunnelTNorm)
qqmath(mod.size.tunnelTNorm)

## 3.2. Species-specific, normalized swim TPCs -------
# ******************************************************
data.cmTNorm.use$Species_latin<-factor(data.cmTNorm.use$Species_latin)
# 1. pink : "Oncorhynchus gorbuscha"
mod.pink<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus gorbuscha"),])
CI.temp.pink<-confint(mod.pink)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.pink)
data.pred.CI.temp.pink<-as.data.frame(predict(mod.pink, interval = "confidence"))

# 2. sockeye: Oncorhynchus nerka
mod.soc<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus nerka"),])
CI.temp.soc<-confint(mod.soc)
plot(mod.soc)
data.pred.CI.temp.soc<-as.data.frame(predict(mod.soc, interval = "confidence"))

# 3. trouts: Oncorhynchus mykiss
mod.trouts<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus mykiss"),])
CI.temp.trouts<-confint(mod.trouts)
# plot(mod.trouts)
data.pred.CI.temp.trouts<-as.data.frame(predict(mod.trouts, interval = "confidence"))

# 4. chinook: Oncorhynchus tshawytscha
mod.chin<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus tshawytscha"),])
CI.temp.chin<-confint(mod.chin)
# plot(mod.chin)
data.pred.CI.temp.chin<-as.data.frame(predict(mod.chin, interval = "confidence"))

# 5. coho: Oncorhynchus kisutch
mod.coho<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus kisutch"),])
CI.temp.coho<-confint(mod.coho)
# plot(mod.coho)
data.pred.CI.temp.coho<-as.data.frame(predict(mod.coho, interval = "confidence"))

# 6. Atlantic: Salmo salar
mod.salar<-lm(SWIM_cms.prop0~ poly(Temp_test_mean, 2, raw = TRUE), na.action=na.omit, data = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Salmo salar"),])
CI.temp.salar<-confint(mod.salar)
# plot(mod.salar)
data.pred.CI.temp.salar<-as.data.frame(predict(mod.salar, interval = "confidence"))

# ******************************************************
# 4. Function use (species specific, get CI, predicted data, plots) -----------
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus gorbuscha"),],
                   model = mod.pink,
                   species = "Oncorhynchus gorbuscha",
                   temp.sum = temp.sum,
                   dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus nerka"),],
                   model = mod.soc,
                   species = "Oncorhynchus nerka",
                   temp.sum = temp.sum,
                   dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus kisutch"),],
                   model = mod.coho,
                   species = "Oncorhynchus kisutch",
                   temp.sum = temp.sum,
                   dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Salmo salar"),],
                   model = mod.salar,
                   species = "Salmo salar",
                   temp.sum = temp.sum,
                   dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus mykiss"),],
                   model = mod.trouts,
                   species = "Oncorhynchus mykiss",
                   temp.sum = temp.sum,
                   dataset = "normalized")
species.temp.plots(dd = data.cmTNorm.use[(data.cmTNorm.use$Species_latin == "Oncorhynchus tshawytscha" & !is.na(data.cmTNorm.use$Temp_test_mean)),],
                   model = mod.chin,
                   species = "Oncorhynchus tshawytscha",
                   temp.sum = temp.sum,
                   dataset = "normalized")

# rbind the returend datasets for plotting 
dd_Temp<-rbind(Oncorhynchusgorbuscha_Tdd,
      Oncorhynchuskisutch_Tdd,
      Oncorhynchusmykiss_Tdd,
      Oncorhynchusnerka_Tdd,
      Oncorhynchustshawytscha_Tdd,
      Salmosalar_Tdd)


# figures: --------------
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
  scale_shape_manual(values = c(21,23))+
  stat_smooth(se=F, method = "lm", formula = y ~ x + I(x^2), size = 1)+
  ylim(0, 110)
ggformat(p2.cmNormMIN, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="")
p2.cmNormMIN<-p2.cmNormMIN+theme(legend.position = "none")
p2.cmNormMIN







