# ******************************
# Import libraries, source data. -----------
library(lme4)
library(ggsci)
library(ggridges)
library(gridExtra)
library(grid)
library(cowplot)
library(tidyverse)
library(broom)
# detach(name = plyr)
library(merTools) # for CI of lmer models 
library(dplyr)
library(lattice)

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

# sanity check everything is subset correctly 
nrow(dataF) + nrow(dataLab) == nrow(data) + nrow(data.ttf)

# 3. Mixed models: size-swim scaling  ------------
## 3.0. the regression datasets: -----
data.cm.tunnel<-data.cm[data.cm$Test_performance2=="Ucrit" | data.cm$Test_performance2=="Umax",]
data.cm.field<-data.cm[data.cm$Test_performance2=="Field",]
data.cm.field.l<-data.cm[data.cm$Test_performance2=="Field" & data.cm$SWIM_cms < 250,]
data.cm.field.h<-data.cm[data.cm$Test_performance2=="Field" & data.cm$SWIM_cms >= 250,]
data.cm.swim<-data.cm[data.cm$Test_performance2=="Swim",]
data.cm.jump<-data.cm[data.cm$Test_performance2=="Jump",]

size.sum.Lab<-dataLab %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

size.sum.field<-data.cm.field %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

size.sum.tunnel<-data.cm.tunnel %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

size.sum.swim<-data.cm.swim %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

size.sum.jump<-data.cm.jump %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE), maxsize = max(LENGTH_cm, na.rm = TRUE), n_studies = length(unique(Reference_number_1)))

## 3.1. All data - mixed models, species random intercept -------
mod.size.lab<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), dataLab, na.action = na.exclude)
mod.size.tunnel<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.tunnel, na.action = na.exclude)
mod.size.field<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field, na.action = na.exclude)
mod.size.field.l<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.l, na.action = na.exclude)
mod.size.field.h<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.h, na.action = na.exclude)
mod.size.jump<-lm(SWIM_cms ~ LENGTH_cm, data.cm.jump, na.action = na.exclude)
mod.size.swim<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.swim, na.action = na.exclude)

mod.size.labS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, dataLab, na.action = na.exclude)
mod.size.tunnelS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.tunnel, na.action = na.exclude)
mod.size.fieldS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field, na.action = na.exclude)
mod.size.field.lS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field.l, na.action = na.exclude)
mod.size.field.hS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field.h, na.action = na.exclude)
mod.size.swimS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.swim, na.action = na.exclude)

# in all cases, the model with species as random effect is better 
BIC(mod.size.labS, mod.size.lab)
BIC(mod.size.tunnelS, mod.size.tunnel)
BIC(mod.size.fieldS, mod.size.field) 
BIC(mod.size.field.hS, mod.size.field.h)
BIC(mod.size.field.lS, mod.size.field.l)
BIC(mod.size.swimS, mod.size.swim)

# REPORTED <<< 
summary(mod.size.lab)
summary(mod.size.tunnel)
summary(mod.size.field)
summary(mod.size.field.h)
summary(mod.size.field.l)
summary(mod.size.jump)
summary(mod.size.swim)
car:::Anova(mod.size.tunnel, type ="II")
car:::Anova(mod.size.field.h, type ="II")
car:::Anova(mod.size.field.l, type ="II")
car:::Anova(mod.size.field, type ="II")

# assumptions
qqmath(mod.size.lab)
qqmath(mod.size.tunnel)
qqmath(mod.size.field) # can see the outliers on this, overall ok
qqmath(mod.size.field.h)
qqmath(mod.size.field.l)
qqmath(mod.size.swim)
plot(mod.size.jump)

# fixed effects (coefs for plotting)
size.lab.int<-round(fixef(mod.size.lab)[1], 2)
size.lab.slope<-round(fixef(mod.size.lab)[2], 2)
size.lab.n<-unlist(summary(mod.size.lab)[[3]][2])[1]

size.field.int<-round(fixef(mod.size.field)[1], 2)
size.field.slope<-round(fixef(mod.size.field)[2], 2)
size.field.n<-unlist(summary(mod.size.field)[[3]][2])[1]

size.field.h.int<-round(fixef(mod.size.field.h)[1], 2)
size.field.h.slope<-round(fixef(mod.size.field.h)[2], 2)
size.field.h.n<-unlist(summary(mod.size.field.h)[[3]][2])[1]

size.field.l.int<-round(fixef(mod.size.field.l)[1], 2)
size.field.l.slope<-round(fixef(mod.size.field.l)[2], 2)
size.field.l.n<-unlist(summary(mod.size.field.l)[[3]][2])[1]

# *********** END *************************************



# *********** START ***************************************
## 3.2. Field. species-specific, cm/s -------
# 1. pink : "Oncorhynchus gorbuscha"
mod.size.field.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),])
CI.pink<-confint(mod.size.field.pink)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod.size.field.pink)
data.pred.CI.field.pink<-as.data.frame(predict(mod.size.field.pink, interval = "confidence")) 

# 2. sockeye: Oncorhynchus nerka
mod.size.field.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),])
CI.soc<-confint(mod.size.field.soc)
# plot(mod.size.field.soc)
data.pred.CI.field.soc<-as.data.frame(predict(mod.size.field.soc, interval = "confidence")) 

# 3. trouts: Oncorhynchus mykiss
mod.size.field.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),])
CI.trouts<-confint(mod.size.field.trouts)
# plot(mod.size.field.trouts)
data.pred.CI.field.trouts<-as.data.frame(predict(mod.size.field.trouts, interval = "confidence")) 

# 4. chinook: Oncorhynchus tshawytscha
mod.size.field.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),])
CI.chin<-confint(mod.size.field.chin)
# plot(mod.size.field.chin) # bad
data.pred.CI.field.chin<-as.data.frame(predict(mod.size.field.chin, interval = "confidence")) 

# 5. chum: Oncorhynchus keta
mod.size.field.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),])
CI.chum<-confint(mod.size.field.chum)
# plot(mod.size.field.chum)
data.pred.CI.field.chum<-as.data.frame(predict(mod.size.field.chum, interval = "confidence")) 

# summaries 
data.cm.field.sp<-data.cm.field[!(data.cm.field$Species_latin == "Oncorhynchus masou" | # n = 4
                                 data.cm.field$Species_latin == "Salmo salar" | # n = 3
                                 data.cm.field$Species_latin == "Oncorhynchus kisutch" | # n = 5
                                    data.cm.field$Species_latin == "Oncorhynchus spp." ),] # 2
data.cm.field.sp$Species_latin<-factor(data.cm.field.sp$Species_latin)
# No species specific fits, just not that many data points, low size range, not reliable
data.cm.field.sp %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))
# summary of all fits
data.cm.field.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()
# *********** END *************************************

# only low swim speeds (comparable with Ucrit, < 250 cm/s)

data.cm.field.l.sp<-data.cm.field.l[!(data.cm.field.l$Species_latin == "Oncorhynchus masou" |
                                    data.cm.field.l$Species_latin == "Salmo salar" |
                                    data.cm.field.l$Species_latin == "Oncorhynchus kisutch" |
                                    data.cm.field.l$Species_latin == "Oncorhynchus spp."),]
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

# And high speeds:
data.cm.field.h.sp<-data.cm.field.h[!(data.cm.field.h$Species_latin == "Oncorhynchus masou" |
                                    data.cm.field.h$Species_latin == "Salmo salar" |
                                    data.cm.field.h$Species_latin == "Oncorhynchus kisutch" |
                                    data.cm.field.h$Species_latin == "Oncorhynchus spp."),]
data.cm.field.h.sp$Species_latin<-factor(data.cm.field.h.sp$Species_latin)


# No species specific fits, just not that many data points, low size range, not reliable
data.cm.field.h.sp %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))

# for summary of all fits 
data.cm.field.h.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()
# *********** END **************************************


## 3.3. Tunnel swim speeds: species-specific, cm/s ---------
# ******************************************************
# species specific fits, CI of models, resid plots
# 1. pink : "Oncorhynchus gorbuscha"
mod.size.tunnel.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus gorbuscha"),])
CI.tunnelpink<-confint(mod.size.tunnel.pink)
# plot(mod.size.tunnel.pink)
data.pred.CI.tunnel.pink<-as.data.frame(predict(mod.size.tunnel.pink, interval = "confidence")) 

# 2. sockeye: Oncorhynchus nerka
mod.size.tunnel.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus nerka"),])
CI.tunnelsoc<-confint(mod.size.tunnel.soc)
# plot(mod.size.tunnel.soc)
data.pred.CI.tunnel.soc<-as.data.frame(predict(mod.size.tunnel.soc, interval = "confidence")) 

# 3. trouts: Oncorhynchus mykiss
mod.size.tunnel.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus mykiss"),])
CI.tunneltrouts<-confint(mod.size.tunnel.trouts)
# plot(mod.size.tunnel.trouts)
data.pred.CI.tunnel.trouts<-as.data.frame(predict(mod.size.tunnel.trouts, interval = "confidence")) 

# 4. chinook: Oncorhynchus tshawytscha
mod.size.tunnel.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus tshawytscha"),])
CI.tunnelchin<-confint(mod.size.tunnel.chin)
# plot(mod.size.tunnel.chin) 
data.pred.CI.tunnel.chin<-as.data.frame(predict(mod.size.tunnel.chin, interval = "confidence")) 

# 5. chum: Oncorhynchus keta
mod.size.tunnel.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus keta"),])
CI.tunnelchum<-confint(mod.size.tunnel.chum)
# plot(mod.size.tunnel.chum)
data.pred.CI.tunnel.chum<-as.data.frame(predict(mod.size.tunnel.chum, interval = "confidence")) 

# 6. Atlantics: Salmo salar
mod.size.tunnel.salar<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Salmo salar"),])
CI.tunnelsalar<-confint(mod.size.tunnel.salar)
# plot(mod.size.tunnel.salar)
data.pred.CI.tunnel.salar<-as.data.frame(predict(mod.size.tunnel.salar, interval = "confidence")) 

# 7. coho : "Oncorhynchus kisutch"
mod.size.tunnel.coho<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus kisutch"),])
CI.tunnelcoho<-confint(mod.size.tunnel.coho)
# plot(mod.size.tunnel.coho)
data.pred.CI.tunnel.coho<-as.data.frame(predict(mod.size.tunnel.coho, interval = "confidence")) 

# summary of all fits 
data.cm.tunnel.sp<-data.cm.tunnel[!(data.cm.tunnel$Species_latin == "Oncorhynchus masou"),]
data.cm.tunnel.sp$Species_latin<-factor(data.cm.tunnel.sp$Species_latin)
data.cm.tunnel.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()

      # ## 3.4. Lab swim speeds: species-specific, cm/s ---------
      # # ******************************************************
      # # species specific fits, CI of models, resid plots
      # # 1. pink : "Oncorhynchus gorbuscha"
      # mod.size.Lab.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus gorbuscha"),])
      # CI.Labpink<-confint(mod.size.Lab.pink)
      # # plot(mod.size.Lab.pink)
      # data.pred.CI.Lab.pink<-as.data.frame(predict(mod.size.Lab.pink, interval = "confidence")) 
      # 
      # # 2. sockeye: Oncorhynchus nerka
      # mod.size.Lab.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus nerka"),])
      # CI.Labsoc<-confint(mod.size.Lab.soc)
      # # plot(mod.size.Lab.soc)
      # data.pred.CI.Lab.soc<-as.data.frame(predict(mod.size.Lab.soc, interval = "confidence")) 
      # 
      # # 3. trouts: Oncorhynchus mykiss
      # mod.size.Lab.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus mykiss"),])
      # CI.Labtrouts<-confint(mod.size.Lab.trouts)
      # # plot(mod.size.Lab.trouts)
      # data.pred.CI.Lab.trouts<-as.data.frame(predict(mod.size.Lab.trouts, interval = "confidence")) 
      # 
      # # 4. chinook: Oncorhynchus tshawytscha
      # mod.size.Lab.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus tshawytscha"),])
      # CI.Labchin<-confint(mod.size.Lab.chin)
      # # plot(mod.size.Lab.chin) 
      # data.pred.CI.Lab.chin<-as.data.frame(predict(mod.size.Lab.chin, interval = "confidence")) 
      # 
      # # 5. chum: Oncorhynchus keta
      # mod.size.Lab.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus keta"),])
      # CI.Labchum<-confint(mod.size.Lab.chum)
      # # plot(mod.size.Lab.chum)
      # data.pred.CI.Lab.chum<-as.data.frame(predict(mod.size.Lab.chum, interval = "confidence")) 
      # 
      # # 6. Atlantics: Salmo salar
      # mod.size.Lab.salar<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Salmo salar"),])
      # CI.Labsalar<-confint(mod.size.Lab.salar)
      # # plot(mod.size.Lab.salar)
      # data.pred.CI.Lab.salar<-as.data.frame(predict(mod.size.Lab.salar, interval = "confidence")) 
      # 
      # # 7. coho : "Oncorhynchus kisutch"
      # mod.size.Lab.coho<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = dataLab[(dataLab$Species_latin == "Oncorhynchus kisutch"),])
      # CI.Labcoho<-confint(mod.size.Lab.coho)
      # # plot(mod.size.Lab.coho)
      # data.pred.CI.Lab.coho<-as.data.frame(predict(mod.size.Lab.coho, interval = "confidence")) 

# summary of all fits 
dataLab.sp<-dataLab[!(dataLab$Species_latin == "Oncorhynchus masou"),]
dataLab.sp$Species_latin<-factor(dataLab.sp$Species_latin)
dataLab.sp %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup()

# ******************************************************
# *********** END **************************************


# ******************************************************
# 4. Temperature: mixed models ------------
# 4.1. Size standardize  _________________-
scaling_slope_TL <- fixef(mod.size.lab)[2]
dataLab$LENGTH_cm_scaled<-55
dataLab$SWIM_cms_scaled<- dataLab$SWIM_cms + (scaling_slope_TL * ( dataLab$LENGTH_cm -dataLab$LENGTH_cm_scaled) )  
# data set for temp perf curves 
dataLab.use<-dataLab[!c(is.na(dataLab$Temp_test_mean) | is.na(dataLab$SWIM_cms) | is.na(dataLab$LENGTH_cm)),] # exclude NAs from SWIM_cms (n=2), and those without temperature

## 4.2. Tunnel, Lab swim TPCs, lmer  w/ species random intercept ------
# Ucrit, Umax, and 'Swim' and 'jump' data as recorded; NOT Field:
mod.tmep.Lab<-lmer(SWIM_cms_scaled ~ poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data = dataLab.use, REML = FALSE)
mod.tmep.Lab3<-lmer(SWIM_cms_scaled ~ poly(Temp_test_mean, 3, raw = TRUE) + (1|Species_latin), data = dataLab.use, REML = FALSE)

BIC(mod.tmep.Lab3, mod.tmep.Lab)# 2nd order poly is better

dataLab.use[dataLab.use$Species_latin == "Oncorhynchus masou",] # n = 1, checking 
temp.sumI<-dataLab.use %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), mintemp = min(Temp_test_mean), maxtemp = max(Temp_test_mean), n_studies = length(unique(Reference_number_1)))

# summary of all fits
# actual data << REPORTED 
dataLab.use %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms ~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup() %>% 
  as.data.frame()
# ******************************************************

# 4. Function use (species specific, get CI, predicted data, plots) ------------
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),],
                   model=mod.size.field.pink,
                   species="Oncorhynchus gorbuscha",
                   sum.file=size.sum.field, 
                   test = "field")
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),],
                   model=mod.size.field.chin,
                   species="Oncorhynchus tshawytscha",
                   sum.file=size.sum.field,
                   test = "field")
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),],
                   model=mod.size.field.soc,
                   species="Oncorhynchus nerka",
                   sum.file=size.sum.field,
                   test = "field")
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),],
                   model=mod.size.field.trouts,
                   species="Oncorhynchus mykiss",
                   sum.file=size.sum.field,
                   test = "field")
species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),],
                   model=mod.size.field.chum,
                   species="Oncorhynchus keta",
                   sum.file=size.sum.field)

# Lab temp only, (standardised size, no size)
species.temp.size.fits(species = "Oncorhynchus gorbuscha", 
                   temp.sum = temp.sumI, 
                   dataform = "raw",
                   scale.fish.size = T)
species.temp.size.fits(species = "Oncorhynchus nerka",
                   temp.sum = temp.sumI, 
                   dataform = "raw", scale.fish.size = T)
species.temp.size.fits(species = "Oncorhynchus kisutch",
                   temp.sum = temp.sumI, 
                   dataform = "raw",scale.fish.size = T)
species.temp.size.fits(species = "Salmo salar",
                   temp.sum = temp.sumI, 
                   dataform = "raw",scale.fish.size = T)
species.temp.size.fits(species = "Oncorhynchus mykiss",
                   temp.sum = temp.sumI, 
                   dataform = "raw",scale.fish.size = T)
species.temp.size.fits(species = "Oncorhynchus tshawytscha",
                   temp.sum = temp.sumI, 
                   dataform = "raw",scale.fish.size = T)
species.temp.size.fits(species = "Oncorhynchus keta",
                   temp.sum = temp.sumI, 
                   dataform = "raw",scale.fish.size = T)

# raw with size in the model 
species.temp.size.fits(species = "Oncorhynchus gorbuscha", 
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Oncorhynchus nerka",
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Oncorhynchus kisutch",
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Salmo salar",
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Oncorhynchus mykiss",
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Oncorhynchus tshawytscha",
                   temp.sum = temp.sumI, 
                   dataform = "raw")
species.temp.size.fits(species = "Oncorhynchus keta",
                   temp.sum = temp.sumI,
                   dataform = "raw")


# rbind the function-returned datasets fir ech species, with predicted CIs. / used for plotting 
dd_TempI<-rbind(Oncorhynchusgorbuscha_Tdd_full,
               Oncorhynchuskisutch_Tdd_full,
               Oncorhynchusmykiss_Tdd_full,
               Oncorhynchusnerka_Tdd_full,
               Oncorhynchustshawytscha_Tdd_full,
               Salmosalar_Tdd_full)
               # Oncorhynchusketa_Tdd_full)

dd_sizetunnel<-rbind(Oncorhynchusgorbuscha_Ldd,
                    Oncorhynchuskisutch_Ldd,
                    Oncorhynchuskisutch_Ldd,
                    Oncorhynchusmykiss_Ldd,
                    Oncorhynchusnerka_Ldd,
                    Oncorhynchustshawytscha_Ldd,
                    Salmosalar_Ldd)
dd_sizeField<-rbind(
      Oncorhynchusgorbuscha_Fdd,
      Oncorhynchusketa_Fdd,
      Oncorhynchusmykiss_Fdd,
      Oncorhynchusnerka_Fdd,
      Oncorhynchustshawytscha_Fdd)

# ## Topt: find the peaks for species-specific fits (all data except field) -------
# P.chin<-function(x){coef(modI.chin)[1]  + x^1 * coef(modI.chin)[2] + x^2*coef(modI.chin)[3] }
# o.chin<- optimize(f = P.chin, interval = c(8,20), maximum = TRUE)
# 
# P.chum<-function(x){coef(modI.chum)[1]  + x^1 * coef(modI.chum)[2] + x^2*coef(modI.chum)[3] }
# o.chum<- optimize(f = P.chum, interval = c(0,30), maximum = TRUE)
# 
# P.coho<-function(x){coef(modI.coho)[1]  + x^1 * coef(modI.coho)[2] + x^2*coef(modI.coho)[3] }
# o.coho<- optimize(f = P.coho, interval = c(5,18), maximum = TRUE)
# 
# P.salar<-function(x){coef(modI.salar)[1]  + x^1 * coef(modI.salar)[2] + x^2*coef(modI.salar)[3] }
# o.salar<- optimize(f = P.salar, interval = c(3,23), maximum = TRUE)
# 
# P.trouts<-function(x){coef(modI.trouts)[1]  + x^1 * coef(modI.trouts)[2] + x^2*coef(modI.trouts)[3] }
# o.trouts<- optimize(f = P.trouts, interval = c(6,20), maximum = TRUE)
# 
# P.soc<-function(x){coef(modI.soc)[1]  + x^1 * coef(modI.soc)[2] + x^2*coef(modI.soc)[3] }
# o.soc<- optimize(f = P.soc, interval = c(8,26), maximum = TRUE)
# 
# P.pink<-function(x){coef(modI.pink)[1]  + x^1 * coef(modI.pink)[2] + x^2*coef(modI.pink)[3] }
# o.pink<- optimize(f = P.pink, interval = c(6,28), maximum = TRUE)

# create a data frame with the estimated peaks 
fit.peaks<-data.frame(
  opt.T = c(o.coho$maximum, o.pink$maximum, o.salar$maximum, o.trouts$maximum, o.soc$maximum), # o.chin$maximum, 
  opt.speed = c( o.coho$objective, o.pink$objective, o.salar$objective, o.trouts$objective, o.soc$objective), # o.chin$objective,
  Species_latin = c("Oncorhynchus kisutch", "Oncorhynchus gorbuscha", "Salmo salar", "Oncorhynchus mykiss", "Oncorhynchus nerka") # "Oncorhynchus tshawytscha", 
)

# Predicted data:  Swim, jump, ucrit, umax
predsLab <- predictInterval(mod.size.lab, newdata = dataLab, n.sims = 999) # nas produced where NAs in the data, using whole datasets for predicting 
dataLab$fit.mod.ALL2 <- predict(mod.size.lab)
dataLab$fit.mod.ALL.CI.l<-predsLab[,3]
dataLab$fit.mod.ALL.CI.h<-predsLab[,2]

# Predicted data: tunnel Ucrit and Umax
preds <- predictInterval(mod.size.tunnel, newdata = data.cm.tunnel, n.sims = 999) # nas produced where NAs in the data, using whole datasets for predicting 
data.cm.tunnel$fit.mod.ALL2 <- predict(mod.size.tunnel)
data.cm.tunnel$fit.mod.ALL.CI.l<-preds[,3]
data.cm.tunnel$fit.mod.ALL.CI.h<-preds[,2]

# Predicted data: field 
data.cm.field$fit.mod.ALL2 <- predict(mod.size.field)
data.cm.field.l$fit.mod.ALL2 <- predict(mod.size.field.l)
data.cm.field.h$fit.mod.ALL2 <- predict(mod.size.field.h)

# Fig7. Scaling  ------------
## Fig 7A [lab] ------
p1.cm.Lab<-ggplot(data=dataLab, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                    colour=Species_latin,
                                    label=Reference_number_1))+
  geom_rect(data = data ,mapping = aes(xmin = 57, xmax = 62, ymin = 230, ymax = 600, fill = NULL),  fill = "white", color = "grey", linetype = "dotted", alpha = .2)+
  annotate("text", x = 63, y = 500, label = "Jump", hjust = 0, size = 5)+
  annotate(geom = "text", y = 880, x = 50, hjust = 0, color = "black", size = 3.5,
         label = bquote( "Speed = " * .(size.lab.int) ~ "+" ~ .(size.lab.slope) ~ "BL (n = " * .(size.lab.n) * ")"))+
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
  annotate(geom = "text", x =15, y = 800, label = "Lab tests", color = "black", size=5, hjust = 0)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6)+
  xlim(15, 100)+
  ylim(0,900)
ggformat(p1.cm.Lab, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm.Lab<-p1.cm.Lab+theme(legend.position = "none")
p1.cm.Lab

## Fig 7B [field] ------
p1.cmF<-ggplot(data=data.cm.field, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                      colour=Species_latin,
                                      label=Reference_number_1))+
  annotate(geom = "text", y = 880, x = 30, hjust = 0, color = "black", size = 3.5,
         label = bquote( "Speed = " * .(size.field.int) ~ "+" ~ .(size.field.slope) ~ "BL (n = " * .(size.field.n) * ")"))+
  annotate(geom = "text", y = 830, x = 30, hjust = 0, color = "black", size = 3.5,
         label = bquote( "Speed (> 250 cm/s) = " * .(size.field.h.int) ~ "+" ~ .(size.field.h.slope) ~ "BL (n = " * .(size.field.h.n) * ")"))+
  annotate(geom = "text", y = 780, x = 30, hjust = 0, color = "black", size = 3.5,
         label = bquote( "Speed (< 250 cm/s) = " * .(size.field.l.int) ~ .(size.field.l.slope) ~ "BL (n = " * .(size.field.l.n) * ")"))+
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
  annotate(geom = "text", x =15, y = 800, label = "Field", color = "black", size=5, hjust = 0)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2, data = data.cm.field.h)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6, data = data.cm.field.h)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2, data = data.cm.field.l)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6, data = data.cm.field.l)+
  geom_hline(yintercept = 250, color = "grey", size=0.3, lty=2)+
  xlim(15, 100)+
  ylim(0,900)
ggformat(p1.cmF, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cmF<-p1.cmF+theme(legend.position = "none")
# p1.cmF


# Fig6. Temperature and swim ----------------------
# Fig6B. main fish with Topt ----------
dd_TempI_Topt<-dd_TempI[!duplicated(dd_TempI$Topt),]
dd_TempI_Topt$y <- c(10, 2, 10, 10, 10 ,10 )
dd_TempI_Topt$x <- c(28.164158, 16.8, 14.5, 16.9,  8.282343, 22.999926)

p2.PREDFIT_full <- ggplot(data = dd_TempI, aes(y=pred.mod, x=Temp_test_mean, fill=Species_latin,
                                        colour = Species_latin,
                                        group = Species_latin,
                                        label = round(Topt,1)))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  # geom_ribbon(aes(ymin = pred.modCI.l,
  #                 ymax = pred.modCI.h), alpha = 0.3)+
  geom_segment(mapping = aes(x = Topt, y = 15, xend = Topt, yend = 30, colour = Species_latin, fill = NULL, shape = NULL),
               arrow = arrow(length = unit(0.25, "cm")), lty="solid", size=0.9, alpha = 0.6)+
  geom_line(aes(y=pred.mod, x= Temp_test_mean), size=2, lineend = "round")+ 
  geom_text(data = dd_TempI_Topt, mapping = aes(x = x, y = y, colour = Species_latin, fill = NULL, shape = NULL),
                  angle = 0, size= 4,  fontface = "bold", alpha = 1)+
  # geom_text(mapping = aes( x =fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch","opt.T"], y = 0, colour = Species_latin, fill = NULL, shape = NULL), angle = 90, label = round(fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch","opt.T"],1), size=2.6, label.size = 0, fontface = "bold",  data = fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch",], alpha = 1)+
  # geom_text(mapping = aes( x =21, y = 8), label = "Ucrit & Umax", color = "black", size=4.5)+
  # geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black", size=0.3)+  
  scale_shape_manual(values = c(21,23))+
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30))+
  ylim(0, 250)
ggformat(p2.PREDFIT_full, print=F, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.PREDFIT_full<-p2.PREDFIT_full+theme(legend.position = "none",
                                       plot.margin=margin(t = 0, unit="cm", r = 0.1, l = 0.5))
p2.PREDFIT_full

# Fig6A. size distr ----------
p2.size_hist<-
  ggplot(data[!data$Species_latin == "Oncorhynchus spp.", ], aes(x = LENGTH_cm, y = Species_latin, fill = Species_latin, color = Species_latin)) +
  geom_density_ridges(alpha = 0.5, scale = 3,rel_min_height = 0.01,size=0.3)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_x_continuous(limits = c(25, 120), breaks = c(30, 40, 50, 60, 70, 80, 90))+
  annotate(geom = "text", y = 8.2, x = 98, hjust = 0, color = "#64B0BC", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 7.2, x = 98, hjust = 0, color = "#70Af81", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 6.2, x = 98, hjust = 0, color = "#EA573D", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 5.2, x = 98, hjust = 0, color = "#615B70", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 4.2, x = 98, hjust = 0, color = "#446699", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 3.2, x = 98, hjust = 0, color = "#FBC063", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 2.2, x = 98, hjust = 0, color = "#FB9A62", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 1.2, x = 98, hjust = 0, color = "#D292CD", size = 3,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus gorbuscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus gorbuscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus gorbuscha", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
theme_minimal()+
  xlab("Length (cm)")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(vjust = +2),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black", size =12),
        panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.5), 
        legend.position = "none",
        plot.margin = unit(c(t = 0, r = 0, b =0, l=0), unit = "cm"))

legend.plot<-
  ggplot(data[c(!data$Species_latin == "Oncorhynchus spp."), ], aes(x = LENGTH_cm, fill = Species_latin, color = Species_latin)) +
  geom_histogram(alpha=0.8)+
  guides(color=guide_legend(ncol = 2, byrow=TRUE)) +
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  theme( legend.title = element_blank(),
        legend.text = element_text(face = "italic"))
        # legend.key.size = unit(c(0.5), "cm"))


legend <- cowplot::get_legend(legend.plot)

grid2<-plot_grid(legend,
           Oncorhynchusgorbuscha_Tdd_plot,
          Oncorhynchusketa_Tdd_plot,
          Salmosalar_Tdd_plot,
          nrow = 4,
          ncol = 1,
          labels = c( "", "b", "d","f"),
          label_x = c(0.2, 0.2, 0.2, 0.2),
          label_y = c(0.96, 0.96, 0.96, 0.96),
          align = "hvr")
grid3<-plot_grid(Oncorhynchusnerka_Tdd_plot,
           Oncorhynchuskisutch_Tdd_plot,
          Oncorhynchusmykiss_Tdd_plot,
          Oncorhynchustshawytscha_Tdd_plot,
          label_x = c(0.2, 0.2, 0.2, 0.2),
          label_y = c(0.96, 0.96, 0.96, 0.96),
          nrow = 4,
          ncol = 1,
          labels = c("a", "c", "e", "g"),
          align = "v")

grid1<-cowplot::plot_grid(p2.size_hist,
                   p2.PREDFIT_full,
                   nrow = 2,
                   ncol =1, 
                   labels = "AUTO",
                   rel_heights = c(0.5, 1)) 

cowplot::plot_grid(grid1, grid2, grid3,
                   ncol=3, nrow=1,
                   rel_widths = c(1, 0.5, 0.5)) %>%
ggsave(filename = "../../ms_exports/Figures/Fig6AB_hist_TPC_update.png",
         width = 11.5, height =9, units = "in" )

cowplot::plot_grid(p1.cm.Lab, p1.cmF,
                   nrow = 1, ncol =2,
                   labels = "AUTO",
                   align = "hv",
                   label_x = c(0.18, 0.18),
                   label_y = c(0.9)) %>%
  ggsave(filename = "../../ms_exports/Figures/Fig7AB-swim_update.png",
         width = 10, height = 5, units = "in")




## [ Not used ] Tunnel tests, swim size ------
p1.cm<-ggplot(data=data.cm.tunnel, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                       colour=Species_latin,
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
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2)+
  geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6)+
  xlim(15, 100)+
  ylim(0, 260)
ggformat(p1.cm, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm<-p1.cm+theme(legend.position = "none")
# p1.cm
# p1.cm<-ggMarginal(p1.cm, groupColour = TRUE, groupFill = TRUE,  type = "histogram", alpha = 1,
#            yparams = list(),  xparams = list(binwidth = 3))


