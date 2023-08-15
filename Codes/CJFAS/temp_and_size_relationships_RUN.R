
# last run aug 15 2023

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
library(here)
library(ggformat2)  # ggplot formatting from github 

source("./Codes/get_dataset.R")
source("./Codes/CJFAS/temp_and_size_species_mod_fnxns.R")
source("./Codes/table_BIC.R")


# 1. Import and organize data sets --------
data.all<-get.adult.salmonid.swim.data(
  data.file = "./Data/Files/Kraskura_salmonSwim_analysis_aug2023.csv")

# available datasets: data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim

# full data set, all tests and measurements
data<-as.data.frame(data.all[1])
# ttf data 
data.ttf<-data[c(data$Test_performance2=="TTF"),]
# field and lab, no TTF - TFF as a whole different category 
data<-data[!c(data$Test_performance2=="TTF"),]
# field data 
dataF<-as.data.frame(data.all[7]) # includes field, jump, and data collected at fishways
# lab data
dataLab<-as.data.frame(data.all[8]) # no TTF
dataLab<-dataLab[!c(dataLab$Test_performance2=="TTF"),]    

# absolute and relative swim speed data
data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]
data.cm<-data[!is.na(data$SWIM_cms),]
data.cm$Reference_number_1<-factor(data.cm$Reference_number_1)

# sanity check everything is subset correctly 
nrow(dataF) + nrow(dataLab) + nrow(data.ttf)  == nrow(data) + nrow(data.ttf)

# 3. Mixed models: TEMP and SIZE swim  ------------
## 3.0. the regression datasets: -----
data.cm.tunnel<-data.cm[c(data.cm$Test_performance2=="Ucrit" | data.cm$Test_performance2=="Umax") & ! data.cm$Species_latin == "Oncorhynchus masou",] # Masu salmon has 1 observation, take out from random effect. 
data.cm.lab<-data.cm[c(data.cm$Test_performance3=="LAB") & c(!is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]

data.cm.field<-data.cm[c(data.cm$Test_performance3=="FIELD" & !is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]
data.cm.field.l<-data.cm[c(data.cm$Test_performance3=="FIELD" & data.cm$SWIM_cms < 250 & !is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]
data.cm.field.h<-data.cm[c(data.cm$Test_performance3=="FIELD" & data.cm$SWIM_cms >= 250 & !is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]

data.cm.swim<-data.cm[c(data.cm$Test_performance2=="Swim" & !is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]
data.cm.jump<-data.cm[c(data.cm$Test_performance2=="Jump" & !is.na(data.cm$Temp_test_mean) & !is.na(data.cm$LENGTH_cm)),]

## 3.1. All data - mixed models, species random intercept -------
mod.size.labS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, dataLab, na.action = na.exclude)
mod.size.tunnelS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.tunnel, na.action = na.exclude)
mod.size.fieldS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field, na.action = na.exclude)
mod.size.field.lS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field.l, na.action = na.exclude)
mod.size.field.hS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.field.h, na.action = na.exclude)
mod.size.swimS<-lm(SWIM_cms~ LENGTH_cm + Species_latin, data.cm.swim, na.action = na.exclude)

mod.size.labT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), dataLab, na.action = na.exclude)
mod.size.tunnelT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), data.cm.tunnel, na.action = na.exclude)
mod.size.fieldT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), data.cm.field, na.action = na.exclude)
mod.size.field.lT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), data.cm.field.l, na.action = na.exclude)
mod.size.field.hT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), data.cm.field.h, na.action = na.exclude)
mod.size.swimT<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE), data.cm.swim, na.action = na.exclude)

mod.size.labTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, dataLab, na.action = na.exclude)
mod.size.tunnelTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, data.cm.tunnel, na.action = na.exclude)
mod.size.fieldTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, data.cm.field, na.action = na.exclude)
mod.size.field.lTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, data.cm.field.l, na.action = na.exclude)
mod.size.field.hTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, data.cm.field.h, na.action = na.exclude)
mod.size.swimTS<-lm(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + Species_latin, data.cm.swim, na.action = na.exclude)

mod.size.labTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, dataLab, na.action = na.exclude)
mod.size.tunnelTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, data.cm.tunnel, na.action = na.exclude)
mod.size.fieldTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, data.cm.field, na.action = na.exclude)
mod.size.field.lTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, data.cm.field.l, na.action = na.exclude)
mod.size.field.hTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, data.cm.field.h, na.action = na.exclude)
mod.size.swimTS.0<-lm(SWIM_cms~ LENGTH_cm + Temp_test_mean + Species_latin, data.cm.swim, na.action = na.exclude)

mmod.size.lab<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), dataLab, na.action = na.exclude)
mmod.size.tunnel<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.tunnel, na.action = na.exclude)
mmod.size.field<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field, na.action = na.exclude)
mmod.size.field.l<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.l, na.action = na.exclude)
mmod.size.field.h<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.field.h, na.action = na.exclude)
mmod.size.swim<-lmer(SWIM_cms~ LENGTH_cm + (1|Species_latin), data.cm.swim, na.action = na.exclude)

mmod.size.labTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), dataLab, na.action = na.exclude)
mmod.size.tunnelTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data.cm.tunnel, na.action = na.exclude)
mmod.size.fieldTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data.cm.field, na.action = na.exclude)
mmod.size.field.lTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data.cm.field.l, na.action = na.exclude)
mmod.size.field.hTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data.cm.field.h, na.action = na.exclude)
mmod.size.swimTS<-lmer(SWIM_cms~ LENGTH_cm + poly(Temp_test_mean, 2, raw = TRUE) + (1|Species_latin), data.cm.swim, na.action = na.exclude)

mmod.size.labTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), dataLab, na.action = na.exclude)
mmod.size.tunnelTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), data.cm.tunnel, na.action = na.exclude)
mmod.size.fieldTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), data.cm.field, na.action = na.exclude)
mmod.size.field.lTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), data.cm.field.l, na.action = na.exclude)
mmod.size.field.hTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), data.cm.field.h, na.action = na.exclude)
mmod.size.swimTS.0<-lmer(SWIM_cms~ LENGTH_cm + Temp_test_mean + (1|Species_latin), data.cm.swim, na.action = na.exclude)


# in all cases, the model with species as random effect is better 
ICdelta(BIC(mod.size.labS, mod.size.labT, mod.size.labTS, mod.size.labTS.0,
        mmod.size.lab, mmod.size.labTS, mmod.size.labTS.0)) # TS species random

ICdelta(BIC(mod.size.tunnelS, mod.size.tunnelT, mod.size.tunnelTS, mod.size.tunnelTS.0,
        mmod.size.tunnel, mmod.size.tunnelTS, mmod.size.tunnelTS.0)) # TS species fixed 

ICdelta(BIC(mod.size.fieldS, mod.size.fieldT, mod.size.fieldTS, mod.size.fieldTS.0,
        mmod.size.field, mmod.size.fieldTS, mmod.size.fieldTS.0)) # TS species random

ICdelta(BIC(mod.size.field.lS, mod.size.field.lT, mod.size.field.lTS, mod.size.field.lTS.0,
        mmod.size.field.l, mmod.size.field.lTS, mmod.size.field.lTS.0)) # .l species random

ICdelta(BIC(mod.size.field.hS, mod.size.field.hT, mod.size.field.hTS, mod.size.field.hTS.0,
        mmod.size.field.h, mmod.size.field.hTS, mmod.size.field.hTS.0)) # TS species random

ICdelta(BIC(mod.size.swimS, mod.size.swimT, mod.size.swimTS, mod.size.swimTS.0,
        mmod.size.swim, mmod.size.swimTS, mmod.size.swimTS.0)) # TS.0 species fixed

# make best model into a different name 
best.model.lab<-mmod.size.labTS
best.model.tunnel<-mod.size.tunnelTS
best.model.field<-mmod.size.fieldTS
best.model.fieldl<-mmod.size.field.l
best.model.fieldh<-mmod.size.field.hTS
best.model.swim<-mod.size.swimTS.0

# REPORTED <<< 
summary(best.model.lab)
summary(best.model.tunnel) # species significant, not used for plotting/ main reports 
summary(best.model.field)
summary(best.model.fieldh)
summary(best.model.fieldl)

car:::Anova(best.model.lab, type ="II")
car:::Anova(best.model.tunnel, type ="II")
car:::Anova(best.model.fieldh, type ="II")
car:::Anova(best.model.fieldl, type ="II")
car:::Anova(best.model.field, type ="II")

# assumptions
plot(best.model.lab)
plot(best.model.tunnel)
plot(best.model.field) # can see the outliers on this, overall ok
plot(best.model.fieldh)
plot(best.model.fieldl)
plot(best.model.swim)

hist(resid(best.model.lab))
hist(resid(best.model.tunnel))
hist(resid(best.model.field)) # can see the outliers on this, overall ok
hist(resid(best.model.fieldh))
hist(resid(best.model.fieldl))
hist(resid(best.model.swim))
# hist(resid(best.model.jump))

# # fixed effects (coefs for plotting)
size.lab.int<-round(fixef(best.model.lab)[1], 2)
size.lab.slope<-round(fixef(best.model.lab)[2], 2)
size.lab.n<-unlist(summary(best.model.lab)[[3]][2])[1]

size.field.int<-round(fixef(best.model.field)[1], 2)
size.field.slope<-round(fixef(best.model.field)[2], 2)
size.field.n<-unlist(summary(best.model.field)[[3]][2])[1]

size.field.h.int<-round(fixef(best.model.fieldh)[1], 2)
size.field.h.slope<-round(fixef(best.model.fieldh)[2], 2)
size.field.h.n<-unlist(summary(best.model.fieldh)[[3]][2])[1]

size.field.l.int<-round(fixef(best.model.fieldl)[1], 2)
size.field.l.slope<-round(fixef(best.model.fieldl)[2], 2)
size.field.l.n<-unlist(summary(best.model.fieldl)[[3]][2])[1]

# *********** END *************************************



# ******************************************************
# 4. Temperature and size species specific relationships: ------------

size.sum.Lab<-dataLab[!is.na(dataLab$Temp_test_mean),] %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(),
            minsize = min(LENGTH_cm, na.rm = TRUE),
            maxsize = max(LENGTH_cm, na.rm = TRUE),
            mintemp = min(Temp_test_mean, na.rm = T),
            maxtemp = max(Temp_test_mean, na.rm = T),
            n_studies = length(unique(Reference_number_1)))

size.sum.Lab.Test<-dataLab[!is.na(dataLab$Temp_test_mean),]%>%
  dplyr::group_by(Test_performance2) %>%
  summarise(n=n(),
            n_studies = length(unique(Reference_number_1)))
size.sum.Lab.Test.NOTincluded<-dataLab[is.na(dataLab$Temp_test_mean),]%>%
  dplyr::group_by(Test_performance2) %>%
  summarise(n=n(),
            n_studies = length(unique(Reference_number_1)))

size.sum.field<-data.cm.field %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE),
            maxsize = max(LENGTH_cm, na.rm = TRUE),
            mintemp = min(Temp_test_mean, na.rm = T),
            maxtemp = max(Temp_test_mean, na.rm = T),
            n_studies = length(unique(Reference_number_1)))

size.sum.tunnel<-data.cm.tunnel %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE),
            maxsize = max(LENGTH_cm, na.rm = TRUE),
            mintemp = min(Temp_test_mean, na.rm = T),
            maxtemp = max(Temp_test_mean, na.rm = T),
            n_studies = length(unique(Reference_number_1)))

size.sum.swim<-data.cm.swim %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE),
            maxsize = max(LENGTH_cm, na.rm = TRUE),
            mintemp = min(Temp_test_mean, na.rm = T),
            maxtemp = max(Temp_test_mean, na.rm = T),
            n_studies = length(unique(Reference_number_1)))

size.sum.jump<-data.cm.jump %>%
  dplyr::group_by(Species_latin) %>%
  summarise(n=n(), minsize = min(LENGTH_cm, na.rm = TRUE),
            maxsize = max(LENGTH_cm, na.rm = TRUE),
            mintemp = min(Temp_test_mean, na.rm = T),
            maxtemp = max(Temp_test_mean, na.rm = T),
            n_studies = length(unique(Reference_number_1)))

# summary of all fits
dataLab %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms ~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
write.csv(file = "./ms_exports/Tables/Table_2.csv", row.names = FALSE)

dataLab[is.na(dataLab$SWIM_cms),]
dataLab %>%
  dplyr::group_by(Species_latin) %>%
  count()
# ******************************************************

# raw with size in the model 
species.temp.size.fits(species = "Oncorhynchus gorbuscha", 
                   temp.sum = size.sum.Lab, 
                   dataform = "raw", 
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Oncorhynchus nerka",
                   temp.sum = size.sum.Lab, 
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Oncorhynchus kisutch",
                   temp.sum = size.sum.Lab, 
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Salmo salar",
                   temp.sum = size.sum.Lab, 
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Oncorhynchus mykiss",
                   temp.sum = size.sum.Lab, 
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Oncorhynchus tshawytscha",
                   temp.sum = size.sum.Lab,
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")
species.temp.size.fits(species = "Oncorhynchus keta",
                   temp.sum = size.sum.Lab,
                   dataform = "raw",
                   data.subset = dataLab,
                   data.subsetID = "Lab")

# Field | only species with n > 10; exclude coho, Oncorhynchus spp., and Atlantics
species.temp.size.fits(species = "Oncorhynchus gorbuscha", 
                   temp.sum = size.sum.field, 
                   dataform = "raw", 
                   data.subset = dataF,
                   data.subsetID = "Field",
                   ylim_high = 1000, ylim_low = 0)
species.temp.size.fits(species = "Oncorhynchus nerka",
                   temp.sum = size.sum.field, 
                   dataform = "raw",
                   data.subset = dataF,
                   data.subsetID = "Field",
                   ylim_high = 1000, ylim_low = 0)
species.temp.size.fits(species = "Oncorhynchus mykiss",
                   temp.sum = size.sum.field, 
                   dataform = "raw",
                   data.subset = dataF,
                   data.subsetID = "Field",
                   ylim_high = 1000, ylim_low = 0)
species.temp.size.fits(species = "Oncorhynchus tshawytscha",
                   temp.sum = size.sum.field,
                   dataform = "raw",
                   data.subset = dataF,
                   data.subsetID = "Field",
                   ylim_high = 1000, ylim_low = 0)
species.temp.size.fits(species = "Oncorhynchus keta",
                   temp.sum = size.sum.field,
                   dataform = "raw",
                   data.subset = dataF,
                   data.subsetID = "Field",
                   ylim_high = 1000, ylim_low = 0)

# rbind the function-returned datasets fir each species, with predicted CIs. / used for plotting 
dd_SizeLab<-rbind(Oncorhynchusgorbuscha_dataSizeLab,
               Oncorhynchuskisutch_dataSizeLab,
               Oncorhynchusmykiss_dataSizeLab,
               Oncorhynchusnerka_dataSizeLab,
               Oncorhynchustshawytscha_dataSizeLab,
               Salmosalar_dataSizeLab,
               Oncorhynchusketa_dataSizeLab)

dd_TempLab<-rbind(Oncorhynchusgorbuscha_dataTemp_Lab,
               Oncorhynchuskisutch_dataTemp_Lab,
               Oncorhynchusmykiss_dataTemp_Lab,
               Oncorhynchusnerka_dataTemp_Lab,
               Oncorhynchustshawytscha_dataTemp_Lab,
               Salmosalar_dataTemp_Lab,
               Oncorhynchusketa_dataTemp_Lab)

dd_SizeField<-rbind(Oncorhynchusgorbuscha_dataSizeField,
               Oncorhynchusmykiss_dataSizeField,
               Oncorhynchusnerka_dataSizeField,
               Oncorhynchustshawytscha_dataSizeField,
               Oncorhynchusketa_dataSizeField)

dd_TempField<-rbind(Oncorhynchusgorbuscha_dataTemp_Field,
               Oncorhynchusmykiss_dataTemp_Field,
               Oncorhynchusnerka_dataTemp_Field,
               Oncorhynchustshawytscha_dataTemp_Field,
               Oncorhynchusketa_dataTemp_Field)

pred.temp<-as.data.frame(expand.grid(Temp_test_mean = seq(min(data.cm$Temp_test_mean, na.rm =T), max(data.cm$Temp_test_mean, na.rm =T), 0.5), 
                                   LENGTH_cm = mean(data.cm$LENGTH_cm, na.rm =T),
                                   Species_latin = c("Oncorhynchus nerka")))

pred.size<-as.data.frame(expand.grid(Temp_test_mean = mean(data.cm$Temp_test_mean, na.rm =T), 
                                   LENGTH_cm = seq(min(data.cm$LENGTH_cm, na.rm =T), max(data.cm$LENGTH_cm, na.rm =T), 1),
                                   Species_latin = c("Oncorhynchus nerka")))

# Predicted data:  Swim, jump, ucrit, umax
predsLab <- predictInterval(best.model.lab, newdata = pred.temp, n.sims = 2000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.temp$fit.mod.lab <- predsLab[,1]
pred.temp$fit.mod.lab.CI.l<-predsLab[,3]
pred.temp$fit.mod.lab.CI.h<-predsLab[,2]
predsfield <- predictInterval(best.model.field, newdata = pred.temp, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.temp$fit.mod.field <- predsfield[,1]
pred.temp$fit.mod.field.CI.l<-predsfield[,3]
pred.temp$fit.mod.field.CI.h<-predsfield[,2]
predsfield.l <- predictInterval(best.model.fieldl, newdata = pred.temp, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.temp$fit.mod.field.l <- predsfield.l[,1]
pred.temp$fit.mod.field.l.CI.l<-predsfield.l[,3]
pred.temp$fit.mod.field.l.CI.h<-predsfield.l[,2]
predsfield.h <- predictInterval(best.model.fieldh, newdata = pred.temp, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.temp$fit.mod.field.h <- predsfield.h[,1]
pred.temp$fit.mod.field.h.CI.l<-predsfield.h[,3]
pred.temp$fit.mod.field.h.CI.h<-predsfield.h[,2]

predsLab <- predictInterval(best.model.lab, newdata = pred.size, n.sims = 100000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.size$fit.mod.lab <- predsLab[,1]
pred.size$fit.mod.lab.CI.l<-predsLab[,3]
pred.size$fit.mod.lab.CI.h<-predsLab[,2]
predsfield <- predictInterval(best.model.field, newdata = pred.size, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.size$fit.mod.field <- predsfield[,1]
pred.size$fit.mod.field.CI.l<-predsfield[,3]
pred.size$fit.mod.field.CI.h<-predsfield[,2]
predsfield.l <- predictInterval(best.model.fieldl, newdata = pred.size, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.size$fit.mod.field.l <- predsfield.l[,1]
pred.size$fit.mod.field.l.CI.l<-predsfield.l[,3]
pred.size$fit.mod.field.l.CI.h<-predsfield.l[,2]
predsfield.h <- predictInterval(best.model.fieldh, newdata = pred.size, n.sims = 10000, which = "fixed") # nas produced where NAs in the data, using whole datasets for predicting
pred.size$fit.mod.field.h <- predsfield.h[,1]
pred.size$fit.mod.field.h.CI.l<-predsfield.h[,3]
pred.size$fit.mod.field.h.CI.h<-predsfield.h[,2]


# Predicted data: tunnel Ucrit and Umax
preds <- predict(best.model.tunnel) # nas produced where NAs in the data, using whole datasets for predicting 
data.cm.tunnel$fit.mod.ALL2 <- preds


# Fig7. Scaling  ------------
## Fig 7A [lab] ------
p1.cm.Lab<-ggplot(data=dataLab, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                    colour=Species_latin,
                                    label=Reference_number_1))+
  geom_rect(data = data ,mapping = aes(xmin = 57, xmax = 62, ymin = 230, ymax = 600, fill = NULL),  fill = "white", color = "grey", linetype = "dotted", alpha = .2)+
  annotate("text", x = 63, y = 500, label = "Jump", hjust = 0, size = 5)+
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
  annotate(geom = "text", x =72, y = 885, label = expression(bold("Lab tests")),
           color = "black", size=5, hjust = 0)+
  annotate(geom = "text", y = 835, x = 72, hjust = 0, color = "black", size = 3,
         label = bquote( U[crit] * ": N = " * .(size.sum.Lab.Test$n[3]) ~ " (n = " * .(size.sum.Lab.Test$n_studies[4]) * ")"))+
  annotate(geom = "text", y = 785, x = 72, hjust = 0, color = "black", size = 3,
         label = bquote( U[max] * ": N = " * .(size.sum.Lab.Test$n[4]) ~ " (n = " * .(size.sum.Lab.Test$n_studies[3]) * ")"))+
  annotate(geom = "text", y = 735, x = 72, hjust = 0, color = "black", size = 3,
         label = bquote( Swim * "*: N = " * .(size.sum.Lab.Test.NOTincluded$n[2]) ~ " (n = " * .(size.sum.Lab.Test.NOTincluded$n_studies[2]) * ")"))+
  annotate(geom = "text", y = 685, x = 72, hjust = 0, color = "black", size = 3,
         label = bquote( Jump * "*: N = " * .(size.sum.Lab.Test.NOTincluded$n[1]) ~ " (n = " * .(size.sum.Lab.Test.NOTincluded$n_studies[1]) * ")"))+
  geom_ribbon(data=pred.size,
              aes(y = NULL, ymin = fit.mod.lab.CI.l, ymax = fit.mod.lab.CI.h,
                  fill=Species_latin, colour=Species_latin, group = Species_latin, label = NULL),
              alpha = 0.1, color = "grey30", fill = "grey10", linetype = "dashed")+
  geom_line(data = pred.size, mapping =aes(y=fit.mod.lab, x=LENGTH_cm, label=NULL), color = "black", size=0.6)+
  xlim(15, 100)+
  scale_y_continuous(limits = c(0,900), breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900))
ggformat(p1.cm.Lab, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm.Lab<-p1.cm.Lab +
  theme (axis.ticks.x = element_line(size = 1),
         # axis.text.x = element_text(angle=45, hjust=1, size=12),
         legend.title = element_blank(),
         legend.position = c(0.17,0.75),
         legend.key.size = unit(0.3, 'cm'),
         legend.text = element_text(face = "italic"),
         legend.key.width = unit(0.5, "cm"))
p1.cm.Lab

## Fig 7B [field] ------
p1.cmF<-ggplot(data=data.cm.field, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                      colour=Species_latin,
                                      label=Reference_number_1, 
                                      shape = Test_performance2))+
  annotate(geom = "text", y = 750, x = 15, hjust = 0, color = "black", size = 3,
         label = bquote( "N = " * .(size.field.n)))+
  annotate(geom = "text", y = 700, x = 15, hjust = 0, color = "black", size = 3,
         label = bquote( "N (> 250 cm/s) = " * .(size.field.h.n)))+
  annotate(geom = "text", y = 650, x = 15, hjust = 0, color = "black", size = 3,
         label = bquote( "N (< 250 cm/s) = " * .(size.field.l.n)))+
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
  geom_point(alpha=0.4, size = 2)+
  scale_shape_manual(values = c(21,23))+
  annotate(geom = "text", x =15, y = 800, label = expression(bold("Field")), color = "black", size=5, hjust = 0)+
  # geom_ribbon(data=pred.size,
  #             aes(y = NULL, ymin = fit.mod.field.h.CI.l, ymax = fit.mod.field.h.CI.h,
  #                 fill=Species_latin, colour=Species_latin, group = Species_latin, label =NULL ),
  #             alpha = 0.1, color = "grey30", fill = "grey10", linetype = "dashed")+
  geom_line(data = pred.size, mapping =aes(y=fit.mod.field.h, x=LENGTH_cm, label=NULL, shape = NULL), color = "black", size=0.6, linetype = "dashed")+
  # geom_ribbon(data=pred.size,
  #             aes(y = NULL, ymin = fit.mod.field.l.CI.l, ymax = fit.mod.field.l.CI.h,
  #                 fill=Species_latin, colour=Species_latin, group = Species_latin, label =NULL ),
  #             alpha = 0.1, color = "grey30", fill = "grey10", linetype = "dashed")+
  geom_line(data = pred.size, mapping =aes(y=fit.mod.field.l, x=LENGTH_cm, label=NULL, shape = NULL), color = "black", size=0.6, linetype = "dashed")+
  geom_hline(yintercept = 250, color = "grey", size=0.3, lty=2)+
  xlim(15, 100)+
  scale_y_continuous(limits = c(0,900), breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900))

ggformat(p1.cmF, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cmF<-p1.cmF+theme(legend.position = "none")
p1.cmF


# Fig6. Temperature and swim ----------------------
# Fig6B. main fish with Topt ----------
dd_TempLab_Topt<-dd_TempLab[!duplicated(dd_TempLab$Topt) & !dd_TempLab$Species_latin == "Oncorhynchus keta",]
dd_TempLab_Topt$y <- c(26, 26, 26, 15, 26, 5 )
dd_TempLab_Topt$x <- c(28.164158, 13, 18, 16,8, 17.5)

p2.PREDFIT_full <- ggplot(data = dd_TempLab[!c(dd_TempLab$Species_latin == "Oncorhynchus keta" | dd_TempLab$Species_latin == "Oncorhynchus nerka"),], aes(y=pred.mod, x=Temp_test_mean, fill=Species_latin,
                                        colour = Species_latin,
                                        group = Species_latin,
                                        label = round(Topt,1)))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_ribbon(data=pred.temp,
              aes(y = NULL, ymin = fit.mod.lab.CI.l, ymax = fit.mod.lab.CI.h,
                  fill=NULL, colour=NULL, group = Species_latin, label =NULL),
              alpha = 0.25, fill = "grey60",  linetype = "dashed")+
  geom_segment(data = dd_TempLab[!c(dd_TempLab$Species_latin == "Oncorhynchus keta"),],  mapping = aes(x = Topt, y = 30, xend = Topt, yend = 50, colour = Species_latin, fill = NULL, shape = NULL),
               arrow = arrow(length = unit(0.25, "cm")),
               lty="solid", size=0.2, alpha = 0.2)+
  geom_line(data = dd_TempLab[c(dd_TempLab$Species_latin == "Oncorhynchus nerka"),],
            mapping = aes(y=pred.mod, x=Temp_test_mean, colour = Species_latin,
                                        group = Species_latin,label = round(Topt,1)),
            linetype="dotted", alpha = 0.7, lineend = "round", size = 1.5)+
  geom_line(aes(y=pred.mod, x= Temp_test_mean), size=2, lineend = "round")+ 
  geom_text(data = dd_TempLab_Topt, mapping = aes(x = x, y = y, colour = Species_latin, fill = NULL, shape = NULL),
                  angle = 0, size= 4.5,  fontface = "bold", alpha = 1)+
  # geom_text(mapping = aes( x =fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch","opt.T"], y = 0, colour = Species_latin, fill = NULL, shape = NULL), angle = 90, label = round(fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch","opt.T"],1), size=2.6, label.size = 0, fontface = "bold",  data = fit.peaks[fit.peaks$Species_latin=="Oncorhynchus kisutch",], alpha = 1)+
  # geom_text(mapping = aes( x =21, y = 8), label = "Ucrit & Umax", color = "black", size=4.5)+
  # geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black", size=0.3)+  
  scale_shape_manual(values = c(21,23))+
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30))+
  ylim(0, 200)
ggformat(p2.PREDFIT_full, print=F, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.PREDFIT_full<-p2.PREDFIT_full+theme(legend.position = "none",
                                       plot.margin=margin(t = 0, unit="cm", r = 0.1, l = 0.5))
p2.PREDFIT_full

# Fig6A. size distr ----------
p2.size_hist<-
  ggplot(data[!data$Species_latin == "Oncorhynchus spp.", ], aes(x = LENGTH_cm, y = Species_latin, fill = Species_latin, color = Species_latin)) +
  geom_density_ridges(stat = "binline", alpha = 0.7, scale = 3,size=0.3)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_x_continuous(limits = c(25, 120), breaks = c(30, 40, 50, 60, 70, 80, 90))+
  annotate(geom = "text", y = 8.25, x = 85, hjust = 0, color = "#64B0BC", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 7.25, x = 85, hjust = 0, color = "#70Af81", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 6.25, x = 85, hjust = 0, color = "#EA573D", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 5.25, x = 85, hjust = 0, color = "#615B70", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 4.25, x = 85, hjust = 0, color = "#446699", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 3.25, x = 85, hjust = 0, color = "#FBC063", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 2.25, x = 85, hjust = 0, color = "#FB9A62", size = 4,
           label = bquote(bar(x) ~ .(round(mean(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "(" * .(round(min(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) ~
                            "-" ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "LENGTH_cm"], na.rm = TRUE),2)) * ")"))+
  annotate(geom = "text", y = 1.25, x = 85, hjust = 0, color = "#D292CD", size = 4,
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
  guides(color=guide_legend(ncol = 4, byrow=TRUE)) +
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

# Oncorhynchusnerka_plotTempLab,
grid2<-plot_grid(
          Oncorhynchusgorbuscha_plotTempLab,
          Oncorhynchusketa_plotTempLab,
          Salmosalar_plotTempLab,
          nrow = 3,
          ncol = 1,
          labels = c("A", "C", "E"),
          label_x = c(0.2, 0.2, 0.2),
          label_y = c(0.96, 0.96, 0.96),
          align = "hvr")

grid3<-plot_grid(
          Oncorhynchuskisutch_plotTempLab,
          Oncorhynchusmykiss_plotTempLab,
          Oncorhynchustshawytscha_plotTempLab,
          label_x = c(0.17, 0.17, 0.17),
          label_y = c(0.96, 0.96, 0.96),
          nrow = 3,
          ncol = 1,
          labels = c("B", "D", "F"),
          align = "v")

grid4<-cowplot::plot_grid(grid2, grid3)
grid5<-cowplot::plot_grid(legend, grid4,
                          ncol=1, nrow=2,
                          rel_heights = c(0.1, 1))

grid1<-cowplot::plot_grid(
                          p2.PREDFIT_full,
                          p2.size_hist,
                          nrow = 2,
                          ncol =1, 
                          labels = c("G", "H"),
                          label_x = c(0.05, 0.05),
                          rel_heights = c( 0.9, 0.6)) 

cowplot::plot_grid(grid5, grid1, 
                   ncol=2, nrow=1,
                   label = "AUTO",
                   align = "hv",
                   rel_widths = c(0.5, 0.5,1)) %>%
ggsave(filename = "./ms_exports/Figures/Fig6AB_hist_TPC_update.png",
         width = 10, height =7, units = "in" )

cowplot::plot_grid(p1.cm.Lab, p1.cmF,
                   nrow = 1, ncol =2,
                   labels = "AUTO",
                   align = "hv",
                   label_x = c(0.18, 0.18),
                   label_y = c(0.9)) %>%
  ggsave(filename = "./ms_exports/Figures/Fig7AB-swim_update.png",
         width = 10, height = 5, units = "in")



## POPULATION PLOT SOCKEYE ----------------
dataLab %>%
  dplyr::group_by(Species_latin) %>%
  do(broom::tidy(lm(SWIM_cms ~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, ., na.action=na.exclude))) %>% 
  ungroup() %>% 
  as.data.frame()

dataLab %>%
  dplyr::group_by(Species_latin) %>%
  dplyr::summarize(n = n())
  
data.soc<-dataLab[dataLab$Species_latin == "Oncorhynchus nerka", ]
data.soc.sufficient<-data.soc[c(data.soc$population == "Chilko Lake" | 
                                data.soc$population == "Early Stuart" |
                                data.soc$population == "Gates Creek" |
                                data.soc$population == "Quesnel" |
                                data.soc$population == "Somass River Aggregate" |
                                data.soc$population == "Weaver Creek " |
                                data.soc$population == "Stellako" |
                                data.soc$population == "Lower Adams") & !is.na(data.soc$population),]

data.soc.sufficient<-data.soc.sufficient %>%
    mutate(Surgery2 = if_else(is.na(Surgery), "No Surgery", "Surgery"))

data.soc.sufficient %>%
  dplyr::group_by(population, Surgery2) %>%
  dplyr::summarize(n = n())

chilko<-data.soc[data.soc$population == "Chilko Lake",]
chilko<-chilko[-c(which(grepl("NA", rownames(chilko)))),]
ES<-data.soc[data.soc$population == "Early Stuart",]
ES<-ES[-c(which(grepl("NA", rownames(ES)))),]
GC<-data.soc[data.soc$population == "Gates Creek",]
GC<-GC[-c(which(grepl("NA", rownames(GC)))),]
quesnel<-data.soc[data.soc$population == "Quesnel",]
quesnel<-quesnel[-c(which(grepl("NA", rownames(quesnel)))),]
Somass<-data.soc[data.soc$population == "Somass River Aggregate",]
Somass<-Somass[-c(which(grepl("NA", rownames(Somass)))),]
WC<-data.soc[data.soc$population == "Weaver Creek ",]
WC<-WC[-c(which(grepl("NA", rownames(WC)))),]
LA<-data.soc[data.soc$population == "Lower Adams",]
LA<-LA[-c(which(grepl("NA", rownames(LA)))),]
Ste<-data.soc[data.soc$population == "Stellako",]
Ste<-Ste[-c(which(grepl("NA", rownames(Ste)))),]

mod.chilko<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = chilko)
mod.ES<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = ES)
mod.GC<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = GC)
mod.quesnel<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = quesnel)
mod.Somass<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = Somass)
mod.WC<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = WC)
mod.LA<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = LA)
mod.Ste<-lm(SWIM_cms~ poly(Temp_test_mean, 2, raw = T) + LENGTH_cm, na.action=na.exclude, data = Ste)

summary(mod.chilko)
summary(mod.ES)
summary(mod.GC)
summary(mod.quesnel)
summary(mod.Somass)
summary(mod.WC)
summary(mod.LA)
summary(mod.Ste)

chilko.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(chilko$Temp_test_mean, na.rm =T),
                                                            max(chilko$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(chilko$LENGTH_cm, na.rm =T)))
chilko.pred$pred.val<-predict(mod.chilko, newdata = chilko.pred, interval = "confidence")[,1]
chilko.pred$pred.modCI.h<-predict(mod.chilko, newdata = chilko.pred, interval = "confidence")[,2]
chilko.pred$pred.modCI.l<-predict(mod.chilko, newdata = chilko.pred, interval = "confidence")[,3]
chilko.pred$population<-"Chilko Lake"

ES.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(ES$Temp_test_mean, na.rm =T),
                                                            max(ES$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(ES$LENGTH_cm, na.rm =T)))
ES.pred$pred.val<-predict(mod.ES, newdata = ES.pred, interval = "confidence")[,1]
ES.pred$pred.modCI.h<-predict(mod.ES, newdata = ES.pred, interval = "confidence")[,2]
ES.pred$pred.modCI.l<-predict(mod.ES, newdata = ES.pred, interval = "confidence")[,3]
ES.pred$population<-"Early Stuart"


GC.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(GC$Temp_test_mean, na.rm =T),
                                                            max(GC$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(GC$LENGTH_cm, na.rm =T)))
GC.pred$pred.val<-predict(mod.GC, newdata = GC.pred, interval = "confidence")[,1]
GC.pred$pred.modCI.h<-predict(mod.GC, newdata = GC.pred, interval = "confidence")[,2]
GC.pred$pred.modCI.l<-predict(mod.GC, newdata = GC.pred, interval = "confidence")[,3]
GC.pred$population<-"Gates Creek"


quesnel.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(quesnel$Temp_test_mean, na.rm =T),
                                                            max(quesnel$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(quesnel$LENGTH_cm, na.rm =T)))
quesnel.pred$pred.val<-predict(mod.quesnel, newdata = quesnel.pred, interval = "confidence")[,1]
quesnel.pred$pred.modCI.h<-predict(mod.quesnel, newdata = quesnel.pred, interval = "confidence")[,2]
quesnel.pred$pred.modCI.l<-predict(mod.quesnel, newdata = quesnel.pred, interval = "confidence")[,3]
quesnel.pred$population<-"Quesnel"


Somass.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(Somass$Temp_test_mean, na.rm =T),
                                                            max(Somass$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(Somass$LENGTH_cm, na.rm =T)))
Somass.pred$pred.val<-predict(mod.Somass, newdata = Somass.pred, interval = "confidence")[,1]
Somass.pred$pred.modCI.h<-predict(mod.Somass, newdata = Somass.pred, interval = "confidence")[,2]
Somass.pred$pred.modCI.l<-predict(mod.Somass, newdata = Somass.pred, interval = "confidence")[,3]
Somass.pred$population<-"Somass River Aggregate"

WC.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(WC$Temp_test_mean, na.rm =T),
                                                            max(WC$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(WC$LENGTH_cm, na.rm =T)))
WC.pred$pred.val<-predict(mod.WC, newdata = WC.pred, interval = "confidence")[,1]
WC.pred$pred.modCI.h<-predict(mod.WC, newdata = WC.pred, interval = "confidence")[,2]
WC.pred$pred.modCI.l<-predict(mod.WC, newdata = WC.pred, interval = "confidence")[,3]
WC.pred$population<-"Weaver Creek "

LA.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(LA$Temp_test_mean, na.rm =T),
                                                            max(LA$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(LA$LENGTH_cm, na.rm =T)))
LA.pred$pred.val<-predict(mod.LA, newdata = LA.pred, interval = "confidence")[,1]
LA.pred$pred.modCI.h<-predict(mod.LA, newdata = LA.pred, interval = "confidence")[,2]
LA.pred$pred.modCI.l<-predict(mod.LA, newdata = LA.pred, interval = "confidence")[,3]
LA.pred$population<-"Lower Adams"


Ste.pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(Ste$Temp_test_mean, na.rm =T),
                                                            max(Ste$Temp_test_mean, na.rm =T), 0.5), 
                                       LENGTH_cm = mean(Ste$LENGTH_cm, na.rm =T)))
Ste.pred$pred.val<-predict(mod.Ste, newdata = Ste.pred, interval = "confidence")[,1]
Ste.pred$pred.modCI.h<-predict(mod.Ste, newdata = Ste.pred, interval = "confidence")[,2]
Ste.pred$pred.modCI.l<-predict(mod.Ste, newdata = Ste.pred, interval = "confidence")[,3]
Ste.pred$population<-"Stellako"


data.pop.soc<-rbind(WC.pred, chilko.pred, quesnel.pred, Somass.pred, ES.pred, GC.pred, LA.pred, Ste.pred)
data.pop.soc$Species_latin<-"Oncorhynchus nerka"


plot.soc<-ggplot(data=data.soc.sufficient, mapping = aes(y=SWIM_cms, x=Temp_test_mean, colour=Species_latin, shape = Surgery2))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_point( alpha=1)+
  facet_wrap(.~population)+
  scale_shape_manual(values = c(19, 21, 20, 22, 3, 23))+
  ylim(0, 250)+
  geom_line(data = data.pop.soc, aes(y=pred.val, x=Temp_test_mean, shape = NULL), color = "black")+  
  geom_ribbon(data = data.pop.soc,
              aes(y = NULL,
              shape = NULL,
              ymin = pred.modCI.l,
              ymax = pred.modCI.h,
              fill=Species_latin,
              colour=Species_latin,
              group = population),
              alpha = 0.3)+
  scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
ggformat(plot.soc, print=F, y_title = " Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
plot.soc<-plot.soc+theme(legend.position = "right")
plot.soc



plot.soc2<-ggplot(data=data.soc.sufficient, mapping = aes(y=SWIM_cms, x=Temp_test_mean,
                                                          colour=Species_latin,
                                                          group = population,
                                                          shape = Surgery2,
                                                          label = Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_point( alpha=1, size=1)+
  facet_wrap(.~population, nrow = 4)+
  scale_shape_manual(values = c(19, 1))+
  ylim(0, 250)+
  geom_smooth(method= "lm", formula = y ~ poly(x, 2, raw = T))+
  # geom_text(size=1, check_overlap = T)+
  # geom_line(data = data.pop.soc, aes(y=pred.val, x=Temp_test_mean, group = population), color = "#EA573D", size = 1)+  
  # geom_ribbon(data = data.pop.soc,
  #             aes(y = NULL,
  #             shape = NULL,
  #             ymin = pred.modCI.l,
  #             ymax = pred.modCI.h,
  #             fill=Species_latin,
  #             colour=Species_latin,
  #             group = population),
  #             alpha = 0.3)+
  scale_x_continuous(limits = c(8, 27), breaks = c(8, 10, 15, 20, 25))
ggformat(plot.soc2, print=F, y_title = " Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
plot.soc2<-plot.soc2+theme(legend.position = "top", legend.title = element_blank())
plot.soc2

ggsave(filename = "./ms_exports/Figures/Fig6-SOCKEYE.png",plot.soc,
         width = 7, height = 6, units = "in")



  ## NOT USED --------
  # 
  # # *********** START ***************************************
  # ## 3.2. Field. INCLUDES ONE JUMP STUDY: species-specific, cm/s -------
  # # 1. pink : "Oncorhynchus gorbuscha"
  # mod.size.field.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),])
  # CI.pink<-confint(mod.size.field.pink)
  # par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
  # # plot(mod.size.field.pink)
  # data.pred.CI.field.pink<-as.data.frame(predict(mod.size.field.pink, interval = "confidence")) 
  # 
  # # 2. sockeye: Oncorhynchus nerka
  # mod.size.field.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),])
  # CI.soc<-confint(mod.size.field.soc)
  # # plot(mod.size.field.soc)
  # data.pred.CI.field.soc<-as.data.frame(predict(mod.size.field.soc, interval = "confidence")) 
  # 
  # # 3. trouts: Oncorhynchus mykiss
  # mod.size.field.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),])
  # CI.trouts<-confint(mod.size.field.trouts)
  # # plot(mod.size.field.trouts)
  # data.pred.CI.field.trouts<-as.data.frame(predict(mod.size.field.trouts, interval = "confidence")) 
  # 
  # # 4. chinook: Oncorhynchus tshawytscha
  # mod.size.field.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),])
  # CI.chin<-confint(mod.size.field.chin)
  # # plot(mod.size.field.chin) # bad
  # data.pred.CI.field.chin<-as.data.frame(predict(mod.size.field.chin, interval = "confidence")) 
  # 
  # # 5. chum: Oncorhynchus keta
  # mod.size.field.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),])
  # CI.chum<-confint(mod.size.field.chum)
  # # plot(mod.size.field.chum)
  # data.pred.CI.field.chum<-as.data.frame(predict(mod.size.field.chum, interval = "confidence")) 
  # 
  # # summaries 
  # data.cm.field.sp<-data.cm.field[!(data.cm.field$Species_latin == "Oncorhynchus masou" | # n = 4
  #                                  data.cm.field$Species_latin == "Salmo salar" | # n = 3
  #                                  data.cm.field$Species_latin == "Oncorhynchus kisutch" | # n = 5
  #                                     data.cm.field$Species_latin == "Oncorhynchus spp." ),] # 2
  # data.cm.field.sp$Species_latin<-factor(data.cm.field.sp$Species_latin)
  # # No species specific fits, just not that many data points, low size range, not reliable
  # data.cm.field.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))
  # # summary of all fits
  # data.cm.field.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  #   ungroup()
  # # *********** END *************************************
  # 
  # 
  # # only low swim speeds (comparable with Ucrit, < 250 cm/s)
  # 
  # data.cm.field.l.sp<-data.cm.field.l[!(data.cm.field.l$Species_latin == "Oncorhynchus masou" |
  #                                     data.cm.field.l$Species_latin == "Salmo salar" |
  #                                     data.cm.field.l$Species_latin == "Oncorhynchus kisutch" |
  #                                     data.cm.field.l$Species_latin == "Oncorhynchus spp."),]
  # data.cm.field.l.sp$Species_latin<-factor(data.cm.field.l.sp$Species_latin)
  # 
  # 
  # # No species specific fits, just not that many data points, low size range, not reliable
  # data.cm.field.l.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))
  # 
  # # for summary of all fits 
  # data.cm.field.l.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  #   ungroup()
  # 
  # # And high speeds:
  # data.cm.field.h.sp<-data.cm.field.h[!(data.cm.field.h$Species_latin == "Oncorhynchus masou" |
  #                                     data.cm.field.h$Species_latin == "Salmo salar" |
  #                                     data.cm.field.h$Species_latin == "Oncorhynchus kisutch" |
  #                                     data.cm.field.h$Species_latin == "Oncorhynchus spp."),]
  # data.cm.field.h.sp$Species_latin<-factor(data.cm.field.h.sp$Species_latin)
  # 
  # 
  # # No species specific fits, just not that many data points, low size range, not reliable
  # data.cm.field.h.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   summarise(n=n(), minsize = min(LENGTH_cm), maxsize = max(LENGTH_cm), n_studies = length(unique(Reference_number_1)))
  # 
  # # for summary of all fits 
  # data.cm.field.h.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  #   ungroup()
  # # *********** END **************************************
  # 
  # 
  # ## 3.3. Tunnel swim speeds UCRIT AND UMAX ONLY: species-specific, cm/s ---------
  # # ******************************************************
  # # species specific fits, CI of models, resid plots
  # # 1. pink : "Oncorhynchus gorbuscha"
  # mod.size.tunnel.pink<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus gorbuscha"),])
  # CI.tunnelpink<-confint(mod.size.tunnel.pink)
  # # plot(mod.size.tunnel.pink)
  # data.pred.CI.tunnel.pink<-as.data.frame(predict(mod.size.tunnel.pink, interval = "confidence")) 
  # 
  # # 2. sockeye: Oncorhynchus nerka
  # mod.size.tunnel.soc<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus nerka"),])
  # CI.tunnelsoc<-confint(mod.size.tunnel.soc)
  # # plot(mod.size.tunnel.soc)
  # data.pred.CI.tunnel.soc<-as.data.frame(predict(mod.size.tunnel.soc, interval = "confidence")) 
  # 
  # # 3. trouts: Oncorhynchus mykiss
  # mod.size.tunnel.trouts<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus mykiss"),])
  # CI.tunneltrouts<-confint(mod.size.tunnel.trouts)
  # # plot(mod.size.tunnel.trouts)
  # data.pred.CI.tunnel.trouts<-as.data.frame(predict(mod.size.tunnel.trouts, interval = "confidence")) 
  # 
  # # 4. chinook: Oncorhynchus tshawytscha
  # mod.size.tunnel.chin<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus tshawytscha"),])
  # CI.tunnelchin<-confint(mod.size.tunnel.chin)
  # # plot(mod.size.tunnel.chin) 
  # data.pred.CI.tunnel.chin<-as.data.frame(predict(mod.size.tunnel.chin, interval = "confidence")) 
  # 
  # # 5. chum: Oncorhynchus keta
  # mod.size.tunnel.chum<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus keta"),])
  # CI.tunnelchum<-confint(mod.size.tunnel.chum)
  # # plot(mod.size.tunnel.chum)
  # data.pred.CI.tunnel.chum<-as.data.frame(predict(mod.size.tunnel.chum, interval = "confidence")) 
  # 
  # # 6. Atlantics: Salmo salar
  # mod.size.tunnel.salar<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Salmo salar"),])
  # CI.tunnelsalar<-confint(mod.size.tunnel.salar)
  # # plot(mod.size.tunnel.salar)
  # data.pred.CI.tunnel.salar<-as.data.frame(predict(mod.size.tunnel.salar, interval = "confidence")) 
  # 
  # # 7. coho : "Oncorhynchus kisutch"
  # mod.size.tunnel.coho<-lm(SWIM_cms~ LENGTH_cm, na.action=na.exclude, data = data.cm.tunnel[(data.cm.tunnel$Species_latin == "Oncorhynchus kisutch"),])
  # CI.tunnelcoho<-confint(mod.size.tunnel.coho)
  # # plot(mod.size.tunnel.coho)
  # data.pred.CI.tunnel.coho<-as.data.frame(predict(mod.size.tunnel.coho, interval = "confidence")) 
  # 
  # # summary of all fits 
  # data.cm.tunnel.sp<-data.cm.tunnel[!(data.cm.tunnel$Species_latin == "Oncorhynchus masou"),]
  # data.cm.tunnel.sp$Species_latin<-factor(data.cm.tunnel.sp$Species_latin)
  # data.cm.tunnel.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  #   ungroup()
  # 
  # ## 3.4. Lab swim speeds: SWIM, UCRIT, UMAX, 1 JUMP STUDY: species-specific, cm/s ---------
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
  # 
  # # summary of all fits 
  # dataLab.sp<-dataLab[!(dataLab$Species_latin == "Oncorhynchus masou"),]
  # dataLab.sp$Species_latin<-factor(dataLab.sp$Species_latin)
  # dataLab.sp %>%
  #   dplyr::group_by(Species_latin) %>%
  #   do(broom::tidy(lm(SWIM_cms~ LENGTH_cm, ., na.action=na.exclude))) %>% 
  #   ungroup()
  # 
  # # ******************************************************
  # # *********** END **************************************
  # 
  # 
  # 
  # ## 4. Function use (species specific, get CI, predicted data, plots) ------------
  # species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus gorbuscha"),],
  #                    model=mod.size.field.pink,
  #                    species="Oncorhynchus gorbuscha",
  #                    sum.file=size.sum.field, 
  #                    test = "field")
  # species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus tshawytscha"),],
  #                    model=mod.size.field.chin,
  #                    species="Oncorhynchus tshawytscha",
  #                    sum.file=size.sum.field,
  #                    test = "field")
  # species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus nerka"),],
  #                    model=mod.size.field.soc,
  #                    species="Oncorhynchus nerka",
  #                    sum.file=size.sum.field,
  #                    test = "field")
  # species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus mykiss"),],
  #                    model=mod.size.field.trouts,
  #                    species="Oncorhynchus mykiss",
  #                    sum.file=size.sum.field,
  #                    test = "field")
  # species.size.plots(dd=data.cm.field[(data.cm.field$Species_latin == "Oncorhynchus keta"),],
  #                    model=mod.size.field.chum,
  #                    species="Oncorhynchus keta",
  #                    sum.file=size.sum.field)
  # 
  # # # Lab temp only, (standardised size, no size)
  # # species.temp.size.fits(species = "Oncorhynchus gorbuscha", 
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",
  # #                    scale.fish.size = T,)
  # # species.temp.size.fits(species = "Oncorhynchus nerka",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw", scale.fish.size = T)
  # # species.temp.size.fits(species = "Oncorhynchus kisutch",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",scale.fish.size = T)
  # # species.temp.size.fits(species = "Salmo salar",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",scale.fish.size = T)
  # # species.temp.size.fits(species = "Oncorhynchus mykiss",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",scale.fish.size = T)
  # # species.temp.size.fits(species = "Oncorhynchus tshawytscha",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",scale.fish.size = T)
  # # species.temp.size.fits(species = "Oncorhynchus keta",
  # #                    temp.sum = temp.sumI, 
  # #                    dataform = "raw",scale.fish.size = T)
  # # 
  # 
  # ## [ Plot: Not used ] Tunnel tests, swim size ------
  # p1.cm<-ggplot(data=data.cm.tunnel, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
  #                                        colour=Species_latin,
  #                                        label=Reference_number_1))+
  #   scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
  #                      labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
  #                      values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  #   scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
  #                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
  #                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  #   geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
  #                     ymax=SWIM_cms+SWIM_cms_SD), size =0.2, alpha=0.6)+
  #   geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
  #                      xmax=LENGTH_cm+Length_error), size =0.2, alpha=0.4)+
  #   geom_point(size=2, alpha=0.4)+
  #   geom_hline(yintercept = 250, color = "grey", size=0.3, lty=2)+
  #   scale_shape_manual(values = c(21,23))+
  #   geom_text(mapping = aes( x =31, y = 230), label = "Ucrit & Umax", color = "black", size=4.5)+
  #   geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), size=2)+
  #   geom_line(aes(y=fit.mod.ALL2, x=LENGTH_cm), color = "black", size=0.6)+
  #   xlim(15, 100)+
  #   ylim(0, 260)
  # ggformat(p1.cm, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
  # p1.cm<-p1.cm+theme(legend.position = "none")
  # # p1.cm
  # # p1.cm<-ggMarginal(p1.cm, groupColour = TRUE, groupFill = TRUE,  type = "histogram", alpha = 1,
  # #            yparams = list(),  xparams = list(binwidth = 3))
