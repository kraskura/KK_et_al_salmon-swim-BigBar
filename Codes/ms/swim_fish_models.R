# Modelling with Metafor ----------
# libraries: -------

# links
#  https://www.metafor-project.org/doku.php/tips:multiple_factors_interactions

# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/metareg.html

# package meta in R 
# https://ebmh.bmj.com/content/22/4/153

library(metafor)
library(metafor)
library(tidyverse)
library(reshape2)
if (!require("magrittr")) install.packages("magrittr")
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
# prep dataset

source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

data<-as.data.frame(data.all[1])
# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))

# mi = vector to specify the frequencies of the complement of the event of interest or the group means.
# sdi = vector to specify the standard deviations.
# ni = vector to specify the sample/group sizes.
# data = optional data frame containing the variables given to the arguments above.

data.test.cms.indiv<-data[data$Indiv_group=="indiv",c("Species_latin", "Year_published", "Key_origin", "N_morphometrics", "Indiv_group",
                           "Size_MEAN_kg", "Size_error_kg_SD",
                            "LENGTH_cm","Length_error_SD", 
                           "Sex_F_M", "Temp_test_mean", "Test_performance", "Test_performance2", 
                            "swim_speed_MAX_cm_s", 
                           "N_swim_speed", "FishID", "Reference_number_1", "Duration_swim",
                          "SWIM_cms" , "SWIM_cms_SD", "SWIM_cms_source" )]

data.test.BLs.indiv<-data[,c("Species_latin", "Year_published", "Key_origin", "N_morphometrics", "Indiv_group",
                       "Size_MEAN_kg", "Size_error_kg_SD",
                       "LENGTH_cm","Length_error_SD", 
                       "Sex_F_M", "Temp_test_mean", "Test_performance", "Test_performance2", 
                       "swim_speed_MEAN_BL_s", "swim_speed_MAX_BL_s","swim_error_BLs_SD",
                       "N_swim_speed", "FishID", "Reference_number_1", "Duration_swim" )]
detach(package:plyr)    
library(dplyr)
data.test.BLs.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
  dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published) %>% 
  summarise(swim_speed_MEAN_BL_s = mean(swim_speed_MEAN_BL_s, na.rm = TRUE),
            swim_error_BLs_SD = sd(swim_speed_MEAN_BL_s, na.rm = TRUE),
            N_swim_speed = length(swim_speed_MEAN_BL_s),
            Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
            Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
            LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
            Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
            N_morphometrics = length(LENGTH_cm),
            Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame()

data.test.BLs.gr<-rbind(data.test.BLs.gr.sum, 
      data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
       "Reference_number_1", "Year_published" ,"swim_speed_MEAN_BL_s", "swim_error_BLs_SD",   
       "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
       "Length_error_SD", "N_morphometrics","Temp_test_mean")])

# means only
# https://stats.stackexchange.com/questions/156754/method-of-meta-analysis-of-studies-to-determine-mean-blood-level

# data.test1 <- data.test[,c("SWIM_cms_imputed", "swim_speed_MEAN_cm_s",
#                    "SWIM_cms_SD_imputed", "swim_error_cm_s_SD",
#                    "N_swim_speed", "N_morphometrics",
#                    "FishID", "Reference_number_1", "Temp_test_mean",
#                    "Species_latin", "Year_published", "Key_origin", "Sex_F_M" ,
#                    "Length_cm_value", "Length_MEAN_cm")]


# model dataset -------
# data.test1<-data.test1[complete.cases(data.test1),]
data.test.cms$N_morphometrics<-as.numeric(data.test.cms$N_morphometrics)
data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$N_swim_speed) & !is.na(data.test.BLs.gr$swim_error_BLs_SD) & !is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) , ]
data.test.BL[, c("N_swim_speed","swim_error_BLs_SD","swim_speed_MEAN_BL_s" )]

dat <- escalc(measure="MN", mi=swim_speed_MEAN_BL_s, sdi=swim_error_BLs_SD,
              ni=N_swim_speed, data=data.test.BL)

dat$Reference_number_1<-factor(dat$Reference_number_1)
dat$Species_latin<-factor(dat$Species_latin)
dat$Sex_F_M<-factor(dat$Sex_F_M)


# models -------
mod.sex.1 <- rma.mv(yi, vi, mods = ~Species_latin + Sex_F_M, random = ~ 1 | Reference_number_1,  data=dat)
mod.temp.1<- rma.mv(yi, vi, mods = ~Species_latin + Temp_test_mean, random = ~ 1 | Reference_number_1, data=dat[!is.na(dat$Temp_test_mean),])
mod.sex.temp.1 <- rma.mv(yi, vi, mods = ~Species_latin + Sex_F_M + Temp_test_mean, random = ~ 1 | Reference_number_1,  data=dat[!is.na(dat$Temp_test_mean),])

# the variance/weight in model  -------
wi   <- 1/sqrt(dat$vi)
size <- 0.5 + 1.2 * (wi - min(wi))/(max(wi) - min(wi)) # << size for the plot ta show this 

# predict model and CIs ---------
pred.sex.1 <- predict(mod.sex.1)
dat$mod.sex.1.pred<-pred.sex.1$pred
dat$mod.sex.1.ci.lb<-pred.sex.1$ci.lb
dat$mod.sex.1.ci.ub<-pred.sex.1$ci.ub

pred.temp.1 <- predict(mod.temp.1)
dat[!is.na(dat$Temp_test_mean), "mod.temp.1.pred"]<-pred.temp.1$pred
dat[!is.na(dat$Temp_test_mean), "mod.temp.1.ci.lb"]<-pred.temp.1$ci.lb
dat[!is.na(dat$Temp_test_mean), "mod.temp.1.ci.ub"]<-pred.temp.1$ci.ub

pred.sex.temp.1 <- predict(mod.sex.temp.1)
dat[!is.na(dat$Temp_test_mean), "mod.sex.temp.1.pred"]<-pred.sex.temp.1$pred
dat[!is.na(dat$Temp_test_mean), "mod.sex.temp.1.ci.lb"]<-pred.sex.temp.1$ci.lb
dat[!is.na(dat$Temp_test_mean), "mod.sex.temp.1.ci.ub"]<-pred.sex.temp.1$ci.ub


# sumamry model ----- 
summary(mod.sex.1)
summary(mod.temp.1)
summary(mod.sex.temp.1)
# forest plot to visualize results -------
# https://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html
# forest(mod.temp.1, slab = paste(data.test.BL$Reference_number_1, as.character(data.test.BL$year), sep = ", "))
# forest(mod.sex.1, slab = paste(data.test.BL$Reference_number_1, as.character(data.test.BL$year), sep = ", "))

# funnel plot --------
# A common way to investigate potential publication bias in a meta-analysis is the funnel plot. Asymmetrical distribution indicates potential publication bias.
# https://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html
funnel(mod.temp.1, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)
funnel(mod.sex.1)
funnel(mod.sex.temp.1)

### set up 2x2 array for plotting
par(mfrow=c(2,2))
### draw funnel plots
funnel(mod.temp.1, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)
funnel(mod.temp.1, yaxis="vi", main="Sampling Variance")
funnel(mod.temp.1, yaxis="seinv", main="Inverse Standard Error")
funnel(mod.temp.1, yaxis="vinv", main="Inverse Sampling Variance")

### draw funnel plots
funnel(mod.sex.temp.1, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)
funnel(mod.sex.temp.1, yaxis="vi", main="Sampling Variance")
funnel(mod.sex.temp.1, yaxis="seinv", main="Inverse Standard Error")
funnel(mod.sex.temp.1, yaxis="vinv", main="Inverse Sampling Variance")

# model - data plots ------
# BL/s
dat$wi<-wi
ggplot(data = dat, aes(Species_latin, swim_speed_MEAN_BL_s, size=wi))+
  geom_point(pch=21)+
  xlab("Species")+
  ylab("Observed Mean (BL/s)")+
  # geom_errorbar()+
  geom_point(aes(Species_latin, mod.sex.1.pred, color = Species_latin), size = 3, pch=21)+
  # geom_point(aes(Species_latin, mod.temp.1.pred, color = Species_latin), size = 3, color="red", pch=21)+
  theme_classic()

ggplot(data = dat, aes(shape =Species_latin,y= swim_speed_MEAN_BL_s, size=wi, x = Sex_F_M))+
  geom_point(pch=21)+
  xlab("Sex")+
  ylab("Observed Mean (BL/s)")+
  # geom_errorbar()+
  geom_point(aes(shape=Species_latin, y= mod.sex.1.pred, x = Sex_F_M, color = Sex_F_M), size = 3, pch=21)+
  # geom_point(aes(Species_latin, mod.temp.1.pred, color = Species_latin), size = 3, color="red", pch=21)+
  theme_classic()

ggplot(data = dat, aes(Temp_test_mean, swim_speed_MEAN_BL_s, size = wi))+
  geom_point(aes(fill = Species_latin), pch=21, color = "black")+
  labs(x="Species", y="Observed Mean")+
  # geom_line(aes(Temp_test_mean, mod.temp.1.pred), size = 1)+
  geom_line(aes(x=Temp_test_mean, y=mod.temp.1.ci.lb), lty = "solid", size = 0.5, color = "grey")+
  geom_line(aes(x=Temp_test_mean, y=mod.temp.1.ci.ub), lty = "solid", size = 0.5, color = "grey")+
  theme_classic()+
  facet_wrap(.~Species_latin)




