
library(mgcViz)
pkgs <- c("mgcv", "lme4", "ggplot2", "vroom", "dplyr", "forcats", "tidyr")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)


source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/table_BIC.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

data<-as.data.frame(data.all[1])
# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))



data.test.cms.indiv<-data[data$Indiv_group=="indiv", c("Species_latin", "Year_published", "Key_origin", "N_morphometrics", "Indiv_group",
                                                      "Size_MEAN_kg", "Size_error_kg_SD",
                                                      "LENGTH_cm","Length_error_SD", 
                                                      "Sex_F_M", "Temp_test_mean", "Test_performance", "Test_performance2", 
                                                      "swim_speed_MAX_cm_s", 
                                                      "N_swim_speed", "FishID", "Reference_number_1", "Duration_swim",
                                                      "SWIM_cms" , "SWIM_cms_SD", "SWIM_cms_source" )]

data.test.BLs.indiv<-data[data$Indiv_group=="indiv", c("Species_latin", "Year_published", "Key_origin", "N_morphometrics", "Indiv_group",
                             "Size_MEAN_kg", "Size_error_kg_SD",
                             "LENGTH_cm","Length_error_SD",  
                             "Sex_F_M", "Temp_test_mean", "Test_performance", "Test_performance2", 
                             "swim_speed_MEAN_BL_s", "swim_speed_MAX_BL_s","swim_error_BLs_SD",
                             "N_swim_speed", "FishID", "Reference_number_1", "Duration_swim" )]
detach(package:plyr)    
library(dplyr)
# BLs
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

# cm/s
data.test.cms.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
  dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published, SWIM_cms_source) %>% 
  summarise(SWIM_cms = mean(SWIM_cms, na.rm = TRUE),
            SWIM_cms_SD = sd(SWIM_cms, na.rm = TRUE),
            N_swim_speed = length(SWIM_cms),
            Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
            Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
            LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
            Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
            N_morphometrics = length(LENGTH_cm),
            Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame()

# BL/s
data.test.BLs.gr<-rbind(data.test.BLs.gr.sum, 
                        data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
                                                          "Reference_number_1", "Year_published" , "swim_speed_MEAN_BL_s", "swim_error_BLs_SD",   
                                                          "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
                                                          "Length_error_SD", "N_morphometrics","Temp_test_mean")])

# cm/s
data.test.cms.gr<-rbind(data.test.cms.gr.sum, 
                        data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
                                                          "Reference_number_1", "Year_published" ,"SWIM_cms_source",
                                                          "SWIM_cms", "SWIM_cms_SD",   
                                                          "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
                                                          "Length_error_SD", "N_morphometrics","Temp_test_mean" )])

data$Species_latin<-as.factor(data$Species_latin)
data$Sex_F_M<-as.factor(data$Sex_F_M)

# general pots before analysis -----------
ggplot(data, aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, shape = Sex_F_M))+
  geom_point()+
  theme_light()+
  facet_wrap(.~Species_latin)

# same but exclude species with sample size < 10 
ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  theme_light()+
  facet_wrap(.~Species_latin)+
  geom_smooth()
  



# model dataset -------

# data.test1<-data.test1[complete.cases(data.test1),]
data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$N_swim_speed) & !is.na(data.test.BLs.gr$swim_error_BLs_SD) & !is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) , ]
data.test.BL[, c("N_swim_speed","swim_error_BLs_SD","swim_speed_MEAN_BL_s" )]

data.test.cm<-data.test.cms.gr[!is.na(data.test.cms.gr$N_swim_speed) & !is.na(data.test.cms.gr$SWIM_cms_SD) & !is.na(data.test.cms.gr$SWIM_cms) , ]
data.test.cm[, c("N_swim_speed","SWIM_cms_SD","SWIM_cms" )]

data.model<-data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),
                 c( "N_swim_speed", "Reference_number_1",  "Key_origin",  "FishID",
                   "Species_latin","Sex_F_M", "Temp_test_mean",
                   "LENGTH_cm", "SWIM_cms", "SW_FW", "Test_performance2")]
# 
nrow(data.model)
names(data.model)
cols = c(1,7:9)
cols_fact = c(2, 3, 4, 5, 6, 10, 11)
data.model[,cols]<-lapply(data.model[,cols], as.numeric)
data.model[,cols_fact]<-lapply(data.model[cols_fact], factor)
str(data.model)


# weights in gam ----------
# weights prior weights on the contribution of the data to the log likelihood. Note that a
# weight of 2, for example, is equivalent to having made exactly the same observation twice. If you want to reweight the contributions of each datum without
# changing the overall magnitude of the log likelihood, then you should normalize
# the weights (e.g. weights <-weights/mean(weights)).
# 

data.model$weights<-data.model$N_swim_speed/mean(data.model$N_swim_speed, na.rm = TRUE)


# SIMR
# Allows user to calculate power for generalized linear mixed models from lme4 using Monte Carlo simulations
# https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module_glmm2.pdf


# weights
# https://www.r-bloggers.com/2020/12/weighting-confusion-matrices-by-outcomes-and-observations-2/
# https://www.bryanshalloway.com/2020/12/08/weighting-classification-outcomes/#weights-of-observations-during-and-prior-to-modeling
  
# equivalent to linear model
# add size models 
# 1. linear model: 
m1_lmer0 <- lmer(SWIM_cms ~  (1 | FishID) + ( 1 | Key_origin) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
m1_lmer1 <- lmer(SWIM_cms ~ Temp_test_mean + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
# m1_lmer2 <- lmer(SWIM_cms ~ LENGTH_cm + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
m1_lmer3 <- lmer(SWIM_cms ~ Species_latin + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)

m1_lmer1.1 <- lmer(SWIM_cms ~ Temp_test_mean + SW_FW + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
m1_lmer1.2 <- lmer(SWIM_cms ~ Temp_test_mean + Sex_F_M + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
m1_lmer1.3 <- lmer(SWIM_cms ~ Temp_test_mean + Species_latin +  (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)

# m1_lmer1.4 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + SW_FW + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
# m1_lmer1.5 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
m1_lmer1.6 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin +  (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)


# m1_lmer1.7 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + SW_FW + Sex_F_M + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
m1_lmer1.8 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + SW_FW + Species_latin + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
m1_lmer1.9 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M + Species_latin + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)

# m1_lmer2.1 <- lmer(SWIM_cms ~ LENGTH_cm + SW_FW + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
# m1_lmer2.2 <- lmer(SWIM_cms ~ LENGTH_cm + Sex_F_M + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)
m1_lmer2.3 <- lmer(SWIM_cms ~ LENGTH_cm + Species_latin +  (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)
# m1_lmer2.4 <- lmer(SWIM_cms ~ LENGTH_cm + Temp_test_mean +  (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE)

m1_lmerFULL <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + SW_FW + Species_latin + Sex_F_M + (1 | FishID) + ( 1 | Key_origin)+ (1 | Reference_number_1), data = data.model, REML = FALSE, weights = N_swim_speed)

library(simr)
powerSim(m1_lmer1, nsim = 10)
powerSim(m1_lmer1, fixed("Temp_test_mean"), nsim = 10)

## FULL MODELS from previous analysis
# fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
# fit_BL<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")

# Gams cant include fish as random effect, run too long and crash. 

m1_gam0 <- gam(SWIM_cms ~  + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m1_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3) + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m1_gam2 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3) + Species_latin + s(Key_origin, bs = "re"), data = data.model)

# m2_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin)  + s(FishID, bs = "re") + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),]) # << far too long
m2_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin)  +  s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m2_gam2 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin) +  s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m2_gam3 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin) + Sex_F_M + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m2_gam4 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin) + SW_FW + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])
m2_gam4 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3,fx = TRUE, by = Species_latin) + SW_FW + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),])

lmerTable<-BIC(m1_lmer0, m1_lmer1, m1_lmer3, m1_lmer1.1, m1_lmer1.2, m1_lmer1.3, m1_lmer1.6,  m1_lmer1.8, m1_lmer1.9,m1_lmer2.3, m1_lmerFULL)
BICdelta(lmerTable)
gamTable<-BIC(m1_gam0, m1_gam1, m1_gam2, m2_gam1, m2_gam2, m2_gam3, m2_gam4)
BICdelta(gamTable)

# best models:
# 1) m1_lmer1
# 2) m2_gam4

# model checking: https://www.maths.ed.ac.uk/~swood34/mgcv/check-select.pdf
par(mfrow=c(2,2))
gam.check(m2_gam4)
plot(residuals(m2_gam4))
plot(m2_gam4,residuals=TRUE)
vis.gam(m2_gam4,view=c( "Temp_test_mean", "Species_latin"),theta= 135,phi=40)

plot(m2_gam4)
preds <- get_gam_predictions(m2_gam4, Temp_test_mean, )

modelBIC<-BIC(m1_lmer0, m1_lmer1, m1_lmer3, m1_lmer1.1, m1_lmer1.2, m1_lmer1.3, m1_lmer1.6,  m1_lmer1.8, m1_lmer1.9,m1_lmer2.3, m1_lmerFULL, m1_gam0, m1_gam1, m1_gam2, m2_gam1, m2_gam2, m2_gam3, m2_gam4)
BICdelta(modelBIC)





# model - data plots ------
# BL/s

# ggplot(data = data.model, aes(Species_latin, SWIM_cms))+
#   geom_point(pch=21)+
#   xlab("Species")+
#   ylab("Observed Mean (BL/s)")+
#   theme_classic()

ggplot(data = data.model, aes(Species_latin, SWIM_cms))+
  geom_point(pch=21)+
  xlab("Species")+
  ylab("Observed Mean (cm/s)")+
  theme_classic()

ggplot(data = data.model[complete.cases(data.model),], aes(Temp_test_mean, SWIM_cms, color = Species_latin))+
  geom_point(pch=21)+
  xlab("Temperature")+
  geom_line(aes(color=Species_latin, y= predict(m2_gam4), x = Temp_test_mean))+
  ylab("Observed Mean (cm/s)")+
  facet_wrap(SW_FW~Species_latin, nrow=2)+
  theme_classic()

ggplot(data = data.model[complete.cases(data.model),], aes(shape =Species_latin,y= SWIM_cms, x = Sex_F_M))+
  geom_point(pch=21)+
  xlab("Sex")+
  ylab("Observed Mean (BL/s)")+
  # geom_errorbar()+
  geom_point(aes(shape=Species_latin, y= predict(m2_gam4), x = Sex_F_M, color = Sex_F_M), size = 3, pch=21)+
  # geom_point(aes(Species_latin, mod.temp.1.pred, color = Species_latin), size = 3, color="red", pch=21)+
  theme_classic()






