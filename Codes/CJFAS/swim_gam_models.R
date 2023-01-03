
# library(metafor)
pkgs <- c("mgcv", "mgcViz", "lme4", "ggplot2", "vroom", "dplyr", "forcats", "tidyr", "forestplot", "meta", "ggsci")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)
# *************************

# source: ---------
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/table_BIC.R")
# *************************


# get data, housekeeping: -------------
data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

# available datasets:
# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))

# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data<-data[!c(data$Test_performance2=="TTF"),]
data.ttf<-data[c(data$Test_performance2=="TTF"),]

data$Species_latin<-as.factor(data$Species_latin)
data$Sex_F_M<-as.factor(data$Sex_F_M)
data$Temp_test_mean<-round(data$Temp_test_mean) # round temp to cluster per degree

# # BLs
# data.test.BLs.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
#   dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published) %>% 
#   summarise(swim_speed_MEAN_BL_s = mean(swim_speed_MEAN_BL_s, na.rm = TRUE),
#             swim_error_BLs_SD = sd(swim_speed_MEAN_BL_s, na.rm = TRUE),
#             N_swim_speed = length(swim_speed_MEAN_BL_s),
#             Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
#             Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
#             LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
#             Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
#             N_morphometrics = length(LENGTH_cm),
#             Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
#   as.data.frame()
# 
# # cm/s
# data.test.cms.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
#   dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published, SWIM_cms_source) %>% 
#   summarise(SWIM_cms = mean(SWIM_cms, na.rm = TRUE),
#             SWIM_cms_SD = sd(SWIM_cms, na.rm = TRUE),
#             N_swim_speed = length(SWIM_cms),
#             Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
#             Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
#             LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
#             Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
#             N_morphometrics = length(LENGTH_cm),
#             Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
#   as.data.frame()
# 
# # BL/s
# data.test.BLs.gr<-rbind(data.test.BLs.gr.sum, 
#                         data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
#                                                           "Reference_number_1", "Year_published" , "swim_speed_MEAN_BL_s", "swim_error_BLs_SD",   
#                                                           "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
#                                                           "Length_error_SD", "N_morphometrics","Temp_test_mean")])
# 
# # cm/s
# data.test.cms.gr<-rbind(data.test.cms.gr.sum, 
#                         data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
#                                                           "Reference_number_1", "Year_published" ,"SWIM_cms_source",
#                                                           "SWIM_cms", "SWIM_cms_SD",   
#                                                           "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
#                                                           "Length_error_SD", "N_morphometrics","Temp_test_mean" )])


# general pots before analysis -----------
# breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha",
# "Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
# labels=c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", 
#          "S. salar", "O. masou", "O. mykiss"))
# values("grey", "#ea573d", "#d292cd", "#fb9a62", "#fbc063", "#70af81", "#64b0bc",
#        "#446699", " #615b70")


ggplot(data[data$SWIM_cms_source=="reported" & data$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M))+
  geom_point(data=data[data$SWIM_cms_source=="estimated" & data$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M), fill = "white", alpha = 0.2)+
  geom_point()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
    values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+  theme_light()+
  facet_wrap(.~Species_latin)

ggplot(data, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M))+
  geom_point()+
  theme_light()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+ 
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")


# same but exclude species with sample size < 10 
ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth()

# body size
ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." |data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),], 
       aes(y=SWIM_cms, x=LENGTH_cm, color = Temp_test_mean, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." | data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),], 
       aes(y=SWIM_cms, x=LENGTH_cm, color = Temp_test_mean))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  geom_smooth(method = "gam")
  
ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." | data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),], 
       aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Temp_test_mean, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." | data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),], 
       aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Temp_test_mean))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  geom_smooth(method = "gam")

# saltwater versus freshwater
ggplot(data[!c(data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." | data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),], 
       aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin))+
  geom_point()+
  scale_color_viridis_d()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_d()+
  facet_grid(.~SW_FW)+
  theme_light()


# model dataset -------

# data.test1<-data.test1[complete.cases(data.test1),]
# data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$N_swim_speed) & !is.na(data.test.BLs.gr$swim_error_BLs_SD) & !is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) , ]
# # data.test.BL[, c("N_swim_speed","swim_error_BLs_SD","swim_speed_MEAN_BL_s" )]
# 
# data.test.cm<-data.test.cms.gr[!is.na(data.test.cms.gr$N_swim_speed) & !is.na(data.test.cms.gr$SWIM_cms_SD) & !is.na(data.test.cms.gr$SWIM_cms) , ]
# # data.test.cm[, c("N_swim_speed","SWIM_cms_SD","SWIM_cms" )]

data.model<-data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp." | data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Salmo salar"),
                 c( "N_swim_speed", "Reference_number_1",  "Key_origin",  "FishID",
                   "Species_latin","Sex_F_M", "Temp_test_mean",
                   "LENGTH_cm", "SWIM_cms", "SWIM_cms_SD", "SW_FW", "Test_performance2")]


nrow(data.model)
names(data.model)
cols = c(1,7:10)
cols_fact = c(2, 3, 4, 5, 6, 11, 12)
data.model[,cols]<-lapply(data.model[,cols], as.numeric)
data.model[,cols_fact]<-lapply(data.model[cols_fact], factor)
str(data.model)

# weights in gam ----------
# weights prior weights on the contribution of the data to the log likelihood. Note that a
# weight of 2, for example, is equivalent to having made exactly the same observation twice. If you want to reweight the contributions of each datum without
# changing the overall magnitude of the log likelihood, then you should normalize
# the weights (e.g. weights <-weights/mean(weights)).

data.model$vi<-data.model$SWIM_cms_SD^2/data.model$N_swim_speed # the variance
data.model$weights<-1/data.model$vi
data.model<-data.model[!is.na(data.model$SWIM_cms),]
  
for(i in 1:nrow(data.model)){ # weight is 1 if individual fish recording
  if(data.model$N_swim_speed[i]==1 | is.na(data.model$N_swim_speed[i])){
    data.model$vi[i]<-1
  }
}
  

# SIMR
# Allows user to calculate power for generalized linear mixed models from lme4 using Monte Carlo simulations
# https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module_glmm2.pdf

# weights
# https://www.r-bloggers.com/2020/12/weighting-confusion-matrices-by-outcomes-and-observations-2/
# https://www.bryanshalloway.com/2020/12/08/weighting-classification-outcomes/#weights-of-observations-during-and-prior-to-modeling
# The weight is 1 / v1  >> have weight of 1 for indiv fish 

# Notes and guidance of model construction:
# weights: inverse variation 
# SW and FW category  - not included, very imbalanced amouts of data
# FishID not accounted in the gam model, too computationally heavy, but reference and the origin are. 
# use ML

m1_lmer0 <- lmer(SWIM_cms ~  (1 | FishID)  + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer1 <- lmer(SWIM_cms ~ Temp_test_mean + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer2 <- lmer(SWIM_cms ~ LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer3 <- lmer(SWIM_cms ~ Species_latin + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)

m1_lmer1.2 <- lmer(SWIM_cms ~ Temp_test_mean + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer1.3 <- lmer(SWIM_cms ~ Temp_test_mean + Species_latin +  (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer1.4 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)

m1_lmer1.5 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)
m1_lmer1.6 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin +  (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)

m1_lmerFULL <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model, REML = FALSE, weights = 1/vi)

library(simr)
powerSim(m1_lmer1, nsim = 10)
powerSim(m1_lmer1, fixed("Temp_test_mean"), nsim = 10)

## FULL MODELS from previous analysis
# fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
# fit_BL<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")

# gams-- https://multithreaded.stitchfix.com/blog/2015/07/30/gam/ 

# Gams cant include fish as random effect, run too long and crash. 
# m2_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, k=3, by = Species_latin)  + s(FishID, bs = "re") + s(Key_origin, bs = "re"), data = data.model[complete.cases(data.model),]) # << far too long

smoothing_level<-0.5
K_level<-10

m1_gam0 <- gam(SWIM_cms ~ 1 + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m1_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean,  bs='ps', sp = smoothing_level, k = K_level)  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m1_gam2 <- gam(SWIM_cms ~ s(Temp_test_mean,  bs='ps', sp = smoothing_level, k = K_level) + Species_latin  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m1_gam3 <- gam(SWIM_cms ~ s(Temp_test_mean,  bs='ps', sp = smoothing_level, k = K_level) + Species_latin + Sex_F_M  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m1_gam4 <- gam(SWIM_cms ~ s(Temp_test_mean,  bs='ps', sp = smoothing_level, k = K_level) + s(LENGTH_cm,  bs='ps', sp = smoothing_level) + Species_latin + Sex_F_M  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")

m2_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, by = Species_latin,  bs='ps', sp = smoothing_level, k = K_level)   + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m2_gam2 <- gam(SWIM_cms ~ s(Temp_test_mean, by = Species_latin,  bs='ps', sp = smoothing_level, k = K_level) + LENGTH_cm  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m2_gam3 <- gam(SWIM_cms ~ s(Temp_test_mean, by = Species_latin,  bs='ps', sp = smoothing_level, k = K_level) + Sex_F_M  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")

m3_gam1 <- gam(SWIM_cms ~ s(Temp_test_mean, by = Species_latin,  bs='ps', sp = smoothing_level, k = K_level) + s(LENGTH_cm, bs='ps', sp = smoothing_level)  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi, method = "ML")
m3_gam3 <- gam(SWIM_cms ~ s(Temp_test_mean, by = Species_latin,  bs='ps', sp = smoothing_level, k = K_level) + s(LENGTH_cm, bs='ps', sp = smoothing_level) + Sex_F_M  + s(Reference_number_1, bs = "re"), data = data.model, weights = 1/vi,method = "ML")

# confounding  - saltwater /  freshwater to body size and species

# model summaries comparison, selection -------------
lmerTable<-BIC(m1_lmer0, m1_lmer1, m1_lmer2, m1_lmer3, m1_lmer1.1, m1_lmer1.2, m1_lmer1.3, m1_lmer1.4, m1_lmer1.5, m1_lmer1.6, m1_lmerFULL)
ICdelta(lmerTable, IC = "BIC")

gamTable<-BIC(m3_gam3, m3_gam1,
              m2_gam3, m2_gam2, m2_gam1,
              m1_gam4, m1_gam3, m1_gam2, m1_gam1, m1_gam0)
ICdelta(gamTable)

modTable<-BIC(m1_lmer0, m1_lmer1, m1_lmer2, m1_lmer3, m1_lmer1.2, m1_lmer1.3, m1_lmer1.4, m1_lmer1.5, m1_lmer1.6, m1_lmerFULL,
              m3_gam3, m3_gam1,
              m2_gam3, m2_gam2, m2_gam1,
              m1_gam4, m1_gam3, m1_gam2, m1_gam1, m1_gam0)
ICdelta(modTable, IC = "BIC")


# best GAM model: -------
m3_gam1
b<-m3_gam1
summary(b)

# GAM checking ---------
# https://statistique.cuso.ch/fileadmin/statistique/part-3.pdf

# 1. residual checking:
# Deviance (default by residuals() function), Pearson, working and raw residuals are defined for a GAM in the same way as for any GLM.
# The key assumptions are
# 1. The assumed mean variance relationship is correct, so that scaled residuals have constant variance.
# 2. The response data are independent, so that the residuals appear approximately so.
# 
# 1.1 Residuals potted against fitted values.
# 1.2. Residuals potted against predictor variables (those included and those dropped).
gam.check(b)

# 2. Distribution checking
qq.gam(b)
rsd <- residuals(b) # deviance residuals
qq.gam(b,rep=100);
plot(fitted(b),rsd);


# 3. Checking k the basis dimension
plot(b, residuals=TRUE, pch=19, cex=.3)

viz.b1<-getViz(b)
print(plot(viz.b1, allTerms = T), pages = 1) # Calls print.plotGam()
plot(viz.b1)

plot(viz.b1, allTerms = TRUE) +
  geom_hline(yintercept = 0)
qq(viz.b1, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(viz.b1, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))


# predict
preddf <- expand.grid(Temp_test_mean = seq(1, 30, 1), LENGTH_cm = (50), Species_latin = levels(data.model$Species_latin),
                      Reference_number_1 = 0)
f <- predict.gam(b, newdata=preddf, se.fit = TRUE)
preddf$fit<-as.data.frame(f[1])
preddf$fitSE<-as.data.frame(f[2])
preddf$fit <- unlist(preddf$fit)
preddf$fitSE <- unlist(preddf$fitSE)

p <- predict(b, type="lpmatrix")
beta <- coef(b)[grepl("Temp_test_mean", names(coef(b)))]
s <- p[,grepl("Temp_test_mean", colnames(p))] %*% beta

# model checking: https://www.maths.ed.ac.uk/~swood34/mgcv/check-select.pdf


# model - data plots ------
vis.gam(b,view=c( "Temp_test_mean", "LENGTH_cm"))


ggplot(data.model, aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  geom_point(pch=23)+
  geom_point(data=data.model, aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin, size = vi), color = "black", alpha = 0.4, pch=21)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#EA573D", "#D292CD", "#FB9A62", "#FBC063", "#70Af81", "#64B0BC","#446699", "#615B70"))+  theme_light()+
  facet_wrap(.~Species_latin)+
  geom_line(data = preddf, aes(x=Temp_test_mean, y = fit, color = Species_latin, group = Species_latin))










