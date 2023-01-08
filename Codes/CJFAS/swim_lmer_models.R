
# library(metafor)
pkgs <- c("mgcv", "mgcViz", "lme4", "ggplot2", "vroom", "dplyr", "forcats", "tidyr", "forestplot", "meta", "ggsci", "DHARMa")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)
#install.packages("DHARMa")
#install.packages("car")

# *************************

# 0. source data and code: ---------
source("/Users/kristakraskura/Github_repositories//KK_et_al_salmon-swim-BigBar/Codes/PSC/table_BIC.R")
source("/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
# *************************


# 1. import and wrangle the data -------------
data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Data/Files/Kraskura_salmonSwim_analysis_jan2022.csv")

# available datasets:
# return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))

# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data.ttf<-data[c(data$Test_performance2=="TTF"),]
data<-data[!c(data$Test_performance2=="TTF"),]

data$Species_latin<-as.factor(data$Species_latin)
data$Sex_F_M<-as.factor(data$Sex_F_M)
data$Temp_test_mean<-round(data$Temp_test_mean) # round temp to cluster per degree



# model dataset -------

# data.test1<-data.test1[complete.cases(data.test1),]
# data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$N_swim_speed) & !is.na(data.test.BLs.gr$swim_error_BLs_SD) & !is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) , ]
# data.test.BL[, c("N_swim_speed","swim_error_BLs_SD","swim_speed_MEAN_BL_s" )]
# 
# data.test.cm<-data.test.cms.gr[!is.na(data.test.cms.gr$N_swim_speed) & !is.na(data.test.cms.gr$SWIM_cms_SD) & !is.na(data.test.cms.gr$SWIM_cms) , ]
# data.test.cm[, c("N_swim_speed","SWIM_cms_SD","SWIM_cms" )]

data.model<-data[!c(data$Species_latin == "Oncorhynchus spp."),
                 c( "N_swim_speed", "Reference_number_1",  "Key_origin",  "FishID",
                    "Species_latin","Sex_F_M", "Temp_test_mean",
                    "LENGTH_cm", "SWIM_cms", "SWIM_cms_SD", "SW_FW", "Test_performance2")]

nrow(data.model) # 1655 (jan 3 2023)
names(data.model)
cols = c(1,7:10)
cols_fact = c(2, 3, 4, 5, 6, 11, 12)
data.model[,cols]<-lapply(data.model[,cols], as.numeric)
data.model[,cols_fact]<-lapply(data.model[cols_fact], factor)
str(data.model)

# 3. Models  ------
## 3.1 weights in mixed models ----------
# weights prior weights on the contribution of the data to the log likelihood. Note that a
# weight of 2, for example, is equivalent to having made exactly the same observation twice. 
# If we want to reweigh the contributions of each datum without
# changing the overall magnitude of the log likelihood, then you should normalize
# the weights (e.g. weights <-weights/mean(weights)).

data.model$vi<-data.model$SWIM_cms_SD^2/data.model$N_swim_speed # the variance
data.model$weights<-1/data.model$vi
data.model<-data.model[!is.na(data.model$SWIM_cms),]

for(i in 1:nrow(data.model)){ # weight is 1 if individual fish recording
  if(data.model$N_swim_speed[i]==1 | is.na(data.model$N_swim_speed[i])){
    data.model$vi[i]<-1
  }
  
  if(is.na(data.model$vi[i])){
    print(median(data.model$SWIM_cms_SD, na.rm =TRUE)^2/data.model$N_swim_speed[i] )
    data.model$vi[i]<-median(data.model$SWIM_cms_SD, na.rm =TRUE)^2/data.model$N_swim_speed[i] 
  }
}
plot(data.model$SWIM_cms_SD, data.model$N_swim_speed, ylim = c(0, 50), xlim = c(0, 50) )

data.model.cm2<-data.model[complete.cases(data.model[, c("SWIM_cms", "vi",
                                         "Reference_number_1", "FishID", "Species_latin",
                                         "Temp_test_mean", "Key_origin", "Sex_F_M", "LENGTH_cm", "N_swim_speed")]),c("SWIM_cms", "vi",
                       "Reference_number_1", "FishID", "Species_latin",
                       "Temp_test_mean", "Key_origin", "Sex_F_M", "LENGTH_cm", "N_swim_speed")]

data.model.cm<-data.model[complete.cases(data.model[, c("SWIM_cms", "vi",
                                                        "Reference_number_1", "FishID", "Species_latin",
                                                        "Temp_test_mean", "Key_origin", "Sex_F_M", "LENGTH_cm")]),c("SWIM_cms", "vi", "Reference_number_1", "FishID", "Species_latin", "Temp_test_mean", "Key_origin", "Sex_F_M", "LENGTH_cm")]
# SIMR
# Allows user to calculate power for generalized linear mixed models from lme4 using Monte Carlo simulations
# https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module_glmm2.pdf

# weights
# https://www.r-bloggers.com/2020/12/weighting-confusion-matrices-by-outcomes-and-observations-2/
# https://www.bryanshalloway.com/2020/12/08/weighting-classification-outcomes/#weights-of-observations-during-and-prior-to-modeling

# equivalent to linear model
# add size models 
# 1. linear model: 

# Notes and guidance of model construction:
# weights: inverse variation 
# SW and FW category  - not included, very imbalanced amouts of data
# FishID not accounted in the gam model, too computationally heavy, but reference and the origin are. 
# use ML

## 3.2. weighed mixed models: weights = inverse variation  (lmer) ------

m1w1_lmer0 <- lmer(SWIM_cms ~  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1 <- lmer(SWIM_cms ~ Temp_test_mean + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer2 <- lmer(SWIM_cms ~ LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer3 <- lmer(SWIM_cms ~ Species_latin -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)

m1w1_lmer1.2 <- lmer(SWIM_cms ~ Temp_test_mean + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.3 <- lmer(SWIM_cms ~ Temp_test_mean + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.4 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.5 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.6 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)

m1w1_lmerFULL <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin  -1 + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)

## 3.3. weighed mixed models: weights = n swim (lmer) ------
m1w2_lmer0 <- lmer(SWIM_cms ~  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer1 <- lmer(SWIM_cms ~ Temp_test_mean + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer2 <- lmer(SWIM_cms ~ LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer3 <- lmer(SWIM_cms ~ Species_latin -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)

m1w2_lmer1.2 <- lmer(SWIM_cms ~ Temp_test_mean + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer1.3 <- lmer(SWIM_cms ~ Temp_test_mean + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer1.4 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer1.5 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)
m1w2_lmer1.6 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)

m1w2_lmerFULL <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin  -1 + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm2, REML = FALSE, weights = N_swim_speed)

## 3.4. mixed models: no weights  (lmer) ------

m1_lmer0 <- lmer(SWIM_cms ~  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer1 <- lmer(SWIM_cms ~ Temp_test_mean + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer2 <- lmer(SWIM_cms ~ LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer3 <- lmer(SWIM_cms ~ Species_latin -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)

m1_lmer1.2 <- lmer(SWIM_cms ~ Temp_test_mean + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer1.3 <- lmer(SWIM_cms ~ Temp_test_mean + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer1.4 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer1.5 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Sex_F_M  -1 + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)
m1_lmer1.6 <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin -1 +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)

m1_lmerFULL <- lmer(SWIM_cms ~ Temp_test_mean + LENGTH_cm + Species_latin  -1 + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE)



## 3.2. mixed models: poly 3rd order, weights = inverse variation  (lmer) ------
 
m1w1_lmer1p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)

m1w1_lmer1.2p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.3p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + Species_latin +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.4p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + LENGTH_cm + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.5p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + LENGTH_cm + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)
m1w1_lmer1.6p <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + LENGTH_cm + Species_latin +  (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)

m1w1_lmerFULLp <- lmer(SWIM_cms ~ poly(Temp_test_mean, degree = 3) + LENGTH_cm + Species_latin + Sex_F_M + (1 | FishID) + (1 | Reference_number_1), data = data.model.cm, REML = FALSE, weights = 1/vi)



## FULL MODELS from previous analysis
# fit_CM<-lmer(swim_speed_MEAN_cm_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")
# fit_BL<-lmer(swim_speed_MEAN_BL_s ~ Sex_F_M + SW_FW + Test_performance2 + Indiv_group + Species_latin +
#                Size_MEAN_kg * Temp_test_mean + (1|Reference_number_1) + (1|FishID), data=data, REML=FALSE, na.action = "na.exclude")


# model summaries comparison, selection -------------
lmerTable <- BIC(m1w1_lmer0, m1w1_lmer1, m1w1_lmer2, m1w1_lmer3,
                 m1w1_lmer1.4, m1w1_lmer1.2, m1w1_lmer1.3,  m1w1_lmer1.5, m1w1_lmer1.6, m1w1_lmerFULL,
                 m1w1_lmer1p, m1w1_lmer1.2p, m1w1_lmer1.3p, m1w1_lmer1.4p, m1w1_lmer1.5p, m1w1_lmer1.6p, m1w1_lmerFULLp)
ICdelta(lmerTable, IC = "BIC")

lmerTable <- BIC(m1w1_lmer0, m1w1_lmer1, m1w1_lmer2, m1w1_lmer3,
                 m1w1_lmer1.4, m1w1_lmer1.2, m1w1_lmer1.3,  m1w1_lmer1.5, m1w1_lmer1.6, m1w1_lmerFULL,
                 m1w1_lmer1p, m1w1_lmer1.2p, m1w1_lmer1.3p, m1w1_lmer1.4p, m1w1_lmer1.5p, m1w1_lmer1.6p, m1w1_lmerFULLp)
ICdelta(lmerTable, IC = "BIC")

# best model (jan 3 2023)-------- 
mod_swim<-m1w1_lmer2
plot(mod_swim)
plot(mod_swim, resid(., scaled=TRUE) ~ fitted(.) | Species_latin , abline = 0)
qqmath(mod_swim)
summary(mod_swim)

# should fit overdispersion models 

# assumptions, normal resid distr around predictors/ linearity: ----------
plot(resid(mod_swim), data.model.cm$Species_latin)
plot(resid(mod_swim), data.model.cm$LENGTH_cm)
plot(resid(mod_swim), data.model.cm$Temp_test_mean)

# assumptions, homogeneous variance: ----------
plot(mod_swim) # all data visualized, need to be symmetrical, looks good
# lavenes test
lev.mod1<-lm(abs(resid(mod_swim))^2 ~ Reference_number_1, data=data.model.cm) #ANOVA of the squared residuals
lev.mod2<-lm(abs(resid(mod_swim))^2 ~ FishID, data=data.model.cm) #ANOVA of the squared residuals
lev.mod3<-lm(abs(resid(mod_swim))^2 ~ Species_latin, data=data.model.cm) #ANOVA of the squared residuals
lev.mod4<-lm(abs(resid(mod_swim))^2 ~ 1, data=data.model.cm) #ANOVA of the squared residuals
anova(lev.mod1) # sign
anova(lev.mod2) # sign
anova(lev.mod3) # sign
anova(lev.mod4)

# assumptions, normally distr residuals: ----------
# additional fitting 
require("lattice")
qqnorm(resid(mod_swim))
qqmath(mod_swim)
hist(data.model.cm$SWIM_cms, breaks = 100)
plot(fitted(mod_swim)~data.model.cm$SWIM_cms)
abline(b = 1, a =0)

# looks over-dispesrsed data 

data.model.cm$pred<- predict(mod_swim, type="response")

# Fig Supl 1B. --------
funnel<-ggplot(data.model.cm, aes(x=pred, y=SWIM_cms/pred, label = Reference_number_1))+
  geom_point(pch=21)+
  geom_hline(yintercept = 1, lty=2, color = "grey")
  # geom_text(chseck_overlap = T)
ggformat(funnel, title = "", y_title = "Standard Ratio", x_title = " Expected value (cm/s)")
funnel<-funnel +theme (axis.ticks.x = element_line(size = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm"))
funnel


# plots --------
ggplot(data = data.model.cm, aes(Species_latin, SWIM_cms, color = Temp_test_mean))+
geom_point(pch=21, position = position_jitterdodge())+
  scale_color_viridis_c()+
  xlab("Species")+
  ylab("Observed Mean (cm/s)")+
  theme_classic()

