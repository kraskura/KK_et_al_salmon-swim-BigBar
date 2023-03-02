
# library(metafor)
pkgs <- c("mgcv", "lme4", "ggplot2", "vroom", "dplyr", "pryr", "forcats", "tidyr", "forestplot", "meta")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)
# *************************

# source: ---------
source("/Users/kristakraskura/Github_repositories//KK_et_al_salmon-swim-BigBar/Codes/PSC/table_BIC.R")
source("/Users/kristakraskura/Github_repositories/KK_et_al_salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
# *************************


# 1. Get data, housekeeping: -------------
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

# sanity check
data[which(data$SWIM_cms < data$swim_speed_MEAN_cm_s),c("swim_speed_MEAN_BL_s","swim_speed_MEAN_cm_s",  "SWIM_cms", "SWIM_cms_source", "swim_speed_MAX_cm_s")]

# get means from individual datasets 
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
# detach(package:plyr)    
# library(dplyr)

# BL/s
data.test.BLs.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
  dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published, Temp_test_mean, Key_origin) %>% 
  summarise(swim_speed_MEAN_BL_s = mean(swim_speed_MEAN_BL_s, na.rm = TRUE),
            swim_error_BLs_SD = sd(swim_speed_MEAN_BL_s, na.rm = TRUE),
            N_swim_speed = length(swim_speed_MEAN_BL_s),
            Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
            Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
            LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
            Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
            N_morphometrics = length(LENGTH_cm)) %>% 
            # Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame()

# cm/s
data.test.cms.gr.sum<-data[data$Indiv_group=="indiv",] %>% 
  dplyr::group_by(Sex_F_M , Species_latin , Test_performance2, Swim_Conditions2, Reference_number_1, Year_published, SWIM_cms_source, Temp_test_mean, Key_origin) %>% 
  summarise(SWIM_cms = mean(SWIM_cms, na.rm = TRUE),
            SWIM_cms_SD = sd(SWIM_cms, na.rm = TRUE),
            N_swim_speed = length(SWIM_cms),
            Size_MEAN_kg = mean(Size_MEAN_kg, na.rm = TRUE),
            Size_error_kg_SD = sd(Size_MEAN_kg, na.rm = TRUE),
            LENGTH_cm = mean(LENGTH_cm, na.rm = TRUE),
            Length_error_SD = sd(LENGTH_cm, na.rm = TRUE),
            N_morphometrics = length(LENGTH_cm)) %>% 
            # Temp_test_mean = mean( Temp_test_mean, na.rm = TRUE)) %>% 
  as.data.frame()

# dat.cm[which(data.test.cms.gr.sum$N_swim_speed==1),]

# BL/s
data.test.BLs.gr<-rbind(data.test.BLs.gr.sum, 
                        data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
                                                          "Reference_number_1", "Year_published" ,"Temp_test_mean","Key_origin",
                                                          "swim_speed_MEAN_BL_s", "swim_error_BLs_SD",   
                                                          "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
                                                          "Length_error_SD", "N_morphometrics")])

# cm/s
data.test.cms.gr<-rbind(data.test.cms.gr.sum, 
                        data[data$Indiv_group=="group", c("Sex_F_M", "Species_latin", "Test_performance2", "Swim_Conditions2",    
                                                          "Reference_number_1", "Year_published" ,"SWIM_cms_source", "Temp_test_mean","Key_origin", 
                                                          "SWIM_cms", "SWIM_cms_SD",   
                                                          "N_swim_speed","Size_MEAN_kg","Size_error_kg_SD","LENGTH_cm",           
                                                          "Length_error_SD", "N_morphometrics")])
# add 0 to SD for individual fish swim data 
# cm/s
for (i in 1:nrow(data.test.cms.gr)){
  if(!is.na(data.test.cms.gr$N_swim_speed[i]) & data.test.cms.gr$N_swim_speed[i]== 1 & is.na(data.test.cms.gr$SWIM_cms_SD[i])){
    data.test.cms.gr$SWIM_cms_SD[i]<-0
  }else{
    next
  }
  print(i)
}

# BL/s
for (i in 1:nrow(data.test.BLs.gr)){
  if(!is.na(data.test.BLs.gr$N_swim_speed[i]) & data.test.BLs.gr$N_swim_speed[i]== 1 & is.na(data.test.BLs.gr$swim_error_BLs_SD[i])){
    data.test.BLs.gr$swim_error_BLs_SD[i]<-0
  }else{
    next
  }
  print(i)
}

# data.test1<-data.test1[complete.cases(data.test1),]
# data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$N_swim_speed) & !is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) ,]
data.test.BL<-data.test.BLs.gr[!is.na(data.test.BLs.gr$swim_speed_MEAN_BL_s) ,]
data.test.BL[, c("N_swim_speed","swim_error_BLs_SD","swim_speed_MEAN_BL_s" )]

dat.BL<-data.test.BL
dat.BL$Reference_number_1<-factor(dat.BL$Reference_number_1)
dat.BL$Species_latin<-factor(dat.BL$Species_latin)
dat.BL$Sex_F_M<-factor(dat.BL$Sex_F_M)
dat.BL$yi<-dat.BL$swim_speed_MEAN_BL_s  # the mean in 'yi'
dat.BL$vi<-dat.BL$swim_error_BLs_SD^2/dat.BL$N_swim_speed # the variance
dat.BL$xi<-1:nrow(dat.BL)
dat.BL$xii<-1:nrow(dat.BL)
dat.BL<-dat.BL %>% 
  mutate(xi = reorder(xi, -yi))
dat.BL<-dat.BL %>% 
  mutate(xii = reorder(xii, -Temp_test_mean))
dat.BL$xii<-as.numeric(dat.BL$xii)

wi.all <- 1/sqrt(dat.BL$vi[c(dat.BL$N_swim_speed!=1 & !is.na(dat.BL$N_swim_speed))])
wi.min <- min(wi.all, na.rm = T)
wi.max <-max(wi.all, na.rm = T) 
for(i in 1:nrow(dat.BL)){
  if(dat.BL$N_swim_speed[i]!=1 & !is.na(dat.BL$vi[i])){
    wi <- 1/sqrt(dat.BL$vi[i])
    dat.BL$size[i] <- 0.5 + 1.2 * (wi - wi.min)/(wi.max - wi.min) 
    print(c(wi, (0.5 + 1.2 * (wi - wi.min)/(wi.max - wi.min) )))# << size for the plot ta show this 
  }else{
    dat.BL$size[i] <- 0.5
  }
}

# data.test.cm<-data.test.cms.gr[!is.na(data.test.cms.gr$N_swim_speed) & !is.na(data.test.cms.gr$SWIM_cms_SD) & !is.na(data.test.cms.gr$SWIM_cms) & !is.na(data.test.cms.gr$Key_origin) , ]
data.test.cm<-data.test.cms.gr[ !is.na(data.test.cms.gr$SWIM_cms) , ]
data.test.cm[, c("N_swim_speed","SWIM_cms_SD","SWIM_cms" )]

dat.cm<-data.test.cm
dat.cm$Reference_number_1<-factor(dat.cm$Reference_number_1)
dat.cm$Species_latin<-factor(dat.cm$Species_latin)
dat.cm$Sex_F_M<-factor(dat.cm$Sex_F_M)
dat.cm$yi<-dat.cm$SWIM_cms  # the mean in 'yi'
dat.cm$vi<-dat.cm$SWIM_cms_SD^2/dat.cm$N_swim_speed # the variance
# 
# hist(dat.cm$vi[-c(which(dat.cm$vi == max(dat.cm$vi, na.rm=TRUE)))]) # plot variance
# hist(dat.cm$vi) # plot variance
# 
dat.cm$size<-0 # the variance set to zero for
# dat.cm$size[!is.na(dat.cm$vi)]<-dat.cm$vi[!is.na(dat.cm$vi)]
# dat.cm$size[c(which(dat.cm$vi > 200))] <- 200
# dat.cm$size <- dat.cm$size/10
wi.all <- 1/sqrt(dat.cm$vi[c(dat.cm$N_swim_speed!=1 & !is.na(dat.cm$N_swim_speed))])
wi.min <- min(wi.all, na.rm = T)
wi.max <-max(wi.all, na.rm = T) 
# << size for the plot ta show this 
dat.cm$anaerob<-0

for(i in 1:nrow(dat.cm)){
  if(dat.cm$N_swim_speed[i]!=1 & !is.na(dat.cm$vi[i])){
    wi <- 1/sqrt(dat.cm$vi[i])
    dat.cm$size[i] <- 0.5 + 1.2 * (wi - wi.min)/(wi.max - wi.min) 
    # print(c(wi, (0.5 + 1.2 * (wi - wi.min)/(wi.max - wi.min) )))# << size for the plot ta show this 
  }else{
    dat.cm$size[i] <- 0.5
  }
  
  if(!is.na(dat.cm$LENGTH_cm[i])){
    if((dat.cm$SWIM_cms[i] / dat.cm$LENGTH_cm[i]) >= 2){
      dat.cm$anaerob[i]<- 1
      print((dat.cm$SWIM_cms[i] / dat.cm$LENGTH_cm[i]))
    }
  }
}

# vi <- sdi^2/ni
dat.cm$xi<-as.numeric(1:nrow(dat.cm))
dat.cm$xii<-as.numeric(1:nrow(dat.cm))
# species grouped to plot 
dat.cm$Species_latin2<-as.character(dat.cm$Species_latin)
dat.cm$Species_latin2[which(dat.cm$Species_latin == "Oncorhynchus masou" | dat.cm$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
dat.cm$Species_latin2<-factor(dat.cm$Species_latin2)
# nrow(dat.cm[!is.na(dat.cm$SWIM_cms_SD),])
# is predicted > 2 BL/s = 

dat.cm<-dat.cm %>% 
  mutate(xi = reorder(xi, -yi))
dat.cm$xi<-as.numeric(dat.cm$xi)

dat.cm<-dat.cm %>% 
  mutate(xii = reorder(xii, -Temp_test_mean))
dat.cm$xii<-as.numeric(dat.cm$xii)

# wi   <- 1/sqrt(dat.cm$vi)
# size <- 0.5 + 1.2 * (wi - min(wi))/(max(wi) - min(wi)) # << size for the plot ta show this 
is.na(dat.cm$Species_latin2)
dat.cm[dat.cm$SWIM_cms>300,]
dat.cm$Species_latin<-factor(dat.cm$Species_latin)

## 1.1. Plot and visualize all data -----
ggplot(data=dat.cm, aes(x = xi, y = yi, size = size, shape = Species_latin))+
  # geom_text(mapping= aes( x = xi, y=5), size = 2)+
  # geom_hline(yintercept =100, linetype=2, col = "grey", lwd=0.5)+
  geom_hline(yintercept =200, linetype=2, col = "black", lwd=0.1)+
  geom_errorbar(data = (dat.cm[!is.na(dat.cm$SWIM_cms_SD),]), mapping = aes(ymin = yi-(SWIM_cms_SD), ymax = yi+(SWIM_cms_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  # geom_errorbar(dat.cm[dat.cm$SWIM_cms_source == "estimated",], mapping = aes(ymin = yi-(SWIM_cms_SD), ymax = yi+(SWIM_cms_SD), col=Temp_test_mean), size = 1)+ 
  geom_point(data = subset(dat.cm[dat.cm$SWIM_cms_source == "estimated",], !is.na(Species_latin)) , mapping = aes(x = xi, y = yi, color = Temp_test_mean), fill = "white", stroke=0.5)+
  geom_point(data =  subset(dat.cm[dat.cm$SWIM_cms_source == "reported",], !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  scale_shape_manual(breaks = c("Oncorhynchus masou", "Oncorhynchus spp.", "Oncorhynchus keta", "Oncorhynchus tshawytscha", 
                                "Oncorhynchus gorbuscha", "Oncorhynchus kisutch", "Oncorhynchus mykiss",  "Oncorhynchus nerka","Salmo salar"), values= c(25, 23, 21, 21, 21, 21, 21, 21, 21))+
  geom_point(data = subset(dat.cm[dat.cm$anaerob == 1,], !is.na(Species_latin)), mapping = aes(x = xi, y = anaerob), pch="-", color = "black", fill = "white", size = 2, alpha= 0.9)+
  xlab('')+ 
  ylim(0, 800)+
  ylab(expression(Swim[Abs]~speed~(cm/s)))+
  facet_wrap(~Species_latin2, strip.position="left", nrow=2, drop = TRUE) +
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  # ylim(0, 300)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

ggplot(data=dat.BL, aes(x = xi, y = yi, size=size))+
  geom_hline(yintercept =200, linetype=2, col = "black", lwd=0.1)+
  geom_errorbar(data = (dat.cm[!is.na(dat.cm$SWIM_cms_SD),]), mapping = aes(ymin = yi-(SWIM_cms_SD), ymax = yi+(SWIM_cms_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data = subset(dat.cm[dat.cm$SWIM_cms_source == "estimated",], !is.na(Species_latin)) , mapping = aes(x = xi, y = yi, color = Temp_test_mean), fill = "white", stroke=0.5)+
  geom_point(data =  subset(dat.cm[dat.cm$SWIM_cms_source == "reported",], !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  xlab('')+ 
  ylim(0, 800)+
  ylab(expression(Swim[Abs]~speed~(cm/s)))+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  # ylim(0, 300)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

ggplot(data=dat.cm, aes(x = xii, y = yi, size=size))+
  geom_hline(yintercept =200, linetype=2, col = "black", lwd=0.1)+
  geom_errorbar(data = (dat.cm[!is.na(dat.cm$SWIM_cms_SD),]), mapping = aes(ymin = yi-(SWIM_cms_SD), ymax = yi+(SWIM_cms_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data = subset(dat.cm[dat.cm$SWIM_cms_source == "estimated",], !is.na(Species_latin)) , mapping = aes(x = xii, y = yi, color = Temp_test_mean), fill = "white", stroke=0.5)+
  geom_point(data =  subset(dat.cm[dat.cm$SWIM_cms_source == "reported",], !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  xlab('')+ 
  ylim(0, 800)+
  ylab(expression(Swim[Abs]~speed~(cm/s)))+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  # ylim(0, 300)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()


# BL/s *************************************
# BL/s *************************************
# BL/s *************************************
ggplot(data=dat.BL, aes(x = xi, y = yi,  size = size))+
  geom_hline(yintercept =2, linetype=2, col = "black", lwd=0.1)+
  geom_errorbar(aes(ymin = yi-(swim_error_BLs_SD), ymax = yi+(swim_error_BLs_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5)+
  xlab('')+ 
  ylim(c(0, 5))+
  ylab(expression(Swim[Relat]~speed~(BL/s)))+
  facet_wrap(~Species_latin, strip.position="left", nrow=2,  drop=TRUE) +
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

ggplot(data=dat.BL, aes(x = as.numeric(xi), y = yi, size=size))+
  geom_errorbar(data = (dat.BL[!is.na(dat.BL$swim_error_BLs_SD),]), mapping = aes(x = as.numeric(xi), ymin = yi-(swim_error_BLs_SD), ymax = yi+(swim_error_BLs_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_hline(yintercept =2, linetype=2, col = "black", lwd=0.1)+
  geom_point(data = subset(dat.BL, !is.na(Species_latin)) , mapping = aes(x = as.numeric(xi), y = yi, color = Temp_test_mean), fill = "white", stroke=0.5)+
  geom_point(data =  subset(dat.BL, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  xlab('')+ 
  ylim(0, 5)+
  ylab(expression(Swim[Relativ]~speed~(BL/s)))+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  # ylim(0, 300)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

ggplot(data=dat.BL, aes(x = xii, y = yi, size=size))+
  geom_hline(yintercept =2, linetype=2, col = "black", lwd=0.1)+
  geom_errorbar(data = (dat.BL[!is.na(dat.BL$swim_error_BLs_SD),]), mapping = aes(ymin = yi-(swim_error_BLs_SD), ymax = yi+(swim_error_BLs_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data = subset(dat.BL, !is.na(Species_latin)) , mapping = aes(x = xii, y = yi, color = Temp_test_mean), fill = "white", stroke=0.5)+
  geom_point(data =  subset(dat.BL, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  xlab('')+ 
  # ylim(0, 800)+
  ylab(expression(Swim[Relat]~speed~(cm/s)))+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  # ylim(0, 300)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()





# 2. Meta-Analysis models - [not used in the main text]  -------
# model dataset ***********************
# https://stats.stackexchange.com/questions/156754/method-of-meta-analysis-of-studies-to-determine-mean-blood-level
# 1. sampling variances of the means

dat.BL <- escalc(measure="MN", mi=swim_speed_MEAN_BL_s, sdi=swim_error_BLs_SD,
              ni=N_swim_speed, data=data.test.BL)
dat.BL$swim_speed_MEAN_BL_s == dat.BL$yi # the mean in 'yi'
dat.BL$swim_error_BLs_SD^2/dat.BL$N_swim_speed == dat.BL$vi
# vi <- sdi^2/ni

dat.cm <- escalc(measure="MN", mi=SWIM_cms, sdi=SWIM_cms_SD,
                 ni=N_swim_speed, data=data.test.cm)


## rma.mv models -------
# https://stats.stackexchange.com/questions/116659/mixed-effects-meta-regression-with-nested-random-effects-in-metafor-vs-mixed-mod
mod0 <- rma.mv(yi, vi, mods = ~ 1, random = list(~1|Reference_number_1, ~1|Key_origin), method = "ML",  data=dat.cm)
  mod1 <- rma.mv(yi, vi, mods = ~ Temp_test_mean, random = list(~1|Reference_number_1, ~1|Key_origin),method = "ML",  data=dat.cm)
mod2 <- rma.mv(yi, vi, mods = ~ Temp_test_mean + Species_latin , random = list(~1|Reference_number_1, ~1|Key_origin), method = "ML", data=dat.cm)
mod3 <- rma.mv(yi, vi, mods = ~ Temp_test_mean + Species_latin + Sex_F_M , random = list(~1|Reference_number_1, ~1|Key_origin),method = "ML",  data=dat.cm)
  mod5 <- rma.mv(yi, vi, mods = ~ Temp_test_mean + Sex_F_M , random = list(~1|Reference_number_1, ~1|Key_origin), method = "ML", data=dat.cm)

mod2.1 <- rma.mv(yi, vi, mods = ~ Temp_test_mean, random = list(~1|Reference_number_1, ~1|Key_origin, ~ 1|Species_latin), method = "ML",  data=dat.cm)
  mod3.1 <- rma.mv(yi, vi, mods = ~ Temp_test_mean + Sex_F_M , random = list(~1|Reference_number_1, ~1|Key_origin, ~ 1|Species_latin), method = "ML", data=dat.cm)

bictable<-BIC(mod0 , mod1, mod2, mod3, mod5, mod2.1,mod3.1)
BICdelta(bictable)


# 3. Diagnostics ----------
# best
model<-mod5 ### !!! enter best model here 
summary(model)
plot(residuals(model), main="Resid mod6")

## S3 method for class 'rma.mv'
hatvalues(model)


forest(dat.cm$yi, dat.cm$vi,
      # xlim=c(-2.5,3.5),      ### adjust horizontal plot region limits
      order="obs",             ### order by size of yi
      slab=NA, annotate=FALSE, ### remove study labels and annotations
      efac=0,                  ### remove vertical bars at end of CIs
      pch=19,                  ### changing point symbol to filled circle
      col="gray40",            ### change color of points/CIs
      psize=2,                 ### increase point size
      cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
      lty=c("solid","blank"))  ### remove horizontal line at top of plot
### draw points one more time to make them easier to see
points(sort(dat.cm$yi), nrow(dat.cm):1, pch=19, cex=0.5)



# NON-LINEAR models
# https://www.metafor-project.org/doku.php/tips:non_linear_meta_regression



## the variance/weight in model  -------
wi   <- 1/sqrt(dat.cm$vi)
size <- 0.5 + 1.2 * (wi - min(wi))/(max(wi) - min(wi)) # << size for the plot ta show this 

## predict model and CIs ---------
pred1 <- predict(model)
dat.cm$model<-pred1$pred
dat.cm$model.ci.lb<-pred1$ci.lb
dat.cm$model.ci.ub<-pred1$ci.ub


# forest plot to visualize results -------
# https://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html
forest(mod.temp.1, slab = paste(data.test.BL$Reference_number_1, as.character(data.test.BL$year), sep = ", "))
# forest(mod1, slab = paste(data.test.BL$Reference_number_1, as.character(data.test.BL$year), sep = ", "))

# funnel plot --------
# A common way to investigate potential publication bias in a meta-analysis is the funnel plot. Asymmetrical distribution indicates potential publication bias.
# https://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html
funnel(model, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)

### set up 2x2 array for plotting
par(mfrow=c(2,2))
### draw funnel plots
funnel(model, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)
funnel(model, yaxis="vi", main="Sampling Variance")
funnel(model, yaxis="seinv", main="Inverse Standard Error")
funnel(model, yaxis="vinv", main="Inverse Sampling Variance")

# model - data plots ------
# BL/s
dat.cm$wi<-wi
ggplot(data = dat.cm, aes(Species_latin, SWIM_cms, size=wi))+
  geom_point(pch=21)+
  xlab("Species")+
  ylab("Observed Mean (cm/s)")+
  # geom_errorbar()+
  geom_point(aes(Species_latin, model, color = Species_latin), size = 5, pch=23)+
  # geom_point(aes(Species_latin, mod.temp.1.pred, color = Species_latin), size = 3, color="red", pch=21)+
  theme_classic()

ggplot(data = dat.cm, aes(shape =Species_latin,y= SWIM_cms, size=wi, x = Sex_F_M))+
  geom_point(pch=21)+
  xlab("Sex")+
  ylab("Observed Mean (BL/s)")+
  # geom_errorbar()+
  geom_point(aes(shape=Species_latin, y= mod1.pred, x = Sex_F_M, color = Sex_F_M), size = 3, pch=21)+
  # geom_point(aes(Species_latin, mod.temp.1.pred, color = Species_latin), size = 3, color="red", pch=21)+
  theme_classic()

ggplot(data = dat.cm, aes(Temp_test_mean, SWIM_cms, size = wi))+
  geom_point(aes(fill = Species_latin), pch=21, color = "black")+
  labs(x="Species", y="Observed Mean")+
  # geom_line(aes(Temp_test_mean, mod.temp.1.pred), size = 1)+
  geom_line(aes(x=Temp_test_mean, y=mod.temp.1.ci.lb), lty = "solid", size = 0.5, color = "grey")+
  geom_line(aes(x=Temp_test_mean, y=mod.temp.1.ci.ub), lty = "solid", size = 0.5, color = "grey")+
  theme_classic()+
  facet_wrap(.~Species_latin)







