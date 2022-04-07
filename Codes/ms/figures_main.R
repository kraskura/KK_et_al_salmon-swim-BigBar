

library(ggsci)
library(ggridges)
library(cowplot)

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
data.cm$anaerob<-as.factor(data.cm$anaerob)
data.BL.pink<-data.BL[data.BL$Species_latin == "Oncorhynchus gorbuscha",]
data.cm.pink<-data.cm[data.cm$Species_latin == "Oncorhynchus gorbuscha",]

data.BL.soc<-data.BL[data.BL$Species_latin == "Oncorhynchus nerka",]
data.cm.soc<-data.cm[data.cm$Species_latin == "Oncorhynchus nerka",]

data.BL.coho<-data.BL[data.BL$Species_latin == "Oncorhynchus kisutch",]
data.cm.coho<-data.cm[data.cm$Species_latin == "Oncorhynchus kisutch",]

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


# settings, colors ----------
# scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
#                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
#                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
#   scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
#                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
#                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
# *******************************************

# Suppl figs -----------
# data.test[c(data.test$Reference_number_1==89 | data.test$Reference_number_1==9), ]
# ggplot(data[which(!is.na(data$SWIM_cms)),])+
#   geom_histogram(mapping = aes(SWIM_cms, fill =length_speed_source ))+
#   # geom_histogram(mapping = aes(SWIM_cms-swim_speed_MEAN_cm_s), fill = "green3")+
#   facet_grid(.~length_speed_source)+
#   theme(legend.position = "top")

# plot_si0<-ggplot(data[which(!is.na(data$SWIM_cms)),])+
#   geom_point(mapping = aes(y = SWIM_cms, x = LENGTH_cm, fill = length_speed_source), pch=21)+
#   # geom_histogram(mapping = aes(SWIM_cms-swim_speed_MEAN_cm_s), fill = "green3")+
#   facet_grid(.~length_speed_source)+
#   geom_text(mapping = aes(y = SWIM_cms, x = LENGTH_cm, label = Reference_number_1), check_overlap = TRUE, size=1)
# ggformat(plot=plot_si0, y_title ="Swim speed, cm/s", x_title = "Body length, cm")
# plot_si0<-plot_si0+theme(legend.position = "top")
# plot_si0

plot_si1<-ggplot(data[c(data$Species_latin == "Oncorhynchus mykiss" | data$Species_latin == "Oncorhynchus nerka"| data$Species_latin == "Oncorhynchus tshawytscha" | data$Species_latin == "Salmo salar"), ],
                 aes(y = LENGTH_cm, x = Size_MEAN_kg, color = Length_cm_value_source))+
  geom_point(pch=21)+
  scale_color_d3()+
  facet_wrap(.~Species_latin, nrow = 3)+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                     xmax=Size_MEAN_kg+Size_error_kg), size=0.1)+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error), size=0.1)
ggformat(plot=plot_si1, x_title ="Size (kg)", y_title = "Length (cm)")

plot_si2<-ggplot(data, aes(y = LENGTH_cm, x = Size_MEAN_kg, color = Length_cm_value_source))+
  geom_point(pch=21)+
  scale_color_d3()+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                     xmax=Size_MEAN_kg+Size_error_kg), size=0.1)+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error), size=0.1)
ggformat(plot=plot_si2, x_title ="Size (kg)", y_title = "Length (cm)")
# 
# plot_si2<-ggplot(data[which(!is.na(data$SWIM_cms)),], aes(y = SWIM_cms, x = swim_speed_MEAN_BL_s, color = Temp_test_mean))+
#   geom_point()+
#   facet_grid(.~Length_cm_value_source)+
#   geom_abline(slope = 50, intercept = 0)
# ggformat(plot=plot_si2, y_title ="Swim speed (est), cm/s", x_title = "Swim speed BL/s")
# 

ggplot(data)+
  geom_point(mapping = aes(y = SWIM_cms, x = LENGTH_cm, size = N_morphometrics, alpha = N_swim_speed))+
  geom_point(mapping = aes(y = swim_speed_MEAN_cm_s, x = LENGTH_cm, size = N_morphometrics, alpha = N_swim_speed), color = "blue")+
  facet_grid(.~Length_cm_value_source)

# data[which(!is.na(data$Size_kg_estimated) & is.na(data$Size_MEAN_kg)),]

# my_data <- escalc(mi = swim_speed_MEAN_cm_s, sdi =swim_speed_error_cm_s, 
#                   ni = N_morphometrics,
#                   data = data.test, measure = "MD")



# Fig Supl A. --------



year_trend2<-ggplot(data = data[!duplicated(data[,c('Reference_number_1')]),], aes(as.numeric(as.character(Year_published))))+
  # geom_point(position=position_jitter(width=0.2), alpha=1, colour="grey3", pch=21, size=2)+
  geom_histogram( aes(as.numeric(as.character(Year_published))), alpha=0.5, colour="white", binwidth = 1, inherit.aes=FALSE)+
  theme_bw()+
  scale_x_continuous(n.breaks = 7)
ggformat(year_trend2, title = "", y_title = "N studies", x_title = "")
year_trend2<-year_trend2 +theme (axis.ticks.x = element_line(size = 1), axis.text.x = element_text(angle=45, hjust=1, size=12), legend.title = element_blank(), legend.key.width = unit(2, "cm"))
year_trend2

png("../DATA and STATS analyses/Swim_year_cm_bySPECIES.png", res = 200, width = 6, height = 5.1, units="in")
dev.off()


# summary ridgeplots, freq plots ------------
# reorder the species:
  data <- data %>%
  mutate(Species_latin = fct_relevel(Species_latin, levels ="Salmo salar", "Oncorhynchus mykiss","Oncorhynchus gorbuscha","Oncorhynchus kisutch", "Oncorhynchus keta","Oncorhynchus nerka","Oncorhynchus tshawytscha"))

ggplot(data[!(data$Species_latin=="Oncorhynchus masou"),], aes(x = LENGTH_cm, y = Species_latin, fill = stat(x))) +
  geom_density_ridges_gradient(
    jittered_points = TRUE,scale = 1,
    position = position_points_jitter(width = 0.02, height = 0.1),
    point_shape = 21, point_size = 1, point_alpha = 1, alpha = 0.3)+
  theme_ridges()+
  xlab("Body length (cm)")+
  scale_fill_steps2(name = "Length (cm)", mid = "#B7B4BD", high = "#5D3E5B", midpoint = 50, low = "#DAD6E0") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(face = "italic"))

ggplot(data[!(data$Species_latin=="Oncorhynchus masou"),], aes(x = Temp_test_mean, y = Species_latin, fill = stat(x))) +
  geom_density_ridges_gradient(
    jittered_points = TRUE,scale = 1,
    position = position_points_jitter(width = 0.02, height = 0.1),
    point_shape = 21, point_size = 1, point_alpha = 1, alpha = 0.3)+
  scale_fill_gradient2(name = "Temperature", low = "#006CB0", high = "#BF3537", midpoint = 15, mid = "#BEA5AB") +
  theme_ridges()+
  xlab("Temperature (ºC)")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(face = "italic"))


# figure 2 -- all plots together --------
cm.all.s<-ggplot(data=data.cm.rep, aes(x = xi, y = SWIM_cms, shape = Species_latin))+
  geom_hline(yintercept =200, linetype=2, col = "black", lwd=0.3)+
  geom_errorbar(data = (data.cm.rep[!is.na(data.cm.rep$SWIM_cms_SD),]), mapping = aes(ymin = SWIM_cms-(SWIM_cms_SD), ymax = SWIM_cms+(SWIM_cms_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data =  subset(data.cm.rep, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  scale_shape_manual(breaks = c("Oncorhynchus masou", "Oncorhynchus spp.", "Oncorhynchus keta", "Oncorhynchus tshawytscha", 
                                "Oncorhynchus gorbuscha", "Oncorhynchus kisutch", "Oncorhynchus mykiss",  "Oncorhynchus nerka","Salmo salar"), values= c(25, 23, 21, 21, 21, 21, 21, 21, 21))+
  geom_point(data = subset(data.cm.rep[c(data.cm.rep$anaerob == 1),], !is.na(Species_latin)), mapping = aes(x = xi, y = as.numeric(anaerob)), pch="-", color = "black", fill = "white", size = 2, alpha= 0.9)+
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
        legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

cm.all<-ggplot(data=data.cm.rep, aes(x = xi, y = SWIM_cms))+
  geom_hline(yintercept =200, linetype=2, col = "black", lwd=0.3)+
  geom_errorbar(data = (data.cm.rep), mapping = aes(ymin = SWIM_cms-(SWIM_cms_SD), ymax = SWIM_cms+(SWIM_cms_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data =  subset(data.cm.rep, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
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
        # legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(), 
        legend.position="none")+
  coord_flip()
cm.all
cm.all.s

cowplot::plot_grid(cm.all, cm.all.s, 
                  nrow = 1, ncol=2, 
                  rel_widths = c(0.5, 1.5)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig2_cms_order_swim.png",
         width = 11, height = 6,units = "in")

# BL/s *************************************
# BL/s *************************************
# BL/s *************************************
BL.all.s <- ggplot(data=data.BL.rep, aes(x = xi, y = swim_speed_MEAN_BL_s, shape = Species_latin))+
  geom_hline(yintercept =2, linetype=2, col = "black", lwd=0.3)+
  geom_errorbar(data = (data.BL.rep[!is.na(data.BL.rep$swim_error_BLs_SD),]), mapping = aes(ymin = swim_speed_MEAN_BL_s-(swim_error_BLs_SD), ymax = swim_speed_MEAN_BL_s+(swim_error_BLs_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data =  subset(data.BL.rep, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  scale_shape_manual(breaks = c("Oncorhynchus masou", "Oncorhynchus spp.", "Oncorhynchus keta", "Oncorhynchus tshawytscha",
                                "Oncorhynchus gorbuscha", "Oncorhynchus kisutch", "Oncorhynchus mykiss",  "Oncorhynchus nerka","Salmo salar"), values= c(21,21,  21, 21, 21, 21, 21, 21, 21))+
  xlab('')+ 
  ylab(expression(Swim[Rel]~speed~(BL/s)))+
  facet_wrap(~Species_latin, strip.position="left", nrow=2, drop = TRUE) +
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_bw()+
  ylim(0, 15)+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold.italic"),
        legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()
BL.all.s

BL.all<-ggplot(data=data.BL.rep, aes(x = xi, y = swim_speed_MEAN_BL_s))+
  geom_hline(yintercept =2, linetype=2, col = "black", lwd=0.3)+
  geom_errorbar(data = (data.BL.rep), mapping = aes(ymin = swim_speed_MEAN_BL_s-(swim_error_BLs_SD), ymax = swim_speed_MEAN_BL_s+(swim_error_BLs_SD), col=Temp_test_mean, size =1), size = 0.5, width = 0.5)+
  geom_point(data =  subset(data.BL.rep, !is.na(Species_latin)), aes(col=Temp_test_mean, fill=Temp_test_mean),  stroke=0.5, show.legend = FALSE)+
  xlab('')+ 
  ylim(0, 15)+
  ylab(expression(Swim[Rel]~speed~(BL/s)))+
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
        # legend.position = "left", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(), 
        legend.position="none")+
  coord_flip()
BL.all
BL.all.s

cowplot::plot_grid(BL.all, BL.all.s, 
                   nrow = 1, ncol=2, 
                   rel_widths = c(0.5, 1.5)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig2_BLs_order_swim.png",
         width = 11, height = 6,units = "in")









# body size --------

p1.cm<-ggplot(data=data.cm[data.cm$SWIM_cms_source=="reported",], aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                                                 colour=Species_latin,
                                                                 # shape=Sex_F_M,
                                                                 label=Reference_number_1,
                                                                group = anaerob))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  geom_point(size=3, alpha=0.7, pch=19)+
  geom_point(data.cm[c(data.cm$anaerob==1 & data.cm$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=LENGTH_cm,
                  fill=Species_latin), pch=21, size=3, colour="black")+
  ylim(0, 1000)+
  xlim(25, 100)
ggformat(p1.cm, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm<-p1.cm+theme(legend.position = "none")
             # legend.text = element_text(face = "italic"))


p1.BL<-ggplot(data=data.BL, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin,
                                colour=Species_latin,
                                # shape=Sex_F_M,
                                label=Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  geom_point(size=3, alpha=0.7, pch=19)+
  geom_point(data.BL[c(data.BL$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin), pch=21, size=3, colour="black", alpha=1)+
  ylim(0, 15)+
  xlim(25, 100)
ggformat(p1.BL, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="")
p1.BL<-p1.BL+theme(legend.position = "none")


p1.cm.coho<-ggplot(data = data.cm.coho[data.cm.coho$SWIM_cms_source=="reported" & data.cm.coho$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.coho[data.cm.coho$SWIM_cms_source=="estimated" & data.cm.coho$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=LENGTH_cm), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 2)+
  geom_point(data.cm.coho[c(data.cm.coho$anaerob==1 & data.cm.coho$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=LENGTH_cm,fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  ylim(20, 300)+
  #   scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.cm.coho, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.cm.coho<-p1.cm.coho+theme(legend.position = "none")


p1.cm.soc<-ggplot(data = data.cm.soc[data.cm.soc$SWIM_cms_source=="reported" & data.cm.soc$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.soc[data.cm.soc$SWIM_cms_source=="estimated" & data.cm.soc$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=LENGTH_cm), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 2)+
  geom_point(data.cm.soc[c(data.cm.soc$anaerob==1 & data.cm.soc$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=LENGTH_cm,
                                                                                                              fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  ylim(20, 300)+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
    scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.cm.soc, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.cm.soc<-p1.cm.soc+theme(legend.position = "none")


p1.cm.pink<-ggplot(data = data.cm.pink[data.cm.pink$SWIM_cms_source=="reported" & data.cm.pink$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.pink[data.cm.pink$SWIM_cms_source=="estimated" & data.cm.pink$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=LENGTH_cm), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 3)+
  geom_point(data.cm.pink[c(data.cm.pink$anaerob==1 & data.cm.pink$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=LENGTH_cm,
                                                                                                                 fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  ylim(20, 300)+
    scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.cm.pink, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.cm.pink<-p1.cm.pink+theme(legend.position = "none")


p1.BL.coho<-ggplot(data = data.BL.coho, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+ 
  geom_point(data.BL.coho[c(data.BL.coho$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  ylim(0, 5)+
    scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.BL.coho, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.BL.coho<-p1.BL.coho+theme(legend.position = "none")


p1.BL.soc<-ggplot(data = data.BL.soc, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+                                                                                                    
  geom_point(data.BL.soc[c(data.BL.soc$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  ylim(0, 5)+
    scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.BL.soc, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.BL.soc<-p1.BL.soc+theme(legend.position = "none")


p1.BL.pink<-ggplot(data = data.BL.pink, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+                                                                                                    
  geom_point(data.BL.pink[c(data.BL.pink$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  geom_errorbarh(aes(xmin=LENGTH_cm-Length_error,
                     xmax=LENGTH_cm+Length_error), size =0.2)+
  ylim(0, 5)+
    scale_x_continuous(limits = c(25, 80), breaks = c(30, 40, 50, 60, 70, 80))+
  theme(legend.position = "none")
ggformat(p1.BL.pink, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="", size_text = 14)
p1.BL.pink<-p1.BL.pink+theme(legend.position = "none")

# save size plots ----------

cowplot::plot_grid(p1.cm, p1.BL, 
                  nrow = 2, ncol =1, align = "hv", 
                  labels = "AUTO", 
                  label_x = c(0.2, 0.2),
                  label_y = c(0.9, 0.9)) %>% 
ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1AB_size_speed.png",
       width = 5, height = 10,units = "in")

cowplot::plot_grid(p1.cm.soc, p1.cm.pink,  p1.cm.coho,
                   nrow = 3, ncol =1, align = "hv", 
                   labels = "auto", 
                   label_x = c(0.24, 0.24, 0.24),
                   label_y = c(0.86, 0.86, 0.86)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1abc_size_speed.png",
         width = 3.2, height = 8,units = "in")

cowplot::plot_grid(p1.BL.soc, p1.BL.pink,  p1.BL.coho,
                   nrow = 3, ncol =1, align = "hv", 
                   labels = "auto", 
                   label_x = c(0.2, 0.2, 0.2),
                   label_y = c(0.86, 0.86, 0.86)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig1abc_sizeBL_speed.png",
         width = 3, height = 8,units = "in")

# temperature --------

p2.cm<-ggplot(data= data.cm[data.cm$SWIM_cms_source=="reported",], aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin,
                                colour=Species_latin,
                                # shape=Sex_F_M,
                                label=Reference_number_1,
                                group = anaerob))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  geom_point(size = 3, alpha=0.7, pch=19)+
  geom_point(data.cm[c(data.cm$anaerob==1 & data.cm$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin), pch=21, size = 3, colour="black", alpha=0.7)+
  ylim(0, 850)
ggformat(p2.cm, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.cm<-p2.cm+theme(legend.position = "none")


p2.BL<-ggplot(data=data.BL, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin,
                                colour=Species_latin,
                                # shape=Sex_F_M,
                                label=Reference_number_1))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size= 3, alpha=0.7, pch=19)+
  geom_point(data.BL[c(data.BL$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin), pch=21, size = 3, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  ylim(0, 15)
ggformat(p2.BL, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="")
p2.BL<-p2.BL+theme(legend.position = "none")


p2.cm.coho<-ggplot(data = data.cm.coho[data.cm.coho$SWIM_cms_source=="reported" & data.cm.coho$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.coho[data.cm.coho$SWIM_cms_source=="estimated" & data.cm.coho$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=Temp_test_mean), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 2)+
  geom_point(data.cm.coho[c(data.cm.coho$anaerob==1 & data.cm.coho$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=Temp_test_mean,fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  ylim(20, 300)+
  xlim(2, 30)+
  theme(legend.position = "none")
ggformat(p2.cm.coho, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.cm.coho<-p2.cm.coho+theme(legend.position = "none")


p2.cm.soc<-ggplot(data = data.cm.soc[data.cm.soc$SWIM_cms_source=="reported" & data.cm.soc$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.soc[data.cm.soc$SWIM_cms_source=="estimated" & data.cm.soc$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=Temp_test_mean), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 2)+
  geom_point(data.cm.soc[c(data.cm.soc$anaerob==1 & data.cm.soc$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=Temp_test_mean,
                                                                                                                 fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  ylim(20, 300)+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  xlim(2, 30)+
  theme(legend.position = "none")
ggformat(p2.cm.soc, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.cm.soc<-p2.cm.soc+theme(legend.position = "none")


p2.cm.pink<-ggplot(data = data.cm.pink[data.cm.pink$SWIM_cms_source=="reported" & data.cm.pink$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  geom_point(data=data.cm.pink[data.cm.pink$SWIM_cms_source=="estimated" & data.cm.pink$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=Temp_test_mean), color = "#A2A6B8", fill = "#D7D7E5", alpha =1, size=2, pch=21)+
  geom_point(size = 3)+
  geom_point(data.cm.pink[c(data.cm.pink$anaerob==1 & data.cm.pink$SWIM_cms_source=="reported"),], mapping = aes(y=SWIM_cms, x=Temp_test_mean,
                                                                                                              fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms-SWIM_cms_SD,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  ylim(20, 300)+
  xlim(2, 30)+
  theme(legend.position = "none")
ggformat(p2.cm.pink, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.cm.pink<-p2.cm.pink+theme(legend.position = "none")


p2.BL.coho<-ggplot(data = data.BL.coho, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+ 
  geom_point(data.BL.coho[c(data.BL.coho$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  ylim(0, 5)+
  xlim(2, 30)+
  theme(legend.position = "none")
ggformat(p2.BL.coho, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.BL.coho<-p2.BL.coho+theme(legend.position = "none")


p2.BL.soc<-ggplot(data = data.BL.soc, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+                                                                                                    
  geom_point(data.BL.soc[c(data.BL.soc$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  ylim(0, 5)+
  xlim(2, 30)+
  theme(legend.position = "none")
ggformat(p2.BL.soc, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.BL.soc<-p2.BL.soc+theme(legend.position = "none")


p2.BL.pink<-ggplot(data = data.BL.pink, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin, fill = Species_latin))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(size = 2)+                                                                                                    
  geom_point(data.BL.pink[c(data.BL.pink$swim_speed_MEAN_BL_s>=2),], mapping = aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin), pch=21, size=2, colour="black", alpha=1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  ylim(0, 5)+
  xlim(2, 30)
ggformat(p2.BL.pink, print=TRUE, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="", size_text = 14)
p2.BL.pink<-p2.BL.pink+theme(legend.position = "none")

# save size plots ----------

cowplot::plot_grid(p2.cm, p2.BL, 
                   nrow = 2, ncol =1, align = "hv", 
                   labels = "AUTO", 
                   label_x = c(0.2, 0.2),
                   label_y = c(0.9, 0.9)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig2AB_temp_speed.png",
         width = 5, height = 10,units = "in")

cowplot::plot_grid(p2.cm.soc, p2.cm.pink,  p2.cm.coho,
                   nrow = 3, ncol =1, align = "hv", 
                   labels = "auto", 
                   label_x = c(0.24, 0.24, 0.24),
                   label_y = c(0.86, 0.86, 0.86)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig2abc_temp_speed.png",
         width = 3.2, height = 8,units = "in")

cowplot::plot_grid(p2.BL.soc, p2.BL.pink,  p2.BL.coho,
                   nrow = 3, ncol =1, align = "hv", 
                   labels = "auto", 
                   label_x = c(0.2, 0.2, 0.2),
                   label_y = c(0.86, 0.86, 0.86)) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig2abc_tempBL_speed.png",
         width = 3, height = 8,units = "in")



# -ttf -----------

ttf.p<-ggplot(data = data.ttf, aes(y=SWIM_cms, x=Duration_swim, color = Species_latin, fill = Species_latin,size = LENGTH_cm))+
  # geom_hline(yintercept = 2, lty=2, lwd=0.5)+
  geom_point(pch=21, alpha=0.6)+
  scale_color_igv()+
  scale_fill_igv()
ggformat(ttf.p, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Swim duration")

ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigS1_ttf.png",
       width = 7, height = 4,units = "in")



# swim tests and swim speeds --------------
swim_test.cm<-ggplot(data = data.cm,
                        aes(y=SWIM_cms, fill=Species_latin, x=Test_performance2, color = Species_latin, group = Test_performance2))+
  geom_hline(yintercept = 200, lwd =0.2, color = "black", lty=2)+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values=c(21, 1))+
  geom_point(size = 2)+
  ylim(0, 850)+
  facet_wrap(.~Species_latin, nrow = 4)+
  geom_point(data.cm[c( data.cm$anaerob==1),],
             mapping = aes(y=SWIM_cms, fill=Species_latin, color=Species_latin, x=Test_performance2, group = Test_performance2),
             pch=21, size=2, colour="black", alpha=1)
ggformat(swim_test.cm, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F)
swim_test.cm<-swim_test.cm +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic"),
        strip.background = element_rect(color="white"),
        legend.position = "none")
swim_test.cm

swim_test.BL<-ggplot(data = data.BL,
                        aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, x=Test_performance2, color = Species_latin, group = Test_performance2))+
  geom_hline(yintercept = 2, lwd =0.2, color = "black", lty=2)+
  geom_boxplot(fill = "white", alpha=0.8, outlier.shape=NA) + # shape=Indiv_group)
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_point(size = 2)+
  scale_y_continuous(breaks=c(0, 2,  5, 8), limits=c(0,8))+
  facet_wrap(.~Species_latin, nrow = 4)+
  geom_point(data.BL[c( data.BL$swim_speed_MEAN_BL_s>=2),],
             mapping = aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, color=Species_latin, x=Test_performance2, group = Test_performance2),
             pch=21, size=2, colour="black", alpha=1)
ggformat(swim_test.BL, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F)
swim_test.BL<-swim_test.BL +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic"),
        strip.background = element_rect(color="white"),
        legend.position = "none")
swim_test.BL

png("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig3_testBL_speed.png",
    res = 300, width = 4.35, height = 10, units="in")
grid.arrange(swim_test.BL)
dev.off()

png("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig3_testcm_speed.png",
    res = 300, width = 5.35, height = 10, units="in")
grid.arrange(swim_test.cm)
dev.off()

# exploratory additional effects - discharge, tests, -------------

library("PerformanceAnalytics")

cond.data.cont <- data[, c("SWIM_cms", "LENGTH_cm",  "Water_flows_cm_s",#
                    "GSI_MEAN", "Temp_test_mean", "Gonad_g")]
cond.data.contBL <- data[, c("swim_speed_MEAN_BL_s", "LENGTH_cm",  "Water_flows_cm_s", "Water_flows_m_3_s",
                           "GSI_MEAN", "Temp_test_mean", "Gonad_g")]
cond.data.cont[] <- lapply(cond.data.cont, function(x) as.numeric(as.character(x)))

cond.data.cat <- data[, c("SWIM_cms", "LENGTH_cm","Temp_test_mean",
                          "Mortality", "Surgery", "Swim_Conditions2",
                          "Species_latin", "Blood","Recovery", "Fish_Conditions")]
chart.Correlation(cond.data.cont,  pch=19, na.action = na.omit)
chart.Correlation(cond.data.cont,  pch=19, na.action = na.omit)


png("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigSI_correll.png",res =300, width = 10, units = "in", height = 10)
    plot(cond.data.cont)
dev.off()
png("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/FigSI_correllBL.png",res =300, width = 10, units = "in", height = 10)
  plot(cond.data.contBL)
dev.off()

ggplot(data = cond.data.cat ,
       aes(y=SWIM_cms, x = Recovery,
           fill=Species_latin,  color = Species_latin))+
  geom_point()
ggplot(data = cond.data.cat ,
       aes(y=SWIM_cms, x = Recovery,
           fill=Species_latin,  color = Species_latin))+
  geom_point()

ggplot(data = cond.data.cat ,
       aes(y=SWIM_cms, x = Mortality,
          color = Species_latin))+
  geom_violin()+
  geom_point(position = position_jitterdodge(), pch=21)+
  theme_classic()+
  facet_wrap(.~Species_latin, nrow=4)+
  stat_summary(fun.y="mean", geom="line", aes(group=factor(grouping)))


# Fish conditions --------------


level_order <- c("fallback", "pass", "mature" , "exercise trained","diet" , "infection", "unhealthy",  "prior anesthetic","spawned",  "density", "density exercise trained","toxicant ") #this vector might be useful for other plots/analyses

fish_condBL<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, x=factor(Fish_Conditions, level = level_order), group = Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA) +
  geom_point(position=position_jitter(width = 0.2, seed=1002), alpha=1, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                   labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                   values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  # scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),
  #                  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))
  coord_flip()# scale_fill_manual(values = c("white", "#386CB0", "dodgerblue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
ggformat(fish_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "", print = F)
fish_condBL<-fish_condBL + theme( #legend.position = "none")
                                  legend.title = element_blank(),
                                  # legend.box = element_rect(fill = "white", colour = "grey30", size=0.5),
                                  legend.position = c(0.77,0.79),
                                  legend.text = element_text(face = "italic", size = 14))
fish_condBL


level_order <- c("fallback", "pass", "mature" , "exercise trained","diet" , "infection", "unhealthy",  "prior anesthetic","spawned",  "density", "density exercise trained","toxicant ") #this vector might be useful for other plots/analyses

data$fillFC<-factor(paste(data$Species_latin, data$SWIM_cms_source, sep=""))
levels.FC<-c("Oncorhynchus gorbuschaestimated", "Oncorhynchus gorbuschaNA" , "Oncorhynchus gorbuschareported",
"Oncorhynchus ketaestimated", "Oncorhynchus ketaNA","Oncorhynchus ketareported" ,
"Oncorhynchus kisutchestimated","Oncorhynchus kisutchreported", 
"Oncorhynchus masouestimated" , "Oncorhynchus masouNA",  
"Oncorhynchus mykissestimated" , "Oncorhynchus mykissreported",    
"Oncorhynchus nerkaestimated" ,"Oncorhynchus nerkaNA","Oncorhynchus nerkareported", 
"Oncorhynchus tshawytschaestimated", "Oncorhynchus tshawytschareported",
"Salmo salarestimated" ,"Salmo salarreported") 
fill.FC<-c("#D7D7E5", "black", "#D292CD", "#D7D7E5", "black", "#FB9A62","#D7D7E5", "#FBC063","#D7D7E5",  "black", "#D7D7E5",  "#615B70", "#D7D7E5", "black","#EA573D", "#D7D7E5", "#70Af81","#D7D7E5", "#64B0BC")
cols.FC<-c("#D292CD", "black", "black",
           "#FB9A62", "black", "black",
           "#FBC063", "black",
           "#446699",  "black",
           "#615B70",  "black",
           "#EA573D", "black","black",
           "#70Af81", "black",
           "#64B0BC", "black")

fish_condcm<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=SWIM_cms, fill=fillFC, color = fillFC, x=factor(Fish_Conditions, level = level_order), group = Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA, color = "black", show.legend = F) +
  geom_point(position=position_jitter(width = 0.2, seed=1002), alpha=1, pch=21, size=2, show.legend = FALSE)+
  # geom_point(data = data[!is.na(data$Fish_Conditions) & data$SWIM_cms_source == "reported",], aes(y=SWIM_cms, fill=Species_latin, x=factor(Fish_Conditions, level = level_order), color = "grey", group = Fish_Conditions))+
  scale_fill_manual(breaks = levels.FC,
                     values = fill.FC)+
  scale_color_manual(breaks = levels.FC,
                    values = cols.FC)+
  # scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),
  #                  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))
  coord_flip()# scale_fill_manual(values = c("white", "#386CB0", "dodgercmue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
ggformat(fish_condcm, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F)
fish_condcm<-fish_condcm + theme( legend.title = element_blank(),
                                  # legend.box = element_rect(fill = "white", colour = "grey30", size=0.5),
                                  legend.position = c(0.77,0.79),
                                  legend.text = element_text(face = "italic", size = 25))
fish_condcm

cowplot::plot_grid( fish_condcm,fish_condBL,
                   nrow = 2, ncol=1, 
                   labels = "AUTO",
                   label_x = c(0.94),
                   label_y = c(0.93, 0.93)
                   ) %>% 
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig7_cms_Fish_Cond.png",
         width = 8, height = 11,units = "in")


# SWIM TESTS --------------

# sample sizes on the plot
dd.label.BL<-data[!is.na(data$Test_performance2) & !is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Test_performance2) %>% 
  summarize(n_stud = length(unique(Reference_number_1)), n = n())
dd.label.BL$label<-paste( dd.label.BL$n, " (", dd.label.BL$n_stud,")", sep="")

dd.label.cm<-data[!is.na(data$Test_performance2) & !is.na(data$SWIM_cms),] %>% 
  group_by(Test_performance2) %>% 
  summarize(n_stud = length(unique(Reference_number_1)), n = n())
dd.label.cm$label<-paste( dd.label.cm$n, " (", dd.label.cm$n_stud,")", sep="")



level_orderTest <- c("Field", "Jump", "Ucrit" , "Umax","Swim")

test_condBL<-ggplot(data = data[!is.na(data$Test_performance2),],
                    aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, x=factor(Test_performance2, levels = level_orderTest), group = Test_performance2))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA) +
  geom_point(position=position_jitter(width = 0.2, seed=1002), alpha=1, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_y_continuous(limits = c(-0.41, 15), breaks = c(0, 3, 6, 9, 12, 15) )+
  geom_text(mapping = aes( x = factor(Test_performance2, levels = level_orderTest),fill=NULL, y = -0.3, label = label), color = "black", size=3, data = dd.label.BL)
# scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),
  #                  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))
  # coord_flip()# scale_fill_manual(values = c("white", "#386CB0", "dodgerblue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
ggformat(test_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "", print = F, size_text = 13)
test_condBL <-test_condBL + theme( #legend.position = "none")
                                      legend.title = element_blank(),
                                      # legend.box = element_rect(fill = "white", colour = "grey30", size=0.5),
                                      legend.position = c(0.71,0.71),
                                      legend.text = element_text(face = "italic", size = 9))
test_condBL


# level_order <- c("fallback", "pass", "mature" , "exercise trained","diet" , "infection", "unhealthy",  "prior anesthetic","spawned",  "density", "density exercise trained","toxicant ") #this vector might be useful for other plots/analyses

test_condcm<-ggplot(data = data[!is.na(data$Test_performance2),], aes(y=SWIM_cms, fill=fillFC, color = fillFC, x=factor(Test_performance2,  levels = level_orderTest), group = Test_performance2))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA, color = "black") +
  geom_point(position=position_jitter(width = 0.2, seed=1002), alpha=1, pch=21, size=2, show.legend = F)+
  scale_fill_manual(breaks = levels.FC,
                    values = fill.FC)+
  scale_color_manual(breaks = levels.FC,
                     values = cols.FC)+
  geom_text(mapping = aes( x = factor(Test_performance2, levels = level_orderTest),fill=NULL, y = -18, label = label), color = "black", size=3, data = dd.label.cm)
  scale_y_continuous(limits = c(-20, 850), breaks = c(0,100, 200, 300, 400, 500, 600,700, 800) )
  # scale_x_discrete(breaks = c("Oncorhynchus spp.", "Oncorhynchus mykiss", "Oncorhynchus gorbuscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Oncorhynchus masou", "Salmo salar"),
  #                  labels=c("Oncorhynchus spp.", "O. mykiss", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha","O. masou","Salmo salar"))
  # coord_flip()# scale_fill_manual(values = c("white", "#386CB0", "dodgercmue", "#386CB0", "red",  "#D9D9D9","#FFFFB3","#7FC97F",   "#A65628", "#FB8072", "grey", "darkgrey"))+
ggformat(test_condcm, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F, size_text = 13)
test_condcm<-test_condcm + theme( legend.title = element_blank(),
                                  # legend.box = element_rect(fill = "white", colour = "grey30", size=0.5),
                                  legend.position = c(0.77,0.79),
                                  legend.text = element_text(face = "italic", size = 9))
test_condcm

cowplot::plot_grid( test_condcm,test_condBL,
                    nrow = 1, ncol=2,
                    labels = "AUTO",
                    label_x = c(0.9, 0.9),
                    label_y = c(0.92)) %>%
  ggsave(filename = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Figures/Fig8_cms_SwimTest.png",
         width = 8.5, height = 5,units = "in")



# field studies ---------
dataF<-as.data.frame(data.all[7])
for ( i in 1:nrow(dataF)){
  if(!dataF$Mortality[i]==0){
    Mortality<-"Mortality"
  }else{
    Mortality<-""
  }
  
  if(!dataF$Recovery[i]==0){
    Recovery<-paste("Recov: ", dataF$Recovery[i], sep="")
  }else{
    Recovery<-""
  }
  
  if(!is.na(dataF$Fish_Conditions[i])){
    Fish_Conditions<-paste("Cond: ", dataF$Fish_Conditions[i], sep="")
  }else{
    Fish_Conditions<-""
  }
  
  # if(!is.na(dataF$Water_flows_cm_s[i])){
  #   Water_flows_cm_s<-paste("Flow: ", dataF$Water_flows_cm_s[i], sep="")
  # }else{
  #   Water_flows_cm_s<-""
  # }
  
  # if(!is.na(dataF$Swim_Conditions2[i])){
  #   Swim_Conditions2<-paste("Migrat: ", dataF$Swim_Conditions2[i], sep="")
  # }else{
  #   Swim_Conditions2<-""
  # }
  
  # cond<-paste(Mortality,Recovery, Fish_Conditions, Water_flows_cm_s, Swim_Conditions2, sep=" ")
  cond<-paste(Mortality,Recovery, Fish_Conditions, sep=" ")
  dataF$cond[i]<-cond
}
# dataF$cond<-paste(dataF$Mortality,dataF$Recovery, dataF$Fish_Conditions, dataF$Water_flows_cm_s, dataF$Swim_Conditions2, sep="-")

datF.sum<-dataF %>% 
  group_by(cond, Species_latin) %>% 
  summarize(mean_swim = mean(SWIM_cms, na.rm =TRUE))
  
ggplot(datF.sum, aes(y = mean_swim, x = Species_latin , color = Species_latin, group = cond))+
  geom_point( size=3 )+
  geom_text( aes(y = mean_swim, x = Species_latin, label=cond))+
  facet_wrap(.~Species_latin, nrow=2, scales = "free_x")













# MISC -----------

ggplot(data[data$SWIM_cms_source=="reported" & data$swim_speed_MEAN_BL_s<5, ], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M))+
  geom_point(data=data[data$SWIM_cms_source=="estimated" & data$swim_speed_MEAN_BL_s<5,], aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M), fill = "white", alpha = 1)+
  geom_point()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  theme_light()+
  facet_wrap(.~Species_latin)

ggplot(data, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin, fill = Species_latin, shape = Sex_F_M))+
  geom_point()+
  theme_light()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+ 
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")


# same but exclude species with sample size < 10 
ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=SWIM_cms, x=Temp_test_mean, color = Species_latin, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth()


# body size
ggplot(data, 
       aes(y=SWIM_cms, x=LENGTH_cm, color = Temp_test_mean, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(data, 
       aes(y=SWIM_cms, x=LENGTH_cm, color = Temp_test_mean))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  geom_smooth(method = "gam")

ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Temp_test_mean, shape = Sex_F_M, group = Species_latin))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  facet_wrap(.~Species_latin, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(data[!c( data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, color = Temp_test_mean))+
  geom_point()+
  scale_color_viridis_c()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_c()+
  theme_light()+
  geom_smooth(method = "gam")

# saltwater versus freshwater
ggplot(data[!c(data$Species_latin == "Oncorhynchus masou" | data$Species_latin == "Oncorhynchus keta" | data$Species_latin == "Oncorhynchus tshawytscha"| data$Species_latin == "Oncorhynchus spp."),], 
       aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, color = Species_latin))+
  geom_point()+
  scale_color_viridis_d()+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_fill_viridis_d()+
  facet_grid(.~SW_FW)+
  theme_light()

