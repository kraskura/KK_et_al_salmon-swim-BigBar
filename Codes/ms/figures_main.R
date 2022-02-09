

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
# reorder teh species:
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

cond.data.cont <- data[, c("SWIM_cms", "LENGTH_cm", "Water_flows_cm_s", "Water_flows_m_3_s",
                    "GSI_MEAN", "Temp_test_mean", "Gonad_g")]
cond.data.cont[] <- lapply(cond.data.cont, function(x) as.numeric(as.character(x)))

cond.data.cat <- data[, c("SWIM_cms", "LENGTH_cm","Temp_test_mean",
                          "Mortality", "Surgery", "Swim_Conditions2",
                          "Species_latin", "Blood","Recovery", "Fish_Conditions")]
chart.Correlation(cond.data.cont,  pch=19, na.action = na.omit)
plot(cond.data.cont)

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

