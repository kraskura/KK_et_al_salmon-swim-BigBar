

library(ggsci)
library(ggridges)
library(cowplot)
library(ggExtra)
library(gridExtra)
library(here)
library(ggformat2)

# Source data and code: ---------
source("./Codes/PSC/table_BIC.R")
source("./Codes/get_dataset.R")
# *************************


# 1. import and wrangle the data -------------
data.all<-get.adult.salmonid.swim.data(
  data.file = "./Data/Files/Kraskura_salmonSwim_analysis_feb2023.csv")

# view(data)
# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])
data.ttf<-data[c(data$Test_performance2=="TTF"),]
data<-data[!c(data$Test_performance2=="TTF" | -c(data$Species_latin=="Oncorhynchus spp.") ),] 
data.BL<-data[!is.na(data$swim_speed_MEAN_BL_s),]
data.cm<-data[!is.na(data$SWIM_cms),]
dataF<-as.data.frame(data.all[7])
dataLab<-as.data.frame(data.all[8])

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

# reported only cm/s
data.cm.rep<-data.cm[data.cm$SWIM_cms_source == "reported",]
data.cm.rep$Species_latin2<-as.character(data.cm.rep$Species_latin)
data.cm.rep$Species_latin2[which(data.cm.rep$Species_latin == "Oncorhynchus masou" | data.cm.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.cm.rep$Species_latin2<-factor(data.cm.rep$Species_latin2)

data.BL.rep<-data.BL[!is.na(data.BL$swim_speed_MEAN_BL_s),]
data.BL.rep$Species_latin2<-as.character(data.BL.rep$Species_latin)
data.BL.rep$Species_latin2[which(data.BL.rep$Species_latin == "Oncorhynchus masou" | data.BL.rep$Species_latin == "Oncorhynchus spp.")]<-"Mixed species"
data.BL.rep$Species_latin2<-factor(data.BL.rep$Species_latin2)

# Figures --------------------------------------------------------------------
## Supplement: Fig Size - length ---------------------------------------------
plot_si1<-ggplot(data[!is.na(data$Length_cm_value_source),],
                 aes(y = LENGTH_cm, x = Size_MEAN_kg, color = Length_cm_value_source))+
  geom_point(pch=21)+
  scale_color_d3()+
  facet_wrap(.~Species_latin, nrow = 3)+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                     xmax=Size_MEAN_kg+Size_error_kg), linewidth=0.1)+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error), linewidth=0.1)+
  guides(color=guide_legend(title="Data Source"))
ggformat(plot=plot_si1, x_title ="Size (kg)", y_title = "Length (cm)", print = F, size_text = 11)
plot_si1<-plot_si1+theme(legend.position = "top")

plot_si2<-ggplot(data, aes(y = LENGTH_cm, x = Size_MEAN_kg, color = Length_cm_value_source))+
  geom_point(pch=21)+
  scale_color_d3()+
  geom_errorbarh(aes(xmin=Size_MEAN_kg-Size_error_kg,
                     xmax=Size_MEAN_kg+Size_error_kg), linewidth=0.1)+
  geom_errorbar(aes(ymin=Length_MEAN_cm-Length_error,
                    ymax=Length_MEAN_cm+Length_error), linewidth=0.1)
ggformat(plot=plot_si2, x_title ="Size (kg)", y_title = "Length (cm)", size_text = 15)
plot_si2<-plot_si2+theme(legend.position = "none")

cowplot::plot_grid(plot_si2, plot_si1,
                  nrow = 2, ncol =1,
                  labels = "AUTO",
                  label_x = c(0.12),
                  label_y = c(0.9, 0.9), 
                  rel_heights = c(0.8,1)) %>%
ggsave(filename = "./ms_exports/Figures/FigS1_WLrship.png",
       width = 6, height = 9,units = "in")


## Fig 3: Years published, histogram --------
year_trend2<-ggplot(data = data[!duplicated(data[,c('Reference_number_1')]),], aes(as.numeric(as.character(Year_published)), fill = Species_latin))+
  geom_histogram( aes(as.numeric(as.character(Year_published)), fill = Species_latin), alpha=0.8, colour="white", binwidth = 1, inherit.aes=FALSE)+
  theme_bw()+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_x_continuous(n.breaks = 7)
ggformat(year_trend2, title = "", y_title = "N studies", x_title = "")
year_trend2<-year_trend2 +
  theme (axis.ticks.x = element_line(size = 1),
         axis.text.x = element_text(angle=45, hjust=1, size=12),
         legend.title = element_blank(),
         legend.position = c(0.15,0.8),
         legend.key.size = unit(0.3, 'cm'),
         legend.text = element_text(face = "italic"),
         legend.key.width = unit(0.5, "cm"))
year_trend2

ggsave2(filename = "./ms_exports/Figures/Fig3_year_hist.png", plot = year_trend2, width = 6, height = 4.1, units="in")


## Fig 4: cm.s and BL.s combined --------
cm.BL.RIDGE.s<-
  ggplot(data=data, aes(x = SWIM_cms, y = Species_latin, group = Species_latin, color = Species_latin, fill = Species_latin))+
  geom_vline(xintercept =c(100, 200,400), linetype=2, col = "black", lwd=0.3)+
  geom_density_ridges(data=data,
                      mapping = aes(x = swim_speed_MEAN_BL_s*100, group = Species_latin),
                      color = "grey50", fill = "grey50", size = 0.3,
                      scale = 0.8, rel_min_height = 0.01, alpha = 0.6, 
                      jittered_points = TRUE, position = position_points_jitter(width = 0.01, height = 0.02),
                      point_shape = 1, point_size = 0.5, point_alpha = 1)+
  geom_density_ridges(scale = 0.8, rel_min_height = 0.01, alpha = 0.7, size = 0.3,jittered_points = TRUE,
                      position = position_points_jitter(width = 0.01, height = 0.02),
                      point_shape = 1, point_size = 0.5, point_alpha = 1)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  xlab('')+
  scale_x_continuous(breaks = c(0, 100, 200, 400, 700, 1000),
                     labels = c("0\n 0", "100\n1", "200\n2", "400\n4", "700\n7", "1000 cm/s \n10 BL/s"))+
  theme_minimal()+
  annotate(geom = "text", y = 8.6, x = 1065, hjust = 0, color = "#64B0BC", size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Salmo salar",])) * ")"))+
  annotate(geom = "text", y = 8.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Salmo salar", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Salmo salar",])) * ")"))+
  
  annotate(geom = "text", y = 7.6, x = 1065, hjust = 0, color = "#70Af81",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus tshawytscha",])) * ")"))+
  annotate(geom = "text", y = 7.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus tshawytscha", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus tshawytscha",])) * ")"))+
  
  annotate(geom = "text", y = 6.6, x = 1065, hjust = 0, color = "#EA573D",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus nerka",])) * ")"))+
  annotate(geom = "text", y = 6.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus nerka", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus nerka",])) * ")"))+
 
  annotate(geom = "text", y = 5.6, x = 1065, hjust = 0, color = "#615B70",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus mykiss",])) * ")"))+
  annotate(geom = "text", y = 5.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus mykiss", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus mykiss",])) * ")"))+
 
  annotate(geom = "text", y = 4.6, x = 1065, hjust = 0, color = "#446699",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus masou",])) * ")"))+
  annotate(geom = "text", y = 4.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus masou", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus masou",])) * ")"))+
  
  annotate(geom = "text", y = 3.6, x = 1065, hjust = 0, color = "#FBC063",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus kisutch",])) * ")"))+
  annotate(geom = "text", y = 3.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus kisutch", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus kisutch",])) * ")"))+
 
  annotate(geom = "text", y = 2.6, x = 1065, hjust = 0, color = "#FB9A62",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus keta",])) * ")"))+
  annotate(geom = "text", y = 2.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus keta", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus keta",])) * ")"))+
 
  annotate(geom = "text", y = 1.6, x = 1065, hjust = 0, color = "#D292CD",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus gorbuscha", "SWIM_cms"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$SWIM_cms) & data$Species_latin == "Oncorhynchus gorbuscha",])) * ")"))+
  annotate(geom = "text", y = 1.2, x = 1065, hjust = 0, color = "black",  size = 2.7,
           label = bquote("Max = " ~ .(round(max(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus gorbuscha", "swim_speed_MEAN_BL_s"], na.rm = TRUE),2)) ~
                            "(" * .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s) & data$Species_latin == "Oncorhynchus gorbuscha",])) * ")"))+
  scale_y_discrete(expand = expansion(add = c(0, 1.05)))+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_text(hjust=1,vjust = 1, face="bold.italic"),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title.y=element_blank(),
        legend.position = "none", 
        strip.background = element_blank())
cm.BL.RIDGE.s

cm.BL.RIDGE<-ggplot(data=data, aes(x = SWIM_cms))+
  geom_vline(xintercept =c(100, 200,400), linetype=2, col = "black", lwd=0.3)+
  geom_density(mapping = aes(x = swim_speed_MEAN_BL_s*100),
                      color = "grey50", fill = "grey50", alpha = 0.6)+
  geom_density(alpha = 0.7, color = "black", fill = "#D5CABD")+
  ylab('Relative Density of Data')+
  annotate(geom = "text", x = 500, y = 0.009, hjust = 0,
           label = bquote("Relative swim speeds BL/s (n = " ~ .(nrow(data[!is.na(data$swim_speed_MEAN_BL_s),])) ~ ")"),
           color = "#5c4d3b", fontface = "bold")+
  annotate(geom = "text", x = 500, y = 0.008, hjust = 0,
           label = bquote("Absolute swim speeds cm/s (n = " ~ .(nrow(data[!is.na(data$SWIM_cms),])) ~ ")"),
           color = "black", fontface = "bold")+
  scale_x_continuous(breaks = c(0, 100, 200, 400,700, 1000),
                     labels = c("0\n 0", "100\n1", "200\n2", "400\n4", "700\n7", "1000 cm/s \n10 BL/s"))+
  theme_minimal()+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(face="bold"),
        legend.position = "none", 
        strip.background = element_blank())
cm.BL.RIDGE

cowplot::plot_grid(cm.BL.RIDGE, cm.BL.RIDGE.s, 
                   nrow = 2, ncol=1, 
                   labels = "AUTO",
                   label_x = c(0.04, 0.04), 
                   label_y = c(0.98, 0.98),
                   rel_widths = c(0.5, 1.5)) %>% 
  ggsave(filename = "./ms_exports/Figures/Fig4_manuscript.png",
         width = 7, height = 6,units = "in")

## Fig 5: swim performance by test --------------

# sample sizes on the plot
dd.label.BL<-data[!is.na(data$Test_performance2) & !is.na(data$swim_speed_MEAN_BL_s),] %>% 
  group_by(Test_performance2) %>% 
  summarize(n_stud = length(unique(Reference_number_1)), n = n())
dd.label.BL$label<-paste( dd.label.BL$n, " (", dd.label.BL$n_stud,")", sep="")

dd.label.cm<-data[!is.na(data$Test_performance2) & !is.na(data$SWIM_cms),] %>% 
  group_by(Test_performance2) %>% 
  summarize(n_stud = length(unique(Reference_number_1)), n = n())
dd.label.cm$label<-paste( dd.label.cm$n, " (", dd.label.cm$n_stud,")", sep="")



level_orderTest <- c("Ucrit" , "Umax",  "Swim", "Jump","Field", "Fishway")

test_condBL<-ggplot(data = data[!is.na(data$Test_performance2),],
                    aes(y=swim_speed_MEAN_BL_s, x=factor(Test_performance2, levels = level_orderTest), group = Test_performance2))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA, show.legend = FALSE) +
  geom_point(mapping = aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, color=Species_latin, x=factor(Test_performance2, levels = level_orderTest), group = interaction(Test_performance2, Species_latin)),
             position=position_dodge(width = 0.6), alpha=0.8, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_y_continuous(limits = c(-0.41, 15), breaks = c(0, 3, 6, 9, 12, 15) )+
  geom_text(mapping = aes( x = factor(Test_performance2, levels = level_orderTest),fill=NULL, y = -0.3, label = label), color = "black", size=3, data = dd.label.BL)
ggformat(test_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "", print = F, size_text = 13)
test_condBL <-test_condBL + theme( #legend.position = "none")
                                      legend.title = element_blank(),
                                      legend.position = "none",
                                      legend.text = element_text(face = "italic", size = 9))

test_condcm<-ggplot(data = data[!is.na(data$Test_performance2),],
                    aes(y=SWIM_cms, x=factor(Test_performance2, levels = level_orderTest), group = Test_performance2))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA, show.legend = FALSE) +
  geom_point(mapping = aes(y=SWIM_cms, fill=Species_latin, color=Species_latin, x=factor(Test_performance2, levels = level_orderTest), group = interaction(Test_performance2, Species_latin)),
             position=position_dodge(width = 0.6), alpha=0.8, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_y_continuous(limits = c(-50, 850) )+
  geom_text(mapping = aes( x = factor(Test_performance2, levels = level_orderTest),fill=NULL, y = -50, label = label), color = "black", size=3, data = dd.label.cm)
ggformat(test_condcm, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F, size_text = 13)
test_condcm <-test_condcm + theme( #legend.position = "none")
                                      legend.title = element_blank(),
                                      legend.position = c(0.18,0.72),
                                      legend.text = element_text(face = "italic", size = 9),
                                      legend.key.size = unit(0.4, "cm"))

cowplot::plot_grid( test_condcm, test_condBL,
                    nrow = 1, ncol=2,
                    labels = "AUTO",
                    label_x = c(0.17, 0.17),
                    label_y = c(0.9)) %>%
  ggsave(filename = "./ms_exports/Figures/Fig8_cms_SwimTest.png",
         width = 9.5, height = 5,units = "in")


## Supplemental Fig 4. [cm/s] Scaling of swimming --------
p1.cm<-ggplot(data=data.cm, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
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
  geom_hline(yintercept = 250, color = "grey", linetype = "dashed")+
  geom_point(size=2, alpha=0.7, pch=19)+
  ylim(0, 1000)+
  xlim(25, 100)
ggformat(p1.cm, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="")
p1.cm<-p1.cm+theme(legend.text = element_text(face = "italic"), 
                   legend.title = element_blank(), 
                   legend.position = "bottom")
# ggMarginal(p1.cm, groupColour = TRUE, groupFill = TRUE,  type = "histogram", alpha = 1,
#            yparams = list(),  xparams = list(binwidth = 3))


p1.cmSI<-ggplot(data=data.cm, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin,
                                  colour=Species_latin,
                                  label=Reference_number_1))+
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
  geom_point(size=2, alpha=0.7, pch=19)+
  facet_wrap(.~Species_latin, nrow = 3)+
  ylim(0, 1000)+
  xlim(25, 100)
ggformat(p1.cmSI, print=F, y_title = "Swim speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 11)
p1.cmSI<-p1.cmSI+theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")

# save
cowplot::plot_grid(p1.cm, p1.cmSI,
                  nrow = 2, ncol =1,
                  align = "vh",
                  labels = "AUTO",
                  label_x = c(0.18),
                  label_y = c(0.9),
                  rel_heights = c(1, 1)) %>%
ggsave(filename = "./ms_exports/Figures/FigS4_abs_size_speed.png",
       width = 5.5, height = 9,units = "in")



## Supplemental Fig 5. [BL/s] Scaling of swimming --------
p1.BL<-ggplot(data=data.BL, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin,
                                colour=Species_latin,
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
  geom_point(size=2, alpha=0.7, pch=19)+
  ylim(0, 15)+
  xlim(25, 100)
ggformat(p1.BL, print=FALSE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="")
p1.BL<-p1.BL+theme(legend.text = element_text(face = "italic"), 
                   legend.title = element_blank(), 
                   legend.position = "bottom")

p1.BLSI<-ggplot(data=data.BL, aes(y=swim_speed_MEAN_BL_s, x=LENGTH_cm, fill=Species_latin,
                                colour=Species_latin,
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
  geom_point(size=2, alpha=0.7, pch=19)+
  facet_wrap(.~Species_latin, nrow = 3)+
  ylim(0, 15)+
  xlim(25, 100)
ggformat(p1.BLSI, print=FALSE, y_title = "Swim speed (BL/s)", x_title = "Body length (cm)", title ="", size_text = 11)
p1.BLSI<-p1.BLSI+theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")

# save
cowplot::plot_grid(p1.BL, p1.BLSI,
                  nrow = 2, ncol =1,
                  align = "v",
                  labels = "AUTO",
                  label_x = c(0.18),
                  label_y = c(0.9),
                  rel_heights = c(1, 1)) %>%
ggsave(filename = "./ms_exports/Figures/FigS5_rel_size_speed.png",
       width = 5.5, height = 9,units = "in")

## Supplemental Fig 2. [cm/s] Temperature performance curves of swimming--------
p2.cm<-ggplot(data= data.cm, aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin,
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
  geom_hline(yintercept = 250, color = "grey", linetype = "dashed")+
  ylim(0, 850)
ggformat(p2.cm, print=FALSE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="")
p2.cm<-p2.cm+theme(legend.text = element_text(face = "italic"), 
                   legend.title = element_blank(), 
                   legend.position = "bottom")

p2.cmSI<-ggplot(data= data.cm, aes(y=SWIM_cms, x=Temp_test_mean,colour=Species_latin,label=Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=SWIM_cms,
                    ymax=SWIM_cms+SWIM_cms_SD), size =0.2)+
  facet_wrap(.~Species_latin, nrow = 3)+
  geom_point(size = 2)+
  geom_hline(yintercept = 250, linetype = "dashed", color ="grey")+
  ylim(0, 850)
ggformat(p2.cmSI, print=FALSE, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 11)
p2.cmSI<-p2.cmSI+theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")

cowplot::plot_grid(p2.cm, p2.cmSI,
                  nrow = 2, ncol =1,
                  align = "v",
                  labels = "AUTO",
                  label_x = c(0.18),
                  label_y = c(0.9),
                  rel_heights = c(1, 1)) %>%
ggsave(filename = "./ms_exports/Figures/FigS2_abs_temp_speed.png",
       width = 5.5, height = 9,units = "in")


## Supplemental Fig 3. [BL/s] Temperature performance curves of swimming--------
p2.BL<-ggplot(data=data.BL, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean, fill=Species_latin,
                                colour=Species_latin,
                                label=Reference_number_1))+
  geom_point(size= 3, alpha=0.7, pch=19)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  ylim(0, 15)
ggformat(p2.BL, print=F, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="")
p2.BL<-p2.BL+theme(legend.text = element_text(face = "italic"), 
                   legend.title = element_blank(), 
                   legend.position = "bottom")


p2.BLSI<-ggplot(data= data.BL, aes(y=swim_speed_MEAN_BL_s, x=Temp_test_mean,colour=Species_latin,
                                   label=Reference_number_1))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  geom_errorbar(aes(ymin=swim_speed_MEAN_BL_s-swim_error_BLs_SD,
                    ymax=swim_speed_MEAN_BL_s+swim_error_BLs_SD), size =0.2)+
  facet_wrap(.~Species_latin, nrow = 3)+
  geom_point(size = 2)+
  ylim(0, 15)
ggformat(p2.BLSI, print=FALSE, y_title = "Swim speed (BL/s)", x_title = "Temperature (ºC)", title ="", size_text = 11)
p2.BLSI<-p2.BLSI+theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")

cowplot::plot_grid(p2.BL, p2.BLSI,
                  nrow = 2, ncol =1,
                  align = "v",
                  labels = "AUTO",
                  label_x = c(0.18),
                  label_y = c(0.9),
                  rel_heights = c(1, 1)) %>%
ggsave(filename = "./ms_exports/Figures/FigS3_rel_temp_speed.png",
       width = 5.5, height = 9,units = "in")


## Supplemental Fig6 Time to fatigue -----------

ttf.p<-ggplot(data = data.ttf[data.ttf$Duration_swim<100,], aes(y=SWIM_cms, x=Duration_swim, size = LENGTH_cm, color = Species_latin, fill = Species_latin))+
  geom_point(pch=1, alpha=1)+
  # geom_point(data =  data.ttf[data.ttf$Duration_swim<100,], 
  #            mapping = aes(y=SWIM_cms, x=Duration_swim, color = Species_latin, size = LENGTH_cm),
  #            pch=1, alpha=1)+
  guides(size = F)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  labs(size="Size (FL, cm)", color="Species", fill="Species")
ggformat(ttf.p, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Swim duration (h)")
ttf.p <- ttf.p + theme(legend.text = element_text(face = "italic"), 
                   legend.title = element_blank(), 
                   legend.position = c(0.85, 0.75))

ttf.p.1H<-ggplot(data = data.ttf[data.ttf$Duration_swim<=1, ], aes(y=SWIM_cms, x=Duration_swim*60, color = Species_latin, fill = Species_latin))+
  # geom_point(pch=21, alpha=1,  show.legend = F)+
  geom_point(mapping = aes(y=SWIM_cms, x=Duration_swim*60, color = Species_latin, size = LENGTH_cm),
             pch=1, alpha=1)+
  guides(size = F, color = F, fill = F)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  labs(size="Size (FL, cm)", color="Species", fill="Species")
ggformat(ttf.p.1H, print=TRUE, y_title = "Swim speed (cm/s)", x_title = "Swim duration (min)")



cowplot::plot_grid(ttf.p, ttf.p.1H,
                    nrow = 2, ncol=1, 
                    labels = "AUTO",
                    label_x = c(0.9),
                    label_y = c(0.9, 0.9)) %>% 
ggsave(filename = "./ms_exports/Figures/FigMain_ttf.png",
         width = 6, height = 9,units = "in")

## Supplemental Fig 6: [cms] Sex -------------------
swim_sex.cm<-ggplot(data = dataLab,
                     aes(y=SWIM_cms, x=Species_latin, color = Species_latin,  shape = Sex_F_M, group = interaction(Sex_F_M, Species_latin)))+
  geom_boxplot(fill = "white", alpha=0.5, outlier.shape=NA) + # shape=Indiv_group)
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values=c(1, 19, 18))+
  geom_point(position = position_dodge(width = .75), size=2)+
  ylim(0,850)
ggformat(swim_sex.cm, title = "", y_title = "Swim speed (cm/s)", x_title = "Species", print = F)
swim_sex.cm <- swim_sex.cm + theme(legend.text = element_text(face = "italic"),
                                   legend.title = element_blank(),
                                   legend.position = "bottom",
                                   axis.text.x = element_blank())


swim_sex.cmF<-ggplot(data = dataF,
                     aes(y=SWIM_cms, x=Species_latin, color = Species_latin,  shape = Sex_F_M, group = interaction(Sex_F_M, Species_latin)))+
  geom_boxplot(fill = "white", alpha=0.5, outlier.shape=NA) + # shape=Indiv_group)
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_shape_manual(values=c(1, 19, 18))+
  geom_point(position = position_dodge(width = .75), size=2)+
  ylim(0,850)
ggformat(swim_sex.cmF, title = "", y_title = "Swim speed (cm/s)", x_title = "Species", print = F)
swim_sex.cmF <- swim_sex.cmF + theme(legend.text = element_text(face = "italic"),
                                   legend.title = element_blank(),
                                   legend.position = "none",
                                   axis.text.x = element_blank())

cowplot::plot_grid( swim_sex.cm, swim_sex.cmF,
                    nrow = 2, ncol=1, 
                    labels = c("A - lab", "B - field"),
                    label_x = c(0.1),
                    label_y = c(0.9, 0.9)) %>% 
  ggsave(filename = "./ms_exports/Figures/FigS6_abs_sex_species.png",
         width = 7, height = 10,units = "in")


## Supplemental Fig 8 - correlations w/ conditions -------------

cond1<-ggplot(data = data ,aes(y=SWIM_cms, x = Gonad_g,  color = Species_latin, shape = Sex_F_M))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1, 19, 18))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  ylim(0, 300)
ggformat(cond1, y_title  = "Swim speed (cm/s)", x_title = "Gonad mass (g)", size_text = 12)
cond1 <- cond1 + theme(legend.text = element_text(face = "italic"),
                                   legend.title = element_blank(),
                                   legend.position = "right")

cond2<-ggplot(data = data ,aes(y=SWIM_cms, x = GSI_MEAN,fill=Species_latin,  color = Species_latin, shape = Sex_F_M))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1, 19, 18))+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  ylim(0, 300)
ggformat(cond2, y_title  = "Swim speed (cm/s)", x_title = "GSI (mass gonads / body mass)", size_text = 12)
cond2 <- cond2 + theme(legend.position = "none")

cond3<-ggplot(data = data ,aes(y=SWIM_cms, x = Water_flows_cm_s,  color = Species_latin))+
  geom_point(pch=19, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                     labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                     values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))
ggformat(cond3, y_title  = "Swim speed (cm/s)", x_title = "Field water flows (cm/s)", size_text = 12)
cond3 <- cond3 + theme(legend.position = "none")


cowplot::plot_grid( cond3, cond2, cond1,
                    nrow = 1, ncol=3, 
                    labels = c("AUTO"),
                    label_x = c(0.2, 0.2, 0.2),
                    label_y = c(0.9, 0.9, 0.9),
                    rel_widths = c(0.85,0.85,1.2)) %>% 
  ggsave(filename = "./ms_exports/Figures/FigS8_abs_correlations.png",
         width = 13, height = 4,units = "in")


 ## Supplemental Fig 9 - fish conditions -------------

fish_condBL<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=swim_speed_MEAN_BL_s, x=factor(Fish_Conditions), group = Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA) +
  geom_point(mapping = aes(y=swim_speed_MEAN_BL_s, fill=Species_latin, color=Species_latin, x=factor(Fish_Conditions), group = Species_latin),
             position=position_dodge(width = 0.5), alpha=0.8, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                   labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                   values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))
ggformat(fish_condBL, title = "", y_title = "Swim speed (BL/s)", x_title = "", print = F)
fish_condBL<-fish_condBL + theme( #legend.position = "none")
                                  legend.title = element_blank(),
                                  legend.position = "top",
                                  legend.text = element_text(face = "italic", size = 14), 
                                  axis.text.x = element_text(size = 12, angle =45, vjust=0.5))

fish_condcm<-ggplot(data = data[!is.na(data$Fish_Conditions),], aes(y=SWIM_cms, x=factor(Fish_Conditions), group = Fish_Conditions))+
  geom_boxplot(fill = "white", alpha=1, outlier.shape=NA) +
  geom_point(mapping = aes(y=SWIM_cms, fill=Species_latin, color=Species_latin, x=factor(Fish_Conditions), group = Species_latin),
             position=position_dodge(width = 0.7), alpha=0.8, pch=21, size=2)+
  scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                   labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                   values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
  scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                    labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"),
                    values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))
ggformat(fish_condcm, title = "", y_title = "Swim speed (cm/s)", x_title = "", print = F)
fish_condcm<-fish_condcm + theme( legend.title = element_blank(),
                                  legend.position = "none",
                                  legend.text = element_text(face = "italic", size = 12),
                                  axis.text.x = element_text(size = 12, angle = 45, vjust=0.5))


cowplot::plot_grid( fish_condcm,fish_condBL,
                   nrow = 2, ncol=1, 
                   labels = "AUTO",
                   label_x = c(0.1),
                   label_y = c(0.90, 0.8),
                   rel_heights = c(0.8,1)) %>% 
  ggsave(filename = "./ms_exports/Figures/FigS9_cms_Fish_Cond.png",
         width = 8, height = 10,units = "in")









