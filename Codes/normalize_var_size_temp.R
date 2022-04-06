
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


# EJE: what happens when you only present data from fish tested in their optimal temperature range? -------------------




