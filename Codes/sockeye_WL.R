
source("/Users/kristakraskura/Github_repositories/Salmon-swim-BigBar/Codes/get_dataset.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")

data.all<-get.adult.salmonid.swim.data(
  data.file = "/Users/kristakraskura/Desktop/BOX/UCSB/Research/Pacific Salmon/Manuscr Swimming Lit Rev /Data files/2022_final_work/Kraskura_salmonSwim_analysis_jan2022.csv")

# view(data)
# all but time to fatigue tests for general analysis:
data<-as.data.frame(data.all[1])

soc<-data[data$Species_latin == "Oncorhynchus nerka", c("Size_MEAN_kg", "Length_MEAN_cm")]

lm(log10(Size_MEAN_kg) ~  log10(Length_MEAN_cm*1000), data = soc)
plot(log10(Length_MEAN_cm) ~ log10(Size_MEAN_kg*1000), data=soc)
plot(Length_MEAN_cm ~ c(Size_MEAN_kg*1000), data=soc)

nls(Length_MEAN_cm ~ a*Size_MEAN_kg^b, start = list(a = -10, b = 3) , data=soc)
