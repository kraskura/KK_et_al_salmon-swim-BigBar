# 4.4. function to plot data ----
species.temp.plots<-function(model, dd, species, temp.sum, dataset){
  
    data.pred.CI<-predict(model, interval = "confidence") 
    dd$pred.mod<-data.pred.CI[,1]
    dd$pred.modCI.h<-data.pred.CI[,2]
    dd$pred.modCI.l<-data.pred.CI[,3]
    n.stud<-temp.sum[temp.sum$Species_latin == species,"n_studies"]
    n<-temp.sum[temp.sum$Species_latin == species,"n"]
    label.plot<-paste("n = ", n, " (", n.stud,")", sep="")
    
    if(dataset == "normalized"){
      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms.prop0, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source, group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point(size = 2, alpha=1)+
        # facet_wrap(.~Species_latin)+
        geom_text(mapping = aes( x = 12, y = 5), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(aes(ymin = pred.modCI.l,
                        ymax = pred.modCI.h), alpha = 0.3)+
        geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
        ylim(0, 120)+
        scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
      ggformat(plot.sp.t, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
      plot.sp.t<-plot.sp.t+theme(legend.position = "none")
      # p2.cmNorm0
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""),dd, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""))
      
      ggsave(filename = paste("../../ms_exports/Figures/Temp_normalized/Sp_", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
      
    }
   
    if(dataset == "raw"){
      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point(size = 2, alpha=1)+
        # facet_wrap(.~Species_latin)+
        geom_text(mapping = aes( x = 12, y = 15), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(aes(ymin = pred.modCI.l,
                        ymax = pred.modCI.h), alpha = 0.3)+
        geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
        ylim(0, 250)+
        scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
      ggformat(plot.sp.t, print=F, y_title = "Swim speed (cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 10)
      plot.sp.t<-plot.sp.t + theme(legend.position = "none", 
                                   plot.margin = margin(t = -0.5, unit="cm", r = 0, l = 0.1))
      
      if(!c(grepl("salar", species, fixed = F) | grepl("tshawytscha", species, fixed = F))){
        plot.sp.t <- plot.sp.t + theme(axis.title.x = element_blank())
      }
      if(c(grepl("kisutch", species, fixed = F) | grepl("mykiss", species, fixed = F) | grepl("tshawytscha", species, fixed = F))){
        plot.sp.t <- plot.sp.t + theme(axis.title.y = element_blank())
      }
      # p2.cmNorm0
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""),dd, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""))
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_plot",sep=""),plot.sp.t, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_plot",sep=""))

      ggsave(filename = paste("../../ms_exports/Figures/Fig6_rawdata_tpcs", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
    }
}

species.size.plots<-function(model, dd, species, sum.file, test){
  
  data.pred.CI<-predict(model, interval = "confidence") 
  dd$pred.mod<-data.pred.CI[,1]
  dd$pred.modCI.h<-data.pred.CI[,2]
  dd$pred.modCI.l<-data.pred.CI[,3]
  n.stud<-sum.file[sum.file$Species_latin == species,"n_studies"]
  n<-sum.file[sum.file$Species_latin == species,"n"]
  label.plot<-paste("n = ", n, " (", n.stud,")", sep="")

  
  plot.sp.s<-ggplot(data=dd, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
    scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                       labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                       values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
    scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                      labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                      values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
    geom_point(size = 2, alpha=1)+
    # facet_wrap(.~Species_latin)+
    scale_shape_manual(values = c(21,23))+
    geom_ribbon(aes(ymin = pred.modCI.l,
                    ymax = pred.modCI.h), alpha = 0.3)+
    geom_line(aes(y=pred.mod, x=LENGTH_cm), color = "black")+
    scale_x_continuous(limits = c(30, 100), breaks = c(30, 40, 50, 60, 70, 80, 90))+
    geom_text(mapping = aes( x = 45, y = 865), label = label.plot, color = "black", size=4.5)
    if(test=="field"){
      plot.sp.s<- plot.sp.s+ 
        geom_text(mapping = aes( x = 45, y = 965), label = "Field", color = "black", size=4.5)
    }
    if(test=="lab"){
      plot.sp.s<- plot.sp.s+
        geom_text(mapping = aes( x = 45, y = 965), label = "Lab", color = "black", size=4.5)
    }
  ggformat(plot.sp.s, print=F, y_title = "Swim Speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 12)
  plot.sp.s<-plot.sp.s+theme(legend.position = "none")
  plot.sp.s
  
  if(test=="field"){
    assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Fdd",sep=""),dd, envir = .GlobalEnv)
    message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Fdd",sep=""))
  } 
  if(test=="lab"){
    assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Ldd",sep=""),dd, envir = .GlobalEnv)
    message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Ldd",sep=""))
  }
  
  ggsave(filename = paste("../../ms_exports/Figures/Size_byTest/Sp_", species,"-", test,".png", sep=""),
         plot = plot.sp.s, width = 3, height = 3)
}

