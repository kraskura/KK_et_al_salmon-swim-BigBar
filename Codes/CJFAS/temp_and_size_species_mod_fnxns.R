# function to plot data ----
# 
# 
# lab data, sufficiently robust with temp and size -----
species.temp.size.fits<-function(species, temp.sum, dataform, scale.fish.size = NULL){
  
    if(dataform == "normalized"){
      
      message("Size not included in the linear model")
      
      dd<-data.cmTNorm.use[(data.cmTNorm.use$Species_latin == species),]
      model<-lm(SWIM_cms.prop0 ~ poly(Temp_test_mean,2, raw = T), na.action=na.exclude, data = dd)
      
      data.pred.CI<-predict(model, interval = "confidence") 
      dd$pred.mod<-data.pred.CI[,1]
      dd$pred.modCI.h<-data.pred.CI[,2]
      dd$pred.modCI.l<-data.pred.CI[,3]
      n.stud<-temp.sum[temp.sum$Species_latin == species,"n_studies"]
      n<-temp.sum[temp.sum$Species_latin == species,"n"]
      label.plot<-paste("n = ", n, " (", n.stud,")", sep="")
    
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
                        ymax = pred.modCI.h, size = NULL))+
        geom_line(aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
        ylim(0, 120)+
        scale_x_continuous(limits = c(3, 27), breaks = c(5, 10, 15, 20, 25))
      ggformat(plot.sp.t, print=F, y_title = "Normalized Swim speed (%, cm/s)", x_title = "Temperature (ºC)", title ="", size_text = 12)
      plot.sp.t<-plot.sp.t+theme(legend.position = "none")
      # p2.cmNorm0
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""),dd, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd",sep=""))
      
      ggsave(filename = paste("../../ms_exports/Figures/Species_temp_plots/normalized/Sp_", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
      
    }
   
    if(dataform == "raw" & is.null(scale.fish.size)){
      
      dd<-dataLab.use[(dataLab.use$Species_latin == species),]
      model<-lm(SWIM_cms ~ poly(Temp_test_mean,2, raw = T) + LENGTH_cm, na.action=na.exclude, data = dd)
      
      dd_pred<-as.data.frame(expand.grid(Temp_test_mean = seq(min(dd$Temp_test_mean), max(dd$Temp_test_mean), 0.5), 
                                         LENGTH_cm = mean(dd$LENGTH_cm, na.rm =T), Species_latin = species))
      
      dd_pred.size<-as.data.frame(expand.grid(Temp_test_mean = mean(dd$Temp_test_mean, na.rm =T), 
                                         LENGTH_cm = seq(min(dd$LENGTH_cm, na.rm =T), max(dd$LENGTH_cm), 1), Species_latin = species))
      
      data.pred.CI<-predict(model, newdata = dd_pred, interval = "confidence") 
      dd_pred$pred.mod<-data.pred.CI[,1]
      dd_pred$pred.modCI.h<-data.pred.CI[,2]
      dd_pred$pred.modCI.l<-data.pred.CI[,3]
      
      data.pred.CI.size<-predict(model, newdata = dd_pred.size, interval = "confidence") 
      dd_pred.size$pred.mod<-data.pred.CI.size[,1]
      dd_pred.size$pred.modCI.h<-data.pred.CI.size[,2]
      dd_pred.size$pred.modCI.l<-data.pred.CI.size[,3]
      
      n.stud<-temp.sum[temp.sum$Species_latin == species,"n_studies"]
      n<-temp.sum[temp.sum$Species_latin == species,"n"]
      label.plot<-paste("n = ", n, " (", n.stud,")", sep="")
      
      # optimal temps 
      P.species<-function(x){coef(model)[1] + x^1 * coef(model)[2] + x^2*coef(model)[3]}
      o.species<- optimize(f = P.species,
                           interval = c(min(dd$Temp_test_mean), max(dd$Temp_test_mean)),
                           maximum = TRUE)
      # 
      # # print(outer(x = c(min(dd$Temp_test_mean), max(dd$Temp_test_mean)),
      # #             y = mean(dd$LENGTH_cm), FUN=P.species))
      
      dd_pred$Topt<-unlist(o.species)[1]
      dd_pred$Topt_SWIM_cms<-as.numeric(unlist(o.species)[2]) + (as.numeric((mean(dd$LENGTH_cm, na.rm =T)) * as.numeric(coef(model)[4])))
      
      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point(size = 2, alpha=1)+
        # facet_wrap(.~Species_latin)+
        geom_text(inherit.aes = FALSE, mapping = aes( x = 12, y = 15), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(data=dd_pred, aes(y = NULL, ymin = pred.modCI.l,
                        ymax = pred.modCI.h,  fill=Species_latin, colour=Species_latin, group = Species_latin ), alpha = 0.3)+
        geom_line(data=dd_pred, aes(y=pred.mod, x=Temp_test_mean), color = "black")+  
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
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""),dd_pred, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_full",sep=""))
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_plot",sep=""),plot.sp.t, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Tdd_plot",sep=""))

      ggsave(filename = paste("../../ms_exports/Figures/Species_temp_plots/rawdata/Fig6_rawdata_tpcs", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
      
      # size plots 
      plot.sp.s<-ggplot(data=dd, aes(y=SWIM_cms, x=LENGTH_cm, fill=Species_latin, colour=Species_latin, group = Species_latin))+
      scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                         labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                         values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
      scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                        labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                        values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
      geom_point(size = 2, alpha=1)+
      scale_shape_manual(values = c(21,23))+
      geom_ribbon(data = dd_pred.size, aes( y = NULL, ymin = pred.modCI.l, ymax = pred.modCI.h), alpha = 0.3)+
      geom_line(data = dd_pred.size, aes(y=pred.mod, x=LENGTH_cm), color = "black")+
      scale_x_continuous(limits = c(30, 100), breaks = c(30, 40, 50, 60, 70, 80, 90))+
      ylim(0, 250)+
      geom_text(mapping = aes( x = 45, y = 240), label = "Lab", color = "black", size=4.5)
      ggformat(plot.sp.s, print=F, y_title = "Swim Speed (cm/s)", x_title = "Body length (cm)", title ="", size_text = 12)
      plot.sp.s<-plot.sp.s+theme(legend.position = "none")
      
      assign(paste(gsub(x=species,pattern = " ",replacement = ""),"_Ldd",sep=""),dd_pred.size, envir = .GlobalEnv)
      message(paste(gsub(x=species,pattern = " ",replacement = ""),"_Ldd",sep=""))
      ggsave(filename = paste("../../ms_exports/Figures/Species_size_plots/Sp_", species,"-lab",".png", sep=""), plot = plot.sp.s, width = 3, height = 3)
    
    }
    
    if(dataform == "raw" & !is.null(scale.fish.size)){
      
      message("Size not included in the linear model")
      dd<-dataLab.use[(dataLab.use$Species_latin == species),]
      model<-lm(SWIM_cms_scaled ~ poly(Temp_test_mean,2, raw = T), na.action=na.exclude, data = dd)
      
      data.pred.CI<-predict(model, interval = "confidence") 
      dd$pred.mod<-data.pred.CI[,1]
      dd$pred.modCI.h<-data.pred.CI[,2]
      dd$pred.modCI.l<-data.pred.CI[,3]
      n.stud<-temp.sum[temp.sum$Species_latin == species,"n_studies"]
      n<-temp.sum[temp.sum$Species_latin == species,"n"]
      label.plot<-paste("n = ", n, " (", n.stud,")", sep="")

      plot.sp.t<-ggplot(data=dd, aes(y=SWIM_cms_scaled, x=Temp_test_mean, fill=Species_latin, colour=Species_latin, shape=SWIM_cms_source,group = Species_latin))+
        scale_color_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                           labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                           values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        scale_fill_manual(breaks = c("Oncorhynchus spp.","Oncorhynchus gorbuscha","Oncorhynchus keta","Oncorhynchus kisutch","Oncorhynchus nerka","Oncorhynchus tshawytscha","Salmo salar", "Oncorhynchus masou", "Oncorhynchus mykiss"),
                          labels = c("Oncorhynchus spp.", "O. gorbuscha", "O. keta", "O. kisutch", "O. nerka", "O. tshawytscha", "S. salar", "O. masou", "O. mykiss"), 
                          values = c("grey", "#D292CD", "#FB9A62", "#FBC063", "#EA573D", "#70Af81", "#64B0BC","#446699", "#615B70"))+
        geom_point()+
        # facet_wrap(.~Species_latin)+
        geom_text(mapping = aes( x = 12, y = 15), label = label.plot, color = "black", size=4.5)+
        scale_shape_manual(values = c(21,23))+
        geom_ribbon(aes(ymin = pred.modCI.l,
                        ymax = pred.modCI.h, size = NULL), alpha = 0.3)+
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

      ggsave(filename = paste("../../ms_exports/Figures/Species_temp_plots/size_scaled/Fig6_rawdata_size_scaled_tpcs", species,".png", sep=""),
             plot = plot.sp.t, width = 3, height = 3)
    }
    
}

# only for field swim -----
species.size.plots<-function(model, dd, species, sum.file, test = "field"){
  
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
  
  ggsave(filename = paste("../../ms_exports/Figures/Species_size_plots/Sp_", species,"-", test,".png", sep=""),
         plot = plot.sp.s, width = 3, height = 3)
}

