
library(tidyverse)

  
get.adult.salmonid.swim.data <- function(data.file){  
  # Import data: --------
  data<-read.csv(data.file)
  # rename column names: 
  names(data) <- c("Reference_number_1", "Species_latin", "Key_origin", "Year_published",
                   "N_morphometrics", "Size_MIN_kg", "Size_MAX_kg", "Size_MEAN_kg",
                   "Size_error_kg", "UNIT_Size_error_kg", "Length_MIN_cm", "Length_MAX_cm",
                   "Length_MEAN_cm", "Length_error", "UNIT_length_error", "Length_UNIT",
                   "Sex_F_M", "Temp_test_mean", "SW_FW", "Test_performance",
                   "Indiv_group","FishID", "swim_speed_MIN_BL_s", "swim_speed_MAX_BL_s", "swim_speed_MEAN_BL_s", 
                   "swim_speed_error_BL_s", "UNIT_swim_error_BLs","Duration_swim",
                   "swim_speed_MIN_cm_s", "swim_speed_MAX_cm_s", "swim_speed_MEAN_cm_s", "swim_speed_error_cm_s", "UNIT_swim_error_cm_s",
                   "Water_flows_cm_s", "Water_flows_m_3_s", "Swim_Conditions", "Fish_Conditions",
                   "GSI_MEAN", "Gonad_g", "Blood", "Mortality",
                   "Surgery", "Recovery", "Tracking", "Data_source", "Reference_2", "N_swim_speed")
  
  # standardize all na's, empty cells, 
  data <- data %>% 
    mutate_all(na_if,"") %>% 
    mutate_all(na_if,"na") 
  
  data$Blood[data$Blood=="n"]<-0
  data$Blood[data$Blood=="y"]<-1
  data$Tracking[data$Tracking=="n"]<-0
  data$Tracking[data$Tracking=="y"]<-1
  data$Mortality[data$Mortality=="n"]<-0
  data$Mortality[data$Mortality=="y"]<-1
  data$Recovery[data$Recovery=="n"]<-0
  data$Surgery[data$Surgery=="n"]<-0

    # combine mean and max speeds
  for (i in 1:nrow(data)){
    if(!is.na(data$swim_speed_MAX_BL_s[i])){
      message(paste("Swim speed changed from: MEAN - ", data$swim_speed_MEAN_BL_s[i], " to MAX - ", data$swim_speed_MAX_BL_s[i], sep = "" ))
      data$swim_speed_MEAN_BL_s[i]<-data$swim_speed_MAX_BL_s[i]
    }
  }
  
  for (i in 1:nrow(data)){
    if(c(!is.na(data$swim_speed_MAX_cm_s[i]))){ 
      message(paste("Swim speed: MEAN - ", data$swim_speed_MEAN_cm_s[i], " to MAX - ", data$swim_speed_MAX_cm_s[i]))
      data$swim_speed_MEAN_cm_s[i]<-data$swim_speed_MAX_cm_s[i]
    }
  }
  
  # standardize all error (se, SE, sd, SD) formatting
  data$UNIT_Size_error_kg<-plyr::revalue(data$UNIT_Size_error_kg, c("se"="SE", "sd"="SD"))
  data$UNIT_length_error<-plyr::revalue(data$UNIT_length_error, c("se"="SE", "sd"="SD"))
  data$UNIT_swim_error_BLs<-plyr::revalue(data$UNIT_swim_error_BLs, c("se"="SE", "sd"="SD"))
  data$UNIT_swim_error_cm_s<-plyr::revalue(data$UNIT_swim_error_cm_s, c("se"="SE", "sd"="SD"))
  
  # print(data$Species_latin[which(nchar(data$Species_latin) == max(nchar(data$Species_latin)))])
  # data[c(887,888),]  # these are two combined Salmonid species O nerka and O kisutch
  data$Species_latin<-as.character(data$Species_latin)
  data[which(data$Species_latin=="Oncorhynchus gorbuscha "),"Species_latin"]<-"Oncorhynchus gorbuscha"
  data[which(data$Species_latin=="Onchorynchus tshawytscha"),"Species_latin"]<-"Oncorhynchus tshawytscha"
  data[which(data$Species_latin=="Oncorhynchus gardneri"),"Species_latin"]<-"Oncorhynchus mykiss"
  data[c(which(nchar(data$Species_latin) == max(nchar(data$Species_latin)))),"Species_latin"]<-"Oncorhynchus spp."
  data$Species_latin<-as.factor(as.character(data$Species_latin))
  
  # group two density measures in just density
  data$Fish_Conditions<-as.character(data$Fish_Conditions)
  data[which(data$Fish_Conditions=="high density, exercise trained"),"Fish_Conditions"]<-"density exercise trained"
  data[which(data$Fish_Conditions=="low density, exercise trained"),"Fish_Conditions"]<-"density exercise trained"
  data[which(data$Fish_Conditions=="high density"),"Fish_Conditions"]<-"density"
  data[which(data$Fish_Conditions=="low density"),"Fish_Conditions"]<-"density"
  
  data$Fish_Conditions<-as.factor(as.character(data$Fish_Conditions))
  
  # make all differnet jumps together in one column
  # make all ucrits (repeat test as well as EMG Ucrits under one - Ucrit)

  # grouping Test performance in broader categories
  data$Test_performance2<-NA
  
  for (i in 1:nrow(data)){
    # if (grepl("Jump", as.character(data$Test_performance[i]), perl = TRUE) & data$Reference_number_1 == "58"){
    #   data$Test_performance2[i]<-"Field"
    # }
    # if (grepl("Jump", as.character(data$Test_performance[i]), perl = TRUE) & data$Reference_number_1 == "40"){
    #   data$Test_performance2[i]<-"Jump"
    # }
    if (grepl("Jump", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Jump"
    }
    if (grepl("Ucrit", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Ucrit"
    }
    if (grepl("Swim", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Swim"
    }
    if (grepl("Umax", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Umax"
    }
    if (grepl("Field", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Field"
    }
    if (grepl("Acceleration", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"Acceleration"
      message("acceleration data")
    }
    if (grepl("TTF", as.character(data$Test_performance[i]), perl = TRUE)){
      data$Test_performance2[i]<-"TTF"
    }
    if (is.na(data$Test_performance2[i])){
      data$Test_performance2[i]<-"other"
      message("Category \"other\" performance noted")
    }
  }
  
  # grouping swim performance in broader categories
  data$Swim_Conditions2<-NA
  
  for (i in 1:nrow(data)){
    if (grepl("Fall", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Fall"
    }
    if (grepl("Fraser", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Fraser"
    }
    if (grepl("Dam", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Dam"
    }
    
    if (grepl("Toyohira", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Toyohira"
    }
    if (grepl("Shibetsu", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Shibetsu"
    }
    # if (grepl("Field", as.character(data$Swim_Conditions[i]), perl = TRUE)){
    #   data$Swim_Conditions2[i]<-"Field"
    # }
    if (grepl("Klickitat", as.character(data$Swim_Conditions[i]), perl = TRUE)){
      data$Swim_Conditions2[i]<-"Klickitat"
    }
    if (is.na(data$Swim_Conditions2[i]) & !is.na(data$Swim_Conditions[i])){
      data$Swim_Conditions2[i]<-"other"
      message(paste ("Change: ", data$Swim_Conditions[i], " --- Category: other"))
    }
  }
  
  # estimate either missing bw or TL based - values from Fishbase.org 
  # units must be in cm and g
  # Fishbase table in: W = a × L^b
  # the intercept a conversion:  conversion from (cm, kg) to (cm, g) as follows: 
  # a'(cm, g) = a (cm, kg)*1000
  
  # set b=3 and calculate the missing pieces
  
  # log10(weight) = log10(a) + b*log10(length)
  
  # pink salmon: 
  pink_a = 0.01175 #(0.00648 - 0.02132)
  pink_b = 3.08 #(2.93 - 3.23)
  coho_a = 0.01120
  coho_b = 3.000
  # sockeye_a1 = 0.01555 
  # sockeye_a2 = 0.01922
  sockeye_a = mean(0.01555)  # estimated from linear relationship locally 
  sockeye_b = 2.94
  chum_a = 0.01413 #(0.00734 - 0.02718)
  chum_b = 3.09 #(2.92 - 3.26)
  atlantic_a = 0.01047 #(0.00847 - 0.01294)
  atlantic_b = 3.02 #(2.97 - 3.07)
  chinook_a = 0.01333
  chinook_b = 3.0
  masu_a = 0.00933 # (0.00563 - 0.01547), 
  masu_b = 3.02 # (2.88 - 3.16)
  trout_a = 0.00955 #(0.00738 - 0.01236)
  trout_b = 3.03 # (2.98 - 3.08)
  
  # data[1,c("Species_latin_name", "Size_MEAN_kg", "Length_MEAN_cm")]
  data$Length_cm_estimated<-NA
  data$Size_kg_estimated<-NA
  data$species_b<-NA
  data$species_a<-NA
  
  # formula to estimate mass
  # log10(weight) = log10(a) + b*log10(length)
  
  # formula to estimate lenght
  # log10(length) = (log10(weight) - log10(a)) / b
  data$Size_kg_value_source <- NA
  data$Length_cm_value_source <- NA
  
  data$Length_cm_estimated<-NA
  data$Size_kg_estimated<-NA
  
  # some have min and max lengths = take the mean, no SD
  data$Length_MEAN_cm[is.na(data$Length_MEAN_cm) & c(!is.na(data$Length_MIN_cm) | !is.na(data$Length_MAX_cm))] <-
    (as.numeric(data$Length_MIN[is.na(data$Length_MEAN_cm) & c(!is.na(data$Length_MIN_cm) | !is.na(data$Length_MAX_cm))]) +
    as.numeric(data$Length_MAX[is.na(data$Length_MEAN_cm) & c(!is.na(data$Length_MIN_cm) | !is.na(data$Length_MAX_cm))])) /2
  
  for (i in 1:nrow(data)){
    if (is.na(data[i,c("Species_latin")])){next}
    
    if (data[i,c("Species_latin")]=="Oncorhynchus mykiss"){ # trout
      data$species_a[i] <- trout_a
      data$species_b[i] <- trout_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(trout_a) + (trout_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(trout_a)) / trout_b)
        data$Length_cm_value_source[i] <- "estimated"
      }
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus masou"){ # masu
      data$species_a[i] <- masu_a
      data$species_b[i] <- masu_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(masu_a) + (masu_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(masu_a)) / masu_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus kisutch"){ # coho
      data$species_a[i] <- coho_a
      data$species_b[i] <- coho_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(coho_a) + (coho_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(coho_a)) / coho_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus keta"){ # chum
      data$species_a[i] <- chum_a
      data$species_b[i] <- chum_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(chum_a) + (chum_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(chum_a)) / chum_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus gorbuscha"){ # pink
      data$species_a[i] <- pink_a
      data$species_b[i] <- pink_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(pink_a) + (pink_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(pink_a)) / pink_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus nerka"){ # sockeye
      data$species_a[i] <- sockeye_a
      data$species_b[i] <- sockeye_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(sockeye_a) + (sockeye_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(sockeye_a)) / sockeye_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
      
    }
    
    if (data[i,c("Species_latin")]=="Salmo salar"){ # atlantic
      data$species_a[i] <- atlantic_a
      data$species_b[i] <- atlantic_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(atlantic_a) + (atlantic_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(atlantic_a)) / atlantic_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
      
    }
    
    if (data[i,c("Species_latin")]=="Oncorhynchus tshawytscha"){ # chinook
      data$species_a[i] <- chinook_a
      data$species_b[i] <- chinook_b
      
      if(is.na(data$Size_MEAN_kg[i]) & !is.na(data$Length_MEAN_cm[i])){
        data$Size_kg_estimated[i] <- 10^(log10(chinook_a) + (chinook_b * log10(data$Length_MEAN_cm[i])))/1000
        data$Size_kg_value_source[i] <- "estimated"
        
      }
      if(is.na(data$Length_MEAN_cm[i]) & !is.na(data$Size_MEAN_kg[i])){
        data$Length_cm_estimated[i] <- 10^((log10(data$Size_MEAN_kg[i] * 1000) - log10(chinook_a)) / chinook_b)
        data$Length_cm_value_source[i] <- "estimated"
        
      }
    }

    # # all fish
    # if (is.na(data$Length_cm_estimated[i])){
    #   data$Length_cm_estimated[i] <- data$Length_MEAN_cm [i]
    #   if (is.na(data$Length_cm_estimated[i])){
    #     data$Length_cm_value_source <- "reported"
    #   }
    # }
    # if (is.na(data$Size_kg_estimated[i])){
    #   data$Size_kg_estimated[i] <- data$Size_MEAN_kg [i]
    #   if (is.na(data$Size_kg_estimated[i])){
    #     data$Size_kg_value_source[i] <- "reported"
    #   }
    # }
    
    
  }
  
  # data$Size_kg_value_source[which(!is.na(data$Size_kg_estimated) & is.na(data$Size_MEAN_kg))]<- "estimated"
  # data$Length_cm_value_source[which(!is.na(data$Length_cm_estimated) & is.na(data$Length_MEAN_cm))]<- "estimated"
  data$Size_kg_value_source[which(!is.na(data$Size_MEAN_kg))]<- "reported"
  data$Length_cm_value_source[which(!is.na(data$Length_MEAN_cm))]<- "reported"
  # view(data[, c("Length_MEAN_cm","Length_cm_estimated", "Length_cm_value_source")])
  
  # data[((!is.na(data$Length_MEAN_cm) | !is.na(data$Length_cm_estimated)) & is.na(data$Length_cm_value_source)),] # confirmed
  
  # Size_kg_value Length_cm_value
  data$Size_kg_value_source<-factor(data$Size_kg_value_source)
  data$Length_cm_value_source<-factor(data$Length_cm_value_source)
  
  data$LENGTH_cm<-NA
  for (i in 1:nrow(data)){
    if(!is.na(data$Length_cm_value_source[i])){
      if(data$Length_cm_value_source[i]=="reported"){
        data$LENGTH_cm[i]<-data$Length_MEAN_cm[i]
      }
      if(data$Length_cm_value_source[i]=="estimated"){
        data$LENGTH_cm[i]<-data$Length_cm_estimated[i]
      }
    }else{
      # print(c(data$Length_MEAN_cm[i], data$Length_cm_estimated[i])) # confirmed, no missing size measurements
    }
  }
  
  # nrow(data[data$Size_kg_value_source=="reported", ])
  # nrow(data[data$Size_kg_value_source=="estimated", ])
  # nrow(data[data$Length_cm_value_source=="reported", ])
  # nrow(data[data$Length_cm_value_source=="estimated", ])
  # get SD from SE 
  # se = sd/(sqrt(n)); sd = se * (sqrt(n))
  
  data$swim_speed_MEAN_BL_s<-as.numeric(data$swim_speed_MEAN_BL_s)
  data$N_morphometrics<-as.numeric(data$N_morphometrics)
  data$N_swim_speed<-as.numeric(data$N_swim_speed)
  # str(data)
  
  data$Size_error_kg_SD<-NA
  data$Length_error_SD<-NA
  data$swim_error_BLs_SD<-NA
  data$swim_error_cm_s_SD<-NA
  
  # conversion between SD and SE
  for (i in 1:nrow(data)){
    # print(i)
    
    if(!is.na(data$UNIT_Size_error_kg[i]) & data$UNIT_Size_error_kg[i] == "SE" & !is.na(data$N_morphometrics[i])){
      if(data$N_morphometrics[i]>0){
        data$Size_error_kg_SD[i]<-data$Size_error_kg[i] * (sqrt(data$N_morphometrics[i]))
      }else{
        message("BODY MASS: No sample size for the SE, cannot calculate SD")
      }
    }
    if(!is.na(data$UNIT_Size_error_kg[i]) & data$UNIT_Size_error_kg[i] == "SD" & !is.na(data$N_morphometrics[i])){
      data$Size_error_kg_SD[i]<-data$Size_error_kg[i]
    }
    
  
    
    if(!is.na(data$UNIT_length_error[i]) &data$UNIT_length_error[i] == "SE" & !is.na(data$N_morphometrics[i]) ){
      if(data$N_morphometrics[i]>0){
        data$Length_error_SD[i]<-data$Length_error[i] * (sqrt(data$N_morphometrics[i]))
      }else{
        message("LENGTH: No sample size for the SE, cannot calculate SD")
      }
    }
    if(!is.na(data$UNIT_length_error[i]) & data$UNIT_length_error[i] == "SD"){
      data$Length_error_SD[i]<-data$Length_error[i]
    }
    
    
    
    if(!is.na(data$UNIT_swim_error_BLs[i]) & data$UNIT_swim_error_BLs[i] == "SE" & !is.na(data$N_swim_speed[i])){
      if(data$N_swim_speed[i]>0){
        data$swim_error_BLs_SD[i]<-data$swim_speed_error_BL_s[i] * (sqrt(data$N_swim_speed[i]))
      }else{
        message("SWIM BL/s: No sample size for the SE, cannot calculate SD")
      }
    }
    if(!is.na(data$UNIT_swim_error_BLs[i]) & data$UNIT_swim_error_BLs[i] == "SD"){
      data$swim_error_BLs_SD[i]<-data$swim_speed_error_BL_s[i]
    }
  
    
    if(!is.na(data$UNIT_swim_error_cm_s[i]) & (data$UNIT_swim_error_cm_s[i]) == "SE" & !is.na(data$N_swim_speed[i])){
      if(data$N_swim_speed[i]>0){
        data$swim_error_cm_s_SD[i]<-data$swim_speed_error_cm_s[i] * (sqrt(data$N_swim_speed[i]))
      }else{
        message("SWIM cm/s: No sample size for the SE, cannot calculate SD")
      }
    }
    if(!is.na(data$UNIT_swim_error_cm_s[i]) & (data$UNIT_swim_error_cm_s[i]) == "SD"){
      data$swim_error_cm_s_SD[i]<-data$swim_speed_error_cm_s[i]
      
    }
    
  }
  
  # Estimate swim speeds -- >> conversion from  BL/s to cm/s when reported is NOT available
  data$SWIM_cms<-NA
  data$SWIM_cms_SD<-NA
  for (i in 1:nrow(data)){
    # print(i)
    if(!is.na(data$swim_speed_MEAN_cm_s[i])){
      data$SWIM_cms[i]<-data$swim_speed_MEAN_cm_s[i]
      data$SWIM_cms_SD[i]<-data$swim_error_cm_s_SD[i]
      # message(paste(i, " = use mean:", round(data$swim_speed_MEAN_cm_s[i],2), "    in SWIM_cms = ", round(data$SWIM_cms[i],2), sep =""))
    }else{
      data$SWIM_cms[i]<-data$LENGTH_cm[i]*data$swim_speed_MEAN_BL_s[i]
      data$SWIM_cms_SD[i]<-data$LENGTH_cm[i]*data$swim_error_BLs_SD[i]
      
      message(paste(i, "= use ESTIMATE: ", round(data$swim_speed_MEAN_BL_s[i],2), " X ", round(data$LENGTH_cm[i],2), "   in SWIM_cms = ",round(data$SWIM_cms[i],2), sep =""))
    }
  }
  
  # indicate source of the swim sped:
  data$SWIM_cms_source<-NA

  # length estimated, swim speed estimated
  data$SWIM_cms_source[which(data$Length_cm_value_source == "estimated" &
                    is.na(data$Length_MEAN_cm) & 
                    is.na(data$swim_speed_MEAN_cm_s) &  # <<<<<< 
                    !is.na(data$SWIM_cms))] <- "estimated"
  
  # length reported, swim speed estimated
  data$SWIM_cms_source[which(data$Length_cm_value_source == "reported" &
                     !is.na(data$Length_MEAN_cm) & 
                     is.na(data$swim_speed_MEAN_cm_s) & # <<<<<< 
                     !is.na(data$SWIM_cms))] <- "estimated"
  
  # swim speed reported, length either way
  data$SWIM_cms_source[which(c(data$Length_cm_value_source == "reported" | data$Length_cm_value_source == "estimated") &
                    !is.na(data$swim_speed_MEAN_cm_s)& # <<<<<< 
                    !is.na(data$SWIM_cms))] <- "reported"
  
  # swim speed reported, no size of any kind
  data$SWIM_cms_source[which(c(is.na(data$Length_MEAN_cm) & is.na(data$Length_cm_value_source) & is.na(data$Length_cm_estimated) & 
                               !is.na(data$swim_speed_MEAN_cm_s) & # <<<<<< 
                               !is.na(data$SWIM_cms)))] <- "reported"
  
  
  # view(data[which(is.na(data$SWIM_cms_source)),])
       
  Fieldswim<-data[c(data$Test_performance=="Field" | data$Reference_number_1 == "58"), ]
  Labswim<-data[!c(data$Test_performance=="Field" | data$Reference_number_1 == "58"), ]
  nrow(Fieldswim) + nrow(Labswim) == nrow(data)
  message(
    paste("Swim speeds from field: total(",  nrow(Fieldswim),"),  N indiv(", length(unique(Fieldswim$FishID)), "),  N studies(", length(unique(Fieldswim$Reference_number)), ")", sep="")
          ) # 24
  message(
    paste("Swim speeds from the lab: total(",  nrow(Labswim),"),  N indiv(", length(unique(Labswim$FishID)), "),  N studies(", length(unique(Labswim$Reference_number)), ")", sep="")
  ) # 24length(unique(Labswim$Reference_number)) # 71
  

  
  # freshawter vs saltwater adults:
  fresh<-data[c(data$SW_FW=="FW"),]
  salt<-data[!c(data$SW_FW=="FW"),] # this includes brackish
  
  male<-data[which(data$Sex_F_M=="M"),]
  female<-data[which(data$Sex_F_M=="F"),]
  mixedsex<-data[which(data$Sex_F_M=="mixed"),]
  
  
  return(invisible(list(data, fresh, salt, male, female, mixedsex, Fieldswim, Labswim)))
  
}
  
  

  
  
  
  
  
  
  
