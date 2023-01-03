
### simulate data -------- 
# inspired by:
# http://www.columbia.edu/~cjd11/charles_dimaggio/DIRE/resources/R/power.pdf

# From Lee et al 2003 swim temp paper Fig 5A GC sockeye
# MO2 = –54.44 + 68.85 / (1 + e^(- (speed–1.58) / 0.087 ))^0.013
sim_data <- as.data.frame(matrix(ncol=2, nrow=length(seq(0 ,10 , 0.1))))
sim_data$V1 <- seq(0, 10, 0.1)
sim_data$V2 <- 50 # cm 
sim_data$mo2_sim <- NA

# d <- sim_data[1,]
# d2 <- sim_data[nrow(sim_data),]
# n <- 20
# sim_data<-rbind(as.data.frame(do.call("rbind", replicate(n, d, simplify = FALSE))), sim_data)
# sim_data<-rbind(sim_data, as.data.frame(do.call("rbind", replicate(n, d2, simplify = FALSE))))
# xx <- seq(-0.5, 5, len = 101)
# plot(xx, yy)

# A+(B-A)/(1+exp((xmid-input)/scal))
mo2_Lee_GC <- function (speed, size=NA) {
  mo2 <- 2 + 3 / (1 + exp((2.5-speed))/0.5) # == SSfpl(xx, *) 
}
                 
for (i in 1: nrow(sim_data)){
  sim_data$mo2_sim[i] <- mo2_Lee_GC (sim_data$V1[i])
} 

plot(x=sim_data$V1/2, y=sim_data$mo2_sim)

sim_data$size_cm3<-30
sim_data$size_cm6<-60
sim_data$size_cm8<-80

sim_data$speed_cm_s<-(sim_data$V1/2)*50
sim_data$speed_cm_s3<-(sim_data$V1/2)*30
sim_data$speed_cm_s6<-(sim_data$V1/2)*60
sim_data$speed_cm_s8<-(sim_data$V1/2)*80

adjustcolor("grey", alpha.f = 0.2)

# when plotting adjust the scale slight to have a bit closer to commonly reported  values 
# y axis = + 3
# x-axis = + 0.5
png("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Salmon/Big Bar Rockslide /FINAL FILES - prep REPORT /CONCEPTUAL-report.png", res = 200, width = 8, height = 4, units="in")
par(mfrow=c(1,2))
plot(x=sim_data$V1/2.9, y=sim_data$mo2_sim+2, type="l", lwd=2, ylab=expression(Oxygen~Consumption~(mgO[2]/kg/min)), xlab="Swim speed (BL/s)")
lines(x=(sim_data$V1/2.9)+0.06, y=sim_data$mo2_sim+2, col="red", lwd=2)
lines(x=(sim_data$V1/2.9)+0.03, y=sim_data$mo2_sim+2, col="blue", lwd=2)
lines(x=(sim_data$V1/2.9)+0.1, y=sim_data$mo2_sim+2, col="forestgreen", lwd=2)
rect(xleft=1.5, ybottom=0, xright=2, ytop=10, col = adjustcolor("grey20", alpha.f = 0.2), border = "transparent") # coloured
abline(v = 2, col="grey30", lwd=2, lty=2)


plot(x=sim_data$speed_cm_s-30 , y=sim_data$mo2_sim+2, type="l", lwd=2, ylab=expression(Oxygen~Consumption~(mgO[2]/kg/min)), xlab="Swim speed (cm/s)")
lines(x=sim_data$speed_cm_s3-30, y=sim_data$mo2_sim+2, col="red",lwd=2)
lines(x=sim_data$speed_cm_s6-30, y=sim_data$mo2_sim+2, col="blue",lwd=2)
lines(x=sim_data$speed_cm_s8-30, y=sim_data$mo2_sim+2, col="forestgreen",lwd=2)
abline(v = 60, col="red", lwd=1, lty=2)
abline(v = 100, col="black", lwd=1, lty=2)
abline(v = 120, col="blue", lwd=1, lty=2)
abline(v = 160, col="forestgreen", lwd=1, lty=2)

# # make this in cm/ s
dev.off()


