
library(tidyverse)

# From Hinch and Rand 1998 
# 1. TBF and size estimations
# ln TBF = lnEMGPI(1.2484 -0.0498(L)) + (0.3767(L) -5.7794).

tbf.d1<-expand.grid(L=seq(30,90, 5), EMGPI =seq(exp(6), exp(7.5), 20))
tbf.d1$lnTBF<-log(tbf.d1$EMGPI)*(1.2484 -0.0498*(tbf.d1$L)) + ((0.3767*(tbf.d1$L) -5.7794))
tbf.d1$TBF<-exp(tbf.d1$lnTBF)
tbf.d1$flow5ms<-500



ggplot(tbf.d1, aes(y=exp(lnTBF), x=EMGPI, colour=L, group=L))+
         geom_line()

ggplot(tbf.d1, aes(y=lnTBF, x=log(EMGPI), colour=L, group=L))+
         geom_line()


 # From Brett 1995 
# BL(swim) = 0.023(TBF) - 1.286

tbf.d1$BLswim<- 0.023*(tbf.d1$TBF) - 1.286
tbf.d1$CMswim<-tbf.d1$BLswim*tbf.d1$L

ggplot(tbf.d1, aes(x=lnTBF, y=BLswim, colour=L, group=L))+
  geom_point()+
  facet_grid(.~L)
  
ggplot(tbf.d1, aes(x=lnTBF, y=CMswim, colour=L, group=L))+
  geom_point()+
  facet_grid(.~L)
                      