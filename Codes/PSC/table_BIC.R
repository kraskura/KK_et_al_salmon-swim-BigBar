
ICdelta<-function(ICtable, IC= "BIC"){
  
  if(IC == "BIC"){
    BIC.t <- ICtable [order(ICtable$BIC), ]
    BIC.t$delta <- round(abs(BIC.t$BIC[1] -  BIC.t$BIC), 5)
    IC.t<-BIC.t 
  }
  if(IC == "AIC"){
    AIC.t <- ICtable [order(ICtable$AIC), ]
    AIC.t$delta <- round(abs(AIC.t$AIC[1] -  AIC.t$AIC), 5)
    IC.t<-AIC.t 
  }
  
  return(IC.t)
}