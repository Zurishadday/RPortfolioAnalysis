getPrices<-function(acciones=NULL,from="2014-01-01",to="2017-01-01", typeVaR){
  
  dataInput <-data.frame(diff(as.matrix(log(data.frame(lapply(
    lapply(acciones,
           getSymbols,src="yahoo",
           from = as.Date(from),
           to = as.Date(to),
           auto.assign = FALSE),Cl))))))
  
  if(typeVaR=="Historical simulation")   VaR<-Historic.VaR(dataInput,acciones)
  if(typeVaR=="Delta Normal")            VaR<-Delta.VaR(dataInput,acciones)  
  if(typeVaR=="Monte Carlo Simulation")  VaR<-MonteCarlo.VaR(dataInput,acciones)
  if(typeVaR=="EWMA Delta Normal")       VaR<-Ewma.Delta.VaR(dataInput,acciones)
  if(typeVaR=="EWMA MonteCarlo")         VaR<-Ewma.MonteCarlo.VaR(dataInput,acciones)
  return(VaR)
}

Historic.VaR<-function(m,number){
  alpha<-rep(1,length(unlist(strsplit(number, " "))))*50000
  Historica<-as.matrix(m)%*%alpha
  Historica<-data.frame(Historica)
  VaR<-1.65*sd(Historica$Historica)-mean(Historica$Historica)
  VaR<-data.frame(xmin=-Inf, xmax=-VaR, ymin=-Inf, ymax=Inf)
  
  printgg<-ggplot(Historica, aes(Historica)) +
    geom_histogram(aes(fill = ..count..), binwidth = 300)+
    xlab("Historical Simulation Histogram")+
    geom_rect(data=VaR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="red", alpha=0.3, inherit.aes = FALSE) +
    annotate("text", label = paste("VaR: $",
                                   as.character(round(-VaR$xmax,2))) ,fontface="bold"  ,
             x = VaR$xmax, y = 25, size = 8, colour = "black")
  
  return(printgg) 
}

Delta.VaR<-function(Asset,number){
  Cov<-cov(Asset)
  alpha<-rep(1,length(unlist(strsplit(number, " "))))*50000
  ##Metodo Delta
  VarianzaDelta= t(alpha)%*%Cov%*%alpha
  DesviacionDelta=sqrt(VarianzaDelta)
  
  ##Se crea el VaR del Delta.
  VaR<-1.65*DesviacionDelta
  VaR<-data.frame (xmin=-Inf, xmax=-VaR, ymin=-Inf, ymax=Inf)
  
  ## Se crea un portafolio de diferentes datos aleatorios de
  ##Media =0  y Desviacion Delta.
  Portafolio <-as.data.frame(rnorm(10000,0,DesviacionDelta))
  
  ## Crea el histograma del Metodo delta
  printgg<-ggplot(Portafolio, aes(`rnorm(10000, 0, DesviacionDelta)`)) +
    geom_histogram(aes(fill = ..count..), binwidth = 300)+
    xlab("Delta Method Histogram")+
    geom_rect(data=VaR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
              fill="red", alpha=0.3, inherit.aes = FALSE) +
    annotate("text", label = paste("VaR: $",
                                   as.character(round(-VaR$xmax,2))) ,fontface="bold"  
             , x = VaR$xmax, y = 200, size = 8, colour = "black")
  
  return(printgg)
}

MonteCarlo.VaR<-function(Asset,number){
  set.seed(58930123)
  number<-length(unlist(strsplit(number, " ")))
  Cov<-cov(Asset)
  Asset<-as.data.frame(na.omit(Asset))
  L<-chol(Cov)
  Mean<-sapply(Asset,mean)
  Montecarlo<-matrix(nrow = number,ncol = 10000)
  for(i in 1:10000){
    Montecarlo[,i] <- Mean + L%*%rnorm(number,0,1)
  }
  Montecarlo<-Montecarlo*50000
  Montecarlo<-rowSums(t(Montecarlo))
  Montecarlo<-data.frame(Montecarlo)
  
  
  ##VaR Montecarlo
  VaR<-1.65*sd(Montecarlo$Montecarlo)
  VaR<-data.frame (xmin=-Inf, xmax=-VaR, ymin=-Inf, ymax=Inf)
  
  
  ##Grafica el histogramas de los diferentes del MONTECARLO.
  printgg<-ggplot(Montecarlo, aes(Montecarlo)) +
    geom_histogram(aes(fill = ..count..), binwidth = 300)+
    xlab("MonteCarlo Simulation Histogram")+
    scale_fill_gradient("Count", low = "yellow")+
    geom_rect(data=VaR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="red", alpha=0.3, inherit.aes = FALSE) +
    annotate("text", label = paste("VaR: ",
                                   as.character(round(-VaR$xmax,2)))   ,
             x = VaR$xmax, y = 200, size = 8, colour = "black")
  
  return(printgg)
  
}

Ewma.Delta.VaR<-function(Asset,number){
  Cov.Ewma<-Cov.ewma(Asset,0.94,length(Asset[,1]))
  alpha<-rep(1,length(unlist(strsplit(number, " "))))*50000
  
  ##Metodo Delta
  VarianzaDelta= t(alpha)%*%Cov.Ewma%*%alpha
  DesviacionDelta=sqrt(VarianzaDelta)
  VaR<-1.65*DesviacionDelta
  VaR<-data.frame (xmin=-Inf, xmax=-VaR, ymin=-Inf, ymax=Inf)
  Portafolio <-as.data.frame(rnorm(10000,0,DesviacionDelta))
  
  printgg<-ggplot(Portafolio, aes(`rnorm(10000, 0, DesviacionDelta)`)) +
    geom_histogram(aes(fill = ..count..), binwidth = 300)+
    xlab("Histograma Metodo Delta")+
    geom_rect(data=VaR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="red", alpha=0.3, inherit.aes = FALSE) +
    annotate("text", label = paste("VaR: $",
                                   as.character(round(-VaR$xmax,2))) ,fontface="bold",
             x = VaR$xmax, y = 600, size = 8, colour = "black")+
    annotate("text", label = paste("EWMA Desviación Estandar:",
                                   as.character(round(DesviacionDelta,2))) ,fontface="bold",
             x = 2900, y = 900, size = 5, colour = "black")+
    labs(title = "EWMA Delta Normal")
  
  return(printgg)
}

Ewma.MonteCarlo.VaR<-function(Asset,number){
  set.seed(58930123)
  number<-length(unlist(strsplit(number, " ")))
  Cov.Ewma<-Cov.ewma(Asset,0.94,length(Asset[,1]))
  Asset<-as.data.frame(na.omit(Asset))
  L<-chol(Cov.Ewma)
  Mean<-sapply(Asset,mean)
  Montecarlo<-matrix(nrow = number,ncol = 10000)
  for(i in 1:10000){
    Montecarlo[,i] <- Mean + L%*%rnorm(number,0,1)
  }
  Montecarlo<-Montecarlo*50000
  Montecarlo<-rowSums(t(Montecarlo))
  Montecarlo<-data.frame(Montecarlo)
  
  
  ##VaR Montecarlo
  VaR<-1.65*sd(Montecarlo$Montecarlo)
  VaR<-data.frame (xmin=-Inf, xmax=-VaR, ymin=-Inf, ymax=Inf)
  
  
  ##Grafica el histogramas de los diferentes del MONTECARLO.
  printgg<-ggplot(Montecarlo, aes(Montecarlo)) +
    geom_histogram(aes(fill = ..count..), binwidth = 300)+
    scale_fill_gradient("Count", low = "yellow")+
    geom_rect(data=VaR, aes(xmin=xmin, xmax=xmax,
                            ymin=ymin, ymax=ymax), fill="red", alpha=0.3, inherit.aes = FALSE) +
    annotate("text", label = paste("VaR: ",as.character(round(-VaR$xmax,2))) 
             , x = VaR$xmax, y = 600, size = 8, colour = "black")+
    annotate("text", label = paste("EWMA Desviación:",
                                   as.character(round(sd(Montecarlo$Montecarlo),2))) ,
             fontface="bold"  , x = 2900, y = 900, size = 4, colour = "black")+
    labs(title = "EWMA Montecarlo")
  
  return(printgg)
  
}

Cov.ewma<-function(Asset,lambda,i){
  
  if (i==1) return(lambda*as.matrix(t(Asset[i,]))%*%as.matrix(Asset[i,]))
  else   
    return(lambda*Cov.ewma(Asset,lambda,i-1)+(1-lambda)*as.matrix(t(Asset[i,]))%*%as.matrix(Asset[i,]))
  
  
}
