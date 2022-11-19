sumi_moi <-function(x){
  media=mean(x)      #media
  mediana=median(x)    #mediana
  Q1=as.numeric(quantile(x,0.25))   #Cuantil 25
  Q2=as.numeric(quantile(x,0.50))   #Cuantil 50
  Q3=as.numeric(quantile(x,0.75))   #Cuantil 75
  #MEDIDAS DE DISPERSION
  rango=max(x)-min(x)   #rango
  iqr=Q3-Q1  #Rango intercuartil  
  LI=Q1-1.5*iqr # limite inferior
  LS=Q3+1.5*iqr # limite superior
  as=3*(mean(x)-median(x))/sd(x) # 
  d9<-as.numeric(quantile(x,0.9)) #  cuantil 90
  d1<-as.numeric(quantile(x,0.1)) #  cuantil 10
  k=(0.5*(Q3-Q1))/(d9-d1) # Kurtosis
  varianza = var(x) # varianza
  sdev = sd(x) # Desviacion estandar
  coefv=sdev/media*100 # coeficiente de variacion
  # Box plot y de densidad
  par(mfrow=c(1,3))
  boxplot(x,horizontal = TRUE)
  plot(density(x, adjust = 5),col = 'red', lwd = 3)
  hist(x, breaks = 20, main= "Histograma" )
  retorno = data.frame(cbind(media, mediana, Q1, Q2, Q3, rango, iqr, LI, LS,
  as, d9, d1, k, varianza, sdev, coefv))
  return(retorno)
  }
