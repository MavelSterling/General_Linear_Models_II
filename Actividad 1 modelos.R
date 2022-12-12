#############  Validacion de supuestos- Modelos Lineales Generales II
#############  Modelo de Regresi??on

### JULIETH NATALIA SALAZAR 
### MAVELYN STERLING       
### KAREN DANIELA LÓPEZ 


B0<-matrix(data=NA,nrow=297,ncol=100,byrow = T)
B1<-matrix(data=NA,nrow=297,ncol=100,byrow = T)
SIG<-matrix(data=NA,nrow=297,ncol=100,byrow = T)
varianzaB0 = varianzaB1 = varianzaSIG =c() # se establecen los vectores
varTeoricaB0 = varTeoricaB1 =c()
n<-seq(4,300,1)
for (j in 1:length(n)){
  
  for (i in 1:100) {
    E <- rnorm(n[j], mean = 0, sd = sqrt(50.3))
    X <- runif(n[j], 30, 50)
    Y <- 15.3 + 8.39 * X + E
    mod <- lm(Y~X)
    B0[j,i] <- mod$coefficient[1]
    B1[j,i] <- mod$coefficient[2]
    SIG[j,i] <- anova(mod)[2,3]
  }
  columna1 <- matrix(1,n[j],1) 
  
  matrizX <- cbind(columna1,X)
  XT<- t(matrizX) # se calcula la matriz transpuesta de X
  
  XTX<- XT %*%matrizX # se multiplica la matriz XT por la matriz X
  
  XTX_inversa<- solve(XTX) # se calcula matriz inversa de XTX

  varTeoricaB0[j] <- XTX_inversa[1,1] * 50.3
  varTeoricaB1[j] <- XTX_inversa[2,2] * 50.3

  
  
  }







############### Calculo de la varianza #######################
varianzaB0=varianzaB1=varianzaSIG=c()
for (k in 1:297) {
  varianzaB0[k]=var(B0[k,])
  varianzaB1[k]=var(B1[k,])
  varianzaSIG[k]=var(SIG[k,])
  
}




############  Calculo de la media ####################
MediaB1=MediaB0=MediaSIG=c()

for (l in 1:297) {
  MediaB0[l]=mean(B0[l,])
  MediaB1[l]=mean(B1[l,])
  MediaSIG[l]=mean(SIG[l,])
}


############## Calculo de la Mediana ##############

MedianaB0=MedianaB1=MedianaSIG=c()
for (t in 1:297) {
  MedianaB0[t]=median(B0[t,])
  MedianaB1[t]=median(B1[t,])
  MedianaSIG[t]=median(SIG[t,])
}


############# Calculo de error cuadratico medio ###########


ECM_B0 = ECM_B1= ECM_SIG = c()

for(m in 1:297){
  ECM_B0[m] = mean(B0[m,]-15.3)^2
  
  ECM_B1[m] = mean(B1[m,]-8.39)^2
  
  ECM_SIG[m] = mean(SIG[m,]-50.3)^2
  }


par(mfrow=c(2,3))

plot(MediaB0, xlab="Muestras", ylab="Media de B0", pch=19)
abline(h=15.3, lty=2, col="red", lwd=2)
plot(MediaB1, xlab="Muestras", ylab="Media de B1", pch=19)
abline(h=8.39, lty=2, col="red", lwd=2)
plot(MediaSIG, xlab="Muestras", ylab="Media de SIG", pch=19)
abline(h=50.3, lty=2, col="red", lwd=2)

plot(MedianaB0, xlab="Muestras", ylab="Mediana de B0", pch=19)
abline(h=15.3, lty=2, col="red", lwd=2)
plot(MedianaB1, xlab="Muestras", ylab="Mediana de B1", pch=19)
abline(h=8.39, lty=2, col="red", lwd=2)
plot(MedianaSIG, xlab="Muestras", ylab="Mediana de SIG", pch=19)
abline(h=50.3, lty=2, col="red", lwd=2)


par(mfrow=c(1,3))

plot(varianzaB0, xlab="Muestras", ylab="Varianza de B0", pch=19)
abline(h=15.3, lty=2, col="red", lwd=2)
plot(varianzaB1, xlab="Muestras", ylab="Varianza de B1", pch=19)
abline(h=8.39, lty=2, col="red", lwd=2)
plot(varianzaSIG, xlab="Muestras", ylab="Varianza de SIG", pch=19)
abline(h=50.3, lty=2, col="red", lwd=2)

x11()
par(mfrow=c(1,3))
plot(ECM_B0, xlab ="Muestras", ylab = "ECM de B0", pch=19)
plot(ECM_B1, xlab ="Muestras", ylab = "ECM de B1", pch=19)
plot(ECM_SIG, xlab ="Muestras", ylab = "ECM de SIG", pch=19)




