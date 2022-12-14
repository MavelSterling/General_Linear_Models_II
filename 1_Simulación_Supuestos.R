# 24 de Mayo de 2019
# Modelo Lineal General II
# Validaci?n de Supuestos

# 1. Ajustemos el siguiente modelo lineal Y = 15.3 + 8.39X + E
#    Mantendremos los mismo supuestos: Correcta especificaci?n, Homogeneidad de 
#    Varianza, Independencia y Normalidad. En este caso, la Varianza se establece
#    en 50.3. Se utilizar? una muestra de tama?o 50.

E <- rnorm(50, mean = 0, sd = sqrt(50.3))
X <- runif(50, 30, 50)
Y <- 15.3 + 8.39 * X + E

# 2. Grafiquemos los datos
par(family="serif")
plot(Y ~ X, xlab="Variable Independiente", ylab="Variable Dependiente",
	pch=19, col="darkgrey")

# 3. El Modelo Poblacional versus El Modelo Ajustado
segments(y0=15.3+8.39*30, x0=30, y1=15.3+8.39*50, x1=50, lty=1, lwd=2)

mod1 <- lm(Y~X)
abline(mod1, col="red", lwd=2)

# 4. An?licemos el Modelo que se ha ajustado
summary(mod1)
anova(mod1)

# 5. Observemos el comportamiento de las estimaciones que se est?n obteniendo
B0 = B1 = SIG = 0
for(i in 1:500){
	ER <- rnorm(50, mean = 0, sd = sqrt(50.3))
	XR <- runif(50, 30, 50)
	YR <- 15.3 + 8.39 * XR + ER
	mod <- lm(YR~XR)
	B0[i] <- mod$coefficient[1]
	B1[i] <- mod$coefficient[2]
	SIG[i] <- anova(mod)[2,3]
}

par(family="serif", mfrow=c(3,3), mar=c(4,1,1,1))
hist(B0, main="", xlab="Intercepto", ylab="", nclass=30, col="grey")
abline(v=15.8, lty=2, col="red", lwd=2)
plot(B0, xlab="Estimaciones", ylab="", pch=19)
abline(h=15.8, lty=2, col="red", lwd=2)
qqnorm(B0, main="", pch=19)
qqline(B0, col="red")
legend("topleft", legend=round(as.numeric(shapiro.test(B0)[2]), 4), bty="n")

hist(B1, main="", xlab="Pendiente", ylab="", nclass=30, col="grey")
abline(v=8.39, lty=2, col="red", lwd=2)
plot(B1, xlab="Estimaciones", ylab="", pch=19)
abline(h=8.39, lty=2, col="red", lwd=2)
qqnorm(B1, main="", pch=19)
qqline(B1, col="red")
legend("topleft", legend=round(as.numeric(shapiro.test(B1)[2]), 4), bty="n")

hist(SIG, main="", xlab="Varianza", ylab="", nclass=30, col="grey")
abline(v=50.3, lty=2, col="red", lwd=2)
plot(SIG, xlab="Estimaciones", ylab="", pch=19)
abline(h=50.3, lty=2, col="red", lwd=2)
qqnorm(SIG, main="", pch=19)
qqline(SIG, col="red")
legend("topleft", legend=round(as.numeric(shapiro.test(SIG)[2]), 4), bty="n")

# 6. Ahora analicemos como se comporta la potencia del contraste de la hip?tesis
#    general del modelo donde se contrasta la significancia del modelo

W <- anova(mod1)[1,4]

# 6.1. Calculamos la Potencia Real, esta se obtiene conociendo el valor de los
#	 par?metros

LambdaReal <- 0.5*sum(I((15.3 + 8.39 * X)^2))/50.3
PotReal <- 1 - pf(q = anova(mod1)[1,4], df1 = 2, df2 = 48, ncp = 2*LambdaReal)

# 6.2. Calculamos la Potencia Estimada, esta se obtiene con la estimaci?n de los
#	 par?metros

LambdaEstimado <- 0.5*sum(I(mod1$fitted.values^2))/anova(mod1)[2,3]
PotEstimada <- 1 - pf(q = anova(mod1)[1,4], df1 = 2, df2 = 48, ncp = 2*LambdaEstimado)

# 6.3. El c?lculo de la potencia depende de qu? tan cerca o qu? tan alejado se encuentre
#	 de la hip?tesis nula. Entonces se podr?a evaluar la cercan?a a la hip?tesis nula
#      y analizar como se afecta la potencia. En este caso, solo concentremonos en la
#      pendiente.

Potencia = 0
B1 <- seq(0, 8.39, length.out=100)
for(i in 1:length(B1)){
	Lambda <- 0.5*sum(I((mod1$coefficient[1] + B1[i]*X)^2))/anova(mod1)[2,3]
	Potencia[i] <- 1 - pf(q = anova(mod1)[1,4], df1 = 2, df2 = 48, ncp = 2*Lambda)
}

par(family="serif")
plot(Potencia~B1, xlab="Hip?tesis Alterna sobre la Pendiente", ylab="C?lculo de Potencia",
	type="l")

# Algunas Actividades
# 1. ?Qu? pasa con el tama?o de muestra? Se puede observar c?mo influye en las
#    caracter?sticas observadas.
# 2. ?Qu? sucede con el supuesto de correcta especificaci?n cuando no se cumple?
#    Se podr?a pensar en un modelo poblacional cuadr?tico y ajustar uno lineal. El
#    tama?o de muestra podr?a influir.
# 3. ?Qu? sucede con el supuesto de homogeneidad de varianza? Pensemos por ejemplo en
#    una varianza crecience y una varianza decreciente. El tama?o de la muestra otra
#    vez.
# 4. ?Qu? podr?a suceder con el supuesto de independencia? Lo primero es: ?c?mo gene-
#    ramos datos correlacionados? Hay que definir una estructura de covarianza (o cor-
#    relaci?n. Se podr?a pensar en el tama?o de muestra, pero es una labor m?s comple-
#    ja.
# 5. Finalmente el supuesto de Normalidad. Se podr?a considerar una distribuci?n de
#    de una variable aleatoria contin?a pero asim?trica y trabajar con la asimetr?a 
#    de la distribuci?n o se podr?a trabajar con una distribuci?n discreta.
# 6. Una ?ltima actividad supone que el verdadero valor del intercepto y la pendiente
#    est?n muy cercanos al cero y la variabilidad es muy alta. En definitiva, el mues-
#    treo nos indica que necesitar?amos un tama?o de muestra m?s grande. ?Qu? pasar?a
#    con todas las actividades anteriores?
