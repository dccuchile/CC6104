# Análisis de Regresión
# Felipe Bravo Márquez

data(USArrests)
attach(USArrests)
# Primero analizamos las correlaciones
cor(USArrests)

#Para ajustar el modelo lineal
reg1<-lm(Murder~Assault,USArrests)
reg1

# Para ver los coeficientes
reg1$coefficients

reg1.coef<-reg1$coefficients
reg1.coef

#Para resumir el modelo
summary(reg1)

#Puedo guardar el resumen
sum.reg1<-summary(reg1)
sum.reg1$r.squared



# Para ver los valores ajustados
reg1$fitted.values

# Puedo ver que la correlación al cuadrado de mis valores ajustados y los reales son los mismo que
# el coeficiente de determinacion
cor(Murder,reg1$fitted.values)^2

cor(Murder, Assault)^2





plot(USArrests$Murder,reg1$fitted.values)

nuevos.arrestos<-data.frame(Assault=c(500,12))
predict.lm(object=reg1,newdata=nuevos.arrestos)
# Esto es equivalente a:
reg1.coef[1]+reg1.coef[2]*nuevos.arrestos

# Regresión Mutilple
reg2<-lm(Rape~Assault+Murder,USArrests)
summary(reg2)

# Grafico el plano de la regresión
library("scatterplot3d")
s3d <- scatterplot3d(USArrests[,c("Assault","Murder","Rape")],
                     type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16, 
                     main="Rape~Assault+Murder")
s3d$plane3d(reg2, lty.box = "solid")


