# Análisis de Regresión
# Felipe Bravo Márquez

library(rethinking)
data(Howell1)
d <- Howell1
cor(d)


# discard non-adults
d2 <- d[ d$age >= 18 , ]
cor(d2)

#Para ajustar el modelo lineal
reg1<-lm(height~weight,d2)
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
cor(d2$height,reg1$fitted.values)^2






plot(d2$height,reg1$fitted.values)

new.weights<-data.frame(weight=c(50,62))
predict.lm(object=reg1,newdata=new.weights)
# this is equivalent to:
reg1.coef[1]+reg1.coef[2]*new.weights[1:2,]

# Regresión Mutilple



reg2<-lm(height~weight+age,d)
summary(reg2)

# Grafico el plano de la regresión
library("scatterplot3d")
s3d <- scatterplot3d(d[,c("weight","age","height")],
                     type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16, 
                     main="height~weight+age")
s3d$plane3d(reg2, lty.box = "solid")


data(WaffleDivorce)
# Predit divorce rate from marriage rate and median age at marriage
reg3<-lm(Divorce~Marriage+MedianAgeMarriage,WaffleDivorce)


