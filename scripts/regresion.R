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
SSE<-sum(reg1$residuals^2)
SST<-sum((d2$height-mean(d2$height))^2)
SSM<-sum((reg1$fitted.values-mean(d2$height))^2)
SSM/SST
1-SSE/SST
1-var(reg1$residuals)/var(d2$height)
cor(d2$height,reg1$fitted.values)^2






mean(reg1$residuals)

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


# Polynomial Regression
d$weight_s <-( d$weight - mean(d$weight) )/sd(d$weight)
reg4 <- lm(height~weight_s+I(weight_s^2),d) 
reg4

# Binary attributes
d$male<-as.factor(d$male)
reg5<-lm(height~male,d)
# Intercept is the mean height of females and coefficient is the difference
reg5$coefficients
# male1 is the average difference of height between male and female
sum(reg5$coefficients)
means<-tapply(d$height,d$male,mean)
means[2]-means[1]

summary(reg5)
# Compare with p-value of t-test (without Welsh correction)
t.test(d$height~d$male, var.equal=T)


# We consider the weight too,
# the model has one single slope for both cases
reg6<-lm(height~weight+male,d)

# The problem is that this model is trying to use the same
# slope relating height to weight for both groups.
#If we want to fit them using lines with separate slopes, 
# we need to include an interaction in the model, 
# which is equivalent to fitting different lines for each of the two groups; 
# this is often denoted by using the ∗ or : symbol in the model.


# interaction (different slopes for each group)
reg7<-lm(height~weight+male+weight:male,d)
# or lm(height~weight*male,d)
reg7



# weight:male1 encodes the difference in slopes between both groups

d.male<-d[d$male==1,]
d.female<-d[d$male==0,]

reg8<-lm(height~weight,d.male)
reg8
reg9<-lm(height~weight,d.female)
reg9

reg7$coefficients["(Intercept)"]
reg9$coefficient["(Intercept)"]

reg7$coefficients["weight"]
reg9$coefficient["weight"]


reg7$coefficients["(Intercept)"]
reg9$coefficient["(Intercept)"]

reg7$coefficients["weight:male1"]
reg8$coefficients["weight"]-reg9$coefficients["weight"]


reg7$coefficients["male1"]
reg8$coefficients["(Intercept)"]-reg9$coefficient["(Intercept)"]