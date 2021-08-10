# Data Exploration and Visualization
# Based on chapter 3 of the book Introduction to Data Mining
# Author: Felipe Bravo-Marquez

data(iris)

#To see the basic statistics we do summary
summary(iris)

#To see the attributes of the data.frame
names(iris)

dim(iris)

#To pass all the variables of the data.frame to the environment
attach(iris)

#frequencies
table(iris$Species)
vec<-c(1,1,1,0,0,3,3,3,3,2)
table(vec)
table(vec)/length(vec)  # Percent frequency

# The mode
my_mode<-function(var){
  frec.var<-table(var)
  value<-which(frec.var==max(frec.var))# Elements with  the maximum value
  as.numeric(names(value))
}

my_mode(vec)
my_mode(iris$Sepal.Length)

#Basic statistics
mean(Sepal.Length)
median(Sepal.Length)
#truncated mean defines the percentage of extreme elements that it does not consider
mean(Sepal.Length, trim=0.1)

# we create a vector of size 10, with mean 20 and sd 10
vec<-rnorm(10,20,10)
mean(vec)
vec.noise<-c(vec,rnorm(1,300,100))
mean(vec.noise)

mean(vec,trim=0.1)
mean(vec.noise,trim=0.1)

median(vec)
median(vec.noise)


# All percentiles
quantile(Sepal.Length,seq(0,1,0.01))
quantile(Sepal.Length,seq(0,1,0.25))

tapply(iris$Petal.Length,iris$Species,summary)
tapply(iris$Petal.Width,iris$Species,summary)
tapply(iris$Sepal.Length,iris$Species,summary)
tapply(iris$Sepal.Width,iris$Species,summary)





summary(iris)


#frequency
table(iris$Species)

# Variability
range(Sepal.Length)
max(Sepal.Length)-min(Sepal.Length)

sd(Sepal.Length)
sepal.var<-var(Sepal.Length)
# This can be equivalenty computed as
myvar<-sum((Sepal.Length-mean(Sepal.Length))^2)/(length(Sepal.Length-1))

var(Sepal.Length)
sd(Sepal.Length)
aad<-function(x,fun=median){
  mean(abs(x-fun(x)))
}
aad(Sepal.Length)
aad(Sepal.Length,mean)

mad(Sepal.Length)
median(abs(Sepal.Length-median(Sepal.Length)))
mad(Sepal.Length,constant=1)
mad(Sepal.Length,center=mean(Sepal.Length),1)

IQR(Sepal.Length)

?quantile
?IQR

###
cov(Sepal.Length,Sepal.Width)
cov(iris[,1:4])
cor(iris[,1:4])


# Contingency Tables
gender<-c("Male", "Female", "Male", "Female", "Female", "Male")
studies<-c("college","postgraduate","high school",
            "postgraduate","high school","college")
table(gender,studies)
weather<-read.table("weather.nominal.csv",header=T,sep=",")
table(weather$outlook,weather$play)
table(weather$temperature,weather$play)
library(modeest)

# Skweness and Kurtosis
library(moments)
#positive skew
skewness(c(1,1,2,3)) 
#symmetrical
skewness(c(1,2,3)) 
#negative skew
skewness(c(1,2,3,3)) 


x <- rnorm(1000, 0,1)
plot(density(x))
kurtosis(x)
# As expected we got a value close to 3!

x<-rexp(1000)
plot(density(x))
kurtosis(x)
# As expected we get a positive excess kurtosis (i.e. greater than 3) since the distribution has fatter tails.

x <-rbeta(1000,2,2)
plot(density(x))
kurtosis(x)
# As expected we get a negative excess kurtosis (i.e. less than 3)
#since the distribution has thiner tails. 

#Visualization

png("imagen.png")
plot(1:10)
dev.off()

plot(rnorm(15,10,5),col="red",type="p",pch=1)
lines(rnorm(15,10,5),col="blue",type="p",pch=1)
lines(rnorm(15,10,5),col="green",type="b",pch=2)
title(main="My Plot")
legend('topright', c("lines","dots","both") , 
       lty=1:3, col=c("red", "blue","green"), bty='n', cex=.75)




## Histograms

hist(Sepal.Length)
lines(density(Sepal.Length))
hist(Sepal.Length,nclass=length(Sepal.Length))

## with ggplot2

library(ggplot2)
# Basic histogram
ggplot(iris, aes(x=Sepal.Length)) + geom_histogram(bins = 10, color="black", fill="white")


plot(density(iris$Sepal.Length),main="Density of Sepal.Length")
plot(density(iris$Sepal.Length),col="red",main="Densidad")


pie(table(iris$Species))

boxplot(Sepal.Length,main="Boxplot Sepal.Length")
boxplot(Sepal.Length~Species,ylab="Sepal.Length")
boxplot(x=iris[,1:4],main="Boxplots Iris")


ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
  geom_boxplot()






plot(Sepal.Width~Sepal.Length, col=Species)
plot(Sepal.Length, Sepal.Width,col=Species,
     pch=as.numeric(Species))
legend('topright', levels(Species) , 
       lty=1, col=1:3, bty='n', cex=.75)


ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3,shape=4)

plot(iris)
pairs(iris)


pairs(iris[,1:4],pch=as.numeric(iris$Species),col=iris$Species)
plot(iris$Sepal.Length,col=as.numeric(iris$Species))

plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species))
legend('topright', levels(Species) , 
       lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.75)


ggpairs(iris[,1:4])


dev.off()

library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width, color=as.numeric(iris$Species))

library(MASS)
parcoord(iris[1:4], col=iris$Species,var.label=T)

iris_sample1<-iris[sample(1:dim(iris)[1],size=6,replace=F),]
rownames(iris_sample1)<-paste(as.character(iris_sample1$Species),1:6)
stars(iris_sample1[1:4])

library("aplpack")
iris_sample<-iris[sample(1:dim(iris)[1],size=16,replace=F),]
faces(iris_sample[1:4],face.type=1,labels=iris_sample$Species)

library(gplots)

library(ggplot2)


qplot(Sepal.Length,data=iris,geom ="histogram",binwidth=0.1)
qplot(Sepal.Length,data=iris,geom ="density")
