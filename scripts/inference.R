




pop <-c(2,3,4,5,6)
mean(pop)
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
sd.p(pop)


library(gtools)
library(tidyverse)
samp_size <- 2
samples<-as_tibble(permutations(length(pop), samp_size,
                                pop, repeats.allowed=TRUE))

samples <- samples %>% rowwise() %>% 
  mutate(sample_mean=mean(c(V1,V2)))
samples

library(ggplot2)
ggplot(samples, aes(x=sample_mean)) + 
  geom_histogram(bins = 9, color="black", fill="white")
ggplot(data.frame(pop), aes(x=pop)) +
  geom_histogram(bins = 5, color="black", fill="white")



mean(samples$sample_mean)
sd.p(samples$sample_mean)


sd.p(pop)/sqrt(samp_size)




alpha <- 0.05
xbar <- 5
sigma <- 2
n <- 20
se <-sigma/sqrt(n)
error <- qnorm(1-alpha/2)*se
left <- xbar-error
right <- xbar+error
left
right


x<-seq(-8,8,length=400)
y1<-dnorm(x)
y2<-dt(x=x,df=1)
y3<-dt(x=x,df=10)
y4<-dt(x=x,df=350)
plot(y1~x,type="l",col="green")
lines(y2~x,type="l",col="blue")
lines(y3~x,type="l",col="black")
lines(y4~x,type="l",col="red")



data(iris)
alpha<-0.05
n<-length(iris$Petal.Length)
xbar<-mean(iris$Petal.Length)
xbar

s<-sd(iris$Petal.Length)
se<-s/sqrt(n)
error<-qt(p=1-alpha/2,df=n-1)*se
left<-xbar-error
left
right<-xbar+error
right

test<-t.test(iris$Petal.Length,conf.level=0.95)
test$conf.int

qnorm(1-0.025)
prop<-prop.test(1219,3532,correct=FALSE)
prop$conf.int





# The bootstrap is useful for creating confidence intervals in cases 
# where we don't have a parametric distribution. 
# One example is for the median; let's look at how that works. 

myboot<-function(x,fun,nRuns,sampleSize,alpha){
values<-vector()
for(i in 1:nRuns){
  samp.i<-sample(x, size = sampleSize, replace = T)
  values[i]<-fun(samp.i)
}
point.est <-fun(x)
se <- sd(values)
l.CI <- quantile(values, alpha/2)
u.CI <- quantile(values, 1-alpha/2)


return(c("Point Estimate"=point.est,
         "Standard error"=se,
         "Lower CI limit" = l.CI,
         "Upper CI limit" = u.CI))
}

myboot(iris$Petal.Length,median,6000,64,0.05)
