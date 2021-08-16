
factorial(20)/(factorial(3)*factorial(17))
choose(20,3)

choose(20,17)


pos.d <- 0.009/(0.009+0.001)
pos.d

neg.notd <- 0.891/(0.891+0.0991)
neg.notd

d.pos <- 0.009/(0.009+0.099)
d.pos

d.neg <- 0.002/(0.001+0.891)

####

a1 <-0.7
a2 <- 0.2
a3 <-0.1
b.a1 <- 0.9 
b.a2<-0.01
b.a3<-0.01
b<-b.a1*a1+b.a2*a2+b.a3*a3
a1.b<-b.a1*a1/b
a1.b



choose(4,2)*0.91^2*(1-0.91)^2
# more compactly
dbinom(x=2,size=4,p=0.91)


pnorm(0.6)-pnorm(-0.4)
pnorm(18,mean=15,sd=5)-pnorm(13,mean=15,sd=5)


1-pnorm(q=(1-3)/sqrt(5))
1-pnorm(q=1,mean=3,sd=sqrt(5))


pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

dnorm(1)
dnorm(-1)
pnorm(0.95)
1-pnorm(-0.95)


x<-seq(-8,8,length=400)
y1<-dnorm(x,mean=0,sd=0.5)
y2<-dnorm(x,mean=0,sd=1) 
y3<-dnorm(x,mean=0,sd=2)
plot(y1~x,type="l",col="red")
lines(y2~x,type="l",col="green")
lines(y3~x,type="l",col="blue")


n <- 125
sigma <- sqrt(5)
mu <-5
pnorm(5.5,mean = 5,sd =sigma/sqrt(n))
# alternatively
pnorm(2.5)

