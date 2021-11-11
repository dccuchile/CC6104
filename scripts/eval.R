sppnames <- c( "afarensis","africanus","habilis",
               "boisei", "rudolfensis","ergaster",
               "sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 
                 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 
             61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc,
                 mass=masskg )


reg.ev.1 <- lm( brain ~ mass , data=d )
reg.ev.2 <- lm( brain ~ mass + I(mass^2)
                , data=d )
reg.ev.3 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3),data=d )
reg.ev.4 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4),data=d )
reg.ev.5 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4)
                + I(mass^5),data=d )
reg.ev.6 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4)+ 
                  I(mass^5)+ I(mass^6),data=d )

summary(reg.ev.1)$r.squared
summary(reg.ev.2)$r.squared
summary(reg.ev.3)$r.squared
summary(reg.ev.4)$r.squared
summary(reg.ev.5)$r.squared
summary(reg.ev.6)$r.squared

reg.ev.0 <- lm( brain ~ 1 , data=d )
summary(reg.ev.0)$r.squared

f <- c( 0.3 , 0.7 )
-sum( f*log2(f) )

f <- c( 0.01 , 0.99 )
-sum( f*log2(f) )



f<-c(0.3,0.7)
q<-c(0.25,0.75)
sum(f* log2(f/ q)) 

q<-f
sum(f* log2(f/ q)) 

library(tidyverse)

t <- 
  tibble(f_1  = .3,
         f_2  = .7,
         q_1  = seq(from = .01, to = .99, by = .01)) %>% 
  mutate(q_2  = 1 - q_1) %>%
  mutate(d_kl = (f_1 * log2(f_1 / q_1)) + (f_2 * log2(f_2 / q_2)))

t %>% 
  ggplot(aes(x = q_1, y = d_kl)) +
  geom_vline(xintercept = .3,  linetype = 2) +
  geom_line(size = 1.5) +
  annotate(geom = "text", x = .4, y = 1.5, label = "q = f", 
           size = 3.5) +
  labs(x = "q(1)",
       y = "Divergence of q from f") 


logLik(reg.ev.1)
logLik(reg.ev.2)

b0<-reg.ev.1$coefficients[1]
b1<-reg.ev.1$coefficients[2]

logLik1 <- sum(log(dnorm(
  d$brain ,
  mean=b0+b1*d$mass ,
  sd=sigma(reg.ev.1))
  ))
logLik1

# is slightly different from logLik because logLik uses the ML estimator of sigma

# the sigma function on the other hand uses and unbiased estimator
sigma(reg.ev.1)

# degrees of freedom are n-2
reg.ev.1$df.residual
sigma.LM <-sqrt(sum(reg.ev.1$residuals^2)/
                  reg.ev.1$df.residual)
sigma.LM

# Sigma ML replaces the denominator by the number of examples
sigma.ML<-sqrt(sum(reg.ev.1$residuals^2)
               /dim(d)[1])

sigma.ML

logLik1.1 <- sum(log(dnorm(
  d$brain ,
  mean=b0+b1*d$mass ,
  sd=sigma.ML)
))
logLik1.1


# Lets computer logLik using quap with flat priors
library(rethinking)

b.reg.ev.1<- quap(
  alist(
    brain ~ dnorm( mu , sigma ) ,
    mu <- b0 + b1*mass
  ) ,
  data=d ,
  start=list(b0=mean(d$brain),b1=0,sigma=sd(d$brain)) ,
  method="Nelder-Mead" )

theta <- coef(b.reg.ev.1)
# compute logLik
logLik1.2 <- sum(log(dnorm(
  d$brain ,
  mean=theta[1]+theta[2]*d$mass ,
  sd=theta[3]) 
  ))
logLik1.2

# deviance


-2*logLik(reg.ev.1)
-2*logLik(reg.ev.2)
-2*logLik(reg.ev.3)
-2*logLik(reg.ev.4)

library(MASS)
rid.ev.4<- lm.ridge(brain ~ mass + I(mass^2)
         + I(mass^3) + I(mass^4),data=d,lambda = 0.1) 
rid.ev.4


-2*logLik(reg.ev.1)+2*3

AIC(reg.ev.1)
AIC(reg.ev.2)
AIC(reg.ev.3)
AIC(reg.ev.4)
AIC(reg.ev.5)


# DIC

n<-10000
b.reg.1.samples<-extract.samples(b.reg.ev.1,n)

post.deviance<-vector()
for(i in 1:n){
  b0<-b.reg.1.samples[i,1]
  b1<-b.reg.1.samples[i,2]
  sig<-b.reg.1.samples[i,3]
  sig<-ifelse(sig<0, 0, sig)
  logLikDic <- sum(log(dnorm(
    d$brain ,
    mean=b0+b1*d$mass ,
    sd=sig) 
  ))
  post.deviance[i]<- -2*logLikDic
}

d.bar<-mean(post.deviance,trim=0.01)
d.hat<- -2*logLik1.2
2*d.bar-d.hat
DIC(b.reg.ev.1)


data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

b.reg1 <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- b0 + b1*weight,
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )

b.reg2 <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- b0 + b1*weight+b2*age,
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    b2 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )




b.reg3 <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- b0 + b1*weight+b2*weight^2,
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    b2 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )

DIC(b.reg1)
DIC(b.reg2)
DIC(b.reg3)

compare(b.reg1,b.reg2,b.reg3)