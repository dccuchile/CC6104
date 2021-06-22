# Marble example posteriors.
theta <-c(0,0.25,0.5,0.75,1)
flat_prior<-c(1/5,1/5,1/5,1/5,1/5)

likelihood<-c(0,3/64,8/64,9/64,0)

posterior1<-flat_prior*likelihood
posterior1<-posterior1/sum(posterior1)

factory_prior<-c(0,3/6,2/6,1/6,0)

posterior2<-factory_prior*likelihood
posterior2<-posterior2/sum(posterior2)

df<-data.frame(theta,likelihood,flat_prior,posterior1,factory_prior,posterior2)

library(ggplot2)
library(reshape2)
d <- melt(df, id.vars="theta")
names(d)[3]<-"Prob"

# Separate plots
ggplot(d, aes(theta,Prob)) + 
#  scale_y_continuous(limits = c(0, 1)) +
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)


library(rethinking)
library(rstan)


globe.qa <- quap(
  alist(
    W ~ dbinom( W+L ,p) , # binomial likelihood
    p ~ dunif(0,1)   # uniform prior
  ) ,
  data=list(W=6,L=3) )

# display summary of quadratic approximation
precis( globe.qa )
sample.quap <- extract.samples(  globe.qa )
dens(sample.quap)


# globe tossing model posterior using grid approximation
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

library(rethinking)
dens(samples)

# theoretical samples
teo.samples<-rbeta(1e4,7,4)
dens(teo.samples)

# Intervals of defined boundaries
pbeta(0.5,7,4)

sum( posterior[ p_grid < 0.5 ] )


sum( samples < 0.5 ) / 1e4


sum( samples > 0.5 & samples < 0.75 ) / 1e4

pbeta(0.75,7,4)-pbeta(0.5,7,4)


quantile( samples , 0.8 )

qbeta(0.8,7,4 )

quantile( samples , c( 0.1 , 0.9 ) )
PI( samples , prob=0.8 )

c("10%"=qbeta(0.1,7,4 ),"90%"=qbeta(0.9,7,4 ))

# Asymetrical Posterior
  p_grid <- seq( from=0 , to=1 , length.out=1000 )
  prior <- rep(1,1000)
  likelihood <- dbinom( 3 , size=3 , prob=p_grid )
  posterior <- likelihood * prior
  posterior <- posterior / sum(posterior)
  samples.a <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
  dens(samples.a,xlim=c(0,0.935))
#alternatively we could sample 
#from the exact posterior
  teo.samples.a<-rbeta(1e4,4,1)
  dens(teo.samples.a,xlim=c(0,0.935))  

PI( samples.a , prob=0.5 )  

HPDI( samples.a , prob=0.5 )

HPDI( samples , prob=0.8 )
  