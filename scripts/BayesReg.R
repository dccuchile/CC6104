
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

b.reg1 <- quap(
  alist(
    height ~ dnorm( b0 + b1*weight, sigma ),
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )


precis( b.reg1, prob=0.95 )

vcov( b.reg1 )

cov2cor( vcov( b.reg1 ) )




# samples from the posterior
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(b.reg1 ) , Sigma=vcov(b.reg1 ) )


post <- extract.samples( b.reg1 )
precis(post,prob=0.95)
post[1:5,]




round( vcov( b.reg1 ) , 3 )

coef(b.reg1)
plot( height ~ weight , data=d2 )
  abline( a=coef(b.reg1)["b0"] , b=coef(b.reg1)["b1"] )


plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( b.reg1 )
b0_map <- mean(post$b0)
b1_map <- mean(post$b1)
curve( b0_map + b1_map*x, add=TRUE )




mu_at_50 <- post$b0 + post$b1 * 50
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
PI( mu_at_50 , prob=0.89 )




# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )


# The function link provides distributions of posterior values for mu
# a posterior distribution of mu for each weight in weight.seq


mu <- link( b.reg1 , data=data.frame(weight=weight.seq) )

mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )


plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )

# Let's implement link ourselves
mu.link <- function(weight) post$b0 + post$b1*weight
mu2 <- sapply( weight.seq , mu.link )
mu2.mean <- apply( mu2 , 2 , mean )
mu2.PI <- apply( mu2 , 2 , PI , prob=0.95 )
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu2.mean )
# plot a shaded region for 89% PI
shade( mu2.PI , weight.seq )


# Predictor Intervals using the Posterior Predictive Distribution
#Let's simulate heights not average heights

#str(sim.height)

weight.sim <- function(weight) 
  rnorm(
    n=nrow(post) ,
    mean=post$b0 + post$b1*weight ,
    sd=post$sigma )
sim.height <- sapply( weight.seq , weight.sim)

#or alternatively
#sim.height <- sim( b.reg1 , data=list(weight=weight.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.PI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )





