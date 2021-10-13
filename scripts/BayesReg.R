
library(rethinking)
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


precis( b.reg1, prob=0.95 )

vcov( b.reg1 )

cov2cor( vcov( b.reg1 ) )

# centering
d2$weight.c <- d2$weight - mean(d2$weight)
b.reg2 <- quap(
  alist(
    height ~ dnorm( b0 + b1*weight.c, sigma ),
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
cov2cor( vcov( b.reg2 ) )


# samples from the posterior
post <- extract.samples( b.reg1, n= 1e4 )
head(post)
sapply(post,mean)

precis(post,prob=0.95)

library(MASS)
post2 <- mvrnorm( n=1e4 , mu=coef(b.reg1 ) , Sigma=vcov(b.reg1 ) )
post2<-as.data.frame(post2)
sapply(post2,mean)

precis(as.data.frame(post2))


dens(post$b0)
dens(post$b1)

post[1:5,]




round( vcov( b.reg1 ) , 3 )


plot( height ~ weight , data=d2 , col=rangi2 )
b0_map <- mean(post$b0)
b1_map <- mean(post$b1)
curve( b0_map + b1_map*x, add=TRUE )




mu_at_50 <- post$b0 + post$b1 * 50
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
HPDI( mu_at_50 , prob=0.95 )




# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )


# The function link provides distributions of posterior values for mu
# a posterior distribution of mu for each weight in weight.seq





# Let's implement link ourselves
mu.link <- function(weight) post$b0 + post$b1*weight
mu <- sapply( weight.seq , mu.link )
dim(mu)
head(mu)

mu.mean <- apply( mu , 2 , mean )
head(mu.mean)
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )

plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 95% HPPDI
shade( mu.HPDI , weight.seq )

# The same could have been done using the link function
mu <- link( b.reg1 , data=data.frame(weight=weight.seq), n=1e4 )



# Predictor Intervals using the Posterior Predictive Distribution
#Let's simulate heights not average heights

#str(sim.height)

height.weight <- function(weight) 
  rnorm(
    n=nrow(post) ,
    mean=post$b0 + post$b1*weight ,
    sd=post$sigma )

rnorm(4,mean=c(-100,100),sd=c(1,50))

sim.height <- sapply( weight.seq , height.weight)
dim(sim.height)


#or alternatively
#sim.height <- sim( b.reg1 ,n=1e4, data=list(weight=weight.seq) )

height.HPDI <- apply( sim.height , 2 , HPDI , prob=0.95 )

# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw HPDI region for simulated heights
shade( height.HPDI , weight.seq )





