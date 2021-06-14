
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# define the average weight, x-bar
xbar <- mean(d2$weight)


m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )


precis( m4.3 )


round( vcov( m4.3 ) , 3 )


plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*x, add=TRUE )


post <- extract.samples( m4.3 )
post[1:5,]

mu_at_50 <- post$a + post$b * ( 50 - xbar )

dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )

PI( mu_at_50 , prob=0.89 )




# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )


# The function link provides a posterior distribution 
# of mu for each weight in weight.seq


mu <- link( m4.3 , data=data.frame(weight=weight.seq) )

mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )


plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )

# alternative
mu.link <- function(weight) post$a + post$b*weight
mu2 <- sapply( weight.seq , mu.link )
mu2.mean <- apply( mu2 , 2 , mean )
mu2.PI <- apply( mu2 , 2 , PI , prob=0.95 )
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu2.mean )
# plot a shaded region for 89% PI
shade( mu2.PI , weight.seq )
