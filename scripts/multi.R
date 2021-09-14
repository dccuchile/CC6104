
### GLMS
library(rethinking)
data(chimpanzees)
d <- chimpanzees


m10.7glm <- glm( pulled_left~ 1 , data=d , family=binomial )

m10.4glm <- glm(
  pulled_left ~ as.factor(actor) + prosoc_left:condition,
  data=d , family=binomial )

summary(m10.4glm)


m10.1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )
precis(m10.1)

m10.2 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)
  ) ,
  data=d )
precis(m10.2)

m10.3 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ) ,
  data=d )
precis(m10.3)





dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  condition = as.integer(d$condition),
  prosoc_left = as.integer(d$prosoc_left))

# ulam can use lists, an integers must be set as integers.

m10.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  )  , data=dat_list , chains=4 , log_lik=TRUE )

precis(m10.4,depth = 2)




## multi-level chimpanzees



m10.5 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left ,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1)
  ) ,
  data=dat_list , warmup=1000 , iter=5000 , chains=4 , cores=3 )

precis(m10.5,depth = 2)


dat_list$block_id <- d$block

m10.6 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a + a_actor[actor] + a_block[block_id] +
      (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a_block[block_id] ~ dnorm( 0 , sigma_block ),
    c(a,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_block ~ dcauchy(0,1)
  ) ,
  data=dat_list, warmup=1000 , iter=6000 , chains=4 , cores=3 )

precis(m10.6,depth=2) # depth=2 displays varying effects
plot(precis(m10.6,depth=2)) # also plot


#### Multi-level reedfrogs

data(reedfrogs)
d <- reedfrogs
str(d)

d$tank <- 1:nrow(d)
dat <- list(
  S = d$surv,
  N = d$density,
  tank = d$tank )
# approximate posterior
m13.1 <- ulam(
  alist(
    S ~ dbinom( N , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( 0 , 1.5 )
  ), data=dat , chains=4 , log_lik=TRUE )


m13.2 <- ulam(
  alist(
    S ~ dbinom( N , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 )
  ), data=dat , chains=4 , log_lik=TRUE )

