
### GLMS
library(rethinking)
data(chimpanzees)
d <- chimpanzees


glm.f1 <- glm( pulled_left~ 1 , data=d , family=binomial )
summary(glm.f1)

glm.b1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )
precis(glm.b1)

glm.f2 <- glm( pulled_left~as.factor(prosoc_left) , data=d , family=binomial )
summary(glm.f2)


glm.b2 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)
  ) ,
  data=d )
precis(glm.b2)


glm.f3 <- glm( pulled_left~prosoc_left+prosoc_left:condition , data=d , family=binomial )
summary(glm.f3)

glm.b3 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ) ,
  data=d )
precis(glm.b3)



dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  condition = as.integer(d$condition),
  prosoc_left = as.integer(d$prosoc_left))

# ulam can use lists, an integers must be set as integers.

glm.b4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  )  , data=dat_list , chains=4 , log_lik=TRUE )

precis(glm.b4,depth = 2)




## multi-level chimpanzees adding varying intercepts on actor.

ml.1 <- ulam(
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

precis(ml.1,depth = 2)


dat_list$block_id <- d$block

ml.2 <- ulam(
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

precis(ml.2,depth=2) # depth=2 displays varying effects
plot(precis(ml.2,depth=2)) # also plot


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

