# MCMC

num_days <- 1e5
positions <- rep(0,num_days)
current <- 10
for ( i in 1:num_days ) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  # move?
  prob_move <- min(proposal/current,1)
  decision <- rbinom(1,1,prob_move)
  current <- ifelse( decision == 1 , proposal , current )
}

library(rethinking)
simplehist(positions,xlab="island",ylab="number of days")




library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
#remove rows with missing values
dd <- d[ complete.cases(d$rgdppc_2000) , ]
# discard columns we are not going to use
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
dd.trim$cont_africa<-as.factor(dd.trim$cont_africa)
summary(dd.trim)


cor(dd.trim$rugged,dd.trim$log_gdp)

dd.A<-dd.trim[dd.trim$cont_africa==1,]
cor(dd.A$rugged,dd.A$log_gdp)

dd.NA<-dd.trim[dd.trim$cont_africa==0,]
cor(dd.NA$rugged,dd.NA$log_gdp)


model<-alist(
  log_gdp ~ dnorm( mu , sigma ) ,
  mu <- b0 + b1*rugged + b2*cont_africa + b3*rugged*cont_africa ,
  b0 ~ dnorm(0,100),
  b1 ~ dnorm(0,10),
  b2 ~ dnorm(0,10),
  b3 ~ dnorm(0,10),
  sigma ~ dcauchy(0,2)
)

quap(model,data=dd.trim)

m.reg1 <- ulam(model ,data=dd.trim )

precis(m.reg1)

pairs( m.reg1)

show(m.reg1 )

stancode(m.reg1) 

traceplot( m.reg1 )



