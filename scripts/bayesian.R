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
