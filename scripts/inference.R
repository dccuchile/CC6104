



# Show that 

library(gtools)
library(tidyverse)
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}

pop <-c(2,3,4,5,6)
samp_size <- 2

mean(pop)
sd.p(pop)

samples<-as_tibble(permutations(length(pop), samp_size, pop, repeats.allowed=TRUE))

samples <- samples %>% rowwise() %>% mutate(sample_mean=mean(c(V1,V2)))



mean(samples$sample_mean)
sd.p(samples$sample_mean)
hist(samples$sample_mean)

sd.p(pop)/sqrt(samp_size)
