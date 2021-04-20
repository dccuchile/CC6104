



# Show that 




pop <-c(2,3,4,5,6)
mean(pop)
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
sd.p(pop)


library(gtools)
library(tidyverse)
samp_size <- 2
samples<-as_tibble(permutations(length(pop), samp_size,
                                pop, repeats.allowed=TRUE))

samples <- samples %>% rowwise() %>% 
  mutate(sample_mean=mean(c(V1,V2)))
samples

library(ggplot2)
ggplot(samples, aes(x=sample_mean)) + 
  geom_histogram(bins = 9, color="black", fill="white")
ggplot(data.frame(pop), aes(x=pop)) +
  geom_histogram(bins = 5, color="black", fill="white")



mean(samples$sample_mean)
sd.p(samples$sample_mean)


sd.p(pop)/sqrt(samp_size)



prop<-prop.test(1219,3532,correct=FALSE)
prop$conf.int
