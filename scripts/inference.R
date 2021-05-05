



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





# The bootstrap is useful for creating confidence intervals in cases 
# where we don't have a parametric distribution. 
# One example is for the median; let's look at how that works. 
#We will start by implementing it by hand, to see more closely how it works.  
#We will start by collecting a sample of individuals from the NHANES dataset, 
#and the using the bootstrap to obtain confidence intervals on the median for
#the Height variable.
# source: https://github.com/statsthinking21/statsthinking21-R/blob/master/07-ResamplingAndSimulation.Rmd

library(NHANES)

# Get adults and remove NA
NHANES_adult <- NHANES %>%
  drop_na(Height) %>%
  subset(Age>=18)


nRuns <- 2500
sampleSize <- 64
# take a sample
heightSample <- 
  NHANES_adult %>%
  sample_n(sampleSize)
# create a function to collect a sample with replacement
bootMedianHeight <- function(df) {
  bootSample <- sample_n(df, dim(df)[1], replace = TRUE)
  return(tibble(medianHeight = median(bootSample$Height)))
}
input_df <- tibble(id=seq(nRuns)) %>%
  group_by(id)
bootMeans <- do(input_df, bootMedianHeight(heightSample))
bootCI <- tibble(`Lower CI limit` = quantile(bootMeans$medianHeight, .025),
                 Median = median(heightSample$Height),
                 `Upper CI limit` = quantile(bootMeans$medianHeight, .975)
)

bootCI


