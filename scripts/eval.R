sppnames <- c( "afarensis","africanus","habilis",
               "boisei", "rudolfensis","ergaster",
               "sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 
                 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 
             61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc,
                 mass=masskg )


reg.ev.1 <- lm( brain ~ mass , data=d )
reg.ev.2 <- lm( brain ~ mass + I(mass^2)
                , data=d )
reg.ev.3 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3),data=d )
reg.ev.4 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4),data=d )
reg.ev.5 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4)
                + I(mass^5),data=d )
reg.ev.6 <- lm( brain ~ mass + I(mass^2)
                + I(mass^3) + I(mass^4)+ 
                  I(mass^5)+ I(mass^6),data=d )

summary(reg.ev.1)$r.squared
summary(reg.ev.2)$r.squared
summary(reg.ev.3)$r.squared
summary(reg.ev.4)$r.squared
summary(reg.ev.5)$r.squared
summary(reg.ev.6)$r.squared

reg.ev.0 <- lm( brain ~ 1 , data=d )
summary(reg.ev.0)$r.squared

f <- c( 0.3 , 0.7 )
-sum( f*log2(f) )

f <- c( 0.01 , 0.99 )
-sum( f*log2(f) )

f<-c(0.3,0.7)
q<-c(0.25,0.75)
sum(f* log2(f/ q)) 

q<-f
sum(f* log2(f/ q)) 

library(tidyverse)

t <- 
  tibble(f_1  = .3,
         f_2  = .7,
         q_1  = seq(from = .01, to = .99, by = .01)) %>% 
  mutate(q_2  = 1 - q_1) %>%
  mutate(d_kl = (f_1 * log2(f_1 / q_1)) + (f_2 * log2(f_2 / q_2)))

t %>% 
  ggplot(aes(x = q_1, y = d_kl)) +
  geom_vline(xintercept = .3,  linetype = 2) +
  geom_line(size = 1.5) +
  annotate(geom = "text", x = .4, y = 1.5, label = "q = f", 
           size = 3.5) +
  labs(x = "q(1)",
       y = "Divergence of q from f") 


logLik(reg.ev.1)
logLik(reg.ev.2)

-2*logLik(reg.ev.1)
-2*logLik(reg.ev.2)
-2*logLik(reg.ev.3)
-2*logLik(reg.ev.4)

library(MASS)
rid.ev.4<- lm.ridge(brain ~ mass + I(mass^2)
         + I(mass^3) + I(mass^4),data=d,lambda = 0.1) 
rid.ev.4

