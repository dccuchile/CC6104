sppnames <- c( "afarensis","africanus","habilis",
               "boisei", "rudolfensis","ergaster",
               "sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 
                 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 
             61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc,
                 mass=masskg )


m6.1 <- lm( brain ~ mass , data=d )
1 - var(resid(m6.1))/var(d$brain)

m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )
m6.3 <- lm( brain ~ mass + I(mass^2)+ I(mass^3) + I(mass^4),data=d )
