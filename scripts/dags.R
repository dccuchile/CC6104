library(dagitty)
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
plot( dag5.1 )


DMA_dag2 <- dagitty('dag{ D <- A -> M }')
plot(DMA_dag2)
impliedConditionalIndependencies( DMA_dag2 )

over <- dagitty("dag{ o -> h; s -> h; s ->c }")
coordinates(over) <- list( x=c(o=0,h=1,s=2,c=3) , y=c(o=0,h=1,s=0,c=1) )
plot(over)
parents( over, "h" )
paths( over, "o", "c" )$path
paths( over, "o", "c" , directed = T)$path


dconnected( "dag{x->m->y}", "x", "y", c() ) # TRUE
dconnected( "dag{x->m->y}", "x", "y", c("m") ) # FALSE
dseparated( "dag{x->m->y}", "x", "y", c() ) # FALSE 
dseparated( "dag{x->m->y}", "x", "y", c("m") ) # TRUE

impliedConditionalIndependencies(over)

fo <- dagitty("dag{ fo -> lo; fo -> do; bp -> do;do->hb }")
coordinates(fo) <- list( x=c(lo=0,fo=1,do=2,hb=2,bp=3) , y=c(fo=0,bp=0,lo=1,do=1,hb=2) )

    plot(fo)
impliedConditionalIndependencies(fo)

library(ggdag)
tidy_dagitty(fo)
ggdag(fo, layout = "circle")

