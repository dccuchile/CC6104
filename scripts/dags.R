library(dagitty)

my_dag<-dagitty('dag {
  bb="0,0,1,1"
  v1 [pos="0.116,0.159"]
  v2 [pos="0.653,0.166"]
  v3 [pos="0.292,0.354"]
  v4 [pos="0.569,0.375"]
  v1 -> v3
  v1 -> v4
  v2 -> v3
  v3 -> v4
}
')
plot(my_dag)
impliedConditionalIndependencies(my_dag)

over <- dagitty("dag{ o -> h; s -> h; s ->c }")
coordinates(over) <- list( x=c(o=0,h=1,s=2,c=3) , y=c(o=0,h=1,s=0,c=1) )
plot(over)

library(ggdag)
tidy_dagitty(over)
ggdag(over, layout="circle")


children(over,"s")
parents( over, "h" )
paths( over, "o", "c" )$path

paths( over, "s", "c",directed = T )$path

impliedConditionalIndependencies(over)
dconnected(over,"o","s",c())
dconnected(over,"o","s",c("h"))
dseparated(over,"o","s",c("h"))


impliedConditionalIndependencies(over)

fo <- dagitty("dag{ fo -> lo; fo -> do; bp -> do;do->hb }")
coordinates(fo) <- list( x=c(lo=0,fo=1,do=2,hb=2,bp=3)
                         , y=c(fo=0,bp=0,lo=1,do=1,hb=2) )
plot(fo)




#Rule 1 d-connected    
dconnected(fo,"fo","hb",c()) 
dseparated(fo,"fo","hb",c()) 
dconnected(fo,"fo","hb",c("do")) 
dseparated(fo,"fo","hb",c("do")) 



dconnected(fo,"lo","do",c()) 
dconnected(fo,"lo","do",c("fo")) 
    
dconnected(fo,"fo","bp",c())
dconnected(fo,"fo","bp",c("do"))
dconnected(fo,"fo","bp",c("hb"))


impliedConditionalIndependencies(fo)



