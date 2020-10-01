# Introducción a R
# Felipe Bravo-Marquez


# R puede ser usado como una calculadora


4*5
2^3
exp(-5)
log(4)

# Declarando variables
a<-1
b=3
assign("tres",3)
d<-a+b
ver<-T # equivalente a TRUE
pal<-"hola"
normales<-rnorm(n=100)

# Declarando funciones
suma<-function(a=2,b=1){
  return(a+b);
}

suma(3,4)
suma()
class(suma)

#Recursividad
fac<-function(n){
  ifelse(n==1,return(1),return(n*fac(n-1)))    
}

#tipos
class(a) 
class(ver)
class(pal)
class(suma)

#Ayuda
help(ls)
?ls
#Para un comando
help("for")

#Para ver mis variables de mi workspace y borrar
objects()
ls()
rm(a)
rm(list=ls())

# Para dejar grabado el workspace en un archivo
save.image("~/la.RData")
#Luego lo cargamos
load("~/la.RData")



# Vectores tipo básico de dato en R
c(1,3,4)
edades<-c(21,33,12,34,23,70,90,80,7,29,14,2,
          88,11,55,24,13,11,56,28,33)
suma<-sum(edades)
largo<-length(edades)
numeros<-c(1,2,3)
numeros+3
numeros*5
numeros^2

#Si opero un vector a un escalar este se recicla
media<-sum(edades)/length(edades)
varianza<-sum((edades-media)^2)/(length(edades)-1)
var(edades)
mean(edades)



# Reciclaje
# si son del mismo largo es uno a uno
a<-c(1,2)
b<-c(3,4)
a+b
a*b
#Si hay uno de largo 1 se aplica a todos
unidad<-1
a+unidad
# Si uno es más largo que otro se recicla
d<-c(4,5,6,9)
a+d
#ojo que el largo del menor debe ser múltiple del largo del mayor
#c(1,2)+c(-9,2,3) # no funciona


#Los elementos de un vector pueden tener nombres
notas<-c(Juan=4.5,Luis=6.2,Romina=3.9,Felipe=2.8,Mariana=6.7)
names(notas)
# El ranking de los alumnos
names(sort(x=notas,decreasing=T))

#acceso a elementos
notas[1] #primer elemento
notas[-2] # Todos menos el segundo
notas[c(1,5)] # primer y quinto elemento
notas[c("Juan","Mariana")] # Sólo Juan y Mariana

# Si concateno vectores de disinto tipo R los pasa al mismo
c("hola",2,T)
c(TRUE,FALSE,500)

#Ojo con los missing values (cuando leamos datos de archivos)
missing_vector<-c(12,15,NA)
missing_vector
missing_vector[!is.na(missing_vector)]

# Secuencias
pares<-seq(from=2,to=20,by=2)
# se simplifica como
unoadiez<-1:10
# 100 multiplos de 4
cuatro_mult<-seq(from=4,by=4,length=100)

#Repeticiones
#repetir 10 15 veces
rep(10,3)
rep(c("hola","chao"),4)


# Vectores Aleatorios
normales<-rnorm(n=5, mean = 10, sd = 4)
uniformes<-runif(n=10, min = 1, max = 10)
poisson<-rpois(n=10, lambda = 3)
binom<-rbinom(n=10,size=2,prob=0.5)



# Gente menor de edad, creo un vector logico 
# R soporta los operadores logicos >,<, ==, <=, >=, != además de & | para and y or
menores<-edades<18
menores
edades[menores]

#promedio de edad de los mayores
mean(edades[edades>=18])


# Variables factor
factor(c("Hombre","Mujer","Mujer","Mujer","Hombre"))
gente<-factor(c("Hombre","Mujer","Mujer","Mujer","Hombre"))
gente
class(gente)
levels(gente)
levels(gente)<-c("Man","Woman")
gente

#Si queremos trabajar con variables categóricas R tiene un tipo llamado factor
#menores y mayores de edad
categ_edades<-ifelse(edades<12,"niño",
                     ifelse(edades<18,"adolescente","adulto"))
class(categ_edades)
#Convierto a factor con as.factor
categ_edades<-as.factor(categ_edades)
levels(categ_edades)


#Quiero saber el tamaña, media y la desviación de cada categoria
#tapply recibe un vector además de otro con algún factor y una función
#aplica la función a cada subconjunto
tapply(edades,categ_edades,length)
tapply(edades,categ_edades,mean)
tapply(edades,categ_edades,sd)




#Carácteres
saludo<-"Hola Mundo"
cat(saludo)
letters

paste("Hola","Chao",sep="-")
paste("persona",1:4, sep="")
paste(saludo,1:3, sep=" ")

substr(saludo,1,4)
#Matrices y arreglos
#se llenan por defecto por columna
matriz_por_col<-matrix(data=1:12,nrow=3,ncol=4)


matriz_por_fil<-matrix(data=1:12,nrow=4,ncol=3,byrow=T)
dim(matriz_por_fil)


#Accediendo a los elementos 
matriz_por_fil[2,] #Segunda fila, todas las columnas
matriz_por_fil[2,1] # Segunda fila, primera columna
matriz_por_fil[-1,-2] # Todo menos la primera fila y la segunda columna

#Para asignarle nombres a las filas
rownames(matriz_por_fil)<-paste("r",1:4,sep="")
colnames(matriz_por_fil)<-paste("c",1:3,sep="")
matriz_por_fil["r2","c3"]

# concatenado
rbind(matriz_por_fil,r5=1:3)
cbind(matriz_por_fil,c4=4:1)


#Multiplicacion de matrices
a<-matriz_por_col %*% matriz_por_fil
t(a)
eigen(a)




#Arreglos son matrices de más dimensiones (también conocidos como tensores)
arreglo<-array(1:8, dim=c(2,2,2))
arreglo[1,2,1]

#Listas y Data Frames
milista<-list(hombre="Pepe",mujer="Juana",
              hijos=3,edades=c(4,8,12))
milista[c(3,4)] # Sublista
milista[[1]]
milista[["hombre"]]
milista$hombre


vectores<-list(normal=rnorm(n=100,mean=10,sd=5),
               poisson=rpois(n=100,lambda=10),
               uniforme=runif(n=100,min=5,max=15))

# Como saco la media se cada grupo
#forma mala
medias<-vector()
desv<-vector()
for(i in 1:length(vectores)){
  medias[i]<-mean(vectores[[i]])
  desv[i]<-sd(vectores[[i]])
}
medias
desv
# Forma inteligente
lapply(vectores,mean)
lapply(vectores,sd)
sapply(vectores,mean)
sapply(vectores,sd)

# En R puedo pasar funciones como parámetros y podría hacer mi propio lapply
myapply<-function(lista,fun,...){
  resultado<-vector(length=length(lista))
  for(i in 1:length(lista)){
    resultado[i]<-fun(lista[[i]],...)
  }
  resultado  
}

# Los data.frame soportan vectores de distinto tipo pero de distinos elementos
edades.frame<-data.frame(edad=edades,categoria=categ_edades)
names(edades.frame)
edades.frame[3,1] # La edad del tercer elemento
edades.frame$edad[1:6] # La edad de los primeros 6 elementos
attach(edades.frame)
edad[1:3]
#Guardando 
write.table(x=edades.frame,file="edades.csv",sep=",",row.names=F)

# Los data.frames se pueden leer desde archivos csv
iris<-read.table(file="iris.csv",header=T,sep="\t")
iris$Sepal.Length
attach(iris)

data()
data(package = .packages(all.available = TRUE))
data(USArrests)

# Muestreo 
sample(edades,size=4,replace=F)

sample(edades,size=100,replace=T)

# Muestreo de un data.frame
USArrests[sample(1:(dim(USArrests)[1]),size=3,replace=F),]



#Datos provistos por las librerias

library(gdata)

data(package="rpart")
plot(USArrests)
cor(USArrests)

#Instalar librerías
install.packages("rpart",dependencies=T)

