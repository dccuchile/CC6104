# Introduction to R
# Felipe Bravo-Marquez


# R can be used as a calculator


4*5
2^3
exp(-5)
log(4)

# Declaring Variables
a<-1
b=3
assign("three",3)
d<-a+b
ver<-T # equivalent to TRUE
word<-"hello"
normals<-rnorm(n=100)

# Functions
my.sum<-function(a=2,b=1){
  return(a+b);
}

my.sum(3,4)
my.sum()
class(my.sum)

#Recursion
fac<-function(n){
  ifelse(n==1,return(1),return(n*fac(n-1)))    
}

#types
class(a) 
class(ver)
class(word)
class(my.sum)

#help
help(ls)
?ls
#for a particular command
help("for")

# View my workspace variables and delete them
objects()
ls()
rm(a)
rm(list=ls())

# We can save all my workspace variables in a file
save.image("~/myworkspace.RData")
# we can load it in a new session
load("~/myworkspace.RData")



# Vectors
c(1,3,4)
ages<-c(21,33,12,34,23,70,90,80,7,29,14,2,
          88,11,55,24,13,11,56,28,33)
a.sum<-sum(ages)
a.length<-length(ages)
a.sum
a.length

numbers<-c(1,2,3)
numbers+3
numbers*5
numbers^2


a.mean<-sum(ages)/length(ages)
a.mean
a.var<-sum((ages-a.mean)^2)/(length(ages)-1)
a.var
var(ages)
mean(ages)



# Recycling
# If both vectors are of the same length 
# and we operate on them, the operation is done element by element
a<-c(1,2)
b<-c(3,4)
a+b
a*b
#Si hay uno de largo 1 se aplica a todos
unidad<-1
a+unidad
# If the vectors are of different lengths, the smaller one recycles its elements:
d<-c(4,5,6,9)
a+d
#ojo que el largo del menor debe ser múltiple del largo del mayor
#c(1,2)+c(-9,2,3) # no funciona


#Los elementos de un vector pueden tener nombres
grades<-c(Juan=4.5,Luis=6.2,Romina=3.9,Felipe=2.8,Mariana=6.7)
names(grades)
# El ranking de los alumnos
names(sort(x=grades,decreasing=T))

#acceso a elementos
grades[1] #primer elemento
grades[-2] # Todos menos el segundo
grades[c(1,5)] # primer y quinto elemento
grades[c("Juan","Mariana")] # Sólo Juan y Mariana

# Si concateno vectores de disinto tipo R los pasa al mismo
c("hola",2,T)
c(TRUE,FALSE,500)

#Ojo con los missing values (cuando leamos datos de archivos)
missing_vector<-c(12,15,NA)
missing_vector
missing_vector[!is.na(missing_vector)]

# Secuencias
my_pairs<-seq(from=2,to=20,by=2)
# se simplifica como
unoadiez<-1:10
# 100 multiplos de 4
mult_fourt<-seq(from=4,by=4,length=100)

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
younger<-ages<18
younger
ages[younger]

#promedio de edad de los mayores
mean(ages[ages>=18])


# Variables factor
factor(c("Hombre","Mujer","Mujer","Mujer","Hombre"))
people<-factor(c("Hombre","Mujer","Mujer","Mujer","Hombre"))
people
class(people)
levels(people)
levels(people)<-c("Man","Woman")
people

#Si queremos trabajar con variables categóricas R tiene un tipo llamado factor
#younger y mayores de edad
categ_ages<-ifelse(ages<12,"child",
                     ifelse(ages<18,"adolescent","adult"))
class(categ_ages)
#Convierto a factor con as.factor
categ_ages<-as.factor(categ_ages)
levels(categ_ages)


#Quiero saber el tamaño, media y la desviación de cada categoria
#tapply recibe un vector además de otro con algún factor y una función
#aplica la función a cada subconjunto
tapply(ages,categ_ages,length)
tapply(ages,categ_ages,mean)
tapply(ages,categ_ages,sd)




#Carácteres
greeting<-"Hello World"
cat(greeting)
letters

paste("Hello","Bye",sep="-")
paste("person",1:4, sep="")
paste(greeting,1:3, sep=" ")

substr(greeting,1,4)
#Matrices y arreglos
#se llenan por defecto por columna
matrix_by_col<-matrix(data=1:12,nrow=3,ncol=4)


matrix_per_row<-matrix(data=1:12,nrow=4,ncol=3,byrow=T)
dim(matrix_per_row)


#Accediendo a los elementos 
matrix_per_row[2,] #Segunda fila, todas las columnas
matrix_per_row[2,1] # Segunda fila, primera columna
matrix_per_row[-1,-2] # Todo menos la primera fila y la segunda columna

#Para asignarle nombres a las filas
rownames(matrix_per_row)<-paste("r",1:4,sep="")
colnames(matrix_per_row)<-paste("c",1:3,sep="")
matrix_per_row["r2","c3"]

# concatenado
rbind(matrix_per_row,r5=1:3)
cbind(matrix_per_row,c4=4:1)


#Multiplicacion de matrices
a<-matrix_by_col %*% matrix_per_row
t(a)
eigen(a)




#Arreglos son matrices de más dimensiones (también conocidos como tensores)
my_array<-array(1:8, dim=c(2,2,2))
my_array[1,2,1]

#Listas y Data Frames
my_list<-list(man="Pepe",woman="Juana",
              children=3,ages=c(4,8,12))
my_list[c(3,4)] # sublist
my_list[[1]]
my_list[["man"]]
my_list$man


vectors<-list(normal=rnorm(n=100,mean=10,sd=5),
              poisson=rpois(n=100,lambda=10),
              uniform=runif(n=100,min=5,max=15))

# Como saco la media se cada grupo
#forma mala
means<-vector()
desv<-vector()
for(i in 1:length(vectors)){
  means[i]<-mean(vectors[[i]])
  desv[i]<-sd(vectors[[i]])
}
means
desv
# Forma inteligente
lapply(vectors,mean)
lapply(vectors,sd)
sapply(vectors,mean)
sapply(vectors,sd)

# En R puedo pasar funciones como parámetros y podría hacer mi propio lapply
my_apply<-function(a_list,fun,...){
  result<-vector(length=length(a_list))
  for(i in 1:length(a_list)){
    result[i]<-fun(a_list[[i]],...)
  }
  result 
}

my_apply(vectors,mean)

# Los data.frame soportan vectores de distinto tipo pero de distinos elementos
ages.frame<-data.frame(age=ages,categ=categ_ages)
names(ages.frame)
ages.frame[3,1] # La edad del tercer elemento
ages.frame$age[1:6] # La edad de los primeros 6 elementos
attach(ages.frame)
age[1:3]
#Guardando 
write.table(x=ages.frame,file="ages.csv",sep=",",row.names=F)

# Los data.frames se pueden leer desde archivos csv
iris<-read.table(file="iris.csv",header=T,sep="\t")
iris$Sepal.Length
attach(iris)

data()
data(package = .packages(all.available = TRUE))
data(USArrests)

# Muestreo 
sample(ages,size=4,replace=F)

sample(ages,size=100,replace=T)

# Muestreo de un data.frame
USArrests[sample(1:(dim(USArrests)[1]),size=3,replace=F),]



#Datos provistos por las librerias

library(gdata)

data(package="rpart")
plot(USArrests)
cor(USArrests)

#Instalar librerías
install.packages("rpart",dependencies=T)
install.packages("NHANES")
install.packages("tidyverse")

#library(tidyverse)
library(NHANES)

library(tibble)
library(dplyr)

# first create the individual variables
#library(tidyverse)
ages.tibble<-as_tibble(ages.frame)
print(ages.tibble)

ages.tibble %>% filter(age < 20) 


weights<-c(60,80,31,70,71,101,59,67,11,78,55,11,
        90,31,65,78,39,35,69,115,63)

ages.tibble <-ages.tibble %>% bind_cols("weights"=weights)

ages.tibble %>% filter(age > 20 & weights > 80)


ages.tibble <- ages.tibble %>%
  # create a new variable called future.age with the age in 10 years
  mutate(future.age = age + 10) 

#modify a variable
mod.ages <- ages.tibble %>% mutate(age=1:21)

# select specififc columns
ages.tibble %>% select(c(weights,categ))

