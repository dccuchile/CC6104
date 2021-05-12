

# Babies
library(UsingR)
data(babyboom)
hist(babyboom$wt)



xbar<-mean(babyboom$wt)
mu0<-3000
sd<-500
n<-nrow(babyboom)
se<-sd/sqrt(n)
se
se^2


#pvalue 
1-pnorm(xbar, mean =mu0, sd =se)
#or
Z.score<-(xbar-mu0)/se
Z.score
p.value<-1-pnorm(Z.score)
p.value

alpha<-0.05
p.value<=alpha

# t-test
s<-sd(babyboom$wt)
s
se<-s/sqrt(n)
se

T.sta<-(xbar-mu0)/se
T.sta
p.value<-1-pt(T.sta,df = n-1)
p.value
# or
t.test(x = babyboom$wt,mu = 3000, alternative = "greater",conf.level = 1-alpha)



#critical region
qnorm(1-alpha)


# Rejection region
qt(0.025,99)
qt(0.975,99)


# Confidence Interval
28-qt(p=0.975,99)*10/sqrt(100)
28+qt(p=0.975,99)*10/sqrt(100)




# P-value by hand Two-sided
data(iris)
mu<-3 # null hypothesis
alpha<-0.05
n<-length(iris$Petal.Length)
xbar<-mean(iris$Petal.Length)
s<-sd(iris$Petal.Length)
se<-s/sqrt(n)
tstat<-(xbar-mu)/(s/sqrt(n))
pvalue<-2*pt(-abs(tstat),df=n-1)
pvalue

# This is the same as 
pt(-tstat,df=n-1)+(1-pt(tstat,df=n-1))


# Running the test directly
t.test(x=iris$Petal.Length,mu=3)


# One sided, alternative greater
t.test(x=iris$Petal.Length,mu=3,alternative = "greater")

