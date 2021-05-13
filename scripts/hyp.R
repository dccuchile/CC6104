

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
se.t<-s/sqrt(n)
se.t

T.sta<-(xbar-mu0)/se.t
T.sta
p.value<-1-pt(T.sta,df = n-1)
p.value
# or
t.test(x = babyboom$wt,mu = 3000, alternative = "greater",conf.level = 1-alpha)


#critical region
# with a gaussian
crit<-qnorm(1-alpha)
crit
Z.score>=crit

# with a T
crit2<-qt(1-alpha, df = n-1)
crit2
T.sta>=crit2

# Two sided critical region

crit.left<-qnorm(alpha/2)
crit.left
crit.right<-qnorm(1-alpha/2)
crit.right

Z.score<=crit.left | Z.score >=  crit.right 

# Now using a T-distribution
crit2.left<-qt(alpha/2,df = n-1)
crit2.left
crit2.right<-qt(1-alpha/2,df = n-1)
crit2.right

T.sta<=crit2.left |T.sta >=  crit2.right

# This is equivalent as calculating a confidence interval
# for the sample mean and check if the null hypothesis
# is contained in the interval
left.conf<-xbar-qt(p=1-alpha/2,n-1)*se.t
left.conf
right.conf<-xbar+qt(p=1-alpha/2,n-1)*se.t
right.conf

mu0 >= left.conf | mu0 <= right.conf


# P-value by hand Two-sided
pvalue<-pt(-T.sta,df=n-1)+(1-pt(T.sta,df=n-1))
pvalue
# or more compactly
2*pt(-abs(T.sta),df=n-1)


# Running the test directly
t.test(x=babyboom$wt,mu=3000, alternative="two.sided"
       ,conf.level = 1-alpha)








# Welsh Test
t.test(babyboom$wt~babyboom$gender)

t.test(babyboom$wt~babyboom$gender,var.equal=T)



