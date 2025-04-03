#que 7
#possible sum
sums<-2:12
#prob for each sum 
probs<-c(1,2,3,4,5,6,5,4,3,2,1)/36
#expected value
E_X<-sum(sums*probs)
print(E_X)
#E(x2)
E_X2<-sum(sums^2*probs)
print(E_X2)
#variance calculate
var<-E_X2-(E_X)^2
print(var)

#ques 8 
func<-function(y){
(3/4)*(1/4)^(sqrt(y)-1)
}
x<-as.integer(readline(prompt="Enter value of X : "))
y<-x^2
proby<-func(y)
print(proby)

#to find exp value and variance 
x<-c(1,2,3,4,5)
y<-x^2
proby<-func(y)
print(proby)
#exp Y
E_Y<-sum(y*proby)
#exp Y2
E_Y2<-sum(y^2*proby)
var<-E_Y2-(E_Y)^2
print(var)


##################################################################

#que 1
#X~B(n,p)=B(n=12,p=1/6)
bino_pmf<-function(x,n,p){
	P_X<-choose(n,x)*p^(x)*(1-p)^(n-x)
	return(P_X)
}

#a=8
n<-12
p<-1/6
P_8<-bino_pmf(8,n,p)
print(P_8)
P_8<-dbinom(8,n,p)
print(P_8)

############## part b ######################
n<-12
p<-1/6
P_X_Lesser_than_7<-sum(bino_pmf(0:7,n,p))
print(P_X_Lesser_than_7)

#c part
p_x_greater_6<-1-sum(bino_pmf(0:6,n,p))
print(p_x_greater_6)

p_x_greater_6<-pbinom(6,size=12,prob=1/6,lower.tail=F)
print(p_x_greater_6)

####### part d ######
p<- pbinom(9,size=12,prob=1/6) - pbinom(6,size=12,prob=1/6)
print(p)

p<-sum(bino_pmf(0:9,12,1/6))-sum(bino_pmf(0:6,12,1/6))
print(p)

###### part e #####
p<- pbinom(8,size=12,prob=1/6) - pbinom(6,size=12,prob=1/6)
print(p)


#Ques-2:
n<-31
p<-0.447
x<-0:n
pmf<-dbinom(x,n,p)
print(pmf)
plot(x,pmf,xlab='x-axis',ylab='y-axis',
     main='Pmf for binomial distribution')


cdf<-pbinom(x,n,p)
print(cdf)
plot(x,cdf,xlab='x-axis',ylab='y-axis',
     main='cdf for binomial distribution')

E_X<-sum(x*pmf)
print(E_X)

E_X_2<-sum(x*x*pmf)
print(E_X_2)

V_X<-E_X_2-(E_X)^2
print(V_X)

S_D<-sqrt(V_X)
print(S_D)


#QUES_3:
n<-2000
p<-0.001
lambda<-n*p
x<-3
P_At_X_3<-dpois(x,lambda)
print(P_At_X_3)

#P(X>2)=1-P(X<=2)=1-F(2)
P_Atleast_2<-1-ppois(2,lambda)
print(P_Atleast_2)

#QUes-4:
n<-100000
p<-0.00002
lambda<-n*p
#P(X>=5)=1-P(X<5)
P_Atleast_5<-1-ppois(4,lambda)
print(P_Atleast_5)

#Ques-5:
lambda<-5
x<-0
P_X_0<-dpois(x,lambda)
print(P_X_0)

lambda<-50
#P(48<X<50)=F(49)-F(48)
P_48_X_50<-ppois(49,lambda)-ppois(48,lambda)
print(P_48_X_50)

#Ques-6:
p<-0.6
x<-seq(0,100,by=2)
P_X_0<-sum(dgeom(x,p))
print(P_X_0)

#Ques-7:
p<-0.05
x<-10
r<-5
P_X_5th_hit<-dnbinom(x,r,p)
print(P_X_5th_hit)


