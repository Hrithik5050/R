#Ques-1:
a<--2
b<-2
print(punif(0,min=a,max=b))
prob<-1-(punif(3/2,min=a,max=b)-punif(1/2,min=a,max=b))
print(prob)

#Ques-2:
#P(X>35)=1-P(X<=35)=1-F(35)
prob1<-1-punif(35,min=0,max=60)
print(prob1)
#P(10<X<25)=F(25)-F(10)
prob2<-punif(25,min=0,max=60)-punif(10,min=0,max=60)
print(prob2)

#Ques-3 X~exp(lambda=1/2):
Exp_pdf<-function(x,lambda){
  f_x<-lambda*exp(-lambda*x)
  return(f_x)
}
a<-as.integer(readline(prompt='Enter the value of a:'))
p_x_4<-Exp_pdf(a,1/2)
print(p_x_4)

#OR

P_X_4<-dexp(4,rate=1/2)
print(P_X_4)

#Ques-3(b) plot pdf
x<-seq(0,10,by=0.05) #by is step size
f_x<-Exp_pdf(x,1/2)
#print(f_x)
plot(x,f_x,xlab='x-axis',ylab='y-axis',
     main='Pdf for Exp dist for lambda=1/2')


#OR Ques-3(b):
x<-seq(0,10,by=0.05)
f_x<-dexp(x,rate=1/2)
plot(x,f_x,xlab="x-axis",ylab="y-axis"
     ,main="Pdf for Exp dist for lambda=1/2")


#Ques-3(c) P atmost 4:
P_atmost_x_4<-pexp(4,rate=1/2)
print(P_atmost_x_4)

#Ques-3(d)
x<-seq(0,10,by=0.05)
F_X<-pexp(x,rate=1/2)
plot(x,F_X,xlab="X-axis",ylab="Y-axis",
     main="cdf for exponential distribution for lambda=1/2")

#Ques-3(e)
n<-1000
x_sim<-rexp(n,rate=1/2)#rexp for random numbers generate krne ke leye
plot(density(x_sim),xlab="simulated x",ylab="density",
     main="simulated data")
hist(x_sim,probability=TRUE,xlab="simulated x",ylab="density",
     main="simulated data")


#Ques-4:
#P(X>3)=1-P(X<=3)=1-F(3)
prob1<-1-pexp(3,rate=2)
print(prob1)

prob2<-1-pexp(1,rate=2)
print(prob2)


#Ques-5:X~N(mean=64.5, sd=5)
prob1<-300*(pnorm(57,mean=64.5,sd=5))
print(prob1)

prob2<-300*(pnorm(72,mean=64.5,sd=5)-pnorm(57,mean=64.5,sd=5))
print(prob2)

#Ques-6: X~gamma(alpha=2, beta=1/3)
#P(X>=1)=1-P(X<1)=1-P(X<=1)=1-F(1)
P_i<-1-pgamma(1, shape=2,scale=1/3)
print(P_i)

#P(X<=c)>=0.70 find c
P_ii<-qgamma(0.70,shape=2,scale=1/3) #qgamma is used for quantile
print(P_ii)

#Ques-7:
#given probabilities
q1<-30 #10th percentile
p1<-0.10  #probability corresponding to q1
q2<-62  #97th percentile
p2<-0.97  #probability corresponding to q2
#use qnorm to find z value corresponding to p1 and p2
z1<-qnorm(p1)  #z value for 10th percentile
z2<-qnorm(p2)  #z value for 97th percentile
#z1<-q1-mu/sigma & z2<-q2-mu/sigma 
#q1<=mu+Sigma * z1 and q2<=mu+sigma * z2
#After solving we get:
sigma<-(q2-q1)/(z2-z1)
mu<-q1-sigma*z1
cat("Mean (mu) ",mu,"\n")
cat("stadard deviation (sigma) ",sigma,"\n")
