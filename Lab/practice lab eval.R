x<-seq(0,2*pi,length.out)
y<-sin(x)
plot(x,y,type='l',col='red',lwd=1)
grid()



factorial<-function(n){
  if(n==0)
   { return(1)
   }else{
    return(n*factorial(n-1))
     }
}
input_number<-as.integer(readline(prompt = "Enter number"))
if(input_number<0){
  print("Enter valid number")
}else{
  factorial_return=factorial(input_number)
    cat("Value is:",factorial_return)
}

n1<-as.integer(readline(prompt="Enter a number:"))
oper<-readline(prompt="Enter a operation")
n2<-as.integer(readline(prompt="Enter second number:"))
n3<-as.f

result<-switch (oper,
  "+" = n1+n2
  "-" =n1-n2
  "*" =n1*n2
  "/" =n1/n2
)
cat("Result is:",result)

value<-c(10,20,30,40)
labels<-c("A","B","C","D")
pie(value,labels)

barplot(value,names.arg = labels)

cond_prob<-function(P_U,P_R_U,P_R){
  result<-(P_U*P_R_U)/(P_R)
  return<-result
}
p_u_1<-1/3
p_u_2<-1/3
p_u_3<-1/3

p_r_1<-8/12
p_r_2<-6/12
p_r_3<-5/12

p_r<-p_u_1*p_r_1+p_u_2*p_r_2+p_u_3*p_r_3

p_urn1_given_red<-cond_prob(p_u_1,p_r_1,p_r)
cat("PROB URN ! GIVEN RED",p_urn1_given_red)
p_urn

x<-c(0,1,2,3,4)
p_x<-c(0.41,0.37,0.16,0.05,0.01)

E_X<-sum(x*p_x)
cat("E(X) is:",E_X)


x<-c(0,1,2,3)
p_x<-c(0.1,0.2,0.2,0.5)

y<- 20*x-24
prob_y<-p_x
E_y<-sum(y*prob_y)
cat("E(Y):",E_y)

x<-c(-3,-1,0,1,2,3,5,8)
cdf_x<-c(0.10,0.30,0.45,0.5,0.75,0.90,0.95,1)

i=0
while(i<8){
  pdf_x<-(cdf_x[i+1]-cdf_x[i])
  i=i+1
  print(pdf_x)
}

for (j in 1:8) {
  if(x[j]%%2==0){
    prob_even=cdf_x[j]-cdf_x[j-1]
    cat("Prob even:",x[j],"is=",prob_even,"\n")
  }
  
}
#prob(1<=x<=8)=F(8)-F(1)

prob_z<-cdf_x[8]-cdf_x[4]t
print(prob_z)

f<-function(t){
  (0.2*t*exp(-0.2*t))
}
E_t<-integrate(f,lower=0,upper=Inf)
print(E_t)

f_x<-function(y){
  (3/4)*(1/4)^(sqrt(y-1))
}
x<-as.integer(readline(prompt = "Enter the value of X:"))
y<-x^2
proby<-f_x(y)
print(proby)


bino_pmf<-function(n,x,p){
  binomial<-(choose(n,x)*(p^x)*(1-p)^(n-x))
}
n<-12
p<-1/6
bino_8<-bino_pmf(n,8,p)
print(bino_8)

n<-31
p<-0.447
x<-0:n
student<-bino_pmf(n,0:n,p)
print(student)
plot(x,student,xlab = 'student',ylab = 'pmf', main = 'PMF distribution')
cdf<-pbinom(x,n,p) #cumulative distribution function
print(cdf)
plot(x,cdf,main='CDF')

n<-2000
p<-0.001
lambda<-n*p
print(dpois(3,lambda))
print(1-ppois(2,lambda))

p<-0.6
x<-seq(0,100,by=2)
print(sum(dgeom(x,p)))

p<-0.05
x<-10
r<-5
print(dnbinom(x,r,p))


a<--2
b<-2
print(punif(0,min=a,max = b))
result<-1-(punif(3/2,min = a,max = b)-punif(1/2,min = a,max = b))
print(result)

a<-0
b<-60
print(1-punif(35,min = a,max = b))

Expo<-function(lambda,x){
  f<-lambda*exp(-lambda*x)
  return(f)
}
print(Expo(1/2,4))

print(dexp(4,1/2))

x<-seq(0,10,by =0.5)
lambda=1/2
pdf<-Expo(lambda,x)
print(pdf)
plot(x,pdf,xlab='X',ylab='pdf',main='PDF')
cdf<-pexp(x,lambda)
print(cdf)
plot(x,cdf,xlab = 'X',ylab = 'Cdf',main='CDF FOR lambda=1/2')


print(300*pnorm(57,64.5,5))
x<-c(0:10)
x<seq(0:10)
print(x)

print(1-pgamma(1,2,1/3))


jpf<-function(x,y){
  f<-(2*x+y)/27
  return(f)
}
x<-c(0:2)
y<-c(0:2)
M1<-matrix(c(jpf(0,0:2),jpf(1,0:2),jpf(2,0:2)),nrow=3,ncol=3,byrow=TRUE)
print(M1)

sum(M1)
px<-apply(M1,1,sum)
print(px)

x<-c(1,2,3,4,5,6,7,8,9)
y<-c(9,8,10,12,11,13,14,16,15)
n<-9
ex<-sum(x)/n
ex
ey<-sum(y)/n
ey
ex2<-sum(x^2)/n
ex2
ey2<-sum(y^2)/n
ey2
exy<-sum(x*y)/n
exy
varx<-ex2-(ex^2)
varx
vary<-ey2-(ey^2)
vary
covxy<-exy-(ex*ey)
covxy
rho<-covxy/sqrt(varx*vary)
cat("Rho:",rho)

rho<-cor(x,y)
rho

n<10000
y<-rexp(n,1)
x<-runif(n,min = 0, max = y)
corxy<-cor(x,y)
print(corxy)

library(pracma)
f<-function(x,y){
  (2/5)*(x+4*y)
}
i<-integral2(f,xmin = 0,xmax = 1,ymin = 0,ymax = 1)
print(i)

fun<-function(x,y){
  f<-(2*x+y)/27
  return(f)
}

x<-c(0:2)
y<-c(0:2)

M<-matrix(c(fun(0,0:2),fun(1,0:2),fun(2,0:2)),nrow = 3,ncol=3,byrow=TRUE)
print(M)
fx<-apply(M,2,sum)
fx

regyonx<-lm(y~x)
cat("Reg lin y on x:Y=",coef(regyonx)[1],"+",coef(regyonx)[2],"*x\n")
