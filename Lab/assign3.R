#Ques-1
#Define the values of X and their corresponding prob
X<-c(0,1,2,3,4)
P_X<-c(0.41,0.37,0.16,0.05,0.01)
#Calculate the expected value of E(X)
E_X<-sum(X*P_X)
cat("E(X):",E_X,"/n")
#OR
Exp_Val<-weighted.mean(X,P_X)
print(Exp_Val)
#OR
Exp_Val<-c(X %*% P_X)  # %*% can be used for calculating sum of 
# element wise product between two vectors
print(Exp_Val)

#Calculate E(X^2)
E_X2<-sum((X^2)*P_X)
cat("E(X^2): ",E_X2,"\n")

#Calculte E(2X + 3)
E_2X_plus_3<-2*E_X+3
cat("E(2X+3): ",E_2X_plus_3,"\n")

#Calculate the variance Var(X)
Var_X<-E_X2-E_X^2
cat("Var(X):",Var_X,"\n")

#Calculate the variance Var(2X - 3)
Var_2X_minus_3<-4*Var_X
cat("Var(2X - 3):",Var_2X_minus_3,"\n")

#Ques-2:
#Revenue Y=h(x)= 24X+4(3-X)-36 = 20X-24
x<-c(0,1,2,3)
probx<-c(0.1,0.2,0.2,0.5)
y<-20*x-24
proby<-probx #E(phi(x))=sum phi(x)  P(X=x)=sum(10X-12) P(X=x)

E_Y<-sum(y*proby)
cat("E(Y): ",E_Y,"\n")

#Calculate the expected value of Y^2
E_Y2<-sum((y^2)*proby)
cat("E_Y^2: ",E_Y2,"\n")

#Calculate the variance of Y
Var_Y<-E_Y2-E_Y^2
cat("Var(Y): ",Var_Y,"\n")

#QUES-3:
x<-c(-3,-1,0,1,2,3,5,8)
cdf_x<-c(0.10,0.30,0.45,0.5,0.75,0.90,0.95,1.0)
i=0
while(i<8){
 pdf_x<-c(cdf_x[i+1]-cdf_x[i]) 
 i=i+1
 print(pdf_x)
}
for(j in 1:8){
if(x[j]%%2==0){
prob=cdf_x[j]-cdf_x[j-1]
cat("P( X =",x[j],")","=",prob,"\n")
}
}

#QUES-4:
#E(t)
f1<-function(t){0.2*t*exp(-0.2*t)
  }
Exp_t<-integrate(f1,lower=0,upper=Inf)$value
print(Exp_t)  
#OR:
f_T<-function(t){
  ifelse(t>0,0.2*t*exp(-0.2*t),0)
}
Exp_t_f_T<-integrate(function(t) t*f_T(t),lower=0,upper=Inf)$value
print(Exp_t_f_T)

#E(2t-3)
Exp_2t_3<-2*Exp_t-3
print(Exp_2t_3)  

#E(t^2)
f2<-function(t)((0.2*t*t)*exp(-0.2*t))
Exp_t_2<-integrate(f2,lower=0,upper=Inf)$value
print(Exp_t_2)

#var(t)
Var_T<-Exp_t_2-Exp_t^2
print(Var_T)

#var(2t-3)
Var_2T_3<-4*(Exp_t_2-Exp_t^2)
print(Var_2T_3)

#Ques-5:
#Mean(x)
f1<-function(x)(3*x*exp(-3*x))
Exp_t<-integrate(f1,0,Inf)$value
print(Exp_t) 
#Mean(2x+3)
Exp_2x_plus_3<-2*Exp_t+3
print(Exp_2x_plus_3)
#Second moment of X
f2<-function(x)(3*x*x*exp(-3*x))
Exp_t_2<-integrate(f2,0,Inf)$value
print(Exp_t_2) 
#VAR(X)
Var_x<-Exp_t_2-Exp_t^2
print(Var_x)
#VAR(2x+3)
Var_2x_plus_3<-4*Var_x
print(Var_x)


#Ques-6:
f1<-function(x){0.5*exp(mode(x))}
Exp_t1<-integrate(f1,1,20)$value
print(Exp_t1)

#ques 7
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

