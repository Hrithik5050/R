#Ques-1:
#jpmf matrix
#f(x,y)=(2x+y)/27  x=0,1,2 and y=0,1,2
f<-function(x,y){
  f1<-(2*x+y)/27
  return(f1)
}

x<-c(0:2)
y<-c(0:2)
M1<-matrix(c(f(0,0:2),f(1,0:2),f(2,0:2)),nrow = 3, ncol = 3,byrow=TRUE) 
print(M1)  #byrow used for row-wise means row-wise y hai and column-wise x hai

#check if it is jpmf or not
sum(M1) #sum se double summation work krta hai

#Marginal of x for x=0,1,2
px<-apply(M1,1,sum) #1 represent row sum of matrix M1
print(px)

#Marginal of y for x=0,1,2
py<-apply(M1,2,sum) #2 represent column sum of matrix M1
print(py)

#?apply - used to check what apply command does
# conditional prob for P(X=0|Y=1)
M1[1,2]
py[2]
P_x0_y1<-M1[1,2]/py[2]
print(P_x0_y1)

x<-c(0:2)
y<-c(0:2)
#E(x),E(y),var(x),var(y),cov(x,y)
E_X<-sum(x*px)
E_X
E_X2<-sum(x*x*px)
E_X2
Var_X<-E_X2-E_X^2
Var_X

E_Y<-sum(y*py)
E_Y
E_Y2<-sum(y*y*py)
E_Y2
Var_Y<-E_Y2-E_Y^2
Var_Y

x<-c(0:2)
y<-c(0:2)
f1<-function(x,y){x*y*(2*x+y)/27}
M2<-matrix(c(f1(0,0:2),f1(1,0:2),f1(2,0:2)),nrow=3,ncol=3,byrow=T)
M2
E_XY<-sum(M2)
E_XY
cov_XY<-E_XY-E_X*E_Y
cov_XY
rho<-cov_XY/sqrt(Var_X*Var_Y)
rho

#Ques-2: $ is used to remove errors
#(a) f(x,y)=2/5*(x+4y)  0<x,y<1
library(pracma)
f<-function(x,y){2*(x+4*y)/5}
I<-integral2(f, xmin = 0, xmax = 1, ymin = 0,ymax = 1)
print(I$Q)

#marginal distribution fx(X) at x=1
fx1<-function(y) f(1,y)
gx1<-integral(fx1,0,1)
print(gx1)

#marginal distribution fy(Y) at y=0
fy0<-function(x) f(x,0)
hy0<-integral(fy0,0,1)
print(hy0)


f_XY<-function(x,y){x*y*f(x,y)}
E_XY<-integral2(f_XY,0,1,0,1)  
print(E_XY$Q)

