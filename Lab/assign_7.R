#Ques-1: rho<-cov(X,Y)/sqrt(VarX*VarY)
x<-c(1,2,3,4,5,6,7,8,9)
y<-c(9,8,10,12,11,13,14,16,15)
n<-9
E_x<-sum(x)/n
E_x
E_y<-sum(y)/n
E_y

E_x_2<-sum(x^2)/n
E_x_2
E_y_2<-sum(y^2)/n
E_y_2

E_xy<-sum(x*y)/n
cov_xy<-E_xy-E_x*E_y
var_x<-E_x_2-E_x^2
var_y<-E_y_2-E_y^2

rho<-cov_xy/sqrt(var_x*var_y)
rho


#OR using inbuilt function!
rho<-cor(x,y)
cat("Corr, coeff, rho b/w x and y= ",rho,"\n")

#ALso write Rcode to find lines of regression!
#Y on X i.e. E_YonX

reg_Y_on_X<-lm(y~x)
cat("Reg line Y on X: Y =",coef(reg_Y_on_X)[1],"+",coef(reg_Y_on_X)[2],
    "*x\n")

#Calculate at x=6.2
reg_Y_on_X<-lm(y~x)
cat("Reg line Y on X=6.2: Y =",coef(reg_Y_on_X)[1],"+",coef(reg_Y_on_X)[2],
    "*6.2\n")

reg_Y_on_X<-lm(y~x)
cat("Reg line Y on X=6.2: Y =",coef(reg_Y_on_X)[1]+coef(reg_Y_on_X)[2]*6.2,
    "\n")

#Ques-2:
reg_Y_on_x<-lm(y~x)
cat("Reg line Y on X: Y =",coef(reg_Y_on_X)[1],"+",coef(reg_Y_on_X)[2],
    "*x\n")
#f(x,y)=2 0<x<y<=1
n<-100000
#get sum for y from a uniform distribution in (0 1)
y<-runif(n,min=0,max=1)
#get sum for x from a unif dist in 0<x<=y
x<-runif(n,min=0,max=y)
#calculate corr coeff b/w x and y
corr_coeff<-cor(x,y)
cat("Approx corr coeff(rho): ", corr_coeff,"\n")

#ques-> regression line y on x
Reg_y_on_x<-lm(y~x)  #lm means linear model
cat("Reg line Y on X : Y =",coef(Reg_y_on_x)[1],"+",coef(Reg_y_on_x)[2],
    "*x\n")


#Ques-3:
n<-100000

#Generate samples for Y using exponential distr (rate=1)
y<-rexp(n,rate = 1)

#generate x conditional y so that 0<x<y
x<-runif(n, min=0, max=y)

#calc the coerr coeff b/w x and y
corr_coeff<-cor(x,y)
cat("Approx corr coeff(rho): ", corr_coeff,"\n")
Reg_y_on_x<-lm(y~x)  #lm means linear model
cat("Reg line Y on X : Y =",coef(Reg_y_on_x)[1],"+",coef(Reg_y_on_x)[2],
    "*x\n")


