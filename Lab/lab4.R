#Q1
#x~B(n,p)=B(n=12,p=1/6)
bino_pmf<-function(x,n,p)
{
  P_x<-choose(n,x)*p^(x)*(1-p)^(n-x)
  return(P_x)
}
#a
n<-12
p<-1/6
P_8<-bino_pmf(8,n,p)
print(P_8)
#or for single value
P_8<-dbinom(8,size=12,prob=1/6)
print(P_8)

#b
n<-12
p<-1/6
P_X_lesseq_7<-sum(bino_pmf(0:7,n,p))
print(P_X_lesseq_7)
#or
P_X_lesseq_7<-pbinom(7,size=12,prob = 1/6,lower.tail = TRUE)
print(P_X_lesseq_7)

#c
P_X_greater_6<-1-sum(bino_pmf(0:6,n,p))
print(P_X_greater_6)
#or
P_X_greater_6<-1-pbinom(6,size=12,prob=1/6)
print(P_X_greater_6)
#or
P_X_greater_6<-pbinom(6,size=12,prob=1/6,lower.tail = FALSE)
print(P_X_greater_6)

#d
#P(7<=x<=9)=P(x<=9)-P(x<=6)
n<-12
p<-1/6
P_7_X_9<-sum(bino_pmf(0:9,n,p))-sum(bino_pmf(0:6,n,p))
print(P_7_X_9)

#or
P_7_X_9<-pbinom(9,size=12,prob=1/6)-pbinom(6,size=12,prob=1/6)
print(P_7_X_9)

#or
P_7_X_9<-diff(pbinom(c(6,9),size=12,prob=1/6))
print(P_7_X_9)

#e
#P(7<=x<9)=P(x<9)-P(x<7)=P(x<=8)-P(x<=6)

P_7_X_8<-pbinom(8,12,1/6)-pbinom(6,12,1/6)
print(P_7_X_8)
