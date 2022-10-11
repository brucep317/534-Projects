#parameters
set.seed(111)
n=10000
r=10

#estimate
samp<-runif(2*n,-r,r)
points<-array(samp,dim=c(n,2))
circle<- (sqrt((points[,1])^2+(points[,2])^2))<=r
estimate<-sum(circle)/n
estimate*4


#CI
alpha<-.05
z<-qnorm(1-alpha/2,lower = TRUE)
upper<-4*estimate+4*z*sqrt(estimate*(1-estimate)/n)
lower<-4*estimate-4*z*sqrt(estimate*(1-estimate)/n)
ci<-c(lower,upper)
ci
