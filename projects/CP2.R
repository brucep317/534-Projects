###1


#a
mid_square_rng<-function(x0,n){
  x<-c(x0)
  for(i in 1:n){
    xsq<-x[i]^2
    xstr<-as.character(xsq)
    if(nchar(xstr)<8){
      d=8-nchar(xstr)
      zeroes<-strrep('0',d)
      xstr<-paste0(zeroes,xstr)}
    middle<-substr(xstr,3,6)
    x<-append(x,as.numeric(middle))}
  x<-x[-1]#delete seed
  return(x/10000)
}


#b
mid_square_rng(1010,20)

#c
mid_square_rng(6100,20)

#d
mid_square_rng(3792,20)










##2

#a


lehmer_rng<-function(n,m,a,b,x0){
  x<-c(x0)
  for(i in 1:n){
    xnew<-(x[i]*a +b)%%m
    x<-append(x,xnew)
  }
  x<-x[-1]#delete seed
  return(x/(m-1))
}





#b
library(fmsb)

dev.off()

a.list<-c(3:15)
data<-NULL
for(i in 1:length(a.list)){
  rseq<-lehmer_rng(n=16,m=16,x0=1,b=1,a=a.list[i])
  data<-rbind(data,rseq)
}

maxmin<-rbind(rep(1,16),rep(0,16))
data<-data.frame(rbind(maxmin,data),row.names = c('max','min',paste0('a',3:15)))
#data
par(mfrow=c(1,2))
for(i in 3:15){
  radarchart(data[c(1,2,i),],title = paste0('a=',i))
}


#c
dev.off()

b.list<-c(2:8)
data<-NULL
for(i in 1:length(b.list)){
  rseq<-lehmer_rng(n=16,m=16,x0=1,b=b.list[i],a=5)
  data<-rbind(data,rseq)
}

maxmin<-rbind(rep(1,16),rep(0,16))
data<-data.frame(rbind(maxmin,data),row.names = c('max','min',paste0('b',2:8)))
#data
par(mfrow=c(1,2))
for(i in 2:8){
  radarchart(data[c(1,2,i),],title = paste0('b=',i))
}


#d
lehmer_rng(x0=6,m=100,a=21,b=1,n=20)



#e
dev.off()
par(mfrow=c(1,2))
samp<-lehmer_rng(x0=1,m=2^11,a=1229,b=1,n=5000)
hist(samp)


#f
x<-NULL
y<-NULL
for (i in 2:length(samp)) {
  x<-append(x,samp[i-1])
  y<-append(y,samp[i])
}
plot(x,y)

#g
dev.off()
par(mfrow=c(1,2))

samp<-lehmer_rng(x0=1,m=244944,a=1597,b=51749,n=5000)
hist(samp)

x<-NULL
y<-NULL
for (i in 2:length(samp)) {
  x<-append(x,samp[i-1])
  y<-append(y,samp[i])
}
plot(x,y)


#h
dev.off()
par(mfrow=c(1,2))

samp<-lehmer_rng(x0=1,m=2^31,a=2^16 +3,b=0,n=5000)
hist(samp)

x<-NULL
y<-NULL
for (i in 2:length(samp)) {
  x<-append(x,samp[i-1])
  y<-append(y,samp[i])
}
plot(x,y)


#i
x<-NULL
y<-NULL
z<-NULL
for (i in 3:length(samp)) {
  x<-append(x,samp[i-2])
  y<-append(y,samp[i-1])
  z<-append(z,samp[i])
}
library(scatterplot3d)
dev.off()
scatterplot3d(x,y,z) #use angle= to rotate 

