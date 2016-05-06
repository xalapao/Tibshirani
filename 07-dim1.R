library(scatterplot3d)
set.seed(0)
n = 2000
tt = seq(0,2*pi,length=n)
x = cos(tt)+rnorm(n,sd=0.1)
y = sin(tt)+rnorm(n,sd=0.1)
z = rnorm(n,sd=0.1)
lim = range(c(x,y,z))

xx = cbind(x,y,z)
a = princomp(xx)
v = a$loadings # directions
s = a$scores   # scores

s3d = scatterplot3d(x,y,z,xlim=lim,ylim=lim,zlim=lim,
  xlab="",ylab="",zlab="",grid=FALSE,
  main="First three principal component directions")
s3d$points3d(rbind(c(0,0,0),v[,1]),type="l",lwd=3,col="blue")
s3d$points3d(rbind(c(0,0,0),v[,2]),type="l",lwd=3,col="red")
s3d$points3d(rbind(c(0,0,0),v[,3]),type="l",lwd=3,col="darkgreen")

plot(s[,1],s[,2],main="First two principal component scores",
     xlab="Score 1",ylab="Score 2")

pv = cumsum(a$sdev^2)/sum(a$sdev^2)
plot(1:ncol(xx),pv,type="b",ylim=c(0,1),
xlab="Number of component directions",
ylab="Proportion of variance explained")
