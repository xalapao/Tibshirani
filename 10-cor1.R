#install.packages("classifly")
library(classifly)
data(olives)

dim(olives)
colnames(olives)

olives[1:10,]
table(olives[,1])

as.indmat = function(z) {
  z = as.factor(z)
  l = levels(z)
  b = as.numeric(z==rep(l,each=length(z)))
  return(matrix(b,length(z)))
}

y = as.indmat(olives[,1])
x = as.matrix(olives[,3:10])

cc = cancor(x,y,ycenter=F)
alpha = cc$xcoef
beta = cc$ycoef

alpha[,1:2]*1e4
beta[,1:2]

xvars = x %*% alpha
yvars = y %*% beta

cols = c("red","darkgreen","blue")
par(mar=c(4.5,4.5,0.5,0.5))

plot(xvars[,1:2],col=cols[olives[,1]],
xlab="First canonical x variate",
ylab="Second canonical x variate")

legend("bottomleft",pch=21,col=cols,
legend=c("Region 1","Region 2","Region 3"))

