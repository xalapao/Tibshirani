library(classifly)
library(MASS)

data(olives)
y = as.numeric(olives[,1])
x = as.matrix(olives[,3:10])

n = nrow(x)
p = ncol(x)

a = lda(x,y)

at = a$scaling # In terms of lecture notation, this is A^T 
z = x %*% at

cols = c("red","darkgreen","blue")
plot(z,col=cols[y])
legend("bottomleft",pch=21,col=cols,
       legend=c("Region 1","Region 2","Region 3"))

# We've reduced the problem to only looking at 2, rather
# than 8 dimensions! Plus, now the decision boundaries are
# pretty easy to draw, because it's essentially nearest
# centroid classification

mu = a$means %*% at
pi = a$prior

points(mu,col=cols,pch=19,cex=2)
points(mu,pch=21,cex=2)

getab = function(j,k,mu,pi) {
  b = (mu[k,1]-mu[j,1])/(mu[j,2]-mu[k,2])
  normj = sum(mu[j,]^2)
  normk = sum(mu[k,]^2)
  a = (log(pi[k]/pi[j]) + 0.5*(normj-normk))/(mu[j,2]-mu[k,2])
  return(list(a=a,b=b))
}

# 1 and 2
ab12 = getab(1,2,mu,pi)
abline(a=ab12$a,b=ab12$b,lty=1)

# 1 and 3
ab13 = getab(1,3,mu,pi)
abline(a=ab13$a,b=ab13$b,lty=2)

# 2 and 3
ab23 = getab(2,3,mu,pi)
abline(a=ab23$a,b=ab23$b,lty=3)

## Find points of intersection
zx = (ab12$a-ab13$a)/(ab13$b-ab12$b)
zy = ab12$a + ab12$b*zx

# Now redraw with the appropriate boundaries
plot(z,col=cols[y])
legend("bottomleft",pch=21,col=cols,
       legend=c("Region 1","Region 2","Region 3"))
points(mu,col=cols,pch=19,cex=2)
points(mu,pch=21,cex=2)

segments(zx,zy,-15,ab12$a+ab12$b*(-15))
segments(zx,zy,-7,ab23$a+ab23$b*(-7))
segments(zx,zy,-15,ab13$a+ab13$b*(-15))
