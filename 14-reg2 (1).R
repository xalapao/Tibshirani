set.seed(0)
n = 50
p = 30
x = matrix(rnorm(n*p),nrow=n)

bstar = c(runif(10,0.5,1),runif(20,0,0.3))
mu = as.numeric(x%*%bstar)

par(mar=c(4.5,4.5,0.5,0.5))
hist(bstar,breaks=30,col="gray",main="",
xlab="True coefficients")

set.seed(1)
R = 100

fit = matrix(0,R,n)
err = numeric(R)

for (i in 1:R) {
cat(c(i,", "))
y = mu + rnorm(n)
ynew = mu + rnorm(n)

a = lm(y~x+0)
bls = coef(a)
fit[i,] = x%*%bls
err[i] = mean((ynew-fit[i,])^2)
}

prederr = mean(err)

bias = sum((colMeans(fit)-mu)^2)/n
var = sum(apply(fit,2,var))/n

bias
var
p/n
1 + bias + var
prederr
