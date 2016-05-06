load(file="splines.Rdata")
n = 100
x = 1:n/n

K = 5
folds = vector(mode="list",length=K)
for (k in 1:K) {
  folds[[k]] = seq(k,n,by=K)
}

dfs = 2:30
ndfs = length(dfs)
errs = matrix(0,n,ndfs)
for (k in 1:K) {
  i.tr = unlist(folds[-k])
  i.val = folds[[k]]
  x.tr = x[i.tr]    
  y.tr = y[i.tr]   
  x.val = x[i.val] 
  y.val = y[i.val]
  
  for (j in 1:ndfs) {
    a = smooth.spline(x.tr,y.tr,df=dfs[j])
    yhat = predict(a,x.val)$y
    errs[i.val,j] = (yhat-y.val)^2
  }  
}

cv = colMeans(errs)

errs0 = matrix(0,K,ndfs)
for (k in 1:K) {
  errs0[k,] = colMeans(errs[folds[[k]],])
}
se = apply(errs0,2,sd)/sqrt(K)

# Usual rule
i1 = which.min(cv)
# One standard error rule---note the min!
i2 = min(which(cv<=cv[i1]+se[i1]))

plot(dfs,cv,type="l",ylim=range(c(cv-se,cv+se)))
points(dfs,cv,pch=20)
lines(dfs,cv-se,lty=3)
lines(dfs,cv+se,lty=3)
abline(v=dfs[i1],col="red",lty=2)
abline(v=dfs[i2],col="blue",lty=2)

yhat1 = smooth.spline(x,y,df=dfs[i1])$y
yhat2 = smooth.spline(x,y,df=dfs[i2])$y

plot(x,y)
lines(x,yhat1,col="red",lwd=2)
lines(x,yhat2,col="blue",lwd=2)
