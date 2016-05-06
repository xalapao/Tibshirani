load("playerstats.Rdata")

x = as.matrix(playerstats[,-c(1:5,11)])
a = prcomp(x,center=TRUE,scale=TRUE)
dirs = a$rotation
scrs = a$x

round(dirs[,1:2],3)
namefinish = paste(playerstats[,3],playerstats[,5])

plot(scrs[,1],scrs[,2],xlim=range(scrs[,1])*1.5,ylim=range(scrs[,2])*1.5)
identify(scrs[,1],scrs[,2],labels=namefinish,n=10)

