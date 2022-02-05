#Assignment 2 Problem 5
Company=c("Citigroup","GE","American Intl Group", "BoA", "HSBC", "ExxonMobil", "Royal Dutch/Shell", "BP","ING", "Toyota")
x1=c(108.28,152.36,95.04,65.45,62.97,263.99,265.19,285.06,92.01,165.68)
x2=c(17.05,16.59,10.91,14.14,9.52,25.33,18.54,15.73,8.10,11.13)
x3=c(1484.10,750.33,766.42,1110.46,1031.29,195.26,193.83,191.11,1175.16,211.15)
BigComps=cbind(x1,x2,x3)
X_bar=c(mean(x1),mean(x2),mean(x3))
S_mat=cov(BigComps)
S_inv=solve(S_mat)
gd=c(0,0,0,0,0,0,0,0,0,0)
># Computing the generalized distance
  for (i in 1:10)
  {
    X_bar<-colMeans(BigComps)
    x<-BigComps[i,]-X_bar
    gd[i]<-t(x)%*%S_inv%*%(x)
  }
gd

qchi<-qchisq((seq(1,10)-.5)/10,3) #Chi-square quantiles
xobs<-sort(gd) #Ranked generalized distances

cbind(xobs,qchi)

plot(xobs,qchi,ylab = "Chi-square Quantiles", xlab = "Ordered Generalized Distance", main = "Chi-square plot") 
