#Assignment 2 Problem 5
x1=c(1,2,3,3,4,5,6,8,9,11)
x2=c(18.95,19,17.95,15.54,14,12.95,8.94,7.49,6,3.99)
data1=cbind(x1,x2)
xbar=c(mean(x1),mean(x2))
S_mat=cov(data1)
S_inv=solve(S_mat)
gd=c(0,0,0,0,0,0,0,0,0,0)
># Computing the generalized distance
for (i in 1:10)
  {
    X_bar<-colMeans(data1)
    x<-data1[i,]-X_bar
    gd[i]<-t(x)%*%S_inv%*%(x)
  }
gd

qchisq(.5,2)

qchi<-qchisq((seq(1,10)-.5)/10,2) #Chi-square quantiles
xobs<-sort(gd) #Ranked generalized distances

cbind(gd,xobs,qchi)

plot(xobs,qchi,ylab = "Chi-square Quantiles", xlab = "Ordered Generalized Distance", main = "Chi-square plot") 
