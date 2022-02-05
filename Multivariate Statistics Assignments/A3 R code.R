x1=c(3,4,4,5)
x2=c(6,4,8,6)
x3=c(0,3,3,2)
data=cbind(x1,x2,x3);data
0.75*cov(data)


m=c(26.29,4.69,-4.87)
S=matrix( c(5808.06, 597.84, 222.03, 597.84, 126.05, 23.39, 222.03, 23.09, 23.11), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)


T_sq=t(m)%*%(solve(S))%*%m
T_sq*86

qf(.95, df1=3, df2=84)
qf(.95, df1=2, df2=97)
library(ellipse)
ev <- eigen(S)
ev


y1=c(468,428,514,547,614,501,421,527,527,620,587,541,561,468,614,527,507,580,507,521,574,587,
     488,488,587,421,481,428,640,574,547,580,494,554,647,507,454,427,521,468,587,507,574,507,
     494,541,362,408,594,501,687,633,647,647,614,633,448,408,441,435,501,507,620,415,554,348,
     468,507,527,527,435,660,733,507,527,428,481,507,527,488,607,561,614,527,474,441,607)
y2=c(41,39,53,67,61,67,46,50,55,72,63,59,53,62,65,48,32,64,59,54,52,64,
     51,62,56,38,52,40,65,61,64,64,53,51,58,65,52,57,66,57,55,61,54,53,
     41,47,36,28,68,25,75,52,67,65,59,65,55,51,35,60,54,42,71,52,69,28,
     49,54,47,47,50,70,73,45,62,37,48,61,66,41,69,59,70,49,41,47,67)
y3=c(26,26,21,33,27,29,22,23,19,32,31,19,26,20,28,21,27,21,21,23,25,31,
     27,18,26,16,26,19,25,28,27,28,26,21,23,23,28,21,26,14,30,31,31,23,
     24,25,17,17,23,26,33,31,29,34,25,28,24,19,22,20,21,24,36,20,30,18,
     25,26,31,26,28,25,33,28,29,19,23,19,23,28,28,34,23,30,16,26,32)
par(mfrow = c(1, 3))
qqnorm(y1, pch = 1, frame = FALSE, main = "Q-Q Plot for Social Science & History")
qqline(y1, col = "steelblue", lwd = 2)
qqnorm(y2, pch = 1, frame = FALSE, main = "Q-Q Plot for Verbal")
qqline(y2, col = "steelblue", lwd = 2)
qqnorm(y3, pch = 1, frame = FALSE, main = "Q-Q Plot for Science")
qqline(y3, col = "steelblue", lwd = 2)
par(mfrow = c(1, 3))
plot(y1, y2, main="Scatterplot for Social Science & History and Verbal scores",
     xlab="Social Science & History", ylab="Verbal", pch=19)
plot(y2, y3, main="Scatterplot for Verbal and Science",
     xlab="Verbal", ylab="Science", pch=19)
plot(y3, y1, main="Scatterplot for Science and Social Science & History",
     xlab="Science ", ylab="Social Science & History", pch=19)

snow1=c(12.5,14.5,8,9,19.5,8,9,7,7,
        9,6.5,10.5,10,4.5,7,8.5,6.5,8,
        3.5,8,17.5,10.5,12,6,13)
snow2=c(13.7,16.5,17.4,11,23.6,13.2,32.1,12.3,11.8,
        24.4,18.2,22,32.5,18.7,15.8,15.6,12,12.8,
        26.1,14.5,42.3,17.5,21.8,10.4,25.6)

par(mfrow = c(1, 2))
qqnorm(snow1, pch = 1, frame = FALSE, main="Q-Q plot for x_1")
qqline(snow1, col = "steelblue", lwd = 2)
qqnorm(snow2, pch = 1, frame = FALSE, main="Q-Q plot for x_2")
qqline(snow2, col = "steelblue", lwd = 2)
library(EnvStats)
bc1=boxcox(snow1)
bc2=boxcox(snow2)

s1=log(snow1)
s2=1/(snow2)
par(mfrow = c(1, 2))
qqnorm(s1, pch = 1, frame = FALSE, main="Q-Q plot for y_1") 
qqline(s1, col = "steelblue", lwd = 2)
qqnorm(s2, pch = 1, frame = FALSE, main="Q-Q plot for y_2")
qqline(s2, col = "steelblue", lwd = 2)
snow=cbind(s1,s2);snow

mean(s1)
mean(s2)
cov(snow)

ev_snow <- eigen(cov(snow))
ev_snow


d=c(74.4,201.6)
S_pool=matrix( c(10963.69, 21505.42, 21505.42, 63661.31), # the data elements 
          nrow=2,              # number of rows 
          ncol=2,              # number of columns 
          byrow = TRUE)


T_sq_d=t(d)%*%(solve(S_pool))%*%d
T_sq_d*(45*55)/(45+55)

