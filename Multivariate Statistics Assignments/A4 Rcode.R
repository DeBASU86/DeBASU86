m=c(155.60, 14.70)
R=matrix( c(1, 0.6861, 0.6861, 1), # the data elements 
          nrow=2,              # number of rows 
          ncol=2,              # number of columns 
          byrow = TRUE)
ev <- eigen(R)
ev

S=matrix( c(7476.45, 303.662, 303.62, 26.19), # the data elements 
          nrow=2,              # number of rows 
          ncol=2,              # number of columns 
          byrow = TRUE)
ev_S <- eigen(S)
ev_S

######################
setwd("C:/Users/30719/Documents/Datasets")
#Import data
data1 <- read.table("A4_P4.csv", 
                    header = TRUE,
                    sep = ",")
data1
Disp=cov(data1);Disp
ev_Disp=eigen(Disp);ev_Disp


pc.data1=princomp(data1)
summary(pc.data1)

######################
data2 <- read.table("A4_P5.txt", 
                 header = TRUE)
data2
options(digits=5)
S=cov(data2);S
R=cor(data2);R

S.pca=princomp(data2);summary(S.pca)
lambda_S <- S.pca$sdev^2; lambda_S
head(S.pca)

R.pca=princomp(data2, cor= TRUE);summary(R.pca)
lambda_R <- R.pca$sdev^2; lambda_R
head(R.pca)
######################

R=matrix( c(1,.4919,.2636,.4653,-.2277,.0652,
            .4919,1,.3127,.3506,-.1917,.2045,
            .2635,.3127,1,.4108,.0647,.2493,
            .4653,.3506,.4108,1,-.2249,.2293,
            -.2277,-.1917,.0647,-.2249,1,-.2144,
            .0652,.2045,.2493,.2293,-.2144,1), # the data elements 
          nrow=6,              # number of rows 
          ncol=6,              # number of columns 
          byrow = TRUE)



R_1=matrix( c(1,.4919,.2636,.4653,
              .4919,1,.3127,.3506,
              .2635,.3127,1,.4108,
              .4653,.3506,.4108,1), # the data elements 
            nrow=4,              # number of rows 
            ncol=4,              # number of columns 
            byrow = TRUE)


options(digits=4)
R_1
R

ev_R1=eigen(R_1);ev_R1
ev_R=eigen(R);ev_R
















