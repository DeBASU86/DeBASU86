R11=matrix( c(1,.615,
              .615,1), # the data elements 
            nrow=2,              # number of rows 
            ncol=2,              # number of columns 
            byrow = TRUE)

R12=matrix( c(-.111,-.266,
              -.195,-.085), # the data elements 
            nrow=2,              # number of rows 
            ncol=2,              # number of columns 
            byrow = TRUE)

R21=t(R12)

R22=matrix( c(1,-.269,
              -.269,1), # the data elements 
            nrow=2,              # number of rows 
            ncol=2,              # number of columns 
            byrow = TRUE)

options(digits=4)

ev_R11=eigen(R11);ev_R11
R11inv=solve(R11)
R22inv=solve(R22)
R11invhalf=matrix( c(1.1992,-.4124,
                         -.4124,1.1992), # the data elements 
                       nrow=2,              # number of rows 
                       ncol=2,              # number of columns 
                       byrow = TRUE)

mat_E=R11invhalf%*%R12%*%R22inv%*%R21%*%R11invhalf;mat_E
ev_E=eigen(mat_E);ev_E

R11invhalf%*%c(-.9463,-.3232)
b1_initial=R22inv%*%R21%*%c(-1.0015,.0027);b1_initial
t(b1_initial)%*%R22%*%b1_initial
