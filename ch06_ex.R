# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 6.6
n1 <- 10; n2 <- 20; n3 <- 50
xbar1 <- c(3,2,1); xbar2 <- c(4,1,1); xbar3 <- c(4,2,2)
s1 <- matrix(c(0.3,0.01,0.01,0.01,0.2,0.03,0.01,0.03,0.1), nc=3)
s2 <- matrix(c(0.5,0.11,0.04,0.11,0.3,0.007,0.04,0.007,0.2), nc=3)
s3 <- matrix(c(0.32,0.3,0.01,0.3,0.7,-0.03,0.01,-0.03,0.4), nc=3)

# 6.6.a

# 6.6.b
xbar <- (n1*xbar1+n2*xbar2+n3*xbar3)/(n1+n2+n3)
xbar

# 6.6.c
E <- (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3
E

# 6.6.d
B <- n1*(xbar1-xbar) %*% t(xbar1-xbar) + n2*(xbar2-xbar) %*% t(xbar2-xbar) + n3*(xbar3-xbar) %*% t(xbar3-xbar)
B

# 6.6.e
B+E

# 6.6.f

# 6.6.g
det(E)/det(B+E)


########## 6.7
library(readxl)
fishcook <- read_xls("data/practice/Table6.13_fishcook.xls")
fishcook <- as.data.frame(fishcook)
attach(fishcook)
fc1 <- fishcook[(method==1), 2:5]
fc2 <- fishcook[(method==2), 2:5]
fc3 <- fishcook[(method==3), 2:5]
n1 <- nrow(fc1); n2 <- nrow(fc2); n3 <- nrow(fc3)
n <- n1+n2+n3

# 6.7.a

# 6.7.b

# 6.7.c

# 6.7.d

# 6.7.e
