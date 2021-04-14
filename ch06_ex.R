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

# 6.6.c

# 6.6.d

# 6.6.e

# 6.6.f

# 6.6.g


########## 6.7
library(readxl)
fishcook <- read_xls("data/practice/Table6.13_fishcook.xls")
fishcook <- as.data.frame(fishcook)
attach(fishcook)

# 6.7.a

# 6.7.b

# 6.7.c

# 6.7.d

# 6.7.e
