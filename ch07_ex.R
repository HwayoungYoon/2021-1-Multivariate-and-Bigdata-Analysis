# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 7.2


########## 7.9
S <- matrix(c(1,0.71,0.58,0.56,0.65,0.71,1,0.71,0.6,0.69,0.58,0.71,1,0.75,
              0.71,0.56,0.6,0.75,1,0.74,0.65,0.69,0.71,0.74,1), nc=5)

# 7.9.a

# 7.9.b



########## 7.11
library(readxl)
Hemo <- read_xlsx("data/practice/Table7.5_Blood.xlsx")
Hemo <- as.data.frame(Hemo)
attach(Hemo)

# 7.11.a

# 7.11.b

# 7.11.c

# 7.11.d

# 7.11.e
