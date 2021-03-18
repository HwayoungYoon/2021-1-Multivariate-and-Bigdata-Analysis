# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 2.1. 전치행렬
A <- matrix(c(3,1,-1,5,2,4), nc=3)
# rbind(c(3,-1,2), c(1,5,4))
# cbind(c(3,1), c(-1,5), c(2,4))
A
dim(A)
tA <- t(A)
tA


#####################################
##### 에제 2.2. 벡터 연산
a <- c(1,-2,3)
b <- c(2,2,4)

a+b
t(a)*b
t(a)%*%b
3*a

la <- sqrt(t(a)%*%a)
la
lb <- sqrt(t(b)%*%b)
lb

cos_theta <- t(a)%*%b/(la*lb)
cos_theta


#####################################
##### 예제 2.4. 두 행렬의 곱
A <- matrix(c(3,1,-1,5,2,4), nc=3)
B <- matrix(c(1,-1,2,3,3,5), nc=3)

A+B
t(B)
A%*%t(B)


#####################################
##### 예제 2.7. 2×2 행렬의 결정식
A <- matrix(c(1,-3,2,5), nc=2)
det(A)

##### 예제 2.8. 3×3 행렬의 결정식
A <- matrix(c(1,0,1,2,3,5,1,4,6), nc=3)
det(A)


#####################################
##### 예제 2.9. 역행렬 w/MASS 패키지, ginv()
library(MASS)
A <- matrix(c(1,-3,2,5), nc=2)
# 일반화역행렬 A ginv(A) A = A
# 역행렬 A solve(A) = I
ginv(A)
solve(A)

A <- matrix(c(1,3,2,6), nc=2)
ginv(A)
solve(A)

A <- matrix(c(1,3,2,6.0000000001), nc=2)
ginv(A)
solve(A)

##### 예제 2.10. 3×3 역행렬
library(MASS)
A <- matrix(c(1,0,1,2,3,5,1,4,6), nc=3)
ginv(A)
solve(A)


#####################################
##### 예제 2.12. 직교행렬
A <- matrix(c(1/sqrt(2),-1/sqrt(2),1/sqrt(2),1/sqrt(2)), nc=2)
t(A)%*%A
I <- round(t(A)%*%A, digits = 3)
I


#####################################
##### 예제 2.14. 고유값과 고유벡터
A <- matrix(c(4,2,-5,-3), nc=2)
lambda <- eigen(A)
lambda
# 고유값 1
lambda$values[[1]]
# 고유벡터 1
lambda$vectors[,1]
# 고유값 2
lambda$values[[2]]
# 고유벡터 2
lambda$vectors[,2]

##### 예제 2.15. 대칭행렬과 고유값
A <- matrix(c(3,1,1,3), nc=2)
eigen(A)
trace <- sum(diag(A))
trace
t(A)
det(A)


#####################################
##### 연습문제 2.3.
A <- rbind(c(2,-1), c(-1,2))
A

plot(A[,1], A[,2], pch=19, col="blue")

eigen(A)

# inverse matrix
invA <- solve(A)
eigen(invA)

t(A)%*%A
solve(t(A)%*%A)
A%*%A
eigen(A%*%A)

# spectral decomposition
ea <- eigen(A)
ea1 <- ea$values[[1]]*ea$vectors[,1]%*%t(ea$vectors[,1])
ea2 <- ea$values[[2]]*ea$vectors[,2]%*%t(ea$vectors[,2])
ea1+ea2
A
