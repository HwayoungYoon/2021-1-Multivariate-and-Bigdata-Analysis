########## 2.2
A <- matrix(c(1,-1,4,-1,1,3,4,3,2), nc=3)
B <- matrix(c(3,-2,4,-2,1,0,4,0,2), nc=3)
x <- matrix(c(1,-2,1), nc=1)
y <- matrix(c(3,2,1), nc=1)

# 2.2.a
A+B

# 2.2.b
t(A)

# 2.2.c
t(x)%*%A%*%y

# 2.2.d
t(x)%*%x

# 2.2.e
t(x)%*%A%*%x

# 2.2.f
t(x)%*%y

# 2.2.g
t(A)%*%A

# 2.2.h
A%*%B

# 2.2.i
t(y)%*%B

# 2.2.j
x%*%t(x)

# 2.2.k
x+y

# 2.2.l
x-y

# 2.2.m
t(x-y)

# 2.2.n
x%*%t(y)

# 2.2.o
A-B

# 2.2.p
t(A)+t(B)

# 2.2.q
t(A+B)

# 2.2.r
3*x

# 2.2.s
(t(x)%*%y)^2

# 2.2.t
B%*%A

# 2.2.u
sqrt(t(x)%*%x)

# 2.2.v
lx <- sqrt(t(x)%*%x)
ly <- sqrt(t(y)%*%y)
cos_theta <- t(x)%*%y/(lx*ly)
cos_theta
acos(cos_theta)

# 2.2.w
sum(diag(A))
sum(diag(B))

# 2.2.x
sum(diag(A+B))

# 2.2.y
sum(diag(A%*%B))
sum(diag(B%*%A))

# 2.2.z
t(A%*%B)
t(B)%*%t(A)


########## 2.3
A <- matrix(c(2,-1,-1,2), nc=2)

# 2.3.a
A[1,]
A[2,]
plot(A[1,], A[2,], xlab="1행", ylab="2행")

# 2.3.b
A[,1]
A[,2]
plot(A[,1], A[,2], xlab="1열", ylab="2열")

# 2.3.c
dim(A)
# rank(A)=2

# 2.3.d
eigen(A)

# 2.3.e
library(MASS)
ginv(A)
solve(A)

# 2.3.f
eA <- eigen(A)
eA1 <- eA$values[[1]]*eA$vectors[,1]%*%t(eA$vectors[,1])
eA2 <- eA$values[[2]]*eA$vectors[,2]%*%t(eA$vectors[,2])
eA1; eA2
eA1+eA2
A

# 2.3.g
library(MASS)
eigen(ginv(A))
eigen(solve(A))

# 2.3.h
eigen(A)
# A의 고유값이 모두 양수이므로 양정치행렬이다

# 2.3.i
t(A)%*%A

# 2.3.j
eigen(t(A)%*%A)

# 2.3.k
library(MASS)
ginv(t(A)%*%A)
solve(t(A)%*%A)

# 2.3.l
A%*%A

# 2.3.m
eigen(A%*%A)

# 2.3.n
eAA <- eigen(A%*%A)
eAA1 <- eAA$values[[1]]*eAA$vectors[,1]%*%t(eAA$vectors[,1])
eAA2 <- eAA$values[[2]]*eAA$vectors[,2]%*%t(eAA$vectors[,2])
eAA1; eAA2
eAA1+eAA2
A%*%A


########## 2.4
E <- matrix(c(4,0,0,0,9,0,0,0,1), nc=3)

# 2.4.a
dim(E)
# rank(E)=3

# 2.4.b
eigen(E)

# 2.4.c
library(MASS)
ginv(E)
solve(E)

# 2.4.d
eE <- eigen(E)
eE1 <- eE$values[[1]]*eE$vectors[,1]%*%t(eE$vectors[,1])
eE2 <- eE$values[[2]]*eE$vectors[,2]%*%t(eE$vectors[,2])
eE3 <- eE$values[[3]]*eE$vectors[,3]%*%t(eE$vectors[,3])
eE1; eE2; eE3
eE1+eE2+eE3
E

# 2.4.e
library(MASS)
eigen(ginv(E))
eigen(solve(E))

# 2.4.f
eigen(E)
# E의 고유값이 모두 양수이므로 양정치행렬이다

# 2.4.g
cov2cor(E)

# 2.4.h
det(E)
eE <- eigen(E)
eE$values[[1]] * eE$values[[2]] * eE$values[[3]]

# 2.4.i
sum(diag(E))
eE <- eigen(E)
eE$values[[1]] + eE$values[[2]] + eE$values[[3]]

# 2.4.j
eE <- eigen(E)
eE1 <- sqrt(eE$values[[1]])*eE$vectors[,1]%*%t(eE$vectors[,1])
eE2 <- sqrt(eE$values[[2]])*eE$vectors[,2]%*%t(eE$vectors[,2])
eE3 <- sqrt(eE$values[[3]])*eE$vectors[,3]%*%t(eE$vectors[,3])
eE1; eE2; eE3
hE <- eE1+eE2+eE3
hE

# 2.4.k
eigen(hE)
