########## 3.1
X <- matrix(c(9,5,1,1,3,4), nc=2)

# 3.1.a
colMeans(X)

# 3.1.b
S <- cov(X)
S

# 3.1.c
R <- cor(X)
R

# 3.1.d
det(S)

# 3.1.e
sum(diag(S))

# 3.1.f
plot(X[,1], X[,2], xlab="변수1", ylab="변수2")


########## 3.2
S1 <- matrix(c(1,0,0,0,1,0,0,0,1), nc=3)
S2 <- matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1), nc=3)

# 3.2.a
det(S1)
det(S2)

# 3.2.b
sum(diag(S1))
sum(diag(S2))

# 3.2.c
max(eigen(S1)$values)/min(eigen(S1)$values)
max(eigen(S2)$values)/min(eigen(S2)$values)


########## 3.3
EX1 <- 1
EX2 <- 2
EX3 <- 3
VX1 <- 4
VX2 <- 9
VX3 <- 16
CX12 <- 2
CX13 <- 3
CX23 <- 2
EX <- matrix(c(EX1,EX2,EX3), nc=1)
E <- matrix(c(VX1,CX12,CX13,CX12,VX2,CX23,CX13,CX23,VX3), nc=3)

# 3.3.a
EY1 <- EX1+EX2+EX3
EY2 <- 2*EX1-3*EX2+2*EX3
EY <- matrix(c(EY1,EY2), nc=1)
EY
# E(Y)=(E(Y1), E(Y2))
# E(Y1)=E(X1+X2+X3)
#      =E(X1)+E(X2)+E(X3)
# E(Y2)=E(2X1-3X2+2X3)
#      =E(2X1)-E(3X2)+E(2X3)
#      =2E(X1)-3E(X2)+2E(X3)

# 3.3.b
VY1 <- VX1+VX2+VX3+2*CX12+2*CX13+2*CX23
VY2 <- 4*VX1+9*VX2+4*VX3+2*(-6)*CX12+2*4*CX13+2*(-6)*CX23
CY12 <- 2*VX1-3*VX2+2*VX3-CX12+4*CX13-CX23
CovY <- matrix(c(VY1,CY12,CY12,VY2), nc=2)
CovY
# Cov(Y)=(Var(Y1), Cov(Y1,Y2))
#        (Cov(Y2,Y1), Var(Y2))
# Var(Y1)=Var(X1+X2+X3)
#        =Var(X1)+Var(X2)+Var(X3)+2Cov(X1,X2)+2Cov(X1,X3)+2Cov(X2,X3)
# Var(Y2)=Var(2X1-3X2+2X3)
#        =4Var(X1)+9Var(X2)+4Var(X3)+2(-6)Cov(X1,X2)+2(4)Cov(X1,X3)+2(-6)Cov(X2,X3)
# Cov(Y1,Y2)=Cov(X1+X2+X3,2X1-3X2+2X3)
#           =2Var(X1)-3Var(X2)+2Var(X3)-Cov(X1,X2)+4Cov(X1,X3)-Cov(X2,X3)

# 3.3.c
CorrY <- cov2cor(CovY)
CorrY
