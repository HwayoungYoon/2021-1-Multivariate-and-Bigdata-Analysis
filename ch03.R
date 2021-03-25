# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 3.1.

# 데이터 입력
x1 <- c(90,80,75,70,65)
x2 <- c(80,90,80,70,80)
a <- cbind(x1, x2)
a

# 분산
S <- var(a)
S

# 일반화 표본 분산
g_var <- det(S)
g_var

# 총표본분산
total_var <- var(x1)+var(x2)
total_var

sum(diag(S))

# 표본상관행렬
R <- cor(a)
R <- round(R, digits=3)
R

# 고유값, 고유벡터
ea <- eigen(R)
ea

# 조건수
cond_num <- max(ea$values)/min(ea$values)
cond_num


#####################################
##### 연습문제 3.1.
X <- rbind(c(9,1),c(5,3),c(1,4))
X
colMeans(X)
S <- cov(X)
S
R <- cor(X)
R

det(S)
sum(diag(S))

plot(X[,1], X[,2], pch=19, cex=2, col="pink")


#####################################
##### 연습문제 3.2.
s1 <- matrix(c(1,0,0,0,1,0,0,0,1), nc=3)
s2 <- matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1), nc=3)
s1
s2

# a.
det(s1)
det(s2)

# b.
sum(diag(s1))
sum(diag(s2))

# c. condition number
max(eigen(s1)$values)/min(eigen(s1)$values)
max(eigen(s2)$values)/min(eigen(s2)$values)


#####################################
##### 연습문제 3.3.
# Y1 = X1+X2+X3
# Y2 = 2X1-3X2+2X3
# Y = (Y1,Y2)
# E(Y)=(E(Y1), E(Y2))
# Cov(Y)=(Var(Y1), Cov(Y1,Y2))
#        (Cov(Y2,Y1), Var(Y2))
# E(Y1)=E(X1+X2+X3)
#      =E(X1)+E(X2)+E(X3)
# Var(Y1)=Var(X1+X2+X3)
#        =Var(X1)+Var(X2)+Var(X3)+2Cov(X1,X2)+2Cov(X1,X3)+2Cov(X2,X3)
# E(Y2)=E(2X1-3X2+2X3)
#      =E(2X1)-E(3X2)+E(2X3)
#      =2E(X1)-3E(X2)+2E(X3)
# Var(Y2)=Var(2X1-3X2+2X3)
#        =4Var(X1)+9Var(X2)+4Var(X3)+2(-6)Cov(X1,X2)+2(4)Cov(X1,X3)+2(-6)Cov(X2,X3)
