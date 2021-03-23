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
##### 