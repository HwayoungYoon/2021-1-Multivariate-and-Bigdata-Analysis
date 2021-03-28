# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 4.2.

# 데이터 입력
a <- read.csv("data/나이와수입.csv", header=T)
attach(a)
a
b <- a[,2:3]

# 평균벡터
m <- colMeans(b)
m

# 공분산행렬
S <- cov(b)
S

# scatter plot
plot(age, income)

# 
n <- length(age); n
t <- seq(1,n); t
age_s <- sort(age); age_s
income_s <- sort(income); income_s

# 
edf <- (t-0.5)/n; edf
q <- qnorm(edf, 0, 1); q

# 표 4.3.
cbind(t, age_s, edf, q)
cbind(t, income_s, edf, q)

# sample quantile
plot(q, age_s)

# normalized sample quantile
plot(q, scale(age_s))

# 그림 4.3.
## normal Q-Q plot
qqnorm(age)
## normal Q-Q line
qqline(age)

# 그림 4.4.
## normal Q-Q plot
qqnorm(income)
## normal Q-Q line
qqline(income)


#####################################
##### chisquare plot for multivariate normality

# 데이터 입력





















