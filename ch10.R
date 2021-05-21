# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 10.1 G1 3개 G2 3개

# 데이터 입력
group <- c(1,1,1,2,2,2)
x1 <- c(1,0,2,-3,-2,-1)
x2 <- c(5,3,4,5,7,6)
cbind(group, x1, x2) 

plot(x1, x2, pch=c("1","1","1","2","2","2"))
# abline(intercept, slope) 
abline(26.015/4.67, 5.33/4.67)

# linear discriminat analysis
library(MASS)
ld <- lda(group ~ x1+x2, prior=c(0.5, 0.5)); ld 

# quadratic discriminat analysis
qd <- qda(group ~ x1+x2, prior=c(0.5, 0.5)); qd 

dat <- cbind(group, x1, x2) 

# ld prediction
pc <- predict(ld, data.frame(dat[,2:3])); pc
pc$class
round(pc$posterior, digits=3) 

m <- ld$scaling[[1]]*mean(x1)+ld$scaling [[2]]*mean(x2); m
ld$scaling[[1]]*mean(x1) # 0.529
ld$scaling [[2]]*mean(x2) # 4.635 

ld$scaling[[1]]*x1+ld$scaling[[2]]*x2-m
# -1.059*x1 + 0.927*x2 -m > : 0 G2
# -1.059*x1 + 0.927*x2 -m <= : 0 G1 

pc$x 

ct <- table(group, pc$class); ct 

# total percent correct
correct.per <- sum(diag(ct))/sum(ct); correct.per
# error percent
err.per <- 1-correct.per; err.per 

# qd prediction
qc <- predict(qd, data.frame(dat[,2:3])); qc
qc$class
round(qc$posterior, digits=3)
ct <- table(group, pc$class); ct 

# total percent correct
correct.per <- sum(diag(ct))/sum(ct); correct.per
# error percent
err.per <- 1-correct.per; err.per 

newx <- c(0,-1,-2)
new <- data.frame(rbind(dat, newx)); new 

result <- predict(ld, newdata=new)
round(result$posterior, digits=3) 

# new data result
n <- nrow(dat)
round(result$posterior[(n+1),], digits=3) 

# data.frame
result <- predict(qd, newdata=new)
round(result$posterior, digits=3) 

# multivariate normality test
install.packages("mvnormtest")
library(mvnormtest)
mshapiro.test(t(dat[,2:3])) 

# homogeniety of cov test
install.packages("biotools")
library(biotools)
boxM(dat[,2:3], dat[,1]) 


#####################################
##### 예제 10.4 discriminant analysis

# 데이터 입력
skull <- read.csv("data/skull.csv", header=T)
head(skull)
attach(skull)

n <- dim (skull)[[1]]; n
x <- skull[,2:5]; x 

# Test Multivariate Normality
# multivariate normality test
library(mvnormtest)
mshapiro.test(t(skull[,2:5])) 

# homogeniety of cov test
library(biotools)
boxM(skull[,c("x1","x2","x3","x4")], skull[,c("year")]) 

# linear discriminat analysis
library (MASS)
ld <- lda(year ~ x1+x2+x3+x4, data=skull); ld
pc <- predict(ld, skull)$class; pc
table(year, pc) 

# par("mar")
# par(mar=c(2.4,3,1.5,1))
win.graph()
plot(ld, dimen=2)

res <- cbind(year, pc)
# match
correct <- res[(year==pc),]
correct.rate <- dim(correct)[[1]]/n; correct.rate
error.rate <- 1-correct.rate; error.rate 


#####################################
#####  with CV: leave-one-out cross validation
ldc <- lda (year ~ x1+x2+x3+x4, data=skull, CV=TRUE, prior=c(1/2,1/2))
names(ldc)
results <- data.frame(year, ldc$class, ldc$posterior)
results[1:10,] 

# Summarize crossvalidation
class.table <- table(year, ldc$class); class.table 

# One could make the summary of the classifications nicer by writing a function
# return a list object type
summarize.class <- function(original, classify) {
  class.table <- table(original, classify)
  numb <- rowSums(class.table)
  prop <- round(class.table/numb, 4)
  list(class.table=class.table, prop=prop)
} 

summarize.class(original=year, classify=ldc$class) 


#####################################
##### Simple histogram by the grouping variable
ldahist(data=x1, g=year, type="histogram")
ldahist(data=x2, g=year, type="density")
ldahist(data=x3, g=year, type="both")
ldahist(data=x4, g=year) 


#####################################
##### Quadratic discriminant analysis
# qd <- qda(x, year, data=skull)
qd <- qda(x, year, data=skull, CV=TRUE); qd
qc <- qd$class 

table(year, qc) 

resq <- cbind(year, qc); resq 

# match
correctq <- resq[(year==qc),]
correctq.rate <- dim(correctq)[[1]]/n; correctq.rate
errorq.rate <- 1-correctq.rate; errorq.rate


#####################################
##### 연습문제 10.1

# 데이터 입력
library(MASS)
group <- c(1,1,1,2,2,2)
x1 <- c(-2,0,-1,0,2,1)
x2 <- c(5,3,1,6,4,2)
dat <- data.frame(cbind(group, x1, x2)); dat

# linear discriminat analysis
ld <- lda(group ~ x1+x2); ld
pc <- predict(ld, dat)
attributes(pc)
pc$class
pc$posterior
pc$x

cc <- apply(ld$means %*% ld$scaling, 2, mean); cc

newx <- c(0,-1,2)
dat <- rbind(dat, newx)
newx[-1] %*% ld$scaling

pc <- predict(ld, dat[7,])
pc$posterior

# ld$scaling[[1]] x1 + ld$scaling[[2]] x2 = cc

win.graph()
plot(x1, x2, pch=group)
intercept <- cc/ld$scaling[[2]]
slope <- -ld$scaling[[1]]/ld$scaling[[2]]
abline(intercept, slope)

# partition plot
library(klaR)
partimat(factor(group) ~ x2+x1, data=dat, method="lda")


#####################################
##### 연습문제 10.2

# 데이터 입력
library(MASS)
group <- c(1,1,1,2,2,2,3,3,3)
x1 <- c(-2,0,-1,0,2,1,1,0,-1)
x2 <- c(5,3,1,6,4,2,-2,0,4)
dat <- data.frame(cbind(group, x1, x2)); dat

# group meanvector and covariance
g1 <- dat[(dat$group==1),]; g1
g2 <- dat[(dat$group==2),]; g2
g3 <- dat[(dat$group==3),]; g3
colMeans(g1[,2:3])
colMeans(g2[,2:3])
colMeans(g3[,2:3])
cov(g1[,2:3])
cov(g2[,2:3])
cov(g3[,2:3])

# Using Cross Validation for posterior
ld.cv <- lda(group ~ x1+x2, prior=c(0.25,0.25,0.5), cv=TRUE)
ld.cv$class
ct <- table(group, ld.cv$class); ct
round(ld.cv$posterior, digits=3)

# total percent correct
correct.per <- sum(diag(prop.table(ct))); correct.per
# error percent
err.per <- 1-correct.per; err.per

# Using ld function
## linear discriminat
ld <- lda(group ~ x1+x2, prior=c(0.25,0.25,0.5)); ld
pc <- predict(ld, dat[,2:3]); pc
pc$class
round(pc$posterior, digits=3)

ct <- table(group, pc$class); ct
diag(prop.table(ct, 1))

# total percent correct
correct.per <- sum(diag(ct))/sum(ct); correct.per
# error percent
err.per <- 1-correct.per; err.per

# groupwise mean
ld$means
# LD function coeff
ld$scaling
# (3*2)(2*2): groupwise ld1, ld2
cc <- apply(ld$means %*% ld$scaling, 2, mean); cc

newx <- c(0,-1,-2)
new <- data.frame(rbind(dat, newx)); new

result <- predict(ld, newdata=new[10,])
round(result$posterior, digits=3)

# # ld value check ==> do later
# n <- nrow(dat)
# for (i in 1:n){
#   group.ld <- (ld$means %*% ld$scaling)-rbind(result$x[i,], result$x[i,], result)
#   print(group.ld)
#   temp <- apply((group.ld)^2, 1, sum) # case 6=>2 ??
#   print(i)
#   print(temp)
# }

win.graph()
plot(x1, x2, pch=group)
# scatter plot with 1st two discriminant dimensions
plot(ld)
plot(ld, dimen=1, type="both")

# partition plot
library(klaR)
partimat(factor(group) ~ x1+x2, data=dat, method="lda")
partimat(factor(group) ~ x1+x2, data=dat, method="qda")

pairs(dat[c("x1","x2")], main="my data with 3 groups", pch=22, 
      bg=c("red","yellow","blue"))

# quadratic discriminat analysis
## linear discriminat
qd <- qda(group ~ x1+x2, prior=c(0.25,0.25,0.5)); qd
qd$scaling
pc <- predict(qd, dat); pc
pc$class
round(pc$posterior, digits=3)

ct <- table(group, pc$class); ct
diag(prop.table(ct, 1))

# total percent correct
correct.per <- sum(diag(ct))/sum(ct); correct.per
# error percent
err.per <- 1-correct.per; err.per

newx <- c(0,-1,-2)
new <- data.frame(rbind(dat, newx)); new

# new data
result <- predict(qd, newdata=new[10,])
round(result$posterior, digits=3)
