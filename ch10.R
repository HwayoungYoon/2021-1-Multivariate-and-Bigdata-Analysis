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
skull <- read.csv("skull.csv", header=T)
head(skull)
attach(skull) 

n <- dim (skull)[[1]]; n
x <- skull[,2:5]; x 

# Test Multivariate Normality
# multivariate normality test
library(mvnormtest)
mshapiro.test(t(skull[,2:5])) 

# homogenie
library(biotools)
boxM(skull[,c("x1","x2", "X3","x4")], skull[,c("year")]) 

# linear discriminat analysis
library (MASS)
ld <- lda(year ~ x1+x2+x3+x4, data=skull); ld
pc <- predict(ld, skull)$class; pc
table(year, pc) 

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
ldahist(data=x2, g=year, type="densitsqrt(0.5)y")
ldahist(data=x3, g=year, type="both")
ldahist (data=x4, g=year) 


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



























