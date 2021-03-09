########## 1.1
x11 <- c(3,4,2,6,8,2)
x12 <- c(5,5,4,7,10,5)
x1 <- cbind(x11, x12)
x1

# 1.1.a
m1 = colMeans(x1)
m1
mean(x1)

# 1.1.b
var(x11)
var(x12)

# 1.1.c
cv1 = cov(x1)
cv1

# 1.1.d
cr1 = cor(x1)
cr1

# 1.1.e
plot(x1, xlab="X1", ylab="X2")

# 1.1.f
dist(x1[c(1,3),], method="euclidean")

# 1.1.g
D1 <- rbind(c(cv1[1,1], 0), c(0, cv1[2,2]))
ds1 <- t(x1[1,]-x1[3,]) %*% solve(D1) %*% (x1[1,]-x1[3,])
ds1 <- sqrt(ds1)
ds1

# 1.1.h
dm1 <- t(x1[1,]-x1[3,]) %*% solve(cv1) %*% (x1[1,]-x1[3,])
dm1 <- sqrt(dm1)
dm1

# 1.1.i
library(car)
dataEllipse(x11, x12, xlim=c(1,8), ylim=c(1,10), xlab="X1", ylab="X2")


########## 1.2
x21 <- c(9,2,6,5,8)
x22 <- c(10,8,6,4,10)
x23 <- c(3,4,0,2,1)
x2 <- cbind(x21, x22, x23)
x2

# 1.2.a
m2 = colMeans(x2)
m2
mean(x2)

# 1.2.b
var(x21)
var(x22)
var(x23)

# 1.2.c
cv2 = cov(x2)
cv2

# 1.2.d
cr2 = cor(x2)
cr2

# 1.2.e
x.2 <- as.data.frame(x2)
names(x.2) = c("국어", "영어", "수학")
plot(x.2)

# 1.2.f
dist(x2[2:3,], method="euclidean")

# 1.2.g
D2 <- rbind(c(cv2[1,1], 0, 0), c(0, cv2[2,2], 0), c(0, 0, cv2[3,3]))
ds2 <- t(x2[2,]-x2[3,]) %*% solve(D2) %*% (x2[2,]-x2[3,])
ds2 <- sqrt(ds2)
ds2

# 1.2.h
dm2 <- t(x2[2,]-x2[3,]) %*% solve(cv2) %*% (x2[2,]-x2[3,])
dm2 <- sqrt(dm2)
dm2
