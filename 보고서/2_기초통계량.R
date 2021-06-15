# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")
my_data <- my_data[,c(1,2,3,7,4,8,9,10,11,5,12,13,6)]


##########################################################################

attach(my_data)

# 빈도표
library(summarytools)
view(freq(sex))

# 기초통계량
library(psych)
describeBy(my_data[,c(1,3:13)])

# 히스토그램
win.graph()
par(mfrow=c(3,4))
par(mar=c(3.1,4.1,2.1,2.1))
hist(age, col="lightyellow", xlab="")
hist(education, col="lightyellow", xlab="")
hist(income, col="lightyellow", xlab="")
hist(social.safety, col="lightyellow", xlab="")
hist(restrictions, col="lightpink", xlab="")
hist(health, col="lightpink", xlab="")
hist(healthcare, col="lightpink", xlab="")
hist(stress, col="lightpink", xlab="")
hist(environment.feel, col="lightblue", xlab="")
hist(environmental.cost, col="lightblue", xlab="")
hist(environmental.issues, col="lightblue", xlab="")
hist(prevention.pollution, col="lightblue", xlab="")

# 상자그림
win.graph()
par(mfrow=c(3,1))
par(mar=c(3.1,12.1,2.1,2.1))
boxplot(my_data[,c(1:3,8,13)], col="lightyellow",  horizontal=T, las=2, 
        names=c("age","sex","education","social.safety","income"))
boxplot(my_data[,4:7], col="lightpink",  horizontal=T, las=2, 
        names=c("restrictions","health","healthcare","stress"))
boxplot(my_data[,9:12], col="lightblue", horizontal=T, las=2, 
        names=c("environment.feel","environmental.cost","environmental.issues","prevention.pollution"))


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
