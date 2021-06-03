# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### Titanic 예제 1
install.packages("caret")
install.packages("tree")
library(caret)
library(tree)
library(e1071)

titanic <- read.csv("data/titanic.csv", header=T)
str(titanic)

titanic_all <- titanic[,c(1:3,5:8,10,12)]
head(titanic_all)

titanic_all$Survived <- as.factor(titanic_all$Survived)
titanic_all$Pclass <- as.factor(titanic_all$Pclass)
# titanic_all$Embarked <- as.factor(titanic_all$Embarked)
table(titanic_all$Survived)
set.seed(891)
titanic_part <- createDataPartition(y=titanic_all$Survived, p=0.75, list=F)

titanic_train <- titanic_all[titanic_part,]
head(titanic_train)
titanic_test <- titanic_all[-titanic_part,]
head(titanic_test)

tree_titanic <- tree(Survived~. , data=titanic_train)
plot(tree_titanic)
text(tree_titanic)
tree_titanic

tree_titanic_cv <- cv.tree(tree_titanic, FUN=prune.misclass)
plot(tree_titanic_cv)

titanic_trees <- prune.misclass(tree_titanic, best=8)
plot(titanic_trees)
text(titanic_trees)

titanic_pre <- predict(titanic_trees, titanic_test, type="class")
confusionMatrix(titanic_pre, titanic_test$Survived)

##### Titanic 예제 2
library(caret)
library(e1071)
library(rpart)

tree_titanic <- rpart(Survived~. , data=titanic_train, method="class")
plot(tree_titanic)
text(tree_titanic)

printcp(tree_titanic)
plotcp(tree_titanic)

titanic_tree_p <- prune(tree_titanic, cp=tree_titanic$cptable[which.min(tree_titanic$capable[,"xerror"])])
plot(titanic_tree_p)
text(titanic_tree_p)

tree_titanic_pre <- predict(titanic_tree_p, titanic_test, type="class")
confusionMatrix(tree_titanic_pre, titanic_test$Survived)

##### Titanic 예제 3
install.packages("party")
library(mvtnorm)
library(caret)
library(party)

head(titanic_all)
# character -> factor
titanic_all$Sex <- as.factor(titanic_all$Sex)
levels(titanic_all$Sex)
titanic_all$Embarked <- as.factor(titanic_all$Embarked)
levels(titanic_all$Embarked)

party_titanic <- ctree(Survived~. , data=titanic_all)
plot(party_titanic)

titanic_test$Sex <- as.factor(titanic_test$Sex)
levels(titanic_test$Sex)
titanic_test$Embarked <- as.factor(titanic_test$Embarked)
levels(titanic_test$Embarked)

party_titanic_pre <- predict(party_titanic, titanic_test)
confusionMatrix(party_titanic_pre, titanic_test$Survived)


#####################################
##### Kyphosis




































