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
# Classification Tree with rpart
library(rpart)
data("kyphosis")
head(kyphosis)
str(kyphosis)
attach(kyphosis)
table(Kyphosis)

# grow tree
fit <- rpart(Kyphosis ~ Age+Number+Start, method="class", data=kyphosis)
## display the result
printcp(fit)
## visualize cross-validation results
plotcp(fit)
## detailed summary of splits
summary(fit)

# plot tree
plot(fit, uniform=T, main="Classification Tree for Kyphosis")
text(fit, use.n=T, cox=0.8)

# treee with prob
install.packages("partykit")
library(partykit)
rparty.tree <- as.party(fit)
rparty.tree
plot(rparty.tree)


#####################################
##### Random Forest
install.packages("randomForest")
library(randomForest)

train.idx <- sample(1:nrow(kyphosis), 50)
train.set <- kyphosis[train.idx,]
test.set <- kyphosis[-train.idx,]

r <- randomForest(Kyphosis ~ Age+Number+Start, data=train.set, 
                  importance=T, do.trace=100, ntree=100)
print(r)
plot(r)

predictions <- predict(r, test.set)
table(test.set$Kyphosis, predictions)

par(mfrow=c(3,2))
partialPlot(r, train.set, Age, "absent", sub="absent")
partialPlot(r, train.set, Age, "present", sub="present")
partialPlot(r, train.set, Number, "absent", sub="absent")
partialPlot(r, train.set, Number, "present", sub="present")
partialPlot(r, train.set, Start, "absent", sub="absent")
partialPlot(r, train.set, Start, "present", sub="present")
par(mfrow=c(1,1))

treesize(r)
hist(treesize(r))

attributes(r)
# getTree(r, 1, labelVar=T)

library(devtools)
devtools::install_github("skinner927/reprtree")

library(reprtree)

# random forest best model tree diagram
reprtree:::plot.getTree(r)


