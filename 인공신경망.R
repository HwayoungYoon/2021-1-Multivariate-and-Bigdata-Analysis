# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 인공신경망
library("nnet")
data(iris)
head(iris)
dim(iris)

species_ind <- class.ind(iris$Species)
head(species_ind)
iris2 <- cbind(iris, species_ind)
head(iris2)

train_idx <- sample(1:150,100)
iris_train_df <- iris2[train_idx,]
iris_test_df <- iris2[-train_idx,]

iris_ann <- nnet(x=iris_train_df[,c(1:4)], y=iris_train_df[,c(6:8)],
                 size=10, softmax=T)

iris_pred <- predict(iris_ann, iris_train_df[,c(1:4)], type="class")
table(iris_pred, iris_train_df$Species)

library(devtools)
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")

library(reshape2)
plot.nnet(iris_ann, rep="best")

library(caret)
iris_pred <- predict(iris_ann, iris_test_df[,c(1:4)], type="class")
confusionMatrix(factor(iris_pred), iris_test_df$Species)
