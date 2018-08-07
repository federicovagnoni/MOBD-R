source("data_understanding.R")
source("data_preparation.R")
source("fscore_extraction.R")
source("split.R")
source("data_normalization.R")
library("caret")
library(dplyr)
library(randomForest)
library(PredPsych)



my_dir <- getwd() 
data_calcio <- read.csv(paste(my_dir,"training_R.csv",sep="/"),sep = ";")
data_summary <- data_understanding(data_calcio)
dataset <- data_preparation(data_calcio, 35)

my_split <- split(dataset)
normalized_split <- data_normalization(my_split)


control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="rf", metric=metric, trControl=control)


results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

print(fit.rf)



fit.rf <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method="rf", metric=metric, trControl=control)
rf

summary(dataset)
top_attributes <- fscore_extraction(dataset, 60)
my_split <- split(dataset)
normalized_split <- data_normalization(my_split)

for (i in seq(10,59, by = 5)) {
  set.seed(456)
  control <- trainControl(method="cv", number=5)
  fit.glm <- train(y = normalized_split$label_train, x = normalized_split$scaled_training[,top_attributes[1:i]], method="svmLinear", metric=metric, trControl=control)
  print(i)
  print(fit.glm[["results"]][["Accuracy"]])
  summary(fit.glm)
}

for (i in seq(20,60, by = 5)) {
  set.seed(456)
  normalized_split$scaled_training[,top_attributes[1:i]]
  rf <- train(y = normalized_split$label_train, x = normalized_split$scaled_training[,top_attributes[1:i]], method="rf", metric=metric, trControl=control)
  # rf <- randomForest(y = normalized_split$label_train, x = normalized_split$scaled_training[,top_attributes], ntree = 500, mtry = 2)
  # print(i)
  # print(rf)
  normalized_split$scaled_test[,top_attributes]
  predictions <- predict(rf, normalized_split$scaled_test[,top_attributes])
  r <- confusionMatrix(predictions, normalized_split$label_test)
  print(i)
  print(r[["overall"]])
}

control <- trainControl(method="cv", number=5)
rf_grid <- expand.grid(mtry = c(2,3,4), ntree = c(500,600,700))
for (i in 5:10) {
  rf <- randomForest(label ~ ., data = my_split$training_set, ntree = 500, mtry = i)
  predictions <- predict(rf, my_split$training_set)
  r <- confusionMatrix(predictions, my_split$training_set$label)
  print(i)
  print(r)
  predictions <- predict(rf, my_split$test_set)
  r <- confusionMatrix(predictions, my_split$test_set$label)
  print("sul test")
  print(r)
}

control <- trainControl(method="cv", number=5)
rf <- train(label ~ ., data = my_split$training_set, method="rf", metric=metric, trControl=control)

predictions <- predict(rf, my_split$test_set)
r <- confusionMatrix(predictions, my_split$test_set$label)
print(r)

