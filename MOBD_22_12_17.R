set.seed(123)
library("dplyr")
library("LiblineaR")
library("e1071")
library("FSelector")
library("caret")
library("mlbench")
library(mRMRe)
library(PredPsych)
library(randomForest)
source("data_understanding.R")
source("data_preparation.R")
source("split.R")
source("data_normalization.R")
source("choosing_classifier.R")
source("linear_SVM.R")
source("gaussian_SVM.R")
source("polinomial_SVM.R")
source("setting_linear_SVM.R")
source("c_linear_SVM.R")
source("opt_classifier.R")
my_dir <- getwd() 
data_calcio <- read.csv(paste(my_dir,"training_R.csv",sep="/"),sep = ";")
data_summary <- data_understanding(data_calcio)
cleaned_data <- data_preparation(data_calcio)
#train <- sample(nrow(cleaned_data), 4000, replace = FALSE)
#cleaned_data <- cleaned_data[train,]
# valid_set <- cleaned_data[-train,]

my_split <- split(cleaned_data)
normalized_split <- data_normalization(my_split)

new_summary_attribute <- lapply(cleaned_data,summary)



trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#train_set$label <- as.factor(train_set$label)
#valid_set$label <- as.factor(valid_set$label)

mtry <- sqrt(ncol(cleaned_data))

tuneGrid <- expand.grid(.mtry = mtry)

normalized_split$label_train <- as.factor(normalized_split$label_train)




#importance(results)


#model with all 60 parameters
results <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method = "rf", metric= "Accuracy", tuneGrid = NULL, trControl = trControl)
#results <- train(label~., data = cleaned_data, method = "rf", metric= "Accuracy", trControl = trControl, ntree=500)
print(results)

varImp(results)

index <- order(results$finalModel$importance, decreasing = TRUE)
resul <- index[1:20]


#model with 25 best features
results_2 <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method = "rf", metric= "Accuracy", tuneGrid = NULL, trControl = trControl)
print(results_2)



#model with 20 best features
results_3 <- train(y = normalized_split$label_train, x = normalized_split$scaled_training, method = "rf", metric= "Accuracy", tuneGrid = NULL, trControl = trControl)
print(results_3)



datat <- normalized_split$scaled_training[,resul]

model <- randomForest(y = normalized_split$label_train, x = datat, ntree = 500)
print(model)


predValid <- predict(results$finalModel, normalized_split$scaled_test, type = "class")
mean(predValid == normalized_split$label_test)                    
confusion_matrix <- table(predValid,normalized_split$label_test)

accuracy <- round((confusion_matrix["1","1"]+confusion_matrix["0","0"])/
                    nrow(normalized_split$scaled_test),4)


TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)

print(accuracy)
print(TPR_0)
print(TPR_1)




#dal momento che le classi non sono perfettamente bilanciate
#è preferibile utilizzare uno split training-test bilanciato 

#supponiamo di aver individuato (mediante ricerca in letteratura tre classificatori adatti al nostro 
#problema). Utilizziamo quindi la cross-validation sui dati di training per scegliere il/i migliori.
cross_validation_output <- choosing_classifier(normalized_split) 
cross_validation_output
#il classificatore che sembra risultare più robusto è una Linear SVM (nonostante la SVM con kernel 
#polinomiale presenti un accuratezza media più alta il TPR relativo alla classe B è zero)
#Una volta scelto il classificatore possiamo procedere con il tuning dei parametri modificando ad esempio
#la c del kernel lineare oppure utilizzando un vettore di pesi per penalizzare in modo diverso gli errori rispetto alle tre classi
c_setting <-setting_linear_SVM(normalized_split)
c_setting
#A seguito della modifica della c decidiamo di utilizzare c=0.1 per le successive prove
#una volta ottimizzate le performance del classificatore si può procedere al check dei risultati 
#sul test set che fino a questo punto non era mai stato utilizzato
test_result <- opt_classifier(normalized_split$scaled_training,normalized_split$label_train,normalized_split$scaled_test,normalized_split$label_test)

