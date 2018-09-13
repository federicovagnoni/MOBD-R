source("data_understanding.R")
source("data_preparation.R")
source("fscore_extraction.R")
source("split.R")
source("data_normalization.R")
library("caret")
library(dplyr)
library(randomForest)
library(PredPsych)


# Carichiamo il dataset ed effettuiamo le operazioni preliminare di analisi e preparazione

my_dir <- getwd() 
dataset <- read.csv(paste(my_dir,"training_R.csv",sep="/"),sep = ";")
data_summary <- data_understanding(dataset)
dataset <- data_preparation(dataset)


# Scegliamo il classificatore migliore
# molti di questi, basandosi su funzioni di distanza o similarità, sono sensibili a valori con scale differenti
# pertanto, il dataset verrà normalizzato.

# prepariamo training e test set per poi normalizzarli
my_split <- split(dataset)
normalized_split <- data_normalization(my_split)

# Impostiamo la metodologia di training come "Cross-Validation" 5-fold
control <- trainControl(method="cv", number=5)
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
# Random Forest
# L'algoritmo delle RandomForest è invariante rispetto a eventuale scaling o altre trasformazioni di dati
# pertanto, il suo addestramento si baserà sul training set non mutato.
set.seed(7)
fit.rf <- train(label ~ ., data = my_split$training_set, method="rf", metric=metric, trControl=control)

# Collezioniamo i dati e presentiamoli in una tabella riassuntiva
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, rf=fit.rf))
summary(results)

# Notiamo a questo punto che l'algoritmo delle RandomForest presenta i risultati migliori rispetto agli altri metodi


# Estraiamo a questo punto una classifica delle feature che presentano in ordine decrescente
# il più alto F-Score. E' stato notato in diversi paper che tale operazione di feature selection risulta
# essere determinante per ottenere una classificazione più accurata
top_attributes <- fscore_extraction(dataset)

# Rimpiazziamo inoltre una delle top features che presentano un elevato numero di outliers
dataset$num_imgs <- replace_outlier(dataset$num_imgs)


# Procediamo quindi allo split di training e test set
my_split <- split(dataset)


# Si verifica ora, l'accuratezza delle RandomForest aumentando il numero delle feature da 20 a 60, con passo di 5
for (i in seq(20,60, by = 5)) {
  set.seed(7)
  normalized_split$scaled_training[,top_attributes[1:i]]
  rf <- train(y = normalized_split$label_train, x = normalized_split$scaled_training[,top_attributes[1:i]], method="rf", metric=metric, trControl=control)

  # Verifica l'efficacia sul test set
  predictions <- predict(rf, normalized_split$scaled_test[,top_attributes])
  r <- confusionMatrix(predictions, normalized_split$label_test)
  print(i)
  print(r[["overall"]])
}


# Si determina a questo punto che il miglior risultato è ottenuto tramite l'utilizzo di tutte le feature.
# Ciononostante è allo stesso modo importante osservare come il tempo necessario all'operazione di classificazione
# lo sia allo stesso modo. Il miglior rapporto tempo-accuratezza è ottenuto tramite l'utilizzo delle top-35 features.
# Eseguiamo quindi iil training con cross-validation (5-fold)

set.seed(7)
control <- trainControl(method="cv", number=5)
metric <- "Accuracy"
rf_tot <- train(label ~ ., data = my_split$training_set[,c(top_attributes[1:35],"label")], method="rf", metric=metric, trControl=control)


# Verifichiamo l'efficacia sul test set
# il test set viene inserito in questo punto
# ATTENZIONE: utilizzare le sole top-35 features tra quelle disponibili
predictions <- predict(rf_tot, my_split$test_set[,c(top_attributes[1:35],"label")])
r <- confusionMatrix(predictions, my_split$test_set$label)
print(r)

