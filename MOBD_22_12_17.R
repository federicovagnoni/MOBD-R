set.seed(123)
library("dplyr")
library("LiblineaR")
library("e1071")
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
new_summary_attribute <- lapply(cleaned_data,summary)
#dal momento che le classi non sono perfettamente bilanciate
#è preferibile utilizzare uno split training-test bilanciato 
my_split <- split(cleaned_data)
normalized_split <- data_normalization(my_split)
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

