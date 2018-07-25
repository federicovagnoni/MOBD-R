setting_linear_SVM <- function(my_list){
  set.seed(123)
  #Supponiamo di voler provare tre diversi valori di c
  c_vector <- c(10,1,0.1)
  n_cost <- 1:3
  n_folds <- 10
  folds_i <- sample(rep(1:n_folds, length.out = nrow(my_list$scaled_training)))
  
  #definiamo una lista contenente 3 matrici, in ognuna delle quali salveremo 
  #accuracy e percentuale di istanze classificate correttamente per ogni classe 
  #per ogni run della cross validation (righe della matrice = run della cross validation,
  #colonne = 4 parametri)
  
  classification_parameter <- lapply(n_cost,function(n_classifiers)matrix(0, nrow = n_folds, ncol = 3))
  names(classification_parameter) <- c("c_10","c_1","c_0.1")
  for(j in 1:length(c_vector)){
    for (k in 1:n_folds) {
    # per ogni run associamo al test_i il k-esimo pacchetto
    test_i <- which(folds_i == k)
    train <- my_list$scaled_training[-test_i, ]
    test <- my_list$scaled_training[test_i, ]
    
    label_train <- my_list$label_train[-test_i]
    label_test <- my_list$label_train[test_i]
    
    classification_parameter[[j]][k,] <- c_linear_SVM(train,label_train,test,label_test,c_vector[j])
     
    }
    }
  #per ogni matrice con i risultati,
  #sostituiamo eventuali nan con 0 e calcoliamo la media per ciascuno dei parametri
  
  mean_parameter <- lapply(classification_parameter, function(x){
    x <- replace(x, is.na(x), 0)
    parameter_mean <- apply(x,2,mean)
    return(parameter_mean)
  })
  my_output <- do.call("rbind",mean_parameter)
  colnames(my_output) <- c("Avg_Accuracy","Avg_TPR1","Avg_TPR0")
  return(my_output) 
}