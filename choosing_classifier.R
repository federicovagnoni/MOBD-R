choosing_classifier <- function(my_list){
  set.seed(123)
  #Supponiamo di voler provare tre classificatori: 
  #SVM con Kernel lineare,SVM con kernel gaussiano,
  #e una SVM con kernel polinomiale utilizzando la 
  #cross validation sui dati di training per individuare il miglior 
  #classificatore

  n_classifiers <- 1:3
  n_folds <- 10
  n_folds
  folds_i <- sample(rep(1:n_folds, length.out = nrow(my_list$scaled_training)))
  
  #creiamo una lista contenente 3 matrici, in ognuna delle quali salveremo 
  #accuracy e percentuale di istanze classificate correttamente per ogni classe 
  #per ogni run della cross validation (righe della matrice = run della croos validation,
  #colonne = 3 parametri)
  
  classification_parameter <- lapply(n_classifiers,function(n_classifiers)matrix(0, nrow = n_folds, ncol = 3))
  names(classification_parameter) <- c("Linear_SVM","Gaussian_SVM","Polinomial_SVM")
  for (k in 1:n_folds) {
    # per ogni run associamo al test_i il k-esimo pacchetto
    test_i <- which(folds_i == k)
    train <- my_list$scaled_training[-test_i, ]
    test <- my_list$scaled_training[test_i, ]
    
    label_train <- my_list$label_train[-test_i]
    label_test <- my_list$label_train[test_i]
    
    classification_parameter$Linear_SVM[k,] <- linear_SVM(train,label_train,test,label_test)
    classification_parameter$Gaussian_SVM[k,] <- gaussian_SVM(train,label_train,test,label_test)
    classification_parameter$Polinomial_SVM[k,] <- polinomial_SVM(train,label_train,test,label_test)
    print(classification_parameter)    
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