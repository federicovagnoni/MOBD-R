split <- function(my_data){
  set.seed(123)
  #le istanze vengono raggruppate in base al valore della classe
  #per ognuno dei gruppi così creato si procede allo split training test
  output <- by(my_data, as.factor(my_data$label),function(x){
    smp_size <- floor(0.70 * nrow(x))
    train_ind <- sample(seq_len(nrow(x)), size = smp_size)
    train <- x[train_ind, ]
    test <- x[-train_ind, ]
    my_split <- list(train,test)
    names(my_split) <- c("train","test")
    return(my_split)
  })
  training_set <- rbind(output[[1]]$train,output[[2]]$train)
  test_set <- rbind(output[[1]]$test,output[[2]]$test)
  
  #shuffle row-wise
  training_set <- training_set[sample(nrow(training_set)),]
  test_set <- test_set[sample(nrow(test_set)),]
  
  data_split <- list(training_set,test_set)
  names(data_split) <- c("training_set","test_set") 
  return(data_split)
}