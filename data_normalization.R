#my_data= lista contenete training e test ottenuti 
#utilizzando la funzione split

data_normalization <- function(my_data){
  data_train <- my_data$training_set[,1:(ncol(my_data$training_set)-1)]
  label_train <- my_data$training_set[,ncol(my_data$training_set)]
  
  data_test <- my_data$test_set[,1:(ncol(my_data$test_set)-1)]
  label_test <- my_data$test_set[,ncol(my_data$test_set)]
  
  scaled_training <- scale(data_train,center = T,scale = T)
  scaled_test <- scale(data_test,attr(scaled_training,"scaled:center"),attr(scaled_training,"scaled:scale"))
  
  output <- list(scaled_training,label_train,scaled_test,label_test)
  names(output) <- c("scaled_training","label_train","scaled_test","label_test")
  return(output)
}