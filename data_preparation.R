data_preparation <- function(my_data){
  set.seed(123)
  source("replace_outlier.R")
  
  #rimuoviamo eventuali duplicati
  my_data <- my_data[!duplicated(my_data),]
  
  #separiamo le istanze dalla colonna contenente la classe e convertiamole in una rappresentazione binaria
  my_data <- mutate(my_data, shares = ifelse(shares > 1400, 1, 0))
  
  data_instances <- my_data[,1:(ncol(my_data)-1)]
  label <- my_data[,ncol(my_data)]
  
  
  data_instances <- data_instances[,-1]
  
  my_df <- cbind(data_instances,as.factor(label))
  names(my_df)[ncol(my_df)] <- "label"
  
 
  return(my_df)
}