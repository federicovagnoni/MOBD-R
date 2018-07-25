data_understanding <- function(my_data){
  set.seed(123)
  type_attribute <- lapply(my_data, class)
  data_rows <- nrow(my_data)
  data_cols <- ncol(my_data)
  #duplicati: numero e percentuale
  n_duplicate <- sum(duplicated(my_data))
  perc_duplicate <- round(n_duplicate/data_rows,4)
  #missing value: numero complessivo e percentuale
  n_complete <- sum(complete.cases(my_data))
  n_incomplete <- nrow(my_data)-n_complete
  perc_incomplete <- 1-(round((n_complete/data_rows),4))
  #inseriamo questi parametri in una matrice
  aggregate_information <- rbind(n_duplicate,perc_duplicate,n_incomplete,perc_incomplete)
  rownames(aggregate_information) <- c("number of duplicates","percentage of duplicate","number of incomplete instances","percentage of incomplete")
  
  #summary degli attributi
  summary_attribute <- lapply(my_data, summary)
  
  #valori anomali per ogni attributo numerico
  numeric_attribute <- names(Filter(is.numeric,my_data))
  source("outliers_detection.R")
  outlier_list <- lapply(numeric_attribute,outliers_detection,dataset=my_data)
  names(outlier_list) <- numeric_attribute

  output <- list(aggregate_information,type_attribute,summary_attribute,outlier_list)
  names(output) <- c("aggregate_information","attribute class","summary_attribute","outlier_list")
  return(output)
}



