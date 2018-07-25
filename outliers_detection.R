outliers_detection <- function(attributes_names,dataset){
  set.seed(123)  
  #per ogni attributo vengono individuati gli outliers
    #utilizzando la funzione boxplot
    atb <- dataset[[attributes_names]]
    outlier <- boxplot(atb, plot=F)$out
    n_outlier <- length(outlier)
    perc_outliers <- round(length(outlier)/nrow(dataset),4)
    
    output <- list(n_outlier,outlier,perc_outliers)
    names(output) <- c("number_of_outliers","outliers","percentage_of_outliers")
    return(output)

}