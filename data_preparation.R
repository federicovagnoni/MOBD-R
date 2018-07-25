data_preparation <- function(my_data){
  set.seed(123)
  source("replace_outlier.R")
  #rimuoviamo i duplicati
  my_data <- my_data[!duplicated(my_data),]
  
  #separiamo le istanze dalla colonna contenente la classe
  my_data <- mutate(my_data, shares = ifelse(shares > 1400, 1, 0))
  
  data_instances <- my_data[,1:(ncol(my_data)-1)]
  label <- my_data[,ncol(my_data)]
  
  #rimuoviamo gli attributi nome squadra 1 e nome squadra 2
  #dal momento che sono di tipo character e per convertirli in numerici dovremmo aggiungere
  #molti altri attributi (lezione Matteo conversione da nominal a numeric).
  #nominal_attribute <- which(lapply(data_instances,value)=="factor")
  data_instances <- data_instances[,-1]
  
  #remove or replace missing value
  #prima osservazione:Dall'analisi del summary di ogni attributo notiamo che la feature 
  #recupero.palla.per.fine.azione.2 presenta 33 valori mancanti pertanto piuttosto che 
  #sostituire tali valori eliminiamo
  #questo attributo e per simmetria anche recupero.palla.per.fine.azione.1
  #recupero_palla_fine_azione <- c(which(colnames(data_instances)=="recupero.palla.per.fine.azione.1",arr.ind = T),
  #                                which(colnames(data_instances)=="recupero.palla.per.fine.azione.2",arr.ind = T))
  #data_instances <- data_instances[,-recupero_palla_fine_azione]
  

  #esempio sostituzione di valori mancanti con la media
  #data_instances <- as.data.frame(lapply(data_instances, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  #)

  #Replace outliers: 
  #analizzando la lista degli ouliers per attributo. In un primo momento, decidiamo
  #di sostituire solo quelli inerenti all'attributo minuti di gioco (per il quale è semplice certificare l'anomalia
  #del dato)
  #data_instances$minuti.di.gioco.1T <- replace_outlier(data_instances$minuti.di.gioco.1T)
  my_df <- cbind(data_instances,label)
  names(my_df)[ncol(my_df)] <- "label"
  return(my_df)
}