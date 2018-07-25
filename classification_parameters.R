classification_parameters<-function(confusion_matrix){
  set.seed(3791)
  acc=(confusion_matrix[1,1]+confusion_matrix[2,2])/
    (confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2])
  
  aexp=((confusion_matrix[1,1]+confusion_matrix[2,1])*(confusion_matrix[1,1]+confusion_matrix[1,2]))/
    (confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2])
  
  dexp=((confusion_matrix[2,2]+confusion_matrix[2,1])*(confusion_matrix[2,2]+confusion_matrix[1,2]))/
    (confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2])
  
  Pexp=(aexp+dexp)/(confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2])
  
  kappa=(acc-Pexp)/(1-Pexp)
  
  tprate<-confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2])
  
  return(c(acc,kappa,tprate))
}