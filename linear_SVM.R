library("psych")
#TR=dati di training
#YTR=classe dati di training
#TS=dati di test
#YTS=classe dati di test
#questo classificatore si trova nel package LibLineaR
linear_SVM <- function(TR,YTR,TS,YTS){
  set.seed(123)
  #addestramento
  linear_model<-LiblineaR(data=TR,target=YTR,type=1,cost=1,bias=TRUE,verbose=FALSE) #
  #predizione sui dati di test
  test_prediction <- predict(linear_model,TS,decisionValues = TRUE)
  #matrice di confusione sui dati di test
  confusion_matrix<-table(predicted=test_prediction$predictions,observation=YTS)
  #nella matrice di confusione:
  #elemento [i,j] classe predetta i classe reale j
  print(confusion_matrix)

  accuracy <- round((confusion_matrix["1","1"]+confusion_matrix["0","0"])/nrow(TS),4)
  TPR_1    <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
  TPR_0    <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)
  #Kappa    <- cohen.kappa(confusion_matrix, n.obs=nrow(TS))
  
  return(c(accuracy,TPR_1,TPR_0))
}