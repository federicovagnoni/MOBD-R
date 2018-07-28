#TR=dati di training
#YTR=classe dati di training
#TS=dati di test
#YTS=classe dati di test
#questo classificatore si trova nel package LibLineaR
opt_classifier <- function(TR,YTR,TS,YTS){
  set.seed(123)
  #addestramento utilizzando la migliore c individuata mediante cross validation
  linear_model<-LiblineaR(data=TR,target=YTR,type=1,cost=0.1,bias=TRUE,verbose=FALSE) #
  #predizione sui dati di test
  test_prediction <- predict(linear_model,TS,decisionValues = TRUE)
  #matrice di confusione sui dati di test
  confusion_matrix<-table(predicted=test_prediction$predictions,observation=YTS)
  #nella matrice di confusione:
  #elemento [i,j] classe predetta i classe reale j
  accuracy <- round((confusion_matrix["1","1"]+confusion_matrix["0","0"])/
                      nrow(TS),4)
  
  
  TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
  TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)

  return(c(accuracy,TPR_A,TPR_B))
}