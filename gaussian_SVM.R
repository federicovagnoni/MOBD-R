#TR=dati di training
#YTR=classe dati di training
#TS=dati di test
#YTS=classe dati di test
#questo classificatore si trova nel package e1071
gaussian_SVM <- function(TR,YTR,TS,YTS){
  set.seed(123)
  #addestramento
  gaussian_model<-svm(x=TR,y=YTR,scale=F,type="C-classification",kernel="radial") #
  #predizione sui dati di test
  test_prediction <- predict(gaussian_model,TS)
  #matrice di confusione sui dati di test
  confusion_matrix<-table(predicted=test_prediction,observation=YTS)
  #nella matrice di confusione:
  #elemento [i,j] classe predetta i classe reale j
  accuracy <- round((confusion_matrix["1","1"]+confusion_matrix["0","0"])/
                      nrow(TS),4)

  
  TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
  TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)
  
  return(c(accuracy,TPR_1,TPR_0))
}