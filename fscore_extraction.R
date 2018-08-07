# Ritorna il nome delle feature in ordine decrescente di importanza secondo il Fisher Score
fscore_extraction <- function(dataset, numtop) {
  fscores <- fscore(dataset, classCol = 60, featureCol = c(1:59))
  index <- order(fscores, decreasing = TRUE)
  result <- fscores[index]
  topattributes <- names(result)
  return(topattributes)
}