fscore_extraction <- function(dataset, numtop) {
  fscores <- fscore(dataset, classCol = 60, featureCol = c(1:59))
  print(fscores)
  index <- order(fscores, decreasing = TRUE)
  result <- fscores[index]
  topattributes <- names(result)[1:numtop]
  return(topattributes)
}