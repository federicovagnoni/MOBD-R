replace_outlier <- function(x){
  set.seed(123)
  qnt <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  benchmark1 <- (qnt[1]-(qnt[2]-qnt[1])*1.5)
  benchmark2 <- (qnt[2]+(qnt[2]-qnt[1])*1.5)
  output <- sapply(x,function(y){
    if(y < benchmark1){
      y <- benchmark1
    } else if (y > benchmark2){
      y <- benchmark2
    } else {
      y <- y
    }
    return(y)
  })
  
}
