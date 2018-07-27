library("FSelector")
data("iris")

iris
weidhts <- relief(Species~.,iris,neighbours.count = 5, sample.size = 20)
subset <- cutoff.k(weidhts, 2)
f <- as.simple.formula(subset, "Species")
