
rand = function(i) {
  sample(1:100, 1)
}

k = 20
r = 100

test = function (i) {
  results = sapply(1:k, rand)
  mean(results)
}
 

results = sapply(1:r, test)

x <- seq(0, 100, by = .1)
y <- dnorm(x, mean=mean(results), sd=sd(results))
plot(x, y, type="l", lwd=1, main = paste("k = " , k))