predict = function (x, w) {
  intercept = w[1]
  intercept + as.matrix(x) %*% w[2:length(w)]
}

# Compute the square loss with the estimated values and the observed value.
squareloss = function (y, predicted) {
  sum ((predicted - y) ^ 2)
}

# Split a dataset in a train and test set 50/50
split = function(dataset) {
  samples = sample(1:nrow(dataset), nrow(dataset) / 2, replace = FALSE)
  train = dataset[samples,]
  test = dataset[-samples,]
  list(train = train, test = test)
}

# "Main function"
# Split the bodyfat dataset
# Build a linear model with Weight, Abdomen and Biceps on the train set
# Predict the values on the test set
# Compute the square loss
multi_linear = function() {
  sets = split(bodyfat)
  train = sets$train
  test = sets$test
  linear = lm(BFI ~ Weight + Abdomen + Biceps, data=train)
  x = test[, c('Weight', 'Abdomen', 'Biceps')]
  y = test[, 'BFI']
  predicted = predict(x, linear$coefficients)
  
  list(
    squareloss = squareloss(y, predicted), 
    y = y, 
    x = x,
    predicted = predicted, 
    test = test,
    train = train
  )
}

# Plot the predicted and observed values.
plot_model = function(y, predicted) {
  plot(y, predicted, xlab='Actual BFI', ylab='Predicted BFI')
  abline(0, 1, col = 'blue')
}


bodyfat = read.csv('bodyfat.csv', row.names = 1)

# First part
linearmodel = lm(BFI ~ Weight + Abdomen + Biceps, data=bodyfat)
w = linearmodel$coefficients

x = bodyfat[, c('Weight', 'Abdomen', 'Biceps')]
y = bodyfat[, 'BFI']
predicted = predict(x, w)

plot_model(y, predicted)

all_squareloss = squareloss(y, predicted)


# Second part
multi = multi_linear()

plot_model(multi$y, multi$predicted)


max = 100
losses = sapply(1:max, function(ignored) { multi_linear()$squareloss })
total_squareloss = sum(losses)
average_squareloss = total_squareloss / max


print(all_squareloss / nrow(bodyfat))
print(average_squareloss / (nrow(bodyfat) / 2))
