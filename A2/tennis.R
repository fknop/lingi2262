library(rpart)

dataset = read.csv('playTennis.csv')


classifier = rpart(
  Class ~ ., 
  data = dataset,
  method = 'class',
  minsplit = 2
)


plot(classifier)
text(classifier, use.n = TRUE, pretty = TRUE)

y_pred = predict(classifier, newdata = dataset[-1], type = 'class')