library(rpart)
library(rpart.plot)

dataset = read.csv('playTennis.csv')


classifier = rpart(
  Class ~ ., 
  data = dataset,
  method = 'class',
  minsplit = 2
)


rpart.plot(classifier, extra = 101)


y_pred = predict(classifier, newdata = dataset[-1], type = 'class')