library(rpart)

train_set = read.csv('BostonHouseTrain.csv')
test_set = read.csv('BostonHouseTest.csv')

classifier = rpart(
  class ~ ., 
  data = train_set,
  method = 'class',
  minsplit = 2,
  cp = 0 
)

y_pred_train = predict(classifier, type = 'class')
y_pred_test = predict(classifier, newdata = test_set, type = 'class')

cm_test = table(test_set[, 15], y_pred_test)
cm_train = table(train_set[, 15], y_pred_train)

plot(classifier)
text(classifier, use.n = TRUE, pretty = TRUE)

library(ElemStatLearn)assertthat
set = test_dataset

