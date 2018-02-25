library(rpart)
library(rpart.plot)
library(caTools)

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

rpart.plot(classifier, type= 0, tweak=2, extra = 0, fallen.leaves = FALSE)
text(classifier, use.n = TRUE, pretty = TRUE)


# Q7

split = sample.split(train_set$class, SplitRatio = 0.25)
train_set2 = subset(train_set, split == TRUE)

classifier2 = rpart(
  class ~ .,
  data = train_set2,
  method = 'class',
  minsplit = 2,
  cp = 0
)

y_pred_test2 = predict(classifier2, newdata = test_set, type = 'class')

cm_test2 = table(test_set[, 15], y_pred_test2)
print(cm_test2)

# TODO

# Q8