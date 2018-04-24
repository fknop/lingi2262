library(rpart)
library(rpart.plot)
library(caTools)

set.seed(1)
number_of_nodes = function (tree) {
  length(rownames(tree$frame))
}

train_set = read.csv('BostonHouseTrain.csv')
test_set = read.csv('BostonHouseTest.csv')



random_sample = function(set, ratio) {
  split = sample.split(1:nrow(set), SplitRatio = ratio)
  subset(set, split == TRUE)
}

accuracy = function(cm) {
  sum(diag(cm)) / sum(cm)
}

# 4.1

test_sample = random_sample(test_set, 50)
train_sample = random_sample(train_set, 1/10)

classifier = rpart(
  class ~ . - X, 
  data = train_sample,
  method = 'class',
  minsplit = 2,
  cp = 0 
)

y_pred_test = predict(classifier, newdata = test_sample, type = 'class')

cm_test = table(test_sample[, 15], y_pred_test)

rpart.plot(classifier, type= 0, tweak=2, extra = 0, fallen.leaves = FALSE)


acc = accuracy(cm_test)
alpha = 0.05
norm = qnorm(alpha / 2)

sigma = acc * (1 - acc) / 50

interval = c(acc - sqrt(sigma), acc + sqrt(sigma))

# 4.2

folds = lapply(1:100, function(i) {
  random_sample(test_set, 50)
})

accuracies = unlist(lapply(folds, function(fold) {
  y_pred_test = predict(classifier, newdata = fold, type = 'class')
  cm_test = table(fold[, 15], y_pred_test)
  accuracy(cm_test)
}))

mean_acc = mean(accuracies)
