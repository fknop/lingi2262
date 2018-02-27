library(rpart)
library(rpart.plot)
library(caTools)
library()

number_of_nodes = function (tree) {
  length(rownames(tree$frame))
}

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

split = sample.split(1:nrow(train_set), SplitRatio = 1/4)
train_set2 = subset(train_set, split == TRUE)

classifier2 = rpart(
  class ~ .,
  data = train_set2,
  method = 'class',
  minsplit = 2,
  cp = 0
)

number_nodes = number_of_nodes(classifier2)
y_pred_test2 = predict(classifier2, newdata = test_set, type = 'class')

y_pred_train2 = predict(classifier2, type = 'class')

cm_test2 = table(test_set[, 15], y_pred_test2)
cm_train2 = table(train_set2[, 15], y_pred_train2)
print(number_nodes)
print(cm_train2)
print(cm_test2)

# Q8

random_sample = function(ratio) {
  split = sample.split(1:nrow(train_set), SplitRatio = ratio)
  subset(train_set, split == TRUE)
}

accuracy = function(cm) {
  sum(diag(cm)) / sum(cm)
}

test_all = function(element) {
  ratio = element[1]
  training_size = nrow(train_set) * ratio
  sample = random_sample(ratio)
  classifier = rpart(
    class ~ .,
    data = sample,
    method = 'class',
    minsplit = 2,
    cp = 0
  )
  

  y_pred = predict(classifier, newdata = test_set, type = 'class')
  cm = table(test_set[, 15], y_pred)
  
  
  c(
    number_of_nodes(classifier), 
    training_size,
    accuracy(cm)
  )
}

combinations = expand.grid(
  c(5/100, 10/100, 20/100, 50/100, 99/100), 
  1:10
)


results = apply(combinations, 1, test_all)

frame = data.frame(
  nodes = results[1,], 
  training_size = results[2,],
  accuracy = results[3,]
)

boxplot(
  nodes ~ training_size
  frame,
  xlab = 'Training Size',
  ylab = 'Number of nodes',
  type = 'b', 
  main = 'Nodes by training size'
)

#plot(frame)
#boxplot(frame)

#data.frame(
#  sample_index = 
#  train_size =
#  nodes = 
#  accuracy =
#)




