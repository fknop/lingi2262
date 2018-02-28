library(rpart)
library(rpart.plot)
library(caTools)

set.seed(1)
number_of_nodes = function (tree) {
  length(rownames(tree$frame))
}

train_set = read.csv('BostonHouseTrain.csv')
test_set = read.csv('BostonHouseTest.csv')

classifier = rpart(
  class ~ . - X, 
  data = train_set,
  method = 'class',
  minsplit = 2,
  cp = 0 
)

y_pred_train = predict(classifier, newdata = train_set, type = 'class')
y_pred_test = predict(classifier, newdata = test_set, type = 'class')

cm_test = table(test_set[, 15], y_pred_test)
cm_train = table(train_set[, 15], y_pred_train)

rpart.plot(classifier, type= 0, tweak=2, extra = 0, fallen.leaves = FALSE)
text(classifier, use.n = TRUE, pretty = TRUE)


# Q7

split = sample.split(1:nrow(train_set), SplitRatio = 1/4)
train_set2 = subset(train_set, split == TRUE)

classifier2 = rpart(
  class ~ . - X,
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

test_all = function(element, prune = FALSE) {
  
  
  ratio = element[[1]]
  training_size = nrow(train_set) * ratio
  
  cp = ifelse(prune, 0.02, 0)
  minsplit = ifelse(prune, ifelse(training_size < 50, 5, 15), 2)
  
  sample = random_sample(ratio)
  classifier = rpart(
    class ~ . - X,
    data = sample,
    method = 'class',
    minsplit = minsplit,
    cp = cp
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
  nodes ~ training_size,
  frame,
  xlab = 'Training Size',
  ylab = 'Number of nodes',
  main = 'Nodes by training size'
)

boxplot(
  accuracy ~ training_size,
  frame,
  xlab = 'Training Size',
  ylab = 'Accuracy',
  main = 'Nodes by training size'
)

plot(
  x = factor(frame$training_size, labels = c('5%', '10%', '20%', '50%', '99%')),
  y = frame$nodes,
  frame,
  xlab = 'Training Size',
  ylab = 'Number of nodes',
  type = 'p',
  main = 'Nodes by training size'
)

plot(
  x = factor(frame$training_size, labels = c('5%', '10%', '20%', '50%', '99%')),
  y = frame$accuracy,
  xlab = 'Training Size',
  ylab = 'Accuracy',
  main = 'Accuracy by training size'
)

# Q9

results_q9 = apply(combinations, 1, test_all, prune = TRUE)

frame_q9 = data.frame(
  nodes = results_q9[1,], 
  training_size = results_q9[2,],
  accuracy = results_q9[3,]
)

boxplot(
  nodes ~ training_size,
  frame_q9,
  xlab = 'Training Size',
  ylab = 'Number of nodes',
  main = 'Nodes by training size'
)

boxplot(
  accuracy ~ training_size,
  frame_q9,
  xlab = 'Training Size',
  ylab = 'Accuracy',
  main = 'Nodes by training size'
)

plot(
  x = factor(frame_q9$training_size, labels = c('5%', '10%', '20%', '50%', '99%')),
  y = frame_q9$nodes,
  frame_q9,
  xlab = 'Training Size',
  ylab = 'Number of nodes',
  type = 'p',
  main = 'Nodes by training size'
)

plot(
  x = factor(frame_q9$training_size, labels = c('5%', '10%', '20%', '50%', '99%')),
  y = frame_q9$accuracy,
  xlab = 'Training Size',
  ylab = 'Accuracy',
  main = 'Accuracy by training size'
)


# Q10
# Wat

# Q11


generate_train_set = function () {
  s = sample.int(nrow(train_set), replace = TRUE)
  train_set[s,]
}

generate_sample_tree = function (unused) {
  classifier = rpart(
    class ~ . - X,
    data = generate_train_set(),
    method = 'class',
    control = rpart.control(maxdepth = 2)
  )
}

predict_tree = function(tree) {
  y_pred = predict(tree, newdata = test_set, type = 'class')
  y_pred
}

max_value = function(row) {
  names(which.max(table(t(row))))
}

forest_size = 500

trees = lapply(1:forest_size, generate_sample_tree)

predictions = data.frame(lapply(trees, predict_tree))
colnames(predictions) = 1:10

final_predictions = apply(predictions, 1, max_value)
cm = table(test_set[, 15], final_predictions)
print(cm)
