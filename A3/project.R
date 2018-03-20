library(e1071)
library(caTools)

accuracy = function (cm) {
  sum(diag(cm)) / sum(cm)
}



train_set = read.csv('LettersTrain.csv')
test_set = read.csv('LettersTest.csv')
validation_set = read.csv('LettersValid.csv')


without_feature = c(1, ncol(train_set))

# Removing near zero variance feature
library(caret)
columns_near_zero_var = nearZeroVar(train_set[-without_feature])
train_set = train_set[, -columns_near_zero_var]

# train_set size might have changed
without_feature_train = c(1, ncol(train_set))
without_feature_test = c(1, ncol(test_set))

# Scaling feature
train_set[-without_feature_train] = scale(train_set[-without_feature_train])
test_set[-without_feature_test] = scale(test_set[-without_feature_test])
validation_set[-without_feature_test] = scale(validation_set[-without_feature_test])


# Meaning of cost (C-constant): https://stats.stackexchange.com/questions/31066/what-is-the-influence-of-c-in-svms-with-linear-kernel
classifier = svm(formula = labels ~ . - X,
                 data = train_set,
                 type = 'C-classification',
                 kernel = 'linear',
                 cost = 1)

y_train_pred = predict(classifier, newdata = train_set)
y_validation_pred = predict(classifier, newdata = validation_set)
y_test_pred = predict(classifier, newdata = test_set)

cm_train = table(train_set[, ncol(train_set)], y_train_pred)
cm_test = table(test_set[, ncol(test_set)], y_test_pred)
cm_validation = table(validation_set[, ncol(validation_set)], y_validation_pred)

print(accuracy(cm_test))
print(accuracy(cm_train))
print(accuracy(cm_validation))

random_sample = function(ratio) {
  split = sample.split(1:nrow(train_set), SplitRatio = ratio)
  subset(train_set, split == TRUE)
}

COST = 1
KERNEL = 'radial'
DEGREE = 3

validation_accuracy = function (element) {

  ratio = element[[1]]
  if (ratio == 1) {
    training_set = train_set
  }
  else {
    training_set = random_sample(ratio)
  }

 
   
  classifier = svm(
    formula = labels ~ .- X,
    data = training_set,
    type = 'C-classification',
    kernel = KERNEL,
    cost = COST,
    degree = DEGREE
  )
  

  y_validation_pred = predict(classifier, newdata = validation_set)
  cm_validation = table(validation_set[, ncol(validation_set)], y_validation_pred)
  
  
  c(ratio, accuracy(cm_validation))
}


test_accuracy = function (element) {
  
  ratio = element[[1]]
  if (ratio == 1) {
    training_set = train_set
  }
  else {
    training_set = random_sample(ratio)
  }
  
  
  
  classifier = svm(
    formula = labels ~ .- X,
    data = training_set,
    type = 'C-classification',
    kernel = KERNEL,
    cost = COST,
    degree = DEGREE
  )
  
  
  y_test_pred = predict(classifier, newdata = test_set)
  cm_test = table(test_set[, ncol(test_set)], y_test_pred)
  
  
  c(ratio, accuracy(cm_test))
}

training_sizes = c(20/100, 40/100, 60/100, 80/100, 100/100)

results = apply(expand.grid(training_sizes, 1:10), 1, validation_accuracy)


frame = data.frame(
  x = results[1,],
  y = results[2,]
)

colnames(frame) = c("x", "y")

means = aggregate(. ~ x, frame, mean)

results_test = apply(expand.grid(training_sizes, 1:10), 1, test_accuracy)
frame_test = data.frame(
  x = results_test[1,],
  y = results_test[2,]
)

colnames(frame_test) = c("x", "y")

means_test = aggregate(. ~ x, frame_test, mean)


library(plotly)
font = list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

title = "Validation Set Accuracy"
xaxis = list(title = "Training set ratio", titlefont = font, dtick = 0.1)
yaxis = list(title = "Accuraccy", titlefont = font)

# box plot
p = plot_ly(
  frame, x = ~x, y = ~y, 
  type = "box", 
  boxpoints = "all"
) %>%
  layout(
    title = title,
    xaxis = xaxis,
    yaxis = yaxis
  )
p

# learning curve
p2 = plot_ly(means, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>% 
  layout(
    title = title,
    xaxis = xaxis,
    yaxis = yaxis
)
p2 # show the plot


# box plot test
p3 = plot_ly(
  frame_test, x = ~x, y = ~y, 
  type = "box", 
  boxpoints = "all"
) %>%
  layout(
    title = title,
    xaxis = xaxis,
    yaxis = yaxis
  )
p3

# learning curve test
p4 = plot_ly(means_test, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>% 
  layout(
    title = title,
    xaxis = xaxis,
    yaxis = yaxis
  )
p4 # show the plot
