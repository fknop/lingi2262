# Load Data
load('project.RData')
train_set = trainSet
test_set = testSet

# Categorical values

## Train set
to_convert = sapply(train_set, is.factor)
tmp = sapply(train_set[, to_convert], unclass)
train_set = cbind(train_set[, !to_convert], tmp)

## Test set

to_convert = sapply(test_set, is.factor)
tmp = sapply(test_set[, to_convert], unclass)
test_set = cbind(test_set[, !to_convert], tmp)

# Missing Data

## Train set

for (i in 1:ncol(train_set)) {
  train_set[, i] = ifelse(
                      is.na(train_set[, i]),
                      ave(train_set[, i], FUN = function(x) mean(x, na.rm = TRUE)),
                      train_set[, i]
                    )
  
  #train_set[is.na(train_set[, i]), i] = mean(train_set[, i], na.rm = TRUE)
}

## Test set

for (i in 1:ncol(test_set)) {
  test_set[is.na(test_set[, i]), i] = mean(test_set[, i], na.rm = TRUE)
}

# Removing near zero variance feature
library(caret)
columns_near_zero_var = nearZeroVar(train_set)
train_set = train_set[, -columns_near_zero_var]

# Feature scaling

## Train set
train_set = scale(train_set)

## Test set
test_set = scale(test_set)


# Split train set into validation set with 30% ratio for the validation set
library(caTools)
split = sample.split(1:nrow(train_set), SplitRatio = 3/10)
validation_set = subset(train_set, split == TRUE)
validation_labels = subset(trainLabels, split == TRUE)
final_train_set = subset(train_set, split == FALSE)
train_labels = subset(trainLabels, split == FALSE)


