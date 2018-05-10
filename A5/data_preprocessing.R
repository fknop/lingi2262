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

fix_missing_data = function(set) {
  to_remove = c()
  for (i in 1:ncol(set)) {
    
    na = is.na(set[, i])
    if (sum(na) == nrow(set)) {
      to_remove = c(to_remove, i)
    }
    else {
      set[, i] = ifelse(
        na,
        ave(set[, i], FUN = function(x) mean(x, na.rm = TRUE)),
        set[, i]
      )
    }
  }
  
  set = set[, -to_remove]
  
  set
}

train_set = fix_missing_data(train_set)
test_set = fix_missing_data(test_set)


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


