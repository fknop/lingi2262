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


