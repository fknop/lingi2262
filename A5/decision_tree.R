# Decision Tree classifier
library(rpart)

tmp = final_train_set
tmp$Prediction = train_labels

classifier = rpart(formula = Prediction ~ ., 
                   data = tmp, method = 'class', minsplit = 20, cp = 1)
y_pred = predict(classifier, newdata = validation_set, type = 'class')

cm = table(y_pred, validation_labels)


test_pred = predict(classifier, newdata = test_set, type = 'class')
predictions_test = data.frame(test_pred)
rownames(predictions_test) = rownames(test_set)
colnames(predictions_test) = c('Prediction')
write_results(cm, predictions_test, filename = 'naivebayes.csv')