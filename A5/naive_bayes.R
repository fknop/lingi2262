# Naive Bayes classifier
library(e1071)

classifier = naiveBayes(x = final_train_set, y = train_labels)
y_pred = predict(classifier, newdata = validation_set)

cm = table(y_pred, validation_labels)


test_pred = predict(classifier, newdata = test_set)
predictions_test = data.frame(test_pred)
rownames(predictions_test) = rownames(test_set)
colnames(predictions_test) = c('Prediction')
write_results(cm, predictions_test, filename = 'naivebayes.csv')