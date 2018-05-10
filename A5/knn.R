library(class)
y_pred = knn(train = final_train_set, test = validation_set, cl = train_labels, k = 6)


cm = table(y_pred, validation_labels)

compute_bcr(cm)

svm_test_pred = predict(svm_classifier, newdata = test_set)
svm_predictions_test = data.frame(svm_test_pred)
rownames(svm_predictions_test) = rownames(test_set)
colnames(svm_predictions_test) = c('Prediction')
write_results(svm_cm, svm_predictions_test, filename = 'svm.csv')