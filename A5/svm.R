# SVM classifier
library(e1071)

svm_classifier = svm(x = final_train_set,
                     y = train_labels,
                     type = 'C-classification',
                     kernel = 'linear',
                     cost = 1)


svm_y_pred = predict(svm_classifier, newdata = validation_set)

svm_cm = table(svm_y_pred, validation_labels)
compute_bcr(svm_cm)


svm_test_pred = predict(svm_classifier, newdata = test_set)
svm_predictions_test = data.frame(svm_test_pred)
rownames(svm_predictions_test) = rownames(test_set)
colnames(svm_predictions_test) = c('Prediction')
write_results(svm_cm, svm_predictions_test, filename = 'svm.csv')