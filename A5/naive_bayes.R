# Naive Bayes classifier
library(e1071)

classifier = naiveBayes(x = final_train_set, y = train_labels)
y_pred = predict(classifier, newdata = validation_set)


cm = table(y_pred, validation_labels)