library(class)
y_pred = knn(train = final_train_set, test = validation_set, cl = train_labels, k = 5)