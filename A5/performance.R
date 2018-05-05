compute_bcr = function(confusion_matrix) {
  TP = confusion_matrix[1]
  FP = confusion_matrix[2]
  TN = confusion_matrix[4]
  FN = confusion_matrix[3]
  
  ((TP / (TP + FN)) + (TN / (TN + FP))) / 2
}

write_results = function(confusion_matrix, predictions, filename = 'results.csv') {
  bcr = compute_bcr(confusion_matrix)
  bcr_frame = data.frame(bcr)
  rownames(bcr_frame) = c('BCR')
  colnames(bcr_frame) = c('Prediction')
  results = rbind(bcr_frame, predictions)
  write.csv(results, file = filename)
}