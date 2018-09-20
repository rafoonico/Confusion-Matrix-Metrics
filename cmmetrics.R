#' Confusion Matrix
#'
#' This function creates the confusion matrix. Apart from the most material you will find in the literature, this confusion matrix is composed as it follows:
#'           predict
#'trueValues  0   1
#'         0 TN  FP
#'         1 FN  TP
#' @param predict a vector with the predicted results.
#' @param trueValues the true values observed for the response variable.
#' @keywords accuracy
#' @export
#' @examples
#' cm()

cm = function(predict,trueValues){
  as.matrix(table(trueValues, predict))
}

#' Accuracy function
#'
#' This function calculates the accuracy of your classifier. It's expressed by 'sum(main diagonal) / sum(whole matrix)' (on binary cases, '(TP + TN)/(TP + FP + TN + FN)').
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords accuracy
#' @export
#' @examples
#' ACCfun()

ACCfun = function(matrix){
  sum(diag(matrix))/sum(matrix)
}

#' Specificity function
#'
#' This function calculates the specificity (a.k.a. true negative rate) of your classifier. It's expressed by 'sum(true negatives) / sum(true negatives and false positives)' (on binary cases, 'TN / (TN + FP)').
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the specificity. By default, in binary cases, the row will be the first one.
#' @keywords specificity true negative rate
#' @export
#' @examples
#' PREfun()

SPEfun = function(matrix,rowIndex=1){
  sum(matrix[rowIndex,rowIndex])/sum(matrix[rowIndex,])
}

#' Precision function
#'
#' This function calculates the precision of your classifier. It's expressed by 'true positive / sum(true and false positives)' (on binary cases, 'TP / (FP + TP)').
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the precision. By default, in binary cases, the row will be the last one.
#' @keywords precision
#' @export
#' @examples
#' PREfun()

PREfun = function(matrix,rowIndex=nrow(matrix)){
  matrix[rowIndex,rowIndex]/sum(matrix[,rowIndex])
}

#' Recall function
#'
#' This function calculates the recall (a.k.a. sensitivity or true positive rate) score of your classifier. It's expressed by 'true positive / sum(true positives and false negatives)' (on binary cases, 'TP / (TP + FN)')
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the recall. By default, in binary cases, the row will be the last one.
#' @keywords recall sensitivity true positive rate
#' @export
#' @examples
#' RECfun()

RECfun = function(matrix,rowIndex=nrow(matrix)){
  matrix[rowIndex,rowIndex]/sum(matrix[rowIndex,])
}

#' F1 Score function
#'
#' This function calculates the F1 score of your classifier. It's expressed by the harmonic mean between 'precision' and 'recall' scores: '2 x Precision x Recall / (Precision + Recall)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the F1 Score. By default, in binary cases, the row will be the last one.
#' @keywords F1 Score
#' @export
#' @examples
#' F1fun()

F1fun = function(matrix,rowIndex=nrow(matrix)){
  pre=matrix[rowIndex,rowIndex]/sum(matrix[,rowIndex])
  rec=matrix[rowIndex,rowIndex]/sum(matrix[rowIndex,])
  return(2*pre*rec/(pre+rec))
}

#' False negative rate function
#'
#' This function calculates the false negative rate (a.k.a. type I error) of your classifier. It's expressed by 'sum(false negatives in a row) / sum(whole line)' (on binary cases, 'FN / (FN + TP)').
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the false negative rate. By default, in binary cases, the row will be the last one.
#' @keywords false negative rate error type I
#' @export
#' @examples
#' FNRfun()

FNRfun = function(matrix,rowIndex=nrow(matrix)){
  sum(matrix[rowIndex,-rowIndex])/sum(matrix[rowIndex,])
}

#' False positive rate function
#'
#' This function calculates the false positive rate (a.k.a. type II error) of your classifier. It's expressed by 'sum(false positives in each row) / sum(whole lines)' (on binary cases, 'FP / (FP + TN)').
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @param rowIndex the index of the row you wish to calculate the false positive rate. By default, in binary cases, the row will be the last one.
#' @keywords false positive rate error type II
#' @export
#' @examples
#' FPRfun()

FPRfun = function(matrix,rowIndex=nrow(matrix)){
  m=as.matrix(matrix[-rowIndex,])
  return(sum(m[row(m)!=col(m)])/sum(m))
}



