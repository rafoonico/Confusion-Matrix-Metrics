#' Confusion Matrix
#'
#' This function creates the confusion matrix as it follows:
#'         realValues
#'predicted  Positive Negative
#'  Positive       TP       FP
#'  Negative       FN       TN
#'
#' @param predicted a vector with the predicted results. As default, the values must be 0 for negative outcomes and 1 for positive outcomes. Otherwise, they must be specified with "diffValues=TRUE".
#' @param realValues the real values observed for the response variable. As default, the values must be 0 for negative outcomes and 1 for positive outcomes. Otherwise, they must be specified with "diffValues=TRUE".
#' @param diffValues if TRUE, the user have to declare which outcome is the positive one (i.e: the event of interest).
#' @param positiveIfDiff the positive outcome if 'diffValues=TRUE'
#' @keywords confusion matrix
#' @export
#' @examples
#' cm()

cm = function(predicted,realValues,diffValues = FALSE, positiveIfDiff){
  if(diffValues==FALSE && !(c(0,1) %in% predicted && c(0,1) %in% realValues)){
    stop("Values must be 0 or 1. Otherwise, you must declare 'diffValues=TRUE' and the positive value in 'positiveIfDiff'.")
  }else if(diffValues==TRUE){
    predicted <- factor(ifelse(predicted==positiveIfDiff,"Positive","Negative"),levels=c("Positive","Negative"))
    realValues <- factor(ifelse(realValues==positiveIfDiff,"Positive","Negative"),levels=c("Positive","Negative"))
    return(table(predicted,realValues))
  }else{
    predicted <- factor(ifelse(predicted==1,"Positive","Negative"),levels=c("Positive","Negative"))
    realValues <- factor(ifelse(realValues==1,"Positive","Negative"),levels=c("Positive","Negative"))
    return(table(predicted,realValues))
  }
}

#' Accuracy function
#'
#' This function calculates the accuracy of your classifier. It's expressed by '(TP + TN)/(TP + FP + TN + FN)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords accuracy
#' @export
#' @examples
#' ACCfun()

ACCfun = function(matrix){
  return(sum(diag(matrix))/sum(matrix))
}

#' Specificity function
#'
#' This function calculates the specificity (a.k.a. true negative rate) of your classifier. It's expressed by 'TN / (TN + FP)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords specificity true negative rate
#' @export
#' @examples
#' SPEfun()

SPEfun = function(matrix){
  return(matrix["Negative","Negative"]/sum(matrix[,"Negative"]))
}

#' Precision function
#'
#' This function calculates the precision of your classifier. It's expressed by 'TP / (FP + TP)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords precision
#' @export
#' @examples
#' PREfun()

PREfun = function(matrix){
  return(matrix["Positive","Positive"]/sum(matrix["Positive",]))
}

#' Recall function
#'
#' This function calculates the recall (a.k.a. sensitivity or true positive rate) score of your classifier. It's expressed by 'TP / (TP + FN)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords recall sensitivity true positive rate
#' @export
#' @examples
#' RECfun()

RECfun = function(matrix){
  return(matrix["Positive","Positive"]/sum(matrix[,"Positive"]))
}

#' F1 Score function
#'
#' This function calculates the F1 score of your classifier. It's expressed by the harmonic mean between 'precision' and 'recall' scores: '2 x Precision x Recall / (Precision + Recall)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords F1 Score
#' @export
#' @examples
#' F1fun()

F1fun = function(matrix){
  pre=matrix["Positive","Positive"]/sum(matrix["Positive",])
  rec=matrix["Positive","Positive"]/sum(matrix[,"Positive"])
  return(2*pre*rec/(pre+rec))
}

#' False negative rate function
#'
#' This function calculates the false negative rate (a.k.a. type I error) of your classifier. It's expressed by 'FN / (FN + TP)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords false negative rate error type I
#' @export
#' @examples
#' FNRfun()

FNRfun = function(matrix){
  return(sum(matrix["Negative","Positive"])/sum(matrix[,"Positive"]))
}

#' False positive rate function
#'
#' This function calculates the false positive rate (a.k.a. type II error) of your classifier. It's expressed by 'FP / (FP + TN)'.
#' @param matrix a confusion matrix with the data of your prediction versus realized.
#' @keywords false positive rate error type II
#' @export
#' @examples
#' FPRfun()

FPRfun = function(matrix){
  return(sum(matrix["Positive","Negative"])/sum(matrix[,"Negative"]))
}



