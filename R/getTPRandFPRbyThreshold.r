#' Estimates true and false positive rates
#'
#' This function provides the true and false positive rates (\code{tpr} and \code{fpr}) for a range of thresholds.
#' @param validation_scores \code{data.frame} scores estimated from the training set.
#' It should be comprised of three columns (1. positive scores; 2. negative
#' scores; 3.class).
#' @param label_pos numeric value or factor indicating the positive label.
#' @param thr_range a numerical \code{vector} of thresholds, ranged between 0 and 1. Default:
#' \code{seq(0.01,0.99,0.01)}.
#' @return \code{data.frame} where each row has both (\code{tpr} and \code{fpr}) rates for
#' each threshold value. This function varies the threshold from 0.01 to 0.99 with
#' increments 0.01.
#' @usage getTPRandFPRbyThreshold(validation_scores, label_pos = 1, thr_range = seq(0.01,0.99,0.01))
#' @author Everton Cherman <evertoncherman@gmail.com>
#' @author Andre Maletzke <andregustavom@gmail.com>
#' @examples
#' library(randomForest)
#' library(caret)
#' cv <- createFolds(aeAegypti$class, 2)
#' tr <- aeAegypti[cv$Fold1,]
#' validation <- aeAegypti[cv$Fold2,]
#' scorer <- randomForest(class~., data=tr, ntree=500)
#' scores <- cbind(predict(scorer, validation, type = c("prob")), validation$class)
#' TprFpr <- getTPRandFPRbyThreshold(scores)
#' @export
getTPRandFPRbyThreshold <- function(validation_scores, label_pos = 1, thr_range = seq(0.01,0.99,0.01)){

  TprFpr = NULL
  for (threshold in thr_range)
  {
    total_positive <- length(which(validation_scores[,3]==label_pos))
    total_negative <- length(which(validation_scores[,3]!=label_pos))
    fp <- length(which(validation_scores[,1]>threshold & validation_scores[,3]!=label_pos))
    tp <- length(which(validation_scores[,1]>threshold & validation_scores[,3]==label_pos))
    tpr <- tp/total_positive
    fpr <- fp/total_negative

    threshold <- format(round(threshold, 2), nsmall = 2)
    TprFpr <- rbind(TprFpr,c(threshold,tpr,fpr))
  }

  TprFpr <- as.data.frame(TprFpr)
  names(TprFpr) <- c('thr','tpr','fpr')
  TprFpr <- apply(TprFpr, 2, as.numeric)
  return(TprFpr)
}
