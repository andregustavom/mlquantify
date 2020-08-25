#' Threshold selection method
#'
#' It quantifies events based on testing scores, applying T50 method proposed by
#' Forman (2006). It sets the decision threshold of Binary Classifier where
#' \code{tpr} = 50\%.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @param TprFpr a \code{data.frame} of true positive (\code{tpr}) and false positive
#' (\code{fpr}) rates estimated on training set, using the function
#' \code{getTPRandFPRbyThreshold()}.
#' @return the class distribution in the test set.
#' @references Forman, G. (2006, August). Quantifying trends accurately despite
#' classifier error and class imbalance. In Proceedings of the 12th ACM SIGKDD
#' international conference on Knowledge discovery and data mining (pp. 157-166).<doi.org/10.1145/1150402.1150423>.
#' @usage T50(test, TprFpr)
#' @export
#' @examples
#' library(randomForest)
#' library(caret)
#' cv <- createFolds(aeAegypti$class, 3)
#' tr <- aeAegypti[cv$Fold1,]
#' validation <- aeAegypti[cv$Fold2,]
#' ts <- aeAegypti[cv$Fold3,]
#' # -- Getting a sample from ts with 80 positive and 20 negative instances --
#' ts_sample <- rbind(ts[sample(which(ts$class==1),80),],
#'                    ts[sample(which(ts$class==2),20),])
#' scorer <- randomForest(class~., data=tr, ntree=500)
#' scores <- cbind(predict(scorer, validation, type = c("prob")), validation$class)
#' TprFpr <- getTPRandFPRbyThreshold(scores)
#' test.scores <- predict(scorer, ts_sample, type = c("prob"))
#' T50(test=test.scores[,1], TprFpr=TprFpr)

T50 <- function(test, TprFpr){

  TprFpr <- as.numeric(TprFpr[which.min(abs(as.numeric(TprFpr[,2])-0.5)),])
  dC <- CC(test, TprFpr[1])
  result <- (dC[1] - TprFpr[3])/(TprFpr[2] - TprFpr[3])
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)

}
