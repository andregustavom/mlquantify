#' Threshold selection method
#'
#' It quantifies events based on testing scores, applying the X method (Forman, 2006).
#' Same as T50, but set the threshold where (\code{1} - \code{tpr}) = \code{fpr}.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @param TprFpr a \code{data.frame} of true positive (\code{tpr}) and false positive
#' (\code{fpr}) rates estimated on training set, using the function
#' \code{getTPRandFPRbyThreshold()}.
#' @return the class distribution in the test set.
#' @references Forman, G. (2006, August). Quantifying trends accurately despite classifier
#' error and class imbalance. In Proceedings of the 12th ACM SIGKDD international conference
#' on Knowledge discovery and data mining (pp. 157-166).<doi.org/10.1145/1150402.1150423>.
#' @usage X(test, TprFpr)
#' @export
#' @examples
#' library(randomForest)
#' library(caret)
#' cv <- createFolds(aeAegypti$class, 3)
#' tr <- aeAegypti[cv$Fold1,]
#' validation <- aeAegypti[cv$Fold2,]
#' ts <- aeAegypti[cv$Fold3,]
#'
#' # -- Getting a sample from ts with 80 positive and 20 negative instances --
#' ts_sample <- rbind(ts[sample(which(ts$class==1),80),],
#'                    ts[sample(which(ts$class==2),20),])
#' scorer <- randomForest(class~., data=tr, ntree=500)
#' scores <- cbind(predict(scorer, validation, type = c("prob")), validation$class)
#' TprFpr <- getTPRandFPRbyThreshold(scores)
#' test.scores <- predict(scorer, ts_sample, type = c("prob"))
#' X(test=test.scores[,1], TprFpr=TprFpr)

X <- function(test, TprFpr){

  x_sim <- cbind(round(1-TprFpr[,2],2), round(TprFpr[,3],2))

  idx_thr <- order(abs(x_sim[,1]-x_sim[,2]))[1]

  thr <- TprFpr[idx_thr,1]

  x <- as.data.frame(t(as.numeric(TprFpr[idx_thr,c(2,3)])))

  colnames(x) <- c("tpr", "fpr")

  TprFpr <- x

  dC <- CC(test, thr)

  result <- (dC[[1]] - TprFpr$fpr) / (TprFpr$tpr - TprFpr$fpr)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")

  return(result)
}
