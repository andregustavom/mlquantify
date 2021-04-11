#' Threshold selection method
#'
#' It quantifies events based on testing scores, applying MAX method, according to
#' Forman (2006). Same as T50, but it sets the threshold where \code{tpr}–\code{fpr}
#' is maximized.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @param TprFpr a \code{data.frame} of true positive (\code{tpr}) and false positive (\code{fpr})
#' rates estimated on training set, using the function \code{getTPRandFPRbyThreshold()}.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @references Forman, G. (2006, August). Quantifying trends accurately despite classifier
#' error and class imbalance. In Proceedings of the 12th ACM SIGKDD international conference
#' on Knowledge discovery and data mining (pp. 157-166).<doi.org/10.1145/1150402.1150423>.
#' @usage MAX(test, TprFpr)
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
#' MAX(test=test.scores[,1], TprFpr=TprFpr)

MAX <- function(test, TprFpr){

  TprFpr <- as.data.frame(t(TprFpr[which.max(abs(TprFpr[,2]-TprFpr[,3])),]))

  result <- CC(test, TprFpr$thr)[1]

  if((TprFpr$tpr - TprFpr$fpr)== 0){
    warning("The difference (tpf - fpr) is zero. CC was applied.")
  }else{
    result <- (result - TprFpr$fpr) / (TprFpr$tpr - TprFpr$fpr)
  }

  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)
}
