#' Adjusted Classify and Count
#'
#' It quantifies events based on testing scores using the Adjusted Classify
#' and Count (ACC) method. ACC is an extension of CC, applying a correction
#' rate based on the true and false positive rates (\code{tpr} and \code{fpr}).
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @param TprFpr a \code{data.frame} of true positive (\code{tpr}) and false positive (\code{fpr})
#' rates estimated on training set, using the function \code{getTPRandFPRbyThreshold()}.
#' @param thr threshold value according to the \code{tpr} and \code{fpr} were learned.
#' Default is \code{0.5}.
#' @return the class distribution in the test set.
#' @usage ACC(test, TprFpr, thr=0.5)
#' @references Forman, G. (2006, August). Quantifying trends accurately despite classifier
#' error and class imbalance. In ACM SIGKDD international conference on Knowledge discovery
#' and data mining (pp. 157-166).<doi.org/10.1145/1150402.1150423>.
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
#' ACC(test = test.scores[,1], TprFpr = TprFpr)
#' @export

ACC <- function(test, TprFpr, thr=0.5){
  dC <- CC(test)

  x <- as.data.frame(t(as.numeric(TprFpr[which(TprFpr[,1] == thr),c(2,3)])))
  colnames(x) <- c("tpr", "fpr")
  TprFpr <- x

  result <- (dC[1] - TprFpr$fpr) / (TprFpr$tpr - TprFpr$fpr)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")

  return(result)
}
