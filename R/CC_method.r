#' Classify and Count
#'
#' It quantifies events based on testing scores, applying the Classify and Count (CC). CC is the simplest
#' quantification method that derives from classification (Forman, 2005).
#' @param test a numeric \code{vector} containing the score estimated for the positive class from each
#' test set instance.
#' @param thr a numeric value indicating the decision threshold. A value between 0 and 1 (default = \code{0.5})
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @usage CC(test, thr=0.5)
#' @references Forman, G. (2005). Counting positives accurately despite inaccurate
#' classification. In European Conference on Machine Learning. Springer, Berlin,
#' Heidelberg.<doi.org/10.1007/11564096_55>.
#' @examples
#' library(randomForest)
#' library(caret)
#' cv <- createFolds(aeAegypti$class, 2)
#' tr <- aeAegypti[cv$Fold1,]
#' ts <- aeAegypti[cv$Fold2,]
#'
#' # -- Getting a sample from ts with 80 positive and 20 negative instances --
#' ts_sample <- rbind(ts[sample(which(ts$class==1),80),],
#'                    ts[sample(which(ts$class==2),20),])
#' scorer <- randomForest(class~., data=tr, ntree=500)
#' test.scores <- predict(scorer, ts_sample, type = c("prob"))
#' CC(test = test.scores[,1])
#' @export
CC <- function(test, thr=0.5){

  result <- sum(test >= thr)/length(test)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)

}
