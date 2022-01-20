#' Expectation-Maximization Quantification
#'
#' This method is an instance of the well-known algorithm for finding maximum-likelihood
#' estimates of the model's parameters. It quantifies events based on testing scores,
#' applying the Expectation Maximization for Quantification (EMQ) method proposed by
#' Saerens et al. (2002).
#' @param train a \code{data.frame} of the labeled set.
#' @param test a numeric \code{matrix} of scores predicted from each test set instance.
#' First column must be the positive score.
#' @param it maximum number of iteration steps (default \code{5}).
#' @param e a numeric value for the stop threshold (default \code{1e-4}). If the
#' difference between two consecutive steps is lower or equal than \code{e}, the
#' iterative process will be stopped. If \code{e} is null then the iteration
#' phase is defined by the \code{it} parameter.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @references Saerens, M., Latinne, P., & Decaestecker, C. (2002). Adjusting
#' the outputs of a classifier to new a priori probabilities: a simple procedure.
#' Neural computation.<doi.org/10.1162/089976602753284446>.
#' @usage EMQ(train, test, it=5, e=1e-4)
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
#' EMQ(train=tr, test=test.scores)
#' @export

EMQ <- function(train, test, it=5, e=1e-4){

  pTr <- as.numeric(table(train$class)/nrow(train))
  pTe  <- pTr
  si   <- 1
  test <- as.matrix(test)
  pTe_bf <- c(0,0)
  repeat{
    p_s <- t(apply(test, 1, function(x){x* (pTe / pTr)}))
    p_s <- p_s / apply(p_s, 1, sum)
    pTe = as.numeric(apply(p_s, 2, mean))
    si <- si + 1
    dist_diff <- (abs(pTe[1] - pTe_bf[1]) + abs(pTe[2] - pTe_bf[2]))/2
    pTe_bf <- pTe
    if((si > it)|(dist_diff <= e)){break}
  }
  result <- pTe[1]
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)


}
