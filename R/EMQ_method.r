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
#' @param e a numeric value for the stop threshold (default \code{NULL}). If the
#' difference between two consecutive steps is lower or equal than \code{e}, the
#' iterative process will be stopped. If \code{e} is null then the iteration
#' phase is defined by the \code{it} parameter.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @references Saerens, M., Latinne, P., & Decaestecker, C. (2002). Adjusting
#' the outputs of a classifier to new a priori probabilities: a simple procedure.
#' Neural computation.<doi.org/10.1162/089976602753284446>.
#' @usage EMQ(train, test, it=5, e=NULL)
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
#' test.scores <- predict(scorer, ts_sample, type = c("prob"))
#' EMQ(train=tr, test=test.scores)
#' @export

EMQ <- function(train, test, it=5, e=NULL){

  pTr      <- table(train$class)/nrow(train)
  predTe_s <- test

    p   <- list(predTe_s)
  pTe <- list(pTr)
  nE  <- nrow(test)
  nC  <- 2
  s   <- 1

  if(is.null(e)){
    repeat{
      aux <- matrix(ncol=nC, nrow = nE)
      auxC <- c(1:nC)
      for (ic in 1:nC){
        for(ie in 1:nE){
          numerator   <- (pTe[[s]][ic]/pTr[ic])*predTe_s[ie,ic]
          denominator <- c(1:nC)
          for(ic2 in 1:nC)  denominator[ic2] <- (pTe[[s]][ic2]/pTr[ic2])*predTe_s[ie,ic2]
          aux[ie,ic] <- numerator/sum(denominator)
        }
        auxC[ic] <- sum(p[[s]][,ic])/nrow(test)
      }
      pTe <- c(pTe, list(auxC))
      p   <- c(p, list(aux))
      s <- s + 1

      if(s > it ){break}
    }
  }else{
    repeat{
      aux <- matrix(ncol=nC, nrow = nE)
      auxC <- c(1:nC)
      for (ic in 1:nC){
        for(ie in 1:nE){
          numerator   <- (pTe[[s]][ic]/pTr[ic])*predTe_s[ie,ic]
          denominator <- c(1:nC)
          for(ic2 in 1:nC)  denominator[ic2] <- (pTe[[s]][ic2]/pTr[ic2])*predTe_s[ie,ic2]
          aux[ie,ic] <- numerator/sum(denominator)
        }
        auxC[ic] <- sum(p[[s]][,ic])/nrow(test)
      }
      pTe <- c(pTe, list(auxC))
      p   <- c(p, list(aux))
      s <- s + 1

      if(abs((pTe[[s]][1]-pTe[[s-1]][[1]])) <= e ){break}
    }
  }

  result <- pTe[[s]][1]
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)


}
