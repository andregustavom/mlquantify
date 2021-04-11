#' Quantification method based on Kuiper's test
#'
#' It quantifies events based on testing scores, applying an adaptation of the
#' Kuiper's test for quantification problems.
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @author Denis dos Reis <denismr@gmail.com>
#' @usage KUIPER(p.score, n.score, test)
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
#' test.scores <- predict(scorer, ts_sample, type = c("prob"))
#' KUIPER(p.score = scores[scores[,3]==1,1], n.score = scores[scores[,3]==2,1],
#' test = test.scores[,1])

KUIPER <- function(p.score, n.score, test){

  alpha <- seq(0,1,by=0.01)

  sc_1 <- p.score
  sc_2 <- n.score

  Usc <- test

  u_Usc <- sort(unique(c(Usc, sc_1, sc_2)))

  vDistAll <- NULL
  for(k in 1:length(alpha)){
    vDist <- NULL
    for(i in 1:length(u_Usc)){
      Fx <- length(which(Usc <= u_Usc[i]))/length(Usc)
      FA <- length(which(sc_1 <= u_Usc[i]))/length(sc_1)
      FB <- length(which(sc_2 <= u_Usc[i]))/length(sc_2)

      aux <- (FA*alpha[k]) + (FB*(1-alpha[k]))

      vDist <- c(vDist, Fx-aux)
    }
    vDistAll <- c(vDistAll, (max(vDist)-min(vDist)) )
  }
  result <- alpha[which.min(vDistAll)]
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)

}
