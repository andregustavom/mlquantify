#' Sample Mean Matching
#'
#' SMM is a member of the DyS framework that uses simple means scores to represent the score distribution
#' for positive, negative, and unlabelled scores. Therefore, the class distribution is given by a
#' closed-form equation.
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @usage SMM(p.score, n.score, test)
#' @references Hassan, W., Maletzke, A., Batista, G. (2020). Accurately Quantifying a Billion Instances
#' per Second. In IEEE International Conference on Data Science and Advanced Analytics (DSAA).
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
#' SMM(p.score = scores[scores[,3]==1,1], n.score = scores[scores[,3]==2,1],
#' test = test.scores[,1])
SMM <- function(p.score, n.score, test){

  Sty_1 <- mean(p.score)
  Sty_2 <- mean(n.score)
  Uy    <- mean(test)

  result <- NULL
  vDist <- NULL

  a1 <- Sty_1 - Sty_2
  a2 <- Uy - Sty_2
  result <- a2/a1

  ifelse(result[1] < 0, result[1] <- 0, result[1] <- result[1])
  ifelse(result[1] > 1, result[1] <- 1, result[1] <- result[1])

  result <- c(result, 1 - result)
  names(result) <- c("1", "2")

  return(result)
}
