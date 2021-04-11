#' Sample ORD Dissimilarity
#'
#' It quantifies events based on testing scores applying the framework DyS with the
#' Sample ORD Dissimilarity (SORD) proposed by Maletzke et al. (2019).
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @usage SORD(p.score, n.score, test)
#' @references Maletzke, A., Reis, D., Cherman, E., & Batista, G. (2019). DyS: a
#' Framework for Mixture Models in Quantification. in Proceedings of the The
#' Thirty-Third AAAI Conference on Artificial Intelligence, ser. AAAI’19, 2019.<doi.org/10.1609/aaai.v33i01.33014552>.
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
#' SORD(p.score = scores[scores[,3]==1,1], n.score = scores[scores[,3]==2,1],
#' test = test.scores[,1])
#' @export

SORD <- function(p.score, n.score, test){

  f <- function(x){
    p_w <- x / length(p.score)
    n_w <- (1 - x) / length(n.score)
    t_w <- -1 / length(test)

    p <- cbind(p.score, rep(p_w, length(p.score)))
    n <- cbind(n.score, rep(n_w, length(n.score)))
    t <- cbind(test, rep(t_w, length(test)))

    v <- rbind(p, n, t)
    v <- v[order(v[,1]),]
    acc <- v[1,2]
    total_cost <- 0

    for( i in 2:nrow(v)){
      cost_mul <- v[i,1] - v[i - 1, 1]
      total_cost <- total_cost + abs(cost_mul * acc)
      acc <- acc + v[i,2]
    }
    return(total_cost)
  }
  result <- TernarySearch(0, 1, f, 1e-5)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)
}
