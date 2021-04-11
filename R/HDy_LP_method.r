#' HDy with Laplace smoothing
#'
#' It computes the class distribution using the HDy algorithm proposed by González-Castro et al. (2013)
#' with Laplace smoothing (Maletzke et al. (2019)).
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @author Andre Maletzke <andregustavom@gmail.com>
#' @usage HDy_LP(p.score, n.score, test)
#' @references González-Castro, V., Alaíz-Rodriguez, R., & Alegre, E. (2013). Class
#' distribution estimation based on the Hellinger distance. Information Sciences.<doi.org/10.1016/j.ins.2012.05.028>
#' @references Maletzke, A., Reis, D., Cherman, E., & Batista, G. (2019). DyS: a
#' Framework for Mixture Models in Quantification. in Proceedings of the The
#' Thirty-Third AAAI Conference on Artificial Intelligence, ser. AAAI’19, 2019.
#' <doi.org/10.1609/aaai.v33i01.33014552>.
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
#' HDy_LP(p.score = scores[scores[,3]==1,1], n.score=scores[scores[,3]==2,1],
#' test=test.scores[,1])

HDy_LP <- function(p.score, n.score, test){

  alpha   <- seq(0,1,by=0.01)
  b_sizes <- seq(10,110, length.out = 11)
  result  <- c(1:length(b_sizes))

  for(hi in 1:length(b_sizes)){

    Sty_1 <- getHist(p.score, b_sizes[hi])
    Sty_2 <- getHist(n.score, b_sizes[hi])
    Uy    <- getHist(test, b_sizes[hi])
    vDist <- NULL

    for(k in 1:length(alpha)){
      aux <- (Sty_1*alpha[k])+ (Sty_2*(1-alpha[k]))
      vDist <- c(vDist, as.numeric(DyS_distance(rbind(aux, Uy), method = "hellinger")))
    }
    result[hi] <- alpha[which.min(vDist)]
  }

  result <- stats::median(result)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)

}
