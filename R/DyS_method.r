#' DyS Framework
#'
#' DyS is a framework for quantification data based on mixture models method. It quantifies
#' events based on testing scores, applying the DyS framework proposed by Maletzke et al. (2019).
#' It also works with several similarity functions.
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @param measure measure used to compare the mixture histogram against the
#' histogram obtained from the test set. Several functions can be used (Default:
#' \code{"topsoe"}, \code{"euclidean"}, \code{"jensen_difference"}, \code{"prob_symm"},
#' \code{"taneja"}, \code{"ord"}).
#' @param bins a numeric \code{vector} of number of bins used to construct the histogram for
#' representing the score distribution. (default: \code{seq(2,20,2)}).
#' @param err a numeric value defining the accepted error for the ternary search
#' (default: \code{1e5}).
#' @return the class distribution in the test.
#' @usage DyS(p.score, n.score, test, measure="topsoe", bins=seq(2,20,2), err=1e-5)
#' @references Maletzke, A., Reis, D., Cherman, E., & Batista, G. (2019). DyS: a Framework
#' for Mixture Models in Quantification. in Proceedings of the The Thirty-Third AAAI
#' Conference on Artificial Intelligence, ser. AAAI’19, 2019. <doi.org/10.1609/aaai.v33i01.33014552>.
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
#' DyS(p.score = scores[scores[,3]==1,1], n.score = scores[scores[,3]==2,1],
#' test = test.scores[,1])

DyS <- function(p.score, n.score, test, measure="topsoe", bins=seq(2,20,2), err=1e-5){

  b_sizes <- bins
  result  <- c(1:length(b_sizes))

  for(hi in 1:length(b_sizes)){

    Sty_1 <- getHist(p.score, b_sizes[hi])
    Sty_2 <- getHist(n.score, b_sizes[hi])
    Uy    <- getHist(test, b_sizes[hi])

    f <- function(x){
      return(DyS_distance(rbind((Sty_1*x)+ (Sty_2*(1-x)), Uy), method = measure))
    }
    result[hi] <- TernarySearch(0, 1, f, err)
  }
  result <- stats::median(result)
  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")
  return(result)

}
