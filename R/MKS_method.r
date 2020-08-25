#' Mixable Kolmogorov Smirnov
#'
#' It quantifies events based on testing scores, applying the Mixable Kolmogorov
#' Smirnov (MKS) method proposed by Maletzke et al. (2019).
#' @param p.score a numeric \code{vector} of positive scores estimated either from a
#' validation set or from a cross-validation method.
#' @param n.score a numeric \code{vector} of negative scores estimated either from a
#' validation set or from a cross-validation method.
#' @param test a numeric \code{vector} containing the score estimated for the positive class from
#' each test set instance.
#' @return the class distribution in the test set.
#' @references Maletzke, A., Reis, D., Cherman, E., & Batista, G. (2019). DyS:
#' a Framework for Mixture Models in Quantification. in Proceedings of the The
#' Thirty-Third AAAI Conference on Artificial Intelligence, ser. AAAI’19, 2019.<doi.org/10.1609/aaai.v33i01.33014552>
#' @usage MKS(p.score, n.score, test)
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
#' MKS(p.score = scores[scores[,3]==1,1], n.score = scores[scores[,3]==2,1],
#' test = test.scores)

MKS <- function(p.score, n.score, test){

  alpha <- seq(0,1,by=0.01)

  sc_1 <- p.score
  sc_2 <- n.score

  Usc <- test

  result <- NULL

  u_Usc <- sort(unique(c(Usc, sc_1, sc_2)))

  vDistAll <- NULL
  for(k in 1:length(alpha)){
    vDist <- NULL
    for(i in 1:length(u_Usc)){
      Fx <- length(which(Usc <= u_Usc[i]))/length(Usc)
      FA <- length(which(sc_1 <= u_Usc[i]))/length(sc_1)
      FB <- length(which(sc_2 <= u_Usc[i]))/length(sc_2)

      aux <- (FA*alpha[k]) + (FB*(1-alpha[k]))

      vDist <- c(vDist, abs(Fx-aux))
    }
    vDistAll <- c(vDistAll, max(vDist))
  }


  result <- alpha[which.min(vDistAll)]

  if(result < 0 ) result <- 0
  if(result > 1 ) result <- 1
  result <- c(result, 1 - result)
  names(result) <- c("+", "-")

  return(result)

}
