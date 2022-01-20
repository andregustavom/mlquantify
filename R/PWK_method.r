#' Proportion-weighted k-nearest neighbor
#'
#' It is a nearest-neighbor classifier adapted for working over quantification problems. This
#' method applies a weighting scheme, reducing the weight on neighbors from the majority class.
#' @param train a \code{data.frame} containing the training data.
#' @param y a \code{vector} containing the target values.
#' @param test a \code{data.frame} containing the test data.
#' @param alpha a numeric value defining the proportion-weighted k-nearest neighbor algorithm
#' as proposed by Barranquero et al., (2012). (Default: \code{1}).
#' @param n_neighbors a integer value defining the number of neighbors to use by default for
#' nearest neighbor queries (Default: \code{10}).
#' @return A numeric vector containing the class distribution estimated from the test set.
#' @references Barranquero, J., González, P., Díez, J., & Del Coz, J. J. (2013). On the study
#' of nearest neighbor algorithms for prevalence estimation in binary problems. Pattern Recognition,
#' 46(2), 472-482.<doi.org/10.1016/j.patcog.2012.07.022>
#' @usage PWK(train, y, test, alpha=1, n_neighbors=10)
#' @export
#' @examples
#' library(caret)
#' library(FNN)
#' cv <- createFolds(aeAegypti$class, 2)
#' tr <- aeAegypti[cv$Fold1,]
#' ts <- aeAegypti[cv$Fold2,]
#'
#' # -- Getting a sample from ts with 80 positive and 20 negative instances --
#' ts_sample <- rbind(ts[sample(which(ts$class==1),80),],
#'                    ts[sample(which(ts$class==2),20),])
#' PWK(train=tr[,-which(names(tr)=="class")], y=tr[,"class"], test= ts[,-which(names(ts)=="class")])
PWK <- function(train, y, test, alpha=1, n_neighbors=10){
  Y_cts <- table(y)
  w <- (Y_cts / min(Y_cts)) ** (-1.0/alpha)

  knn_clf <- FNN::knn( apply(train, 2, as.numeric),  apply(test, 2, as.numeric), y, k = n_neighbors)

  classes <- as.numeric(sort(unique(y)))
  N <- nrow(test)
  nn_ind <-  attr(knn_clf, "nn.index")

  CM <- matrix(nrow = N, ncol=length(unique(knn_clf)))
  CM[,] <- 0

  for(i in 1:N)
    for(j in nn_ind[i,])
      CM[i, classes[y[j]]] <- CM[i, classes[y[j]]] + 1

  CM <- CM*as.vector(w)
  pred <- apply(CM, 1, which.max)
  result <- table(pred)/N
  names(result) <- c("+", "-")
  return(result)

}



