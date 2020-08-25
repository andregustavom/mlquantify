
#' Get a normalized histogram from score values.
#'
#' @param scores a numeric vector containing the scores.
#' @param nbins number of histogram bins.
#' @return normalized histogram.
#' @author Andre Maletzke <andregustavom@gmail.com>
#' @usage getHist(scores, nbins)
#' @export
#' @examples
#' positive_scores <- runif(200)^0.3
#' negative_scores <- 1 - runif(200)^0.3
#' getHist(positive_scores, 20)
#' getHist(negative_scores, 20)
getHist <- function(scores, nbins){
  breaks <- seq(0,1,length.out = nbins+1)
  breaks <- c(breaks[-length(breaks)], 1.1)
  re <- rep((1/(length(breaks)-1)),length(breaks)-1)
  for(i in 2:length(breaks)){
    re[i-1] <- (re[i-1] + length(which(scores >= breaks[i-1] & scores < breaks[i])))/(length(scores)+1)
  }
  return(re)
}


#' Implements the Ternary Search algorithm
#'
#' Ternary search is used for finding the best alpha value in DyS framework.
#' @param left a numeric value containing the left bound.
#' @param right a numeric value containing the right bound.
#' @param f unimodal function.
#' @param eps stop criterium.
#' @return  the minimum of a unimodal function \code{f()} within \code{[left, right]}.
#' @author Andre Maletzke <andregustavom@gmail.com>
#' @export
TernarySearch  <- function(left, right, f, eps=1e-4){
  while(TRUE){
    if (abs(left - right) < eps) return((left + right) / 2)

    leftThird  <- left + (right - left) / 3
    rightThird <- right - (right - left) / 3

    if (f(leftThird) > f(rightThird))
      left <- leftThird
    else
      right <- rightThird
  }

}

#' Calculates the distances for DyS method
#'
#'@keywords internal
DyS_distance <- function (x, method= "hellinger"){

  if (method == "ord") {
    x_dif <- x[1,]-x[2,]
    acum <- 0
    aux <- 0
    for(i in 1:length(x_dif)){
      aux <- x_dif[i]+aux
      acum <- acum+aux
    }
    return(abs(acum))
  }
  if(method == "topsoe"){
    re <- 0
    for(i in 1:ncol(x))
      re <- re + ( x[1,i] * log( (2*x[1,i])/(x[1,i] + x[2,i]) ) + x[2,i]*log( (2*x[2,i])/(x[2,i] + x[1,i]) ) )

    return(re)
  }
  if(method == "jensen_difference"){
    re <- 0
    for(i in 1:ncol(x))
      re <- re + (  ((x[1,i]*log(x[1,i]) + x[2,i]*log(x[2,i]) )/2) - ((x[1,i] + x[2,i])/2 ) * log((x[1,i] + x[2,i])/2 )   )

    return(re)
  }
  if(method == "taneja"){
    re <- 0
    for(i in 1:ncol(x))
      re <- re + ( ((x[1,i] + x[2,i])/2) * log( (x[1,i] + x[2,i]) / (2 * sqrt(x[1,i] * x[2,i]) )) )
    return(re)
  }
  if(method == "hellinger"){
    re <- 0
    for(i in 1:ncol(x))
      re <- re + sqrt((x[1,i] * x[2,i]))
    return(2*sqrt(1 - re))
  }

  if(method == "prob_symm"){
    re <- 0
    for(i in 1:ncol(x))
      re <- re + ( (x[1,i] - x[2,i] )^2 / (x[1,i] +x[2,i]) )
    return(2*re)
  }

  stop("measure argument must be a valid option")
}


