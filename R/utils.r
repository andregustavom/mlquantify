getHist <- function(scores, nbins){
  breaks <- seq(0,1,length.out = nbins+1)
  breaks <- c(breaks[-length(breaks)], 1.1)
  re <- rep((1/(length(breaks)-1)),length(breaks)-1)
  for(i in 2:length(breaks)){
    re[i-1] <- (re[i-1] + length(which(scores >= breaks[i-1] & scores < breaks[i])))/(length(scores)+1)
  }
  return(re)
}


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


