#' get L moment analysis of the input distribution
#' 
#' @param dis A distribution, for hydrology usually a time series with only data column without time.
#' @return The mean, L-variation, L-skewness and L-kurtosis of the input distribution
#' @examples
#' dis <- seq(1, 100)
#' getLMom(dis)
#' @export
#' @import moments
getLMom <- function(dis){
  
  LMom <- lmom::samlmu(dis,nmom=4,ratios=T)
  
  mean <- LMom[1]
  LCV <- LMom[2]/LMom[1]
  Lskew <- LMom[3]
  Lkur <- LMom[4]
  
  output <- data.frame(mean=mean,Lcv=LCV,Lskew=Lskew,Lkur = Lkur)
  return (output)
}

#' get moment analysis of the input distribution
#' 
#' @param dis A distribution, for hydrology usually a time series with only data column without time.
#' @return The mean, variation, skewness and kurtosis of the input distribution
#' @examples
#' dis <- seq(1, 100)
#' getMoment(dis)
#' @export
getMoment <- function(dis) {
  mean <- mean(dis,na.rm=T)
  variance <- var(dis,na.rm=T)
  skewness <- skewness(dis,na.rm=T)
  kurtosis <- kurtosis(dis,na.rm=T)
  
  output <- data.frame(mean=mean, Variance = variance, Skewness = skewness, Kurtosis = kurtosis)
  
  return (output)
}
