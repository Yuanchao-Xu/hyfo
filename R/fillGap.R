#' Fill gaps in the rainfall time series.
#'
#' @param dataset A dataframe with first column the time, the rest columns are rainfall data of different gauges
#' @param corPeriod A string showing the period used in the correlation computing, e.g. daily, monthly, annual.
#' @return The filled dataframe
#' @examples
#' b <- read.table(text = '        Date  AAA  BBB  CCC  DDD  EEE
#' 49 1999-12-15 24.8 21.4 25.6 35.0 17.4
#' 50 1999-12-16   NA  0.6  1.5  6.3  2.5
#' 51 1999-12-17   NA 16.3 20.3  2.6 19.2
#' 52 1999-12-18   NA  1.6 18.4  6.3  0.0
#' 53 1999-12-19   NA 36.4 12.5 26.8 24.9
#' 54 1999-12-20   NA  0.0  0.0  0.2  0.0
#' 55 1999-12-21  0.2  0.0  0.0  0.0  0.0
#' 56 1999-12-22  0.0  0.0  0.0  0.0  0.0')
#' 
#' b1 <- fillGap(b)
#' 
# Correlation Coefficient
# AAA       BBB       CCC       DDD       EEE
# AAA 1.0000000 0.9999754 0.9999754 0.9999754 0.9999754
# BBB 0.9999754 1.0000000 1.0000000 1.0000000 1.0000000
# CCC 0.9999754 1.0000000 1.0000000 1.0000000 1.0000000
# DDD 0.9999754 1.0000000 1.0000000 1.0000000 1.0000000
# EEE 0.9999754 1.0000000 1.0000000 1.0000000 1.0000000
# 
# Correlation Order
# 1     2     3     4    
# AAA "BBB" "DDD" "EEE" "CCC"
# BBB "CCC" "DDD" "EEE" "AAA"
# CCC "CCC" "DDD" "EEE" "AAA"
# DDD "CCC" "DDD" "EEE" "AAA"
# EEE "CCC" "DDD" "EEE" "AAA"
# 
# Linear Coeficients
# 1         2         3         4
# AAA 1.1588785 0.7085714 1.4252874 0.9687500
# BBB 0.8718832 0.8774724 1.2277330 0.8628471
# CCC 1.0000000 0.6943610 0.8863647 1.0321909
# DDD 0.9006973 1.0000000 1.0341615 1.4111985
# EEE 0.7351823 0.6612678 1.0000000 0.7015673
# 
# b1
#         Date        AAA  BBB  CCC  DDD  EEE
# 1 1999-12-15 24.8000000 21.4 25.6 35.0 17.4
# 2 1999-12-16  0.6953271  0.6  1.5  6.3  2.5
# 3 1999-12-17 18.8897196 16.3 20.3  2.6 19.2
# 4 1999-12-18  1.8542056  1.6 18.4  6.3  0.0
# 5 1999-12-19 42.1831776 36.4 12.5 26.8 24.9
# 6 1999-12-20  0.0000000  0.0  0.0  0.2  0.0
# 7 1999-12-21  0.2000000  0.0  0.0  0.0  0.0
# 8 1999-12-22  0.0000000  0.0  0.0  0.0  0.0
#' @export
fillGap <- function(dataset,corPeriod = 'daily'){
  message('Fill gaps. First column should be the Date')
  
  Date <- as.Date(dataset[,1])
  data <- data.frame(dataset[,2:dim(dataset)[2]])
  names <- colnames(data)
  
  corN <- fillGap_cor(data, corPeriod = corPeriod, Date = Date)
  cat('\nCorrelation Coefficient\n')
  print(corN)
  
  corOrder <- apply(corN, MARGIN = 1, FUN = function(x) order(-x))
  corOrder <- corOrder[2:dim(corOrder)[1],]
  corOrderName <- t(apply(corOrder, MARGIN = 2, FUN = function(x) names[x]))
  
  cat ('\nCorrelation Order\n')
  colnames(corOrderName) <- seq(1 : dim(corOrderName)[2])
  print (corOrderName)
  
  lmCoef <- fillGap_lmCoef(data, corOrder)
  cat ('\nLinear Coeficients\n')
  rownames(lmCoef) <- seq(1 : dim(corOrderName)[2])
  print (t(lmCoef))
  
  output <- lapply(1:dim(data)[2], fillGap_column, data = data,
                   corOrder = corOrder, lmCoef = lmCoef)
  output <- data.frame(output)
  colnames(output) <- names
  
  output <- cbind(Date, output)
  
  return (output)
}


#' Get monthly rainfall
#' 
#' @param TS A rainfall time series.
#' @param year A list showing the year index of the time series.
#' @param mon A list showing the mon index of the time series.
#' @return the monthly rainfall matrix of the rainfall time series.
#' @export
monthlyPreci <- function(TS,year,mon){
  
  monTS <- tapply(TS, INDEX = list(year, mon), FUN = sum, na.rm = T)
  output <- t(monTS)
  dim(output) <- c(dim(monTS)[1] * dim(monTS)[2],1)
  return (output)
}


fillGap_column <- function(i, data, corOrder, lmCoef){
  TS <- data[,i] # extract target column
  l <- dim(data)[2] # length
  
  for (j in 1:l){
    if (!any(is.na(TS))) break
    NAindex <- which(is.na(TS))
    TS[NAindex] <- lmCoef[j,i] * data[NAindex,corOrder[j,i]]
    
    if (j == l) stop ('Error: One time consists of all NA values')
  }
  
  return(TS)
}


fillGap_cor <- function(data, corPeriod = 'monthly', Date){
  
  names <- colnames(data)
  year <- format(Date,'%Y')
  
  if (corPeriod == 'monthly'){
    #based on monthly rainfall
    mon <- format(Date, '%m')
    monthlyPreci <- lapply(data, FUN = monthlyPreci, year = year, mon = mon)
    corData <- do.call('cbind', monthlyPreci)
  }else if (corPeriod == 'yearly'){
    year <- format(Date, '%Y')
    annualPreci <- lapply(data, FUN = function(x) tapply(x, INDEX = year, FUN = sum, na.rm = T))
    corData <- do.call('cbind', annualPreci)
  }else if (corPeriod == 'daily'){
    corData <- data
  }else{
    stop ('Pleas choose among "daily", "monthly", "yearly".')
  }
  
  corData <- data.frame(na.omit(corData))
  colnames(corData) <- names
  
  corN <- cor(corData)
  
  return (corN)
  
} 

fillGap_lmCoef <- function(data, corOrder){
  l <- dim(data)[2]
  m <- diag(l)# m is the coeficients matrix
  m[lower.tri(m)] <- combn(data, 2, function(x) coef(lm(x[,2] ~ x[,1] + 0)))
  tm <- t(m)
  
  tm[lower.tri(tm)] <- combn(data, 2, function(x) coef(lm(x[,1] ~ x[,2] + 0)))
  
  m <- t(tm)
  
  lmCoef <- lapply(1 : l, function(x) m[x,corOrder[,x]])
  lmCoef <- do.call('rbind',lmCoef)
  rownames(lmCoef) <- colnames(data)
  
  return(t(lmCoef))
}

