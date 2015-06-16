#' Get annual rainfall of different rainfall time series
#' 
#' @param datalist A list containing different time series of different rainfall gauges
#' @return The annual rainfall and the number of missing data of each year and each rainfall gauge, which 
#' will also be plotted.
# @examples
# str(datalist)
# List of 5
# $ AAA:'data.frame':  5602 obs. of  2 variables:
#  ..$ Date: Factor w/ 5602 levels "1999-10-28","1999-10-29",..: 1 2 3 4 5 6 7 8 9 10 ...
# ..$ AAA : num [1:5602] 0 0 0.4 0 0.2 46.6 1.8 0 0 31.2 ...
# $ BBB:'data.frame':  5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ BBB : num [1:5977] 0 1.4 0 0 0 0 0 19.7 42.9 4.7 ...
# $ CCC:'data.frame':	5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ CCC : num [1:5977] 0 0 0 0 0 0 0 11 28.6 6.2 ...
# $ DDD:'data.frame':	6307 obs. of  2 variables:
#   ..$ Date: Factor w/ 6307 levels "1998-01-01","1998-01-02",..: 1 2 3 4 5 6 7 8 9 10 ...
# ..$ DDD : num [1:6307] NA NA NA NA NA NA NA NA NA NA ...
# $ EEE:'data.frame':	5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ EEE : num [1:5977] 0 0 0 0 0 0 0 16.2 30 9 ...
# 
# a1 <- getAnnual(datalist)
# 
# head(a1)
# 
#   yearUnique name annualPreci NANum
# 1       1999  AAA       480.4     5
# 2       2000  AAA      2058.2    10
# 3       2001  AAA      1441.1     0
# 4       2002  AAA      2441.0    35
# 5       2003  AAA      2072.8     0
# 6       2004  AAA      1665.9    16
# 
# tail(a1)
# 
#    yearUnique name annualPreci NANum
# 80       2010  EEE      1641.7     0
# 81       2011  EEE      1886.2     0
# 82       2012  EEE      1709.7     0
# 83       2013  EEE      2581.6     0
# 84       2014  EEE      2283.0     0
# 85       2015  EEE       843.5     0
#' @references
#' Data source:
#' http://meteo.navarra.es/estaciones/mapadeestaciones.cfm
#' http://www4.gipuzkoa.net/oohh/web/esp/02.asp
#' @export
#' @import ggplot2 reshape2
getAnnual <- function(datalist){
  
  data <- lapply(datalist, FUN = getAnnual_dataframe)
  
  data <- do.call('rbind',data)
  
  rownames(data) <- NULL
  
  plotData <- melt(data, var.id = c('yearUnique','name'))
  
  mainLayer <- ggplot(plotData)+
    geom_bar(aes(x = as.Date(yearUnique,format = '%Y'), y = value , fill = name), stat = 'identity')+
    facet_grid(variable ~ name, scales = 'free')
  print (mainLayer)
  
  return (data)
}


#' Get mean annual rainfall of different rainfall time series
#' 
#' @param datalist A list containing different time series of different rainfall gauges
#' @return The mean annual rainfall of each year and each rainfall gauge
# @examples
# str(datalist)
# List of 5
# $ AAA:'data.frame':  5602 obs. of  2 variables:
#  ..$ Date: Factor w/ 5602 levels "1999-10-28","1999-10-29",..: 1 2 3 4 5 6 7 8 9 10 ...
# ..$ AAA : num [1:5602] 0 0 0.4 0 0.2 46.6 1.8 0 0 31.2 ...
# $ BBB:'data.frame':  5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ BBB : num [1:5977] 0 1.4 0 0 0 0 0 19.7 42.9 4.7 ...
# $ CCC:'data.frame':  5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ CCC : num [1:5977] 0 0 0 0 0 0 0 11 28.6 6.2 ...
# $ DDD:'data.frame':	6307 obs. of  2 variables:
#   ..$ Date: Factor w/ 6307 levels "1998-01-01","1998-01-02",..: 1 2 3 4 5 6 7 8 9 10 ...
# ..$ DDD : num [1:6307] NA NA NA NA NA NA NA NA NA NA ...
# $ EEE:'data.frame':	5977 obs. of  2 variables:
#   ..$ Date: Date[1:5977], format: "1999-01-01" "1999-01-02" "1999-01-03" ...
# ..$ EEE : num [1:5977] 0 0 0 0 0 0 0 16.2 30 9 ...
# 
# a <- getAnnual_mean(datalist)
# a
#   name    meanV
# 1  AAA 2017.300
# 2  BBB 2373.287
# 3  CCC 2145.075
# 4  DDD 1680.593
# 5  EEE 1928.640

#' @references
#' Data source:
#' http://meteo.navarra.es/estaciones/mapadeestaciones.cfm
#' http://www4.gipuzkoa.net/oohh/web/esp/02.asp
#' @export
getAnnual_mean <- function(datalist){
  data <- lapply(datalist, FUN = getAnnual_dataframe, output = 'mean')
  data <- do.call('rbind',data)
  rownames(data) <- NULL
  
  return (data)
}




#' Get annual rainfall of the input time series.
#' 
#' @param dataset A dataframe containing one time series, e.g., rainfall from one gauging station.
#' @param output A string showing what kind of output is, e.g. 'series' or 'mean'
#' @return The annual rainfall of each year of the input station.
# @examples
# 
# #a is a time series.
# head(a)
#         Date  AAA
# 1 1999-10-28  0.0
# 2 1999-10-29  0.0
# 3 1999-10-30  0.4
# 4 1999-10-31  0.0
# 5 1999-11-01  0.2
# 6 1999-11-02 46.6
# 
# tail(a)
#            Date   AAA
# 5597 2015-02-22  18.4
# 5598 2015-02-23  32.8
# 5599 2015-02-24  53.6
# 5600 2015-02-25 132.5
# 5601 2015-02-26 113.1
# 5602 2015-02-27  20.0
# 
# 
# a1 <- getAnnual_dataframe(a, output = 'series')
# a1
#      yearUnique name annualPreci NANum
# 1999       1999  AAA       480.4     5
# 2000       2000  AAA      2058.2    10
# 2001       2001  AAA      1441.1     0
# 2002       2002  AAA      2441.0    35
# 2003       2003  AAA      2072.8     0
# 2004       2004  AAA      1665.9    16
# 2005       2005  AAA      1960.7     0
# 2006       2006  AAA      1566.6     0
# 2007       2007  AAA      2094.3     0
# 2008       2008  AAA      2357.1     0
# 2009       2009  AAA      2111.9     0
# 2010       2010  AAA      1657.5     0
# 2011       2011  AAA      2072.6     0
# 2012       2012  AAA      1758.9     0
# 2013       2013  AAA      2614.0     0
# 2014       2014  AAA      2459.2     0
# 2015       2015  AAA       951.8     0

#' @references
#' Data source:
#' http://meteo.navarra.es/estaciones/mapadeestaciones.cfm
#' http://www4.gipuzkoa.net/oohh/web/esp/02.asp
#' @export
getAnnual_dataframe <- function(dataset, output = 'series'){
  Date <- as.Date(dataset[,1])
  year <- format(Date,'%Y')
  yearUnique <- unique(year)
  calcuNum <- c(1: length(yearUnique))
  
  NANum <- length(which(is.na(dataset[,2])))
  
  annualPreci <- tapply(dataset[,2], INDEX = year, FUN = sum, na.rm = T)
  NANum <- tapply(dataset[,2], INDEX = year, function(x) length(which(is.na(x))))
  
  
  firstYearN <- length(which(year == year[1]))
  if (firstYearN < 360) calcuNum[1] <- NA 
  
  lastYearN <- length(which(year == year[length(year)]))
  if (lastYearN < 360) calcuNum[length(yearUnique)] <- NA
  
  calcuNum[which(NANum > 10)] <- NA
  
  meanV <- mean(annualPreci[which(!is.na(calcuNum))])
  
  if (output == 'mean'){
    name <- colnames(dataset)[2]
    output <- data.frame(name,meanV)
    return (output)
  }else if(output == 'series'){
    name <- rep(colnames(dataset)[2],length(calcuNum))
    output <- data.frame(yearUnique,name,annualPreci,NANum)
    return (output)
  }
  
}


