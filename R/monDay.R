#' Monthly data to daily data and the reverse conversion.
#' 
#' @param TS A time series, with first column date, and second column value. The date column should
#' follow the format in \code{as.Date}, i.e. seperate with "-" or "/". Check details for more information.
#' @param method A string showing whether you want to change a daily data to monthly data or monthly
#' data to daily data.e.g. "mon2day" and "day2mon".
#' @details 
#' Note, when you want to change daily data to monthly data, a new date column will be generated,
#' usually the date column will be the middle date of each month, 15th, or 16th. However, if your 
#' time series doesn't start from the beginning of a month or ends to the end of a month, e.g. 
#' from 1999-3-14 to 2008-2-2, the first and last generated date could be wrong. Not only the date, but also the data, because you are 
#' not calculating based on a intact month. 
#' @return converted time series.
#' @examples
#' # Daily to monthly
#' data(testdl)
#' TS <- testdl[[2]] # Get daily data
#' str(TS)
#' TS_new <- monDay(TS, method = 'day2mon')
#' 
#' # Monthly to daily
#' TS <- data.frame(Date = seq(as.Date('1999-9-15'), length = 30, by = '1 month'), 
#' runif(30, 3, 10))
#' TS_new <- monDay(TS, method = 'mon2day')
#' 
#' # More examples can be found in the user manual on http://yuanchao-xu.github.io/hyfo/
#' 
#' @export
#' @importFrom stats aggregate
#' @references 
#' 
#' \itemize{
#' \item R Core Team (2015). R: A language and environment for statistical computing. R Foundation for
#' Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
#' }
#' 
#' 
monDay <- function(TS, method){
  if (length(TS) != 2) {
    stop('Time series not correct, should be two columns, Date and value.')
  } else if (!grepl('-|/', TS[1, 1])) {
    stop('First column is not date or Wrong Date formate, check the format in ?as.Date{base} 
          and use as.Date to convert.')
  } 
  
  
  if (method == 'mon2day') {
  
    data <- apply(TS, MARGIN = 1 , FUN = mon2day)
    
    output <- do.call('rbind', data)
  } else if (method == 'day2mon') {
    Date <- as.Date(TS[, 1])
    year <- format(Date, format = '%Y')
    mon <- format(Date, format = '%m')
    
    data <- aggregate(TS, by = list(year, mon), FUN = mean, na.rm = TRUE)
    data <- data[order(data$Date), ][, 3:4]
    rownames(data) <- 1:dim(data)[1]
    output <- data
  } else {
    stop('method is not correct, check method argument.')
  }

  return (output)
}

#' @importFrom utils tail
#' @references 
#' 
#' \itemize{
#' \item R Core Team (2015). R: A language and environment for statistical computing. R Foundation for
#' Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
#' }
#' 
mon2day <- function(monData) {
  Date <- as.Date(monData[1])
  data <- monData[2]
  
  DateY <- format(Date, format = '%Y')
  DateM <- format(Date, format = '%m')
  DateL <- seq(Date, length = 2, by = '1 months')[2] - Date
  
  DateD <- 1:DateL
  
  start <- as.Date(paste(DateY, DateM, DateD[1], sep = '-'))
  end <- as.Date(paste(DateY, DateM, tail(DateD, 1), sep = '-'))
  
  Date <- seq(start, end, by = '1 day')
  
  dailyData <- data.frame(Date = Date, value = rep(data, DateL))
  
  return(dailyData)
}