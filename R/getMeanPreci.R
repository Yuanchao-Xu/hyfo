#' Get mean rainfall data, e.g. mean annual rainfall, mean monthly rainfall and mean winter rainfall.
#' 
#' @param inputTS A time series with only data column (1 column).
#' @param method A string showing the method used to calculate mean value, e.g., "meanAnnualPreci".
#' more information please refer to details.
#' @param yearIndex A numeric list showing the year index of the time series.
#' @param monthIndex A numeric list showing the month index of the time series.
#' @param fullResults A boolean showing whether the full results are shown, default is FALSE. If 
#' FALSE, only mean value will be returned, if TRUE, the sequence of values will be returned.
#' @param omitNA in the calculation, whether NA is omitted, default is FALSE.
#' @details
#' there are following methods to be selected, "meanAnnualPreci", "winter", "spring", "autumn", "summer".
#' @return The mean value of the input time series or the full results before calculating mean.
#' @examples
#' data(testdl)
#' TS  <- testdl[[1]]
#' year = as.numeric(format(TS[, 1], '%Y'))
#' month = as.numeric(format(TS[, 1], '%m'))
#' 
#' # Get the mean spring precipitation.
#' a <- getMeanPreci(TS[, 2], method = 'spring', yearIndex = year, monthIndex = month)
#' a
#' 
#' # Get the series of spring precipitation, set fullResults = TRUE.
#' a <- getMeanPreci(TS[, 2], method = 'spring', yearIndex = year, monthIndex = month,
#'                   fullResults = TRUE)
#' a
#' 
#' # If missing value is excluded, set omitNA = TRUE.
#' a <- getMeanPreci(TS[, 2], method = 'winter', yearIndex = year, monthIndex = month,
#'                   omitNA = TRUE, fullResults = TRUE)
#' a
#' 
#' # Get special month precipitation, e.g. march.
#' a <- getMeanPreci(TS[, 2], method = 3, yearIndex = year, monthIndex = month,
#'                   fullResults = TRUE)
#' a
#' 
#' # We can also get annual precipitation.
#' a <- getMeanPreci(TS[, 2], method = 'meanAnnualPreci', yearIndex = year, monthIndex = month,
#'                   fullResults = TRUE)
#'
#' @export
getMeanPreci <- function(inputTS, method = NULL, yearIndex = NULL, monthIndex = NULL,
                         fullResults = FALSE, omitNA = FALSE) {
  # First check if all the records are NA.
  if (any(!is.na(inputTS))) {
    #converting daily preci to the wanted preci.
    if (method == 'meanAnnualPreci') {
      ###yearIndex <- startTime$year + 1900
      annualPreci <- tapply(inputTS, INDEX = yearIndex, FUN = sum, na.rm = omitNA)#ggplot is able not to show NA, so choose TRUE
      if (fullResults == TRUE) output <- annualPreci else output <- mean(annualPreci, na.rm = omitNA)
      
    } else if (method == 'winter') {
      #winter is the most tricky part, because it starts from Dec to Feb next year, it's a year-crossing season,
      #so we have to make some changes to the monthIndex
      #e.g.data from 1950.1.1 - 2008.3.31 if we want to calculate the mean winter preci, to calculate winter month
      #December, we have to move the yearIndex one month forwards or two months backwards, to make 12,1,2 in one year      
      ###yearIndex <- startTime$year + 1900
      ###monthIndex <- startTime$mon + 1
      
      #we move the yearIndex one month backwards
      yearIndex_new <- c(yearIndex[32:length(yearIndex)], rep(tail(yearIndex, 1), 31))
      
      winterIndex <- which(monthIndex == 12 | monthIndex == 1 | monthIndex == 2)
      winterYear <- yearIndex_new[winterIndex]#this index is used for calculation
      
      #because we don't have 1949.Dec, so the first winter is not intact, so first two months are elemenated
      
      startIndex <- length(which(winterYear == yearIndex[1])) + 1
      winterOfLastYear <- length(which(winterYear == tail(yearIndex, 1)))
      if (winterOfLastYear > 91) {
        endIndex <- length(winterYear) - 31 #in case the data set ends at Dec.31
      } else if (winterOfLastYear < 90) { # incase the data ends at Jan 31
        endIndex <- length(winterYear) - length(which(winterYear == tail(yearIndex, 1)))
      } else {
        endIndex <- length(winterYear)
      }
      
      inputTS <- inputTS[winterIndex][startIndex:endIndex]#needs two process with inputPreci, first, extract
      #the winter preci, second, delete first two month of 1950
      
      winterYear <- winterYear[startIndex:endIndex]#needs one process, delete two months
      
      seasonalPreci <- tapply(inputTS,INDEX = winterYear, FUN = sum, na.rm = omitNA)
      if (fullResults == TRUE) output <- seasonalPreci else output <- mean(seasonalPreci, na.rm = omitNA)  
      
    } else if (method == 'spring') {
      springIndex <- which(monthIndex == 3 | monthIndex == 4 | monthIndex == 5)
      springYear <- yearIndex[springIndex]
      inputTS <- inputTS[springIndex]
      seasonalPreci <- tapply(inputTS, INDEX = springYear, FUN = sum, na.rm = omitNA)
      if (fullResults == TRUE) output <- seasonalPreci else output <- mean(seasonalPreci, na.rm = omitNA)
      
    } else if (method == 'summer') {
      summerIndex <- which(monthIndex == 6 | monthIndex == 7 | monthIndex == 8)
      summerYear <- yearIndex[summerIndex]
      inputTS <- inputTS[summerIndex]
      seasonalPreci <- tapply(inputTS, INDEX = summerYear, FUN = sum, na.rm = omitNA)
      if (fullResults == TRUE) output <- seasonalPreci else output <- mean(seasonalPreci, na.rm = omitNA)
      
    } else if (method == 'autumn') {
      autumnIndex <- which(monthIndex == 9 | monthIndex == 10 | monthIndex == 11)
      autumnYear <- yearIndex[autumnIndex]
      inputTS <- inputTS[autumnIndex]
      seasonalPreci <- tapply(inputTS, INDEX = autumnYear, FUN = sum, na.rm = omitNA)
      if (fullResults == TRUE) output <- seasonalPreci else output <- mean(seasonalPreci, na.rm = omitNA)
    } else if (is.numeric(method)) {
      month <- method
      monthlyPreci <- tapply(inputTS, INDEX = list(yearIndex, monthIndex), 
                             FUN = sum, na.rm = omitNA)[, month]
      
      if (fullResults == TRUE) output <- monthlyPreci else output <- mean(monthlyPreci, na.rm = omitNA)
    }
  } else {
    output <- NA
  }

  
  return(output)
}
