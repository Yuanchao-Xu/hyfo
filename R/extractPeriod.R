
#' Extract common period or certain period from a list of different dataframes of time series.
#' NOTE: all the dates in the datalist should follow the format in ?as.Date{base}.
#' @param datalist A list of different dataframes of time series .
#' @param startDate A Date showing the start of the extract period, default as NULL.
#' @param endDate A Date showing the end of the extract period, default as NULL.
#' @param commonPeriod A boolean showing whether the common period is extracted. If chosen, startDate and endDate
#' should be NULL.
#' @return A list with all the time series inside containing the same period.
#' @examples
#' # Generate timeseries datalist. Each data frame consists of a Date and a value.
#' 
#' AAA <- data.frame(
#' # date column
#' Date = seq(as.Date('1990-10-28'),as.Date('1997-4-1'),1),
#'  # value column
#' AAA = sample(1:100,length(seq(as.Date('1990-10-28'),as.Date('1997-4-1'),1)), repl = TRUE))
#' 
#' BBB <- data.frame(
#' Date = seq(as.Date('1993-3-28'),as.Date('1999-1-1'),1), 
#' BBB = sample(1:100,length(seq(as.Date('1993-3-28'),as.Date('1999-1-1'),1)), repl = TRUE))
#'  
#' CCC <- data.frame(
#' Date = seq(as.Date('1988-2-2'),as.Date('1996-1-1'),1), 
#' CCC = sample(1:100,length(seq(as.Date('1988-2-2'),as.Date('1996-1-1'),1)), repl = TRUE)) 
#' 
#' list <- list(AAA, BBB, CCC)# dput() and dget() can be used to save and load list file.
#' 
#' list_com <- extractPeriod(list, commonPeriod = TRUE)
#' 
#' # list_com is the extracted datalist.
#' str(list_com)
#' 
#' # If startDate and endDate is provided, the record between them will be extracted.
#' # make sure startDate is later than any startDate in each dataframe and endDate is 
#' # earlier than any endDate in each dataframe.
#' 
#' data(testdl)
#' datalist_com1 <- extractPeriod(testdl, startDate = '1994-1-1', endDate = '1995-10-1')
#' 
#' 
#' @import zoo
#' @export
extractPeriod <- function(datalist, startDate = NULL, endDate = NULL, commonPeriod = FALSE) {
  
  if (!is.null(startDate) & !is.null(endDate) & commonPeriod == FALSE) {
    dataset <- lapply(datalist, extractPeriod_dataset, startDate = startDate, endDate = endDate)
  } else if (is.null(startDate) & is.null(endDate) & commonPeriod == TRUE) {
    
    Dates <- lapply(datalist, extractPeriod_getDate) 
    Dates <- do.call('rbind', Dates)
    
    startDate <- as.Date(max(Dates[, 1]))
    endDate <- as.Date(min(Dates[, 2]))
    
    dataset <- lapply(datalist, extractPeriod_dataset, startDate = startDate, endDate = endDate)
    
  } else {
    stop('Enter startDate and endDate, set commonPeriod as False, or simply set commonPeriod as TRUE')
  }
  
  return(dataset)
}


#' Extract data from a dataframe with startDate and endDate
#' 
#' @param dataset A dataset with first column being a series of date or time.
#' @param startDate A date representing the start date.
#' @param endDate A date representing the end date.
#' @return The extracted dataframe between \code{startDate} and \code{endDate}.
extractPeriod_dataset <- function(dataset, startDate, endDate) {
  
  dataset[, 1] <- as.Date(dataset[, 1])
  
  startIndex <- which(dataset[, 1] == startDate)
  endIndex <- which(dataset[, 1] == endDate)
  if (length(startIndex) == 0 | length(endIndex) == 0) {
    stop('startDate and endDate exceeds the date limits in dataframe. Check datalsit please.')
  }
  output <- dataset[startIndex:endIndex, ]
  
  return(output)  
}

#' @importFrom utils tail
extractPeriod_getDate <- function(dataset) {
  
  if (!grepl('-|/', dataset[1, 1])) {
    stop('First column is not date or Wrong Date formate, check the format in ?as.Date{base}, 
          and use as.Date to convert.')
  }
  start <- as.Date(dataset[1, 1])
  end <- as.Date(tail(dataset[, 1], 1))
  
  
  return(c(start, end))
}
