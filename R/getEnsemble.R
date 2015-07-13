#' getHisEnsem use historical data as the forecasting input time series.
#' 
#' @param TS A time series dataframe, with first column Date, and second column value.
#' @param example A vector containing two strings showing the start and end date, which can be used as an example.
#' This should be the first example, only the data earlier than the example will not be considered.
#' e.g. \code{example = c('1994-3-1', '1996-1-2')}, meaning the example period is from 1994-3-1 to 1996-1-2. And 
#' the program will extract every possible period in TS you provided to generate the ensemble. Check details for 
#' more information.
#' @param interval A number representing the interval of each ensemble member. NOTE: "interval" takes
#' 365 as a year, and 30 as a month, regardless of leap year and months with 31 days. So if you want the interval 
#' to be 2 years, set \code{interval = 730}, which equals 2 * 365 ; if two months, set \code{interval = 60}; 
#' 2 days, \code{interval = 2}, for other numbers that cannot be divided by 365 or 30 without remainder, it will treat the 
#' number as days.By defualt interval is set to be 365, a year.
#' @param buffer A number showing how many days are used as buffer period for models. Check details for more
#' information.
#' 
#' @param plot A boolean showing whether the plot will be shown, default is TRUE.
#' 
#' @details 
#' 
#' \code{interval} doesn't care about leap year and the months with 31 days, it will take 365 as a year, and 30 as a month.
#' e.g., if the interval is from 1999-2-1 to 1999-3-1, you should just set interval to 30, although the real interval is 28
#' days.
#' 
#' \code{example} and \code{interval} controls how the ensemble will be generated. e.g. if the time series is from 
#' 1990-1-1 to 2001-1-1.
#' 
#' if \code{example = c('1992-3-1', '1994-1-1')} and \code{interval = 1095}, note, 1095 = 365 * 3, so the program treat
#' this as 3 years.
#' 
#' Then you are supposed to get the ensemble consisting of following part:
#' 
#' 1. 1992-3-1 to 1994-1-1 first one is the example, and it's NOT start from 1990-3-1.
#' 2. 1995-3-1 to 1997-1-1 second one starts from 1993, because "interval" is 3 years.
#' 3. 1998-3-1 to 2000-1-1
#' 
#' because the last one "2000-3-1 to 2002-1-1", 2002 exceeds the original TS range, so it will not be included.
#' 
#' Sometimes, there are leap years and months with 31 days included in some ensemble part, in which case the length of the data will
#' be different, e.g., 1999-1-1 to 1999-3-1 is 1 day less than 2000-1-1 to 2000-3-1. In this situation,
#' the data will use example as a standard. If the example is 1999-1-1 to 1999-3-1, then the latter one
#' will be changed to 2001-1-1 to 2000-2-29, which keeps the start Date and change the end Date.
#' 
#' If the end date is so important that cannot be changed, try to solve this problem by resetting
#' the example period, to make the event included in the example.
#' 
#' Good set of example and by can generate good ensemble.
#' 
#' \code{buffer}
#' Sometimes the model needs to run for a few days to warm up, before the forecast. E.g., if a forecast starts at
#' '1990-1-20', for some model like MIKE NAM model, the run needs to be started about 14 days. So the input timeseries
#' should start from '1990-1-6'.
#' 
#' Buffer is mainly used for the model hotstart. Sometimes the hot start file cannot contain all the parameters needed,
#' only some important parameters. In this case, the model needs to run for some time, to make other parameters ready
#' for the simulation.
#' 
#' @return A ensemble time series using historical data as forecast.
#' 
#' @examples
#' 
#' data(testdl)
#' 
#' a <- testdl[[1]]
#' 
#' # Choose example from "1994-2-4" to "1996-1-4"
#' b <- getHisEnsem(a, example = c('1994-2-4', '1996-1-4'))
#' 
#' # Default interval is one year, can be set to other values, check help for information.
#' 
#' # Take 7 months as interval
#' b <- getHisEnsem(a, example = c('1994-2-4', '1996-1-4'), interval = 210) 
#' # Take 30 days as buffer
#' b <- getHisEnsem(a, example = c('1994-2-4', '1996-1-4'), interval = 210, buffer = 30)
#' @import reshape2 ggplot2
#' @export

getHisEnsem <- function (TS, example, interval = 365, buffer = 0, plot = TRUE) {
  if (!grepl('-|/', TS[1, 1])) {
    stop('First column is not date or Wrong Date formate, check the format in ?as.Date{base} 
         and use as.Date to convert.')
  } else if (!grepl('-|/', example[1]) | !grepl('-|/', example[1])) {
    stop('Wrong date format in the example, check the format in ?as.Date{base} 
         and use as.Date to convert.')
  } else {
    
    TS[, 1] <- as.Date(TS[, 1])
    example <- as.Date(example ,tz = '')
    exL <- example[2] - example[1]
    

    
    if (interval %% 365 == 0) {
      d <- interval / 365
      
      # Get sequence of start and end date.
      
      startDate <- rev(seq(from = example[1], to = min(TS[, 1]), by = paste(-d, 'years')))
      endDate <- seq(from = example[2], to = max(TS[, 1]), by = paste(d, 'years'))

      n <- length(startDate) + length(endDate) - 1 # example is counted twice, should be subtracted.      
      
      # Generate full start date series.
      startDate <- seq(min(startDate), length = n, by = paste(d, 'years'))
      endDate <- startDate + exL
      
    } else if (interval %% 30) {
      d <- interval / 30
      
      # Get sequence of start and end date.
      
      startDate <- rev(seq(from = example[1], to = min(TS[, 1]), by = paste(-d, 'months')))
      endDate <- seq(from = example[2], to = max(TS[, 1]), by = paste(d, 'months'))
      
      n <- length(startDate) + length(endDate) - 1
      
      startDate <- seq(min(startDate), length = n, by = paste(d, 'months'))
      endDate <- startDate + exL
      
    } else {
      d <- interval
      
      # Get sequence of start and end date.
      
      startDate <- rev(seq(from = example[1], to = min(TS[, 1]), by = paste(-d, 'days')))
      endDate <- seq(from = example[2], to = max(TS[, 1]), by = paste(d, 'days'))
      
      n <- length(startDate) + length(endDate) - 1
      
      startDate <- seq(min(startDate), length = n, by = paste(d, 'days'))
      endDate <- startDate + exL
    }
    
    data <- mapply(FUN = function(x, y) extractPeriod_dataset(dataset = TS, startDate = x, endDate = y),
                   x = startDate, y = endDate)
    
    data <- lapply(1:n, function(x) data.frame(data[, x]))
    
    if (buffer > 0) {
      bufferStart <- example[1] - buffer
      bufferEnd <- example[1] - 1
      bufferTS <- extractPeriod_dataset(TS, bufferStart, bufferEnd)
      
      data <- lapply(data, function(x) rbind(bufferTS, x))
      
    } else if (buffer < 0) {
      stop ('Buffer should be positive, or reset example.')
    }
    
    
    output <- list2Dataframe(data)
    colnames(output) <- c('Date', as.character(startDate))
    
    # Rearrange dataframe to make example the first column.
    ind <- match(c('Date', as.character(example[1])), colnames(output))
    output <- cbind(output[ind], output[-ind])
    ex_date <- seq(from = example[1] - buffer, to = example[2], by = 1)
    output$Date <- ex_date
    colnames(output)[2] <- 'Example'
    
    if (plot == TRUE) {
      
      data_ggplot <- melt(output, id.var = 'Date')
      
      mainLayer <- with(data_ggplot, {
        ggplot(data = data_ggplot) +
          aes(x = Date, y = value, color = variable, group = variable) +
          geom_line(size = 0.3) +
          geom_line(data = data_ggplot[data_ggplot$variable == 'Example', ], size = 2)
            
      })
      print(mainLayer)
    }
    
    return(output)
  }
}






#' getFrcEnsem extract different members' timeseries from forecasting data, if forecasting data has a member session.
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}, there should be a member part in the data part of the dataset.
#' @param cell A vector containing the locaton of the cell, e.g. c(2, 3), default is "mean", representing
#' the spatially averaged value. Check details for more information.
#' @param plot A boolean showing whether the plot will be shown, default is TRUE.
#' 
#' @details 
#' 
#' \code{cell} representing the location of the cell, NOTE: this location means the index of the cell,
#' IT IS NOT THE LONGITUDE AND LATITUDE. e.g., \code{cell = c(2, 3)}, the program will take the 2nd longitude
#' and 3rd latitude, by the increasing order. Longitude comes first.
#' 
#' 
#' @return A ensemble time series extracted from forecating data.
#' 
#' @import reshape2 ggplot2
#' @export
getFrcEnsem <- function(dataset, cell = 'mean', plot = TRUE) {
  # cell should be a vector showing the location, or mean representing the loacation averaged.
  
  checkWord <- c('Data', 'xyCoords', 'Dates')
  if (any(is.na(match(checkWord, attributes(dataset)$names)))) {
    stop('Input dataset is incorrect, it should contain "Data", "xyCoords", and "Dates", 
         check help for details.')
  }
  
  Date <- as.Date(dataset$Dates$start)
  data <- dataset$Data
  
  # Dimension needs to be arranged. Make sure first and second dimension is lat and lon.
  att <- attributes(data)$dimensions
  dimIndex <- seq(1, length(att))
  dimIndex1 <- match(c('lon', 'lat', 'time'), att)# match can apply to simple cases
  dimIndex2 <- dimIndex[-dimIndex1]# choose nomatch
  
  data <- aperm(data, c(dimIndex1, dimIndex2))
  attributes(data)$dimensions <- att[c(dimIndex1, dimIndex2)]
  
  if (!any(attributes(data)$dimensions == 'member')){
    stop('There is no member part in the dataset, check the input
         dataset or change your arguments.')
  }
  
  
  if (length(cell) == 2) {
    data_ensem <- data[cell[1], cell[2], , ]
    meanV <- apply(data_ensem, MARGIN = 1, FUN = mean, na.rm = TRUE)
    data_ensem <- data.frame('mean' = meanV, data_ensem) 
    
  } else if (cell == 'mean') {
    data_ensem <- apply(data, MARGIN = c(3, 4), FUN = mean, na.rm = TRUE)
#    colnames <- 1:ncol(data_ensem)
    meanV <- apply(data_ensem, MARGIN = 1, FUN = mean, na.rm = TRUE)
    data_ensem <- data.frame('mean' = meanV, data_ensem)
    
  } else {
    stop('Wrong cell input, check help for information.')
  }
  
  output <- data.frame(Date, data_ensem)
  
  if (plot == TRUE) {
    
    data_ggplot <- melt(output, id.var = 'Date')
    
    mainLayer <- with(data_ggplot, {
      ggplot(data = data_ggplot) +
        aes(x = Date, y = value, color = variable) +
        geom_line(size = 0.3) +
        geom_line(data = data_ggplot[data_ggplot$variable == 'mean', ], size = 2)
      
    })
    print(mainLayer)
  }
  
  return(output)
  
}

