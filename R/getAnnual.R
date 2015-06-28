#' Get annual rainfall of different rainfall time series
#' 
#' @param datalist A list containing different time series of different rainfall gauges.
#' @param output A string showing the output output.
#' @param minRecords A number showing the minimum accept record number, e.g. for a normal 
#' year(365 days), if \code{minRecords = 360}, it means if a year has less than 360 records
#' of a year, it will be ignored in the mean annual value calculation. Only valid 
#' when \code{output = "mean"}, default is 355.
#' @param ... \code{title, x, y} showing the title and x and y axis of the plot.
#' @return The annual rainfall and the number of missing data of each year and each rainfall gauge, which 
#' will also be plotted. If output "mean" is seleted, the mean annual rainfall will be returned.
#' @examples
#' #datalist is provided by the package as a test.
#' data(testdl)
#' a <- getAnnual(testdl)
#' #set minRecords to control the calculation of annual rainfall.
#' b <- getAnnual(testdl, output = 'mean', minRecords = 350)
#' c <- getAnnual(testdl, output = 'mean', minRecords = 365)
#' 
#' @export
#' @import ggplot2 reshape2 stats
getAnnual <- function(datalist, output = 'series', minRecords = 355, ...) {
  
  data <- lapply(datalist, FUN = getAnnual_dataframe)
  data <- do.call('rbind', data)   
  rownames(data) <- NULL   
  theme_set(theme_bw())
  
  if (output == 'mean') {
    validData <- data[data$recordNum >= minRecords,]
 
    data <- aggregate(validData$AnnualPreci, list(validData$Name), mean)
    colnames(data) <- c('Name', 'AnnualPreci')
    
    mainLayer <- with(data, {
      ggplot(data)+
      geom_bar(aes(x = Name, y = AnnualPreci, fill = Name), stat = 'identity')+
      labs(empty = NULL, ...)#in order to pass "...", arguments shouldn't be empty.
      
    })
    
    print(mainLayer)
    return(data)
    
  } else {
    
    plotData <- with(data, {
      subset(data, select = -recordNum)
    })
      
    plotData <- melt(plotData, var.id = c('Year', 'Name'))
    
    mainLayer <- with(plotData, {
      ggplot(plotData)+
      geom_bar(aes(x = as.Date(Year, format = '%Y'), y = value , fill = Name), 
               stat = 'identity')+
      facet_grid(variable ~ Name, scales = 'free')+
      xlab('Year')+
      ylab(NULL)+
      labs(empty = NULL, ...)+#in order to pass "...", arguments shouldn't be empty.
      theme(plot.title = element_text(size = 20, face = 'bold', vjust = 1))
      #      grid.arrange(mainLayer, ncol = 4)
        
    })
    
    
    print(mainLayer)
    return(data)
  }  
}



#' Get annual rainfall of the input time series.
#' 
#' @param dataset A dataframe containing one time series, e.g., rainfall from one gauging station.
#' the time should follow the format : "1990-1-1"
#' @return The annual rainfall of each year of the input station.
#' @examples
#' data(testdl)
#' getAnnual_dataframe(testdl[[1]])
#' 
#' @export
getAnnual_dataframe <- function(dataset) {
  
  if (!grepl('-|/', dataset[1, 1])) {
    stop ('First column is not date or Wrong Date formate, check the format in ?as.Date{base},
          and use as.Date to convert.')
  }
  Date <- as.Date(dataset[, 1])
  year <- format(Date, '%Y')
  yearUnique <- unique(year)
  calcuNum <- c(1:length(yearUnique))
  
  
  annualPreci <- tapply(dataset[, 2], INDEX = year, FUN = sum, na.rm = TRUE)
  recordNum <- tapply(dataset[, 2], INDEX = year, function(x) length(which(!is.na(x))))
  NANum <- tapply(dataset[, 2], INDEX = year, function(x) length(which(is.na(x))))


  name <- rep(colnames(dataset)[2], length(calcuNum))
  output <- data.frame(Year = yearUnique, Name = name, AnnualPreci = annualPreci,
                        recordNum, NANum)
  return(output)
}


