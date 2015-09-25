#' Get annual rainfall of different rainfall time series
#' 
#' @param datalist A list containing different time series of different rainfall gauges.
#' @param output A string showing the output output.
#' @param minRecords A number showing the minimum accept record number, e.g. for a normal 
#' year(365 days), if \code{minRecords = 360}, it means if a year has less than 360 records
#' of a year, it will be ignored in the mean annual value calculation. Only valid 
#' when \code{output = "mean"}, default is 355.
#' @param dataframe A dataframe with first column Date and the rest columns the value of different
#' gauging stations. Usually an output of \code{list2Dataframe}. When you input dataframe, you have
#' to put \code{dataframe =}, or it will take the input as a datalist.
#' @param ... \code{title, x, y} showing the title and x and y axis of the plot. e.g. \code{title = 'aaa'}
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
#' a1 <- extractPeriod(testdl, comm = TRUE)
#' a2 <- list2Dataframe(a1)
#' getAnnual(dataframe = a2)
#' 
#' a3 <- fillGap(a2)
#' getAnnual(dataframe = a3)
#' 
#' 
#' # More examples can be found in the user manual on http://yuanchao-xu.github.io/hyfo/
#' 
#' @export
#' @import ggplot2 
#' @importFrom reshape2 melt
#' @importFrom stats aggregate
#' @references 
#' 
#' \itemize{
#' \item H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
#' \item Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software,
#' 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
#' \item R Core Team (2015). R: A language and environment for statistical computing. R Foundation for
#' Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
#' }
#' 
#' 
#' 
getAnnual <- function(datalist, output = 'series', minRecords = 355, dataframe = NULL,
                      ...) {
  
  # First check is dataframe input is empty.
  if (!is.null(dataframe)) {
    Date <- as.POSIXlt(dataframe[, 1])
    # Calculate how many gauging stations.
    stations <- colnames(dataframe)[2:ncol(dataframe)]
    
    data <- lapply(stations, function(x) {
      dataframe_new <- data.frame(Date, dataframe[, x])
      colnames(dataframe_new)[2] <- x
      getAnnual_dataframe(dataframe_new)
    })
    
  } else {
    data <- lapply(datalist, FUN = getAnnual_dataframe)
  }
  
  
  
  data <- do.call('rbind', data)
#  After rbind, factor level has to be reassigned in order to be well plotted.
  data$Year <- factor(data$Year, levels = sort(unique(data$Year)), ordered = TRUE)
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
      subset(data, select = c(Year, Name, NANum, AnnualPreci))
    })
    
    plotData <- melt(plotData, var.id = c('Year', 'Name'))
    
    
    mainLayer <- with(plotData, {
      ggplot(plotData) +
      geom_bar(aes(x = Year, y = value , fill = Name), 
               stat = 'identity') +
      facet_grid(variable ~ Name, scale = 'free') +
      xlab('Year') +
      ylab(NULL) +
      labs(empty = NULL, ...) +#in order to pass "...", arguments shouldn't be empty.
      theme(plot.title = element_text(size = 20, face = 'bold', vjust = 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)))
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
# @examples
# data(testdl)
# getAnnual_dataframe(testdl[[1]])
#' 
getAnnual_dataframe <- function(dataset) {
  
  if (!grepl('-|/', dataset[1, 1])) {
    stop ('First column is not date or Wrong Date formate, check the format in ?as.Date{base},
          and use as.Date to convert.')
  }
  Date <- as.Date(dataset[, 1])
  year <- format(Date, '%Y')
  yearUnique <- unique(year)
#  yearUnique <- factor(yearUnique, levels = yearUnique, ordered = TRUE)
  calcuNum <- c(1:length(yearUnique))
  
  
  annualPreci <- tapply(dataset[, 2], INDEX = year, FUN = sum, na.rm = TRUE)
  recordNum <- tapply(dataset[, 2], INDEX = year, function(x) length(which(!is.na(x))))
  NANum <- tapply(dataset[, 2], INDEX = year, function(x) length(which(is.na(x))))


  name <- rep(colnames(dataset)[2], length(calcuNum))
  output <- data.frame(Year = as.numeric(yearUnique), Name = name, AnnualPreci = annualPreci,
                        recordNum, NANum)
  
  #output$Year <- factor(output$Year, levels = output$Year, ordered = TRUE)
  return(output)
}


