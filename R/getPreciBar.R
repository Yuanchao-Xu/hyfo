#' get mean rainfall bar plot of the input dataset
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}.
#' @param method A string showing the calculating method of the input time series, including: "meanMonthly",
#' "annual", and one umber from 1 to 12 representing the month.
#' @param output A string showing the type of the output, if \code{output = 'ggplot'}, the returned 
#' data can be used in ggplot and \code{getPreciBar_comb()}; if \code{output = 'plot'}, the returned data is the plot containing all 
#' layers' information, and can be plot directly or used in grid.arrange; if not set, the data
#' will be returned.
#' @param plotRange A boolean showing whether the range will be plotted.
#' @param ... \code{title, x, y} showing the title and x and y axis of the plot.
#' @return The calculated mean value of the input time series and the plot of the result.
#' @export
getPreciBar <- function(dataset, method, output = 'data', plotRange = T, ...){
  
  data <- dataset$Data
  startTime <- as.POSIXlt(dataset$Dates$start, tz = 'GMT')
  yearIndex <- startTime$year + 1900
  monthIndex <-startTime$mon + 1
  TS <- apply(data, MARGIN = 1,FUN = mean, na.rm = TRUE) 
  
  
  if (method == 'meanMonthly'){
    
    monthlypreci <- tapply(TS, INDEX = list(yearIndex,monthIndex),FUN = sum, na.rm = TRUE)
    meanMonthlyPreci <- apply(monthlypreci, MARGIN = 2, FUN = mean, na.rm = TRUE)
    
    title <- 'Mean Monthly Precipitation'
    xlab <- 'Month'
    
    plotPreci <- data.frame(Index = month.abb[1:12], Preci = meanMonthlyPreci)
    plotPreci$Index <- factor(plotPreci$Index,levels = plotPreci$Index,ordered=T)
    
    if(plotRange){
      maxValue <- apply(monthlypreci, MARGIN = 2, FUN = max, na.rm = TRUE)
      minValue <- apply(monthlypreci, MARGIN = 2, FUN = min, na.rm = TRUE)
      
      plotPreci$maxValue <- maxValue
      plotPreci$minValue <- minValue
      
      ylim <- c(0,max(maxValue,na.rm = TRUE) * 1.1)
      
    }else{
      ylim <- c(0,max(meanMonthlyPreci, na.rm = TRUE) * 1.1)
    }
    
    
  }else if (method == 'annual'){
    annualPreci <- tapply(TS, INDEX = yearIndex, FUN = sum, na.rm = TRUE)
    title <- 'Annual Precipitation'
    xlab <- 'Year'
    plotName <- names(annualPreci)
    
    plotPreci <- data.frame(Index = names(annualPreci),Preci = annualPreci)
    plotPreci$Index <- factor(plotPreci$Index,levels = plotPreci$Index,ordered=T)
    
    ylim <- c(0,max(annualPreci,na.rm = TRUE) * 1.1)
    
  }else if (is.numeric(method)){
    month <- method
    monthlyPreci <- tapply(TS, INDEX = list(yearIndex,monthIndex),FUN = sum)[,month]
    
    plotPreci <- data.frame(Index = names(monthlyPreci), Preci = monthlyPreci)
    plotPreci$Index <- factor(plotPreci$Index, levels = plotPreci$Index, ordered = T)
    
    title <- paste(month.abb[month], 'Precipitation over Whole Period', sep = ' ')
    xlab <- 'Year'
    ylim <- c(0,max(monthlyPreci, na.rm=TRUE) * 1.1)
    
  }else{
    stop (paste('No method called "', method, '", check help for information'))
  }
  
  
  xlim <- c(0,length(rownames(plotPreci))) 
  meanValue <- round(mean(plotPreci$Preci, na.rm = TRUE), 2)
  medianValue <- round(median(plotPreci$Preci,na.rm = T), 2)
  plotMean <- paste('Mean',' = ', meanValue)
  plotMedian <- paste('Median',' = ', medianValue)
  
  plotMax <- round(max(plotPreci$Preci, na.rm = TRUE), 2)
  plotMin <- round(min(plotPreci$Preci, na.rm = TRUE), 2)
  word <- paste('\n\n', paste(' Max', '=', plotMax), ',', paste('Min', '=', plotMin), ',',
                plotMean, ',', plotMedian)
  
  xlab <- paste(xlab, word)
  
  
  mainLayer <- ggplot(plotPreci)+
    geom_bar(aes(x = Index, y = Preci), stat = 'identity', colour = 'black', fill = 'lightblue', width = .6)+
    xlab(xlab)+
    ylab('Precipitation (mm)')+
    ggtitle(title)+
    labs(empty = NULL, ...)+#in order to pass "...", arguments shouldn't be empty.
    theme(plot.title = element_text(size = 20, face = 'bold'),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18))+
#    geom_text(x = min(xlim) + 0.95 * (max(xlim) - min(xlim)), y = min(ylim) + 0.15 * (max(ylim) - min(ylim)),
#              label = word)+
    geom_hline(yintercept = meanValue)+
    geom_text(x = min(xlim) + 0.3 * (max(xlim) - min(xlim)), y = meanValue + 3, vjust = 0, label = 'mean')+
    geom_hline(yintercept = medianValue, colour = 'red')+
    geom_text(x = min(xlim) + 0.6 * (max(xlim) - min(xlim)), y = medianValue + 3, vjust = 0,
              label = 'median', colour='red')
  
  
  if(plotRange){
    if(is.null(plotPreci$maxValue)){
      warning ('There is no plotRange for this method')
      print(mainLayer)
    }else{
      rangeLayer <- geom_errorbar(aes(x = Index,ymax = maxValue, ymin = minValue), width = 0.3)
      print(mainLayer + rangeLayer)
    }
    
  }else{
    print(mainLayer)
  } 
  
  if (output == 'plot') {
    return(mainLayer)
  }else if (output == 'ggplot') {
    plotPreci$Name <- rep(title, dim(plotPreci)[1])
    return(plotPreci)
  }else{
    return(plotPreci)
  }
}


#' Combine bars together
#' @param ... different barplots generated by \code{getPreciBar(, output = 'ggplot')}
#' @param nrow A number showing the number of rows.
#' @param list If input is a list containing different ggplot data, use l\code{list = inputlist}.
#' @return A combined barplot.
#' @export
#' @import ggplot2
getPreciBar_comb <- function(..., list = NULL, nrow = 1){
  if(!is.null(list)){
    data_ggplot <- do.call('rbind', list)
  }else{
    
    bars <- list(...)
    data_ggplot <- do.call('rbind', bars)
  }

  data_ggplot$Name <- factor(data_ggplot$Name, levels = data_ggplot$Name, ordered = T)
  
  theme_set(theme_bw())
  mainLayer <- ggplot(data_ggplot)+
    geom_bar(aes(x = Index, y = Preci), stat = 'identity', colour = 'black', fill = 'lightblue', width = .6)+
    facet_wrap(~Name, nrow = nrow)
  
  print (mainLayer)
}

