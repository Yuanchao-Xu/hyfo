#' get mean rainfall bar plot of the input dataset
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}.
#' @param method A string showing the calculating method of the input time series, e.g., "meanMonthly"
#' @param outputData A boolean showing whether the result will be returned, default is T.
#' @param plotRange A boolean showing whether the range will be plotted.
#' @return The calculated mean value of the input time series and the plot of the result.
#' @export
getPreciBar <- function(dataset, method, outputData = T, plotRange = T){
  
  data <- dataset$Data
  startTime <- as.POSIXlt(dataset$Dates$start,tz = 'GMT')
  yearIndex <- startTime$year + 1900
  monthIndex <-startTime$mon + 1
  TS <- apply(data, MARGIN = 1,FUN = mean, na.rm = TRUE) 
  
  
  if (method == 'meanMonthly'){
    
    monthlypreci <- tapply(TS, INDEX = list(yearIndex,monthIndex),FUN = sum,na.rm = TRUE)
    meanMonthlyPreci <- apply(monthlypreci, MARGIN = 2, FUN = mean, na.rm=TRUE)
    
    title <- 'Mean Monthly Precipitation'
    xlab <- 'Month'
    
    plotPreci <- data.frame(index=month.abb[1:12],preci=meanMonthlyPreci)
    plotPreci$index <- factor(plotPreci$index,levels=plotPreci$index,ordered=T)
    
    if(plotRange){
      maxValue <- apply(monthlypreci, MARGIN = 2, FUN = max, na.rm=TRUE)
      minValue <- apply(monthlypreci, MARGIN = 2, FUN = min, na.rm=TRUE)
      
      plotPreci$maxValue <- maxValue
      plotPreci$minValue <- minValue
      
      ylim <- c(0,max(maxValue,na.rm=TRUE)*1.1)
      
    }else{
      ylim <- c(0,max(meanMonthlyPreci,na.rm=TRUE)*1.1)
    }
    
    
  }else if (method == 'annual'){
    annualPreci <- tapply(TS, INDEX = yearIndex, FUN = sum, na.rm = TRUE)
    title <- 'Annual Precipitation'
    xlab <- 'Year'
    plotName <- names(annualPreci)
    
    plotPreci <- data.frame(index=names(annualPreci),preci=annualPreci)
    plotPreci$index <- factor(plotPreci$index,levels=plotPreci$index,ordered=T)
    
    ylim <- c(0,max(annualPreci,na.rm=TRUE)*1.1)
    
  }else if (is.numeric(method)){
    month <- method
    monthlyPreci <- tapply(TS, INDEX = list(yearIndex,monthIndex),FUN = sum)[,month]
    
    plotPreci <- data.frame(index=names(monthlyPreci),preci=monthlyPreci)
    plotPreci$index <- factor(plotPreci$index,levels=plotPreci$index,ordered=T)
    
    title <- paste(month.abb[month],'Precipitation over Whole Period',sep = ' ')
    xlab <- 'Year'
    ylim <- c(0,max(monthlyPreci,na.rm=TRUE)*1.1)
    
  }
  
  
  xlim <- c(0,length(rownames(plotPreci))) 
  meanValue <- round(mean(plotPreci$preci, na.rm =TRUE),2)
  medianValue <- round(median(plotPreci$preci,na.rm=T),2)
  plotMean <- paste('Mean',' = ',meanValue)
  plotMedian <- paste('Median',' = ',medianValue)
  
  plotMax <- round(max(plotPreci$preci,na.rm=TRUE),2)
  plotMin <- round(min(plotPreci$preci,na.rm=TRUE),2)
  word <- paste(paste(' Max',' = ',plotMax),'\n',paste('Min',' = ',plotMin))
  
  
  
  
  mainLayer <- ggplot(plotPreci)+
    geom_bar(aes(x=index,y=preci),stat='identity',colour='black',fill='lightblue',width=.6)+
    xlab(xlab)+
    ylab('Precipitation (mm)')+
    ggtitle(title)+
    theme(plot.title=element_text(size=20,face='bold'),
          axis.title.x=element_text(size = 18),
          axis.title.y = element_text(size = 18))+
    geom_text(x=min(xlim)+0.95*(max(xlim)-min(xlim)),y=min(ylim)+0.15*(max(ylim)-min(ylim)),label=word)+
    geom_hline(yintercept=meanValue)+
    geom_text(x=min(xlim)+0.3*(max(xlim)-min(xlim)),y=meanValue+3,label=plotMean)+
    geom_hline(yintercept=medianValue,colour='red')+
    geom_text(x=min(xlim)+0.6*(max(xlim)-min(xlim)),y=medianValue+3,label=plotMedian,colour='red')
  
  
  if(plotRange){
    if(is.null(plotPreci$maxValue)){
      stop ('There is no plotRange for this method, set it as False')
    }else{
      rangeLayer <- geom_errorbar(aes(x = index,ymax=maxValue,ymin=minValue),width=0.3)
      print(mainLayer+rangeLayer)
    }
    
  }else{
    print(mainLayer)
  } 
  
  if (outputData == TRUE) return(plotPreci)
}

