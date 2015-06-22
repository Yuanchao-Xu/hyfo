#' Get spatial map of the input dataset.
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}.
#' @param method A string showing different calculating method for the map.
#' @inheritParams getSpatialMap_mat
#' @return A matrix representing the raster map is returned, and the map is plotted.
#' @export
getSpatialMap <- function(dataset, method = NULL, ...){

  #check input dataset
  checkWord <- c('Data', 'xyCoords', 'Dates')
  if(any(is.na(match(checkWord, attributes(dataset)$names)))){
    stop ('Input dataset is incorrect, it should contain "Data", "xyCoords", and "Dates", 
          check help for details.')
  }
  
  
  #range of the dataset just loaded 
  lon <- dataset$xyCoords$x
  lat <- dataset$xyCoords$y
  startTime <- as.POSIXlt(dataset$Dates$start,tz = 'GMT')
  yearIndex <- startTime$year + 1900
  monthIndex <-startTime$mon + 1
  data <- dataset$Data
  
  if (is.null(method)){
    warning ('You should shoose a method, unless input is a matrix directly to be plotted.')
    #in case the dataset is ready to plot and no need to calculate
  }else if(method == 'meanAnnual'){
    #mean value of the annual precipitation over the period of the data 
    #time <- proc.time()
    data_new <- apply(data, MARGIN = c(2,3), FUN = getMeanPreci, yearIndex = yearIndex,  method = 'meanAnnualPreci')
    #newTime <- proc.time() - time
    title  <- 'Mean Annual Precipitation (mm / year)'
    
  }else if(method == 'winter'){
    #mean value of the seasonal precipitation, in this case, winter
    
    #time <- proc.time()
    data_new <- apply(data, MARGIN = c(2,3), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'winter')
    #newTime <- proc.time() - time
    title <- 'Mean Winter Precipitation (mm / winter)'
    
  }else if(method == 'spring'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'spring')
    
    title <- 'Mean Spring Precipitation (mm / spring)'
    
  }else if (method == 'summer'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'summer')
    
    title <- 'Mean Summer Precipitation (mm / summer)'
    
  }else if (method == 'autumn'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'autumn')
    
    title <- 'Mean Autumn Precipitation (mm / autumn)'
    
  }else if(method == 'mean'){
    #sum value of the dataset, this procedure is to get the mean value
    data_new <- apply(data, MARGIN = c(2,3), FUN = mean)
    title <- 'Mean Daily Precipitation (mm / day)'
  }else if(method == 'max'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = max)
    title <- 'Max Daily Precipitation (mm / day)'
  }else if(method == 'min'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = min)
    title <- 'Min Daily Precipitation (mm / day)'
  }else{
    wrongMethod <- method
    stop (paste('no method called',wrongMethod))
  }
  #this is to give attributes to the matrix and better be melted in ggplot.
  colnames(data_new) <- round(lon,2)
  rownames(data_new) <- round(lat,2)
  
  output <- getSpatialMap_mat(data_new, title, ...)
  return (output)
}





#' Get spatial map of the input dataset, and a matrix representing the raster map will be returned.
#' 
#' @param matrix A matrix raster, should be the result of \code{getSpatialMap()}.
#' @param title A string showing the title of the plot, defaut is NULL.
#' @param catchment A catchment file geting from \code{shp2cat()} in the package, if a catchment is available for background.
#' @param points A dataframe, showing other information, e.g., location of the gauging stations. The 
#' the data.frame should be with columes "name, lon, lat, z, value".
#' @param output A string showing the type of the output, if \code{output = 'ggplot'}, the returned 
#' data can be used in ggplot and \code{getSpatialMap_comb()}; if \code{output = 'plot'}, the returned data is the plot containing all 
#' layers' information, and can be plot directly or used in grid.arrange; if not set, the raster matrix data
#' will be returned.
#' @param info A boolean showing whether the information of the map, e.g., max, mean ..., default is T.
#' @param scale A string showing the plot scale, 'identity' or 'sqrt'.
#' @param ... \code{title, y} showing the title and x and y axis of the plot, default is about precipitation.
#' @return A matrix representing the raster map is returned, and the map is plotted.
#' @export
#' @import ggplot2 rgdal plyr
getSpatialMap_mat <- function(matrix, title = NULL, catchment = NULL, points = NULL, output = 'data', 
                              info = T, scale = 'identity', ...){
  #check input
  checkWord <- c('lon', 'lat', 'z', 'value')
  if (is.null(attributes(matrix)$dimnames)){
    stop ('Input matrix is incorrect, check help to know how to get the matrix.')
  }else if (!is.null(catchment) & class(catchment) != "SpatialPolygonsDataFrame"){
    stop ('Catchment format is incorrect, check help to get more details. ')
  }else if (!is.null(points) & any(is.na(match(checkWord, attributes(points)$names)))){
    stop ('Points should be a dataframe with colnames "lon, lat, z, value".')
  }
  
  #ggplot
  #for the aes option in ggplot, it's independent from any other command through all ggplot, and aes() function
  #get data from the main dataset, in this case, data_ggplot. for other functions in ggplot, if it wants to use 
  #data from the main dataset as parameters, it has to use aes() function. if not, it has to use data available 
  #in the environment.
  #in other words, all the parameters in aes(), they have to come from the main dataset. Otherwise, just put them
  #outside aes() as normal parameters.
  
  if (info == T) {
    plotMax <- round(max(matrix,na.rm=TRUE),2)
    plotMin <- round(min(matrix,na.rm=TRUE),2)
    plotMean <- round(mean(matrix,na.rm=TRUE),2)
    plotMedian <- round(median(matrix,na.rm=T),2)
    word <- paste('\n\n', paste('Max', '=', plotMax), ',', paste('Min', '=', plotMin), ',',
                  paste('Mean', '=', plotMean), ',', paste('Median', '=', plotMedian))
  }else{
    word <- NULL
  }
  
  x_word <- paste('Longitude', word)
  world_map <- map_data('world')
  
  data_ggplot <- melt(matrix, na.rm = T)
  colnames(data_ggplot) <- c('lat', 'lon', 'value')
  theme_set(theme_bw())
  mainLayer <- ggplot(data = data_ggplot)+ 
    geom_tile(aes(x = lon, y = lat, fill = value))+
    #scale_fill_gradient(high = 'red', low = 'yellow')+
    scale_fill_gradientn(colours = c('yellow', 'orange', 'red'), na.value = 'transparent',
                         guide = guide_colorbar(title='Rainfall (mm)', barheight = 15), trans = scale)+#usually scale = 'sqrt'
    geom_map(data = world_map, map = world_map, aes(map_id = region), fill='transparent', color='black')+
    #    guides(fill = guide_colorbar(title='Rainfall (mm)', barheight = 15))+
    xlab(x_word)+
    ylab('Latitude')+
    ggtitle(title)+
    labs(empty = NULL, ...)+#in order to pass "...", arguments shouldn't be empty.
    theme(plot.title = element_text(size = 20, face = 'bold'),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18))
  #     geom_rect(xmin=min(lon)+0.72*(max(lon)-min(lon)),
  #               xmax=min(lon)+0.99*(max(lon)-min(lon)),
  #               ymin=min(lat)+0.02*(max(lat)-min(lat)),
  #               ymax=min(lat)+0.28*(max(lat)-min(lat)),
  #               fill='white',colour='black')+
  #   annotate('text', x = min(lon), y = min(lat), label=word, hjust = 0, vjust = -1)
  
  printLayer <- mainLayer
  
  #catchment conversion
  if(is.null(catchment) == FALSE){
    a <- catchment
    a@data$id <- rownames(a@data)
    b <- fortify(a,region='id')
    c <- join(b,a@data,by='id')
    catchmentLayer <- geom_polygon(data=c, aes(long,lat,group=group), color='black', fill='transparent')
    
    printLayer <- printLayer + catchmentLayer
  }
  #plot points
  if(is.null(points) == FALSE){
    pointLayer <- geom_point(data = points,aes(x = lon, y = lat, size = value, colour = z))
    
    printLayer <- printLayer + pointLayer
  }
  
  print (printLayer)
  
  if(output == 'ggplot') {
    data_ggplot$Name <- rep(title, dim(data_ggplot)[1])
    return (data_ggplot)
  }else if (output == 'plot'){
    return(printLayer)
  }else{
    return(matrix)
  }
}


#' Combine maps together
#' @param ... different maps generated by \code{getSpatialMap(, output = 'ggplot')}
#' @param nrow A number showing the number of rows.
#' @param list If input is a list containing different ggplot data, use l\code{list = inputlist}.
#' @return A combined map.
#' @export
#' @import ggplot2
getSpatialMap_comb <- function(..., list = NULL, nrow = 1){
  
  
  if (!is.null(list)){
    data_ggplot <- do.call('rbind', list)
  }else{
    maps <- list(...)
    data_ggplot <- do.call('rbind', maps)
  }
  
  
  world_map <- map_data('world')
  theme_set(theme_bw())
  mainLayer <- ggplot(data = data_ggplot)+ 
    geom_tile(aes(x = lon, y = lat, fill = value))+
    #scale_fill_gradient(high = 'red', low = 'yellow')+
    scale_fill_gradientn(colours = c('yellow', 'orange', 'red'), na.value = 'transparent')+#usually scale = 'sqrt'
    geom_map(data = world_map, map = world_map, aes(map_id = region), fill = 'transparent', color='black')+
    facet_wrap(~ Name, nrow = nrow)
  print (mainLayer)
}

