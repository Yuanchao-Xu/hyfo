#' Get spatial map of the input dataset.
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}.
#' @param method A string showing different calculating method for the map. More information please refer to
#' details.
#' @param member A number showing which member is selected to get, if the dataset has a "member" dimension. Default
#' is NULL, if no member assigned, and there is a "member" in dimensions, the mean value of the members will be
#' taken.
#' @param ... Check \code{?getSpatialMap_mat} for details, e.g., x, y, title, catchment, 
#' points, output,
#' @return A matrix representing the raster map is returned, and the map is plotted.
#' @details
#' There are following methods to be selected, 
#' "meanAnnual": annual rainfall of each year is plotted.  
#' "winter", "spring", "autumn", "summer": MEAN seasonal rainfall of each year is plotted.
#' Month(number 1 to 12): MEAN month rainfall of each year is plotted, e.g. MEAN march rainfall of each year.
#' "mean", "max", "min": mean daily, maximum daily, minimum daily precipitation.
#' @examples
#' 
#' #gridData provided in the package is the result of \code {loadGridData{ecomsUDG.Raccess}}
#' data(tgridData)
#' getSpatialMap(tgridData, method = 'meanAnnual')
#' getSpatialMap(tgridData, method = 'winter')
#' 
#' 
#' getSpatialMap(tgridData, method = 'winter', catchment = testCat)
#' 
#' file <- system.file("extdata", "points.txt", package = "hyfo")
#' points <- read.table(file, header = TRUE, sep = ',' )
#' getSpatialMap(tgridData, method = 'winter', catchment = testCat, points = points)
#' 
#' @export
getSpatialMap <- function(dataset, method = NULL, member = NULL, ...) {

  #check input dataset
  checkWord <- c('Data', 'xyCoords', 'Dates')
  if (any(is.na(match(checkWord, attributes(dataset)$names)))) {
    stop('Input dataset is incorrect, it should contain "Data", "xyCoords", and "Dates", 
          check help for details.')
  }
  
  
  #range of the dataset just loaded 
  lon <- dataset$xyCoords$x
  lat <- dataset$xyCoords$y
  startTime <- as.POSIXlt(dataset$Dates$start, tz = 'GMT')
  yearIndex <- startTime$year + 1900
  monthIndex <-startTime$mon + 1
  data <- dataset$Data
  
  # Dimension needs to be arranged. Make sure first and second dimension is lat and lon.
  att <- attributes(data)$dimensions
  dimIndex <- seq(1, length(att))
  dimIndex1 <- match(c('lat', 'lon', 'time'), att)# match can apply to simple cases
  dimIndex2 <- dimIndex[-dimIndex1]# choose nomatch
  
  
  data <- aperm(data, c(dimIndex1, dimIndex2))
  attributes(data)$dimensions <- att[c(dimIndex1, dimIndex2)]
  
  # Because in the following part, only 3 dimensions are allowed, so data has to be processed.
  if (is.null(member) & any(attributes(data)$dimensions == 'member')) {
    dimIndex3 <- which(attributes(data)$dimensions != 'member')
    data <- apply(data, MARGIN = dimIndex3, FUN = mean, na.rm = TRUE)
  } else if (any(attributes(data)$dimensions == 'member')) {
    dimIndex3 <- which(attributes(data)$dimensions == 'member')
    data <- chooseDim(data, dimIndex3, member, drop = TRUE)
  }
  
  
  
  
  if (is.null(method)) {
    
    warning('You should shoose a method, unless input is a matrix directly to be plotted.')
    #in case the dataset is ready to plot and no need to calculate
    
  } else if (method == 'meanAnnual') { 
    #mean value of the annual precipitation over the period of the data 
    #time <- proc.time()
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex,  method = 'annual')
    #newTime <- proc.time() - time
    title_d  <- 'Mean Annual Precipitation (mm / year)'
    
  } else if (method == 'winter') {
    #mean value of the seasonal precipitation, in this case, winter 
    #time <- proc.time()
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'winter')
    #newTime <- proc.time() - time
    title_d <- 'Mean Winter Precipitation (mm / winter)'
    
  } else if (method == 'spring') {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'spring')    
    title_d <- 'Mean Spring Precipitation (mm / spring)'
    
  } else if (method == 'summer') {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'summer')    
    title_d <- 'Mean Summer Precipitation (mm / summer)'
    
  } else if (method == 'autumn') {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = 'autumn')    
    title_d <- 'Mean Autumn Precipitation (mm / autumn)'
    
  } else if (method == 'mean') {
    
    #sum value of the dataset, this procedure is to get the mean value
    data_new <- apply(data, MARGIN = c(1, 2), FUN = mean)
    title_d <- 'Mean Daily Precipitation (mm / day)'
    
  } else if (method == 'max') {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = max)
    title_d <- 'Max Daily Precipitation (mm / day)'
    
  } else if (method == 'min') {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = min)
    title_d <- 'Min Daily Precipitation (mm / day)'
    
  } else if (is.numeric(method)) {
    
    data_new <- apply(data, MARGIN = c(1, 2), FUN = getMeanPreci, yearIndex = yearIndex, monthIndex = monthIndex, 
                      method = method)    
    title_d <- paste(month.abb[method], 'Precipitation (mm / month)', sep = ' ')
    
  } else {
    wrongMethod <- method
    stop(paste('no method called', wrongMethod))
  }
  # This is to give attributes to the matrix and better be melted in ggplot.
  colnames(data_new) <- round(lon, 2)
  rownames(data_new) <- round(lat, 2)
  
  # If ... also has a title argument, this will cause conflicts. so title has to be renamed as title_d
  # This has to be paid a lot of attention when use ... to pass arguments.
  output <- getSpatialMap_mat(matrix = data_new, title_d = title_d, ...)
  return(output)
}





#' Get spatial map of the input dataset, and a matrix representing the raster map will be returned.
#' 
#' @param matrix A matrix raster, should be the result of \code{getSpatialMap()}, output should be default
#' or 'data'
#' @param title_d A string showing the title of the plot, defaut is NULL.
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
#' @examples
#' data(tgridData)# the result of \code{loadGridData{ecomsUDG.Raccess}}
#' #the output type of has to be default or 'data'.
#' a1 <- getSpatialMap(tgridData, method = 'mean')
#' a2 <- getSpatialMap(tgridData, method = 'max')
#' a3 <- getSpatialMap(tgridData, method = 'winter')
#' a4 <- getSpatialMap(tgridData, method = 'summer')
#' #For example, if we want to investigate the difference between mean value and max.
#' 
#' a5 <- a2 - a1
#' getSpatialMap_mat(a4)
#' 
#' #Or to investigate the difference between winter value and summer value.
#' a6 <- a3 - a4
#' getSpatialMap_mat(a6)
#' 
#' @export
#' @import ggplot2 rgdal plyr maps
getSpatialMap_mat <- function(matrix, title_d = NULL, catchment = NULL, points = NULL, output = 'data', 
                              info = TRUE, scale = 'identity', ...) {
  #check input
  checkWord <- c('lon', 'lat', 'z', 'value')
  if (is.null(attributes(matrix)$dimnames)) {
    stop('Input matrix is incorrect, check help to know how to get the matrix.')
  } else if (!is.null(catchment) & class(catchment) != "SpatialPolygonsDataFrame") {
    stop('Catchment format is incorrect, check help to get more details. ')
  } else if (!is.null(points) & any(is.na(match(checkWord, attributes(points)$names)))) {
    stop('Points should be a dataframe with colnames "lon, lat, z, value".')
  }
  
  #ggplot
  #for the aes option in ggplot, it's independent from any other command through all ggplot, and aes() function
  #get data from the main dataset, in this case, data_ggplot. for other functions in ggplot, if it wants to use 
  #data from the main dataset as parameters, it has to use aes() function. if not, it has to use data available 
  #in the environment.
  #in other words, all the parameters in aes(), they have to come from the main dataset. Otherwise, just put them
  #outside aes() as normal parameters.
  
  if (info == TRUE) {  
    plotMax <- round(max(matrix, na.rm = TRUE), 2)
    plotMin <- round(min(matrix, na.rm = TRUE), 2)
    plotMean <- round(mean(matrix, na.rm = TRUE), 2)
    plotMedian <- round(median(matrix, na.rm = TRUE), 2)
    word <- paste('\n\n', paste('Max', '=', plotMax), ',', paste('Min', '=', plotMin), ',',
                  paste('Mean', '=', plotMean), ',', paste('Median', '=', plotMedian))
  } else {
    word <- NULL
  }
  
  x_word <- paste('Longitude', word)
  world_map <- map_data('world')
  
  data_ggplot <- melt(matrix, na.rm = TRUE)
  colnames(data_ggplot) <- c('lat', 'lon', 'value')
  theme_set(theme_bw())
  
  mainLayer <- with(data_ggplot, {
    
    ggplot(data = data_ggplot) +
    geom_tile(aes(x = lon, y = lat, fill = value)) +
    #scale_fill_gradient(high = 'red', low = 'yellow')+
    scale_fill_gradientn(colours = c('yellow', 'orange', 'red'), na.value = 'transparent',
                         guide = guide_colorbar(title='Rainfall (mm)', barheight = rel(9)), trans = scale) +#usually scale = 'sqrt'
    geom_map(data = world_map, map = world_map, aes(map_id = region), fill='transparent', color='black') +
    #    guides(fill = guide_colorbar(title='Rainfall (mm)', barheight = 15))+
    xlab(x_word) +
    ylab('Latitude') +
    ggtitle(title_d) +
    labs(empty = NULL, ...) +#in order to pass "...", arguments shouldn't be empty.
    theme(plot.title = element_text(size = rel(1.3), face = 'bold'),
          axis.title.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)))
#   geom_rect(xmin=min(lon)+0.72*(max(lon)-min(lon)),
#             xmax=min(lon)+0.99*(max(lon)-min(lon)),
#             ymin=min(lat)+0.02*(max(lat)-min(lat)),
#             ymax=min(lat)+0.28*(max(lat)-min(lat)),
#             fill='white',colour='black')+
#   annotate('text', x = min(lon), y = min(lat), label=word, hjust = 0, vjust = -1)
  
  })
  
  printLayer <- mainLayer
  
  #catchment conversion
  if (is.null(catchment) == FALSE) {
    a <- catchment
    a@data$id <- rownames(a@data)
    b <- fortify(a, region = 'id')
    c <- join(b, a@data, by = 'id')
    catchmentLayer <- with(c, {
      geom_polygon(data = c, aes(long, lat, group = group), color = 'black', 
                                   fill = 'transparent')
    })
      
    
    printLayer <- printLayer + catchmentLayer
  }
  #plot points
  if (is.null(points) == FALSE) {
    pointLayer <- with(points, {
      geom_point(data = points, aes(x = lon, y = lat, size = value, colour = z),
                 guide = guide_legend(barheight = rel(3)))
        
        
    })
    
    printLayer <- printLayer + pointLayer
  }
  
  print(printLayer)
  
  if (output == 'ggplot') {
    data_ggplot$Name <- rep(title_d, dim(data_ggplot)[1])
    return (data_ggplot)
  } else if (output == 'plot') {
    return(printLayer)
  } else {
    return(matrix)
  }
}


#' Combine maps together
#' @param ... different maps generated by \code{getSpatialMap(, output = 'ggplot')}
#' @param nrow A number showing the number of rows.
#' @param list If input is a list containing different ggplot data, use l\code{list = inputlist}.
#' @return A combined map.
#' @examples
#' data(tgridData)# the result of \code{loadGridData{ecomsUDG.Raccess}}
#' #The output should be 'ggplot'
#' a1 <- getSpatialMap(tgridData, method = 'summer', output = 'ggplot')
#' a2 <- getSpatialMap(tgridData, method = 'winter', output = 'ggplot')
#' a3 <- getSpatialMap(tgridData, method = 'mean', output = 'ggplot')
#' a4 <- getSpatialMap(tgridData, method = 'max', output = 'ggplot')
#' getSpatialMap_comb(a1, a2, a3, a4)
#' getSpatialMap_comb(a1, a2, a3, a4, nrow = 2)
#' 
#' @export
#' @import ggplot2 maps
getSpatialMap_comb <- function(..., list = NULL, nrow = 1) {
  
  
  if (!is.null(list)) {
    data_ggplot <- do.call('rbind', list)
  } else {
    maps <- list(...)
    data_ggplot <- do.call('rbind', maps)
  }
  data_ggplot$Name <- factor(data_ggplot$Name, levels = data_ggplot$Name, ordered = TRUE)
  
  world_map <- map_data('world')
  theme_set(theme_bw())
  mainLayer <- with(data_ggplot, {
    ggplot(data = data_ggplot) + 
    geom_tile(aes(x = lon, y = lat, fill = value)) +
    #scale_fill_gradient(high = 'red', low = 'yellow')+
    scale_fill_gradientn(colours = c('yellow', 'orange', 'red'), na.value = 'transparent') +#usually scale = 'sqrt'
    geom_map(data = world_map, map = world_map, aes(map_id = region), fill = 'transparent', color = 'black') +
    facet_wrap(~ Name, nrow = nrow)
  })
  
  suppressWarnings(print(mainLayer))
}




chooseDim <- function(array, dim, value, drop = FALSE) { 
  # Create list representing arguments supplied to [
  # bquote() creates an object corresponding to a missing argument
  indices <- rep(list(bquote()), length(dim(array)))
  indices[[dim]] <- value
  
  # Generate the call to [
  call <- as.call(c(
    list(as.name("["), quote(array)),
    indices,
    list(drop = drop)))
  # Print it, just to make it easier to see what's going on
  print(call)
  
  # Finally, evaluate it
  eval(call)
}

