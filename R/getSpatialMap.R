#' Get spatial map of the input dataset, and a matrix representing the raster map will be returned.
#' 
#' @param dataset A list containing different information, should be the result of reading netcdf file using
#' \code{library(ecomsUDG.Raccess)}.
#' @param catchment A catchment file geting from \code{shp2cat()} in the package, if a catchment is available for background.
#' @param points A shape file showing other information, e.g., location of the gauging stations.
#' @param method A string showing different calculating method for the map.
#' @param outputData A boolean showing whether the raster matrix will be returned, default is T.
#' @param plotScale A string showing the plot scale, e.g., "identity" or "sqrt"
#' @return A matrix representing the raster map is returned, and the map is plotted.
#' @export
#' @import ggplot2 rgdal
getSpatialMap <- function(dataset, catchment = NULL,points = NULL, method = NULL, outputData = TRUE, 
                          plotScale = 'identity'){
  message('used for showing the spatial map for parameters like precipitation.
          different method are provided for analysing the parameters
          catchment needs shape file
          points needs data.frame, with colume "name, lon, lat, z, value" in data.frame()
          method = NULL means no method calculated on the cell')
  
  #range of the dataset just loaded 
  lon <- dataset$xyCoords$x
  lat <- dataset$xyCoords$y
  startTime <- as.POSIXlt(dataset$Dates$start,tz = 'GMT')
  yearIndex <- startTime$year + 1900
  monthIndex <-startTime$mon + 1
  data <- dataset$Data
  
  if (is.null(method)){
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
  }else if(method == 'max'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = max)   
  }else if(method == 'min'){
    data_new <- apply(data, MARGIN = c(2,3), FUN = min)
  }else{
    wrongMethod <- method
    stop (paste('no method called',wrongMethod))
  }
  
  
  zlim <- NULL
  if(method == 'winter'| method == 'spring' | method == 'autumn'| method == 'summer') zlim = c(0,730)
  
  plotMax <- round(max(data_new,na.rm=TRUE),2)
  plotMin <- round(min(data_new,na.rm=TRUE),2)
  plotMean <- round(mean(data_new,na.rm=TRUE),2)
  plotMedian <- round(median(data_new,na.rm=T),2)
  word <- paste(paste('Max',' = ',plotMax),'\n',paste('Min',' = ',plotMin),'\n',
                paste('Mean',' = ',plotMean),'\n',paste('Median',' = ',plotMedian))
  
  #set names for the matrix, in order to be better converted later in ggplot.
  colnames(data_new) <- round(lon,2)
  rownames(data_new) <- round(lat,2)
  
  world_map <- ggplot2::map_data('world')
  
  
  #ggplot
  #for the aes option in ggplot, it's independent from any other command through all ggplot, and aes() function
  #get data from the main dataset, in this case, data_ggplot. for other functions in ggplot, if it wants to use 
  #data from the main dataset as parameters, it has to use aes() function. if not, it has to use data available 
  #in the environment.
  #in other words, all the parameters in aes(), they have to come from the main dataset. Otherwise, just put them
  #outside aes() as normal parameters.
  
  data_ggplot <- reshape2::melt(data_new,na.rm = T)
  colnames(data_ggplot) <- c('lat','lon','value')
  theme_set(theme_bw())
  mainLayer <- ggplot(data = data_ggplot)+ 
    geom_tile(aes(x=lon,y=lat,fill = value))+
    scale_fill_gradientn(colours = c('yellow','orange','red'),
                         na.value='transparent',trans=plotScale)+#usually scale = 'sqrt'
    geom_map(data = world_map,map=world_map,aes(map_id=region),fill='transparent',color='black')+
    guides(fill=guide_colorbar(title='Rainfall (mm)',barheight = 10))+
    xlab('Longitude')+
    ylab('Latitude')+
    ggtitle(title)+
    theme(plot.title=element_text(size=20,face='bold'),
          axis.title.x=element_text(size = 18),
          axis.title.y = element_text(size = 18))+
#     geom_rect(xmin=min(lon)+0.72*(max(lon)-min(lon)),
#               xmax=min(lon)+0.99*(max(lon)-min(lon)),
#               ymin=min(lat)+0.02*(max(lat)-min(lat)),
#               ymax=min(lat)+0.28*(max(lat)-min(lat)),
#               fill='white',colour='black')+
    annotate('text', x = min(lon), y = min(lat), label=word, hjust = 0, vjust = -1)
  
  printLayer <- mainLayer
  
  #catchment conversion
  if(is.null(catchment) == FALSE){
    a <- catchment
    a@data$id <- rownames(a@data)
    b <- fortify(a,region='id')
    c <- plyr::join(b,a@data,by='id')
    catchmentLayer <- geom_polygon(data=c,aes(long,lat,group=group),color='black',fill='transparent')
    
    printLayer <- printLayer + catchmentLayer
  }
  
  if(is.null(points) == FALSE){
    pointLayer <- geom_point(data = points,aes(x = lon, y = lat, size = value, colour = z))
    
    printLayer <- printLayer + pointLayer
  }
  
  print (printLayer)
  
  if(outputData == TRUE) return(data_new)
}

