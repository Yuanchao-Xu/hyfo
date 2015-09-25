#' Collect data from csv for Anarbe case.
#' 
#' Collect data from the gauging stations in spain, catchement Anarbe
#' 
#' @param folderName A string showing the path of the folder holding different csv files.
#' @param output A boolean showing whether the output is given, default is T.
#' @return The collected data from different csv files.
#' @examples
#' 
#' #use internal data as an example.
#' file <- system.file("extdata", "1999.csv", package = "hyfo")
#' folder <- strsplit(file, '1999')[[1]][1]
#' a <- collectData_csv_anarbe(folder)
#' 
#' # More examples can be found in the user manual on http://yuanchao-xu.github.io/hyfo/
#' 
#' @references 
#' 
#' \itemize{
#' \item http://meteo.navarra.es/estaciones/mapadeestaciones.cfm
#' \item R Core Team (2015). R: A language and environment for statistical computing. R Foundation for
#' Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
#' }
#' 
#' @source http://meteo.navarra.es/estaciones/mapadeestaciones.cfm
#' @export
#' @importFrom utils tail
collectData_csv_anarbe <- function(folderName, output = TRUE){
  
  fileNames <- list.files(folderName, pattern='*.csv', full.names = TRUE)
  data <- lapply(fileNames, readColumn_csv_anarbe)
  data <- do.call('rbind', data)
  data <- data[, 1:2]
  data[, 1] <- as.Date(data[, 1], format = '%d/%m/%Y')
  
  #newFileName <- file.choose(new = T)
  #write.table(data_new,file=newFileName,row.names = F, col.names = F,sep=',')
  a <- unlist(strsplit(folderName, '\\\\|/'))
  tarName <- tail(a, 2)[1]
  colnames(data) <- c('Date', tarName)
  
  if (output) return(data)
}


readColumn_csv_anarbe <- function(fileName){
  data <- read.csv(fileName, skip = 3)
  endIndex <- which(data == '', arr.ind = TRUE)[1]-1
  
  data <- data[1:endIndex, ]
  
  if (!is.null(levels(data[, 2]))) {
    data[, 2] <- as.numeric(levels((data[, 2])))[data[, 2]]
  }
  
  colnames(data) <- c('Date', 'target')
  message(fileName)
  
  return(data)
}

