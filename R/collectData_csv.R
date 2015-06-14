#' Collect data from the gauging stations in spain, catchement Anarbe
#' 
#' @param folderName A string showing the path of the folder holding different csv files.
#' @param output A boolean showing whether the output is given, default is T.
#' @return The collected data from different csv files.
#' @export
collectData_csv_anarbe <- function(folderName, output=T){
  fileNames <- list.files(folderName, pattern='*.csv', full.names=T)
  data <- lapply(fileNames, readColumn_csv_anarbe)
  data <- do.call('rbind', data)
  data <- data[,1:2]
  data[,1] <- as.Date(data[,1],format='%d/%m/%Y')
  
  #newFileName <- file.choose(new = T)
  #write.table(data_new,file=newFileName,row.names = F, col.names = F,sep=',')
  a <- unlist(strsplit(folderName,'\\\\|/'))
  tarName <- tail(a,2)[1]
  colnames(data) <- c('Date',tarName)
  
  if (output) return(data)
}


readColumn_csv_anarbe <- function(fileName){
  data <- read.csv(fileName,skip=3)
  endIndex <- which(data=='',arr.ind=T)[1]-1
  
  data <- data[1:endIndex,]
  
  if (!is.null(levels(data[,2]))) {
    data[,2] <- as.numeric(levels((data[,2])))[data[,2]]
  }
  
  colnames(data) <- c('Date','target')
  message (fileName)
  
  return (data)
}

