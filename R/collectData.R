#' Collect data from different csv files.
#' @param folderName A string showing the path of the folder holding different csv files.
#' @param fileType A string showing the file type, e.g. "txt", "csv", "excel".
#' @param range A vector containing startRow, endRow, startColumn, endColumn, e.g., 
#' c(2,15,2,3)
#' @param sheetIndex A number showing the sheetIndex in the excel file, if fileType is excel,
#' sheetIndex has to be provided, default is 1.
#' @return The collected data from different files in the folder.
#' @export
collectData <- function(folderName, fileType = NULL, range = NULL, sheetIndex = 1){
  message ('All the files in the folder should have the same format')
  if (is.null(fileType)) stop ('Please enter fileType, "txt", "csv" or "excel".')
  if(length(range) > 4){
    stop ('"range" should be c(startRow, endRow, startCol, endCol)')
  }else if (is.null(range)){
    stop('"range" can not be blank, e.g., range <- c(startRow, endRow, startCol, endCol).')
  }
  
  if (fileType == 'csv') {
    fileNames <- list.files(folderName, pattern = '*.csv', full.names = T)
    if (fileNames) stop ('No csv file in the folder.')
    
    data <- lapply(fileNames, readCsv, range = range)
    data <- do.call('rbind', data)
  }else if (fileType == 'txt'){
    fileNames <- list.files(folderName, pattern = '*.txt', full.names = T)
    if (length(fileNames) == 0){
      fileNames <- list.files(folderName, pattern = '*.TXT', full.names = T)
    }
    if (length(fileNames) == 0) stop ('No text file in the folder.')
    message ('For txt file, only startRow and endRow will be considered.')
    data <- lapply(fileNames, readTxt, range = range)
    data <- unlist(data)
  }else if (fileType == 'excel'){
    fileNames <- list.files(folderName, pattern = '*.xlsx', full.names = T)
    if (length(fileNames) == 0){
      fileNames <- list.files(folderName, pattern = '*.xls', full.names = T)
    }
    if (length(fileNames) == 0) stop ('No excel in the folder.')
    data <- lapply(fileNames, readExcel, range = range, sheetIndex = sheetIndex)
    checkBind(data, 'rbind')
    data <- do.call('rbind', data)
  }else{
    stop ('fileType should be "txt", "csv" or "excel".')
  }
  
  
  return (data)
  
}

readExcel <- function(fileName, range, sheetIndex){
  data <- xlsx::read.xlsx(fileName, sheetIndex = sheetIndex, rowIndex = seq(range[1], range[2]),
                          colIndex = seq(range[3], range[4])) 
  colnames(data) <- seq(1, dim(data)[2])
  
  message (fileName) 
  return (data)
}

readTxt <- function(fileName, range){
  data <- readLines(fileName)
  data <- data[range[1]:range[2]]
  return (data)
}




readCsv <- function(fileName, range){
  
  data <- read.csv(fileName, skip = range[1] - 1, header = F)
  data <- data[1:(range[2] - range[1] + 1), range[3]:range[4]]
  
  return (data)
}
