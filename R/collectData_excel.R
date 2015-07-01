#' Collect data from different excel files
#' 
#' @param folderName A string showing the folder path.
#' @param keyword A string showing the extracted column, e.g., waterLevel, waterBalance.
#' @param output A boolean showing whether the output is given.
#' @return The collected data from different excel files.
#' @export
#' @importFrom utils write.table
collectData_excel_anarbe <- function(folderName, keyword = NULL, output = TRUE){
  
  newFileName <- file.choose(new = TRUE)
  message ('new file should be located a different location than the excel folder, 
         in order to avoid error.
         At least 2 excels should be in the folder\n')
  
  message ('this function only applies to strange spain dem operation record file, and this strange file changes
         its format in the middle of the record. For other applications, some tiny changes needs to be made.')
  if (is.null(keyword)) stop('key word is needed, e.g."waterLevel".')
  
  fileNames <- list.files(folderName, pattern = '*.xls', full.names = TRUE)
  data <- lapply(fileNames, FUN = readColumn_excel_anarbe, keyword = keyword)
  checkBind(data, 'rbind')
  data <- do.call('rbind', data)
  
  data_new <- data.frame(data)
  
  data_new <- data_new[order(data_new[, 1]), ]
  
  
  startDate <- data_new[1, 1]
  endDate <- data_new[length(data_new[, 1]), 1]
  
  Date <- as.factor(seq(startDate, endDate, by = 1))
  
  if (length(Date) != length(data_new[, 1])) stop('check if the excel files are continuous')
  
  colnames(data_new) <- c('Date', keyword)
  
  write.table(data_new, file = newFileName,
              row.names = FALSE, col.names = TRUE, sep = ',')
  if(output == TRUE) return(data_new)
}


#'@import xlsx
readTable_excel_anarbe <- function(fileName){
  
  index <- tail(strsplit(fileName, '\\.|\\ ')[[1]], 3)
  raw_year <- index[1]
  raw_mon <- index[2]
  
  raw <- read.xlsx(fileName, sheetName='A')
  startRow <- which(raw == 'COTA', arr.ind = TRUE)[1]+4
  startCol <- which(raw == 'COTA',arr.ind = TRUE)[2]-1
  stopRow <- which(raw =='TOTAL',arr.ind = TRUE)[1]-1
  stopCol1 <- startCol + 17
  stopCol2 <- which(raw == 'SUPERFICIE', arr.ind = TRUE)[2]
  data <- cbind(raw[startRow:stopRow,startCol:stopCol1], raw[startRow:stopRow,stopCol2])
  
  
  yearIndex <- rep(raw_year, stopRow-startRow+1)
  monIndex <- rep(raw_mon, stopRow-startRow+1)
  
  data <- cbind(yearIndex, monIndex, data)
  return(data)
}

#'@importFrom utils tail
readColumn_excel_anarbe <- function(fileName, keyword = NULL){
  
  index <- tail(strsplit(fileName, '\\.|\\ ')[[1]],3)
  year <- as.numeric(index[1])
  mon <- as.numeric(index[2])
  
  if (year == 99) {
    year = year + 1900
  } else year = year + 2000
  
  word = c('COTA', 'Cota\n(m)', 'TOTAL', '  TOTAL')
  
  if (keyword == 'waterLevel') {
    searchWord <- c('COTA', 'Cota\n(m)')
  } else if (keyword == 'discharge_ERE') {
    searchWord <- c('AF.ERE-', 'Caudal\n(m??/s)')
  } else if (keyword == 'waterBalance') {
    searchWord <- c('INCREMENTO', 'al Canal Bajo', 'AFORO',
                    'Variaci??n\nvolumen embalsado')
  } else if (keyword == 'surfaceArea') {
    searchWord <- c('SUPERFICIE', 'SUPERFICIE')
  } else if (keyword == 'volume') {
    searchWord <- c('EMBALSADO', 'Volumen\n(m????)')
  }
  
  
  if (year == 1999 | year < 2009 | (year == 2009 & mon < 5)) {
    raw <- xlsx::read.xlsx(fileName, sheetName = 'A')
    startIndex <- which(raw == word[1], arr.ind = TRUE)
    endIndex <- which(raw == word[3], arr.ind = TRUE)
    startRow <- startIndex[1]+4
    endRow <- endIndex[1]-1
    
    dayCol <- endIndex[2]
    day <- raw[startRow:endRow, dayCol]
    
    targetCol <- which(raw == searchWord[1], arr.ind = TRUE)[2]
    
    if (is.na(targetCol)) stop(sprintf('capture nothing in %s', fileName))
    
    if (keyword == 'waterBalance') {
      targetStart <- targetCol
      targetEnd <- which(raw == searchWord[3], arr.ind = TRUE)[2]
      a <- raw[startRow:endRow, targetStart:targetEnd]
      a <- sapply(a, function(x) as.numeric(levels(x)[x]))
      
      if (year == 1999 & mon == 4) {
        
        target <- data.frame(a[, 2] * 86.4, a[, 5] * 86.4, rep(NA, dim(a)[1]), a[, 6] * 86.4,
                             a[, 4] * 86.4, a[, 11] * 86.4, a[, 3], a[, 7], rep(NA, dim(a)[1]), a[, 1])
      } else {
        target <- data.frame(a[, 2] * 86.4, a[, 5] * 86.4, a[, 6] * 86.4, a[, 7] * 86.4, 
                             a[, 4] * 86.4, a[, 12] * 86.4, a[, 3], a[, 8], rep(NA, dim(a)[1]), a[, 1])
      }   
      
    } else {
      target <- raw[startRow:endRow, targetCol]
      if (keyword == 'discharge_ERE') target <- as.numeric(levels(target))[target]/1000
    }
    
  } else {
    raw <- read.xlsx(fileName,sheetName = 'parte del embalse')
    startIndex <- which(raw == word[2], arr.ind = TRUE)
    endIndex <- which(raw == word[4], arr.ind = TRUE)
    startRow <- startIndex[1]+1
    endRow <- endIndex[1]-2
    
    dayCol <- endIndex[2]
    day <- raw[startRow:endRow, dayCol]
    targetCol <- which(raw == searchWord[2], arr.ind=TRUE)[2]
    if (is.na(targetCol)) stop(sprintf('capture nothing in %s', fileName))
    
    if (keyword == 'waterBalance') {
      targetStart <- targetCol
      targetEnd <- which(raw == searchWord[4], arr.ind=TRUE)[2]
      target <- raw[startRow:endRow, targetStart:targetEnd]
      
    } else {
      target <- raw[startRow:endRow, targetCol]
    }
    
  }
  
  
  startDate <- as.Date(paste(year, mon, day[1], sep = '-'))
  endDate <- as.Date(paste(year, mon, tail(day,1), sep = '-'))
  
  Date <- seq(startDate, endDate, 1)
  output <- data.frame(Date, as.vector(target))
  colnames(output) <- c('Date', seq(1, dim(output)[2] - 1))
  message(fileName)  
  return(output)
  
}

