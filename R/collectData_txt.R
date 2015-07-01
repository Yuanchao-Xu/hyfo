#' collect data from different txt.
#' 
#' @param folderName A string showing the folder path.
#' @param output A boolean showing whether the result is given.
#' @param rangeWord A list containing the keyword and the shift. 
#' defaut is set to be used in spain gauging station.
#' @examples 
#' #use internal data as an example.
#' file <- system.file("extdata", "1999.csv", package = "hyfo")
#' folder <- strsplit(file, '1999')[[1]][1]
#' a <- collectData_txt_anarbe(folder)
#' 
#' @references http://www4.gipuzkoa.net/oohh/web/esp/02.asp
#' @source http://www4.gipuzkoa.net/oohh/web/esp/02.asp
#' @return The collected data from different txt files.
#' @export
#' @importFrom utils tail
collectData_txt_anarbe <- function(folderName, output = TRUE, rangeWord = c('Ene       ', -1, 
                                                                   'Total     ', -6)){
  #All the code should be ASCII encode, so there should be no strange symbol.
  if (is.null(rangeWord)) {
    stop ('rangeWord consists of 4 elements:
          1. start word which program can recognise.
          2. shift1, the shift needs to be made. E.g. start word is in line 7, and program
          should read file from line 9, then shift is 9-7 = 2.
          3. end word, as start word
          4. shift2, same as shift1, sometimes can be negative
          
          E.g. rangeWord=c(\"aaa\",2,\"bbb\",-2)
         if no rangeWord, just input c(NULL,NULL,NULL,NULL)')
  
  }
  

  fileNames <- list.files(folderName, pattern = '*.TXT', full.names = TRUE)
  
  data <- lapply(fileNames, FUN = readColumn_txt_anarbe, rangeWord = rangeWord)
  
  data <- do.call('rbind', data)
  
  a <- unlist(strsplit(folderName, '\\\\|/'))
  tarName <- tail(a, 2)[1]
  colnames(data) <- c('Date', tarName)
  
  #newFileName <- file.choose(new = T)
  message('new file should be located a different location than the excel folder,
         in order to avoid error.
         At least 2 excels should be in the folder')
  
  #write.table(data_new,file=newFileName,row.names = F, col.names = F,sep=',')
  
  
  if (output == TRUE) return(data)

}  



anarbe_txt <- function(dataset, x1, x2){
  
  data <- as.matrix(dataset[x1:x2, 2:13])
  startYear <- data[1, 6]
  
  data <- data[5:35, ]
  
  date <- which(data != '          ', arr.ind = TRUE)
  startDate <- date[1, ]
  
  endDate <- date[length(date[, 1]), ]
  
  startDate <- as.Date(paste(startYear, startDate[2], startDate[1], sep = '-'))
  endDate <- as.Date(paste(startYear, endDate[2], endDate[1], sep = '-'))
  
  Date <- as.factor(seq(startDate, endDate, 1))
  
  dim(data) <- c(length(data), 1)
  
  data <- as.numeric(data[which(data != '          '), ])
  
  if (length(data) != length(Date)) {
    stop('check original txt file. for missing value, the symbol is "--", check
         if this symbol is missing somewhere')
  }
  
  output <- data.frame(Date = Date, target = data)
  
  return(output)
  }

#'@importFrom utils read.fwf
readColumn_txt_anarbe <- function(fileName, keyword = NULL, rangeWord = NULL){
  
  a <- read.fwf(fileName, widths = rep(10,13))#read file with fixed width
  
  startRow <- which(a == rangeWord[1], arr.ind = TRUE)[, 1]
  startRow <- startRow + as.numeric(rangeWord[2])
  
  endRow <- which(a == rangeWord[3], arr.ind = TRUE)[, 1]
  endRow <- endRow + as.numeric(rangeWord[4])
  
  data <- mapply(FUN=function(x1, x2) anarbe_txt(dataset = a, x1, x2), startRow, endRow)
  
  data_new <- data.frame(Data = unlist(data[1, ]), target = unlist(data[2, ]))
  message(fileName)
  return(data_new)
}
