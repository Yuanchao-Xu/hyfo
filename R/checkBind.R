#' check if the data is available for \code{rbind()} or \code{cbind()}
#' 
#' @param data A list containing different sublists ready to be processed by \code{do.call('rbind')} 
#' or \code{do.call('cbind')}
#' @param bind A string showing which bind you are going to use can be 'rbind' or 'cbind'
#' @return data can be processed by bind function; data cannot be processed by bind function
#' @examples
#' data <- list(c(1,1,1),c(2,2,2))
#' bind <- 'rbind'
#' checkBind(data,bind)
#' 
#' data(datalist)
#' checkBind(datalist, 'rbind')
#' #
#' @export
checkBind <- function(data, bind){
  # data has to be a list of values, and will be used in do.call('rbind')
  message ('Check if the data list is available for rbind or cbind... \n')
  if (bind == 'rbind'){
    colNum <- sapply(data,function(x) dim(x)[2])
    colLev <- unique(colNum)
    if (length(colLev) != 1){
      dif <- colLev[2]
      difNum <- which(colNum == dif)
      stop (sprintf('Different Colomn number in %s th of the data list \n',difNum))
      
    }
    
  }else if (bind =='cbind'){
    rowNum <- sapply(data,function(x) dim(x)[1])
    rowLev <- unique(rowNum)
    if (length(rowLev) != 1){
      dif <- rowLev[2]
      difNum <- which(rowNum == dif)
      stop (sprintf('Different row number in %s th of the data list \n',rowNum))
      
    }
    
    
  }
  
  message ('Data list is OK')
}
