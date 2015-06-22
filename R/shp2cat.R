#' Get a catchment object from selected shape file.
#' @param filePath A string representing the path of the shape file.
#' @return A catchment object can be used in \code{getSpatialMap()}.
#' @export
#' @import rgdal
shp2cat <- function(filePath){
  
  catName <- tail(strsplit(filePath,'\\\\')[[1]],1)#needs to be four \, caused by some window system problem
  catName1 <- strsplit(catName,'\\.')[[1]][1]
  catName2 <- paste('\\\\',catName, sep = '')
  folderName <- strsplit(filePath,catName2)[[1]]
  n <- list.files(folderName,pattern = catName1)
  if(length(n) == 1) stop ('Please place the shp file in the folder containing
                           full related files, not only the shape file')
  catchment <- rgdal::readOGR(folderName,catName1)
  return (catchment)
}
