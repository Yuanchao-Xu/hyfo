#' Get spatial longitude and latitude range of the input netcdf file
#' 
#' @param filePath A string showing the path of the netcdf file.
#' @return A vector of the range of the longitude and latitude.
#' @export
getSpatialRange <- function(filePath){
  #getting range of longitude and latitude of the reasearch area
  
  lonRange <- downscaleR::dataInventory(filePath)$pr$Dimensions$lon$Values
  latRange <- downscaleR::dataInventory(filePath)$pr$Dimensions$lat$Values
  
  x1 <- head(lonRange,1)
  x2 <- tail(lonRange,1)
  y1 <- head(latRange,1)
  y2 <- tail(latRange,1)
  
  return (c(x1,x2,y1,y2))
  
}
