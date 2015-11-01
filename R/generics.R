# 
# print <- function(object, ...) {
#   UseMethod('print')
# }
# 
# print.biasFactor <- function(object, ...) {
#   msg <- paste('biasFactor of method ', object@method)
#   if (length(object@memberDim)) msgM <- paste('There are', object@memberDim, 'members existing in the forecasting data.')
#   return(c(msg, msgM))
# }

#' #### Generics of biasFactor
# @param object biasFactor object
# @export
# setGeneric('print', function(object) {
#   standardGeneric('print')
# })
# 
# 
# setMethod('print', signature('biasFactor'), function(object) {
#   msg <- paste('biasFactor of method ', object@method)
#   if (length(object@memberDim)) msgM <- paste('There are', object@memberDim, 'members existing in the forecasting data.')
#   return(c(msg, msgM))
# })

# #' @export
# #' @param a biasFactor object
# size <- function(x, ...) {
#   UseMethod('size', x)
# }
# 
# #' @describeIn size
# size.biasFactor <-function(object) {
#   if (length(object@lonLatDim) == 0) {
#     return (1)
#   } else {
#     lonLat <- object@lonLatDim
#     msg <- paste('Grid file with', lonLat[1], 'grids in longitude, ', lonLat[2], 'grids in latitude.')
#     return(msg)
#   }
# }

##### hyfo

# hyfo, TS and datalist should be three kinds of objects, so that many functions in hyfo can be split
# into different generic methods, then no need to set up input = TS or input = hyfo.
# But too much work to re-construct all the functions.

# For new methods, it should set up different generic methods for hyfo, TS, and grid file from 
# downscaleR.
