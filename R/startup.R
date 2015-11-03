## For package updates information

#' @importFrom utils packageDescription
hyfoUpdates <- function(){
  page <- readLines('http://yuanchao-xu.github.io/hyfo/')
  updatesLine <- grep('id=\\"updates"', page)
  versionLine <- updatesLine + 2
  
  version <- unlist(strsplit(page[versionLine], split = ' '))[2]
  version_local <- packageDescription("hyfo")$Version
  
  
  # the first tow digit is the most important part of the version
  version12 <- unlist(strsplit(version, split = "[.]"))[1:2]
  version_local12 <- unlist(strsplit(version_local, split = "[.]"))[1:2]
  
  sameVersion <- version12 == version_local12
  
  # generate message
  version_msg <- strsplit(strsplit(page[versionLine], split = '<p>')[[1]][2], split = '</p>')[[1]]
  infoLine <- versionLine + 2
  info_msg <- strsplit(strsplit(page[infoLine], split = '<p>')[[1]][2], split = '</p>')[[1]]
  install_msg <- 'You can update by type in: devtools::install_gihub("Yuanchao-Xu/hyfo")'
  
  message_out <- NULL
  if (any(sameVersion == FALSE)) {
    message_out <- paste(version_msg, info_msg, install_msg, sep = '\n')
  }
  return(message_out)
}

.onAttach <- function(libname, pkgname) {
  message_out <- suppressWarnings(try(hyfoUpdates(), silent = TRUE))
  if (!is.null(message_out)) {
    if (grepl('Version', message_out)) {
      packageStartupMessage(message_out)
    }
  }
}
