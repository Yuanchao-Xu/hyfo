## For package updates information

#' @importFrom utils packageDescription
hyfoUpdates <- function(){
  page <- readLines('http://yuanchao-xu.github.io/hyfo/')
  updatesLine <- grep('id=\\"updates"', page)
  versionLine <- updatesLine + 2
  
  version <- unlist(strsplit(page[versionLine], split = ' '))[2]
  version_local <- packageDescription("hyfo")$Version
  # generate message
  version_msg <- strsplit(strsplit(page[versionLine], split = '<p>')[[1]][2], split = '</p>')[[1]]
  infoLine <- versionLine + 2
  info_msg <- strsplit(strsplit(page[infoLine], split = '<p>')[[1]][2], split = '</p>')[[1]]
  install_msg <- 'You can update by type in: devtools::install_gihub("Yuanchao-Xu/hyfo")'
  
  message_out <- NULL
  if (version != version_local) {
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
