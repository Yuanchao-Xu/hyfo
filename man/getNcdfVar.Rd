% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncdf.R
\name{getNcdfVar}
\alias{getNcdfVar}
\title{Get variable name of the NetCDF file.}
\usage{
getNcdfVar(filePath)
}
\arguments{
\item{filePath}{A path pointing to the netCDF file.}
}
\value{
The names of the varialbes in the file.
}
\description{
Get variable name in the NetCDF file. After knowning the name, you can use \code{loadNcdf} to load
the target variable.
}
\examples{
# First open the test NETcDF file.
filePath <- system.file("extdata", "tnc.nc", package = "hyfo")

# Then if you don't know the variable name, you can use \code{getNcdfVar} to get variable name
varname <- getNcdfVar(filePath)

# More examples can be found in the user manual on https://yuanchao-xu.github.io/hyfo/

}
\references{
\itemize{
\item David Pierce (2015). ncdf4: Interface to Unidata netCDF (Version 4 or
Earlier) Format Data Files. R package version 1.14.1.
https://CRAN.R-project.org/package=ncdf4
}
}
