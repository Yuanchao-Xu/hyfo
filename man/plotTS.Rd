% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyzeTS.R
\name{plotTS}
\alias{plotTS}
\title{plot time series, with marks on missing value.}
\usage{
plotTS(
  ...,
  type = "line",
  output = "data",
  plot = "norm",
  name = NULL,
  showNA = TRUE,
  x = NULL,
  y = NULL,
  title = NULL,
  list = NULL
)
}
\arguments{
\item{...}{input time series.}

\item{type}{A string representing the type of the time series, e.g. 'line' or 'bar'.}

\item{output}{A string showing which type of output you want. Default is "data", if "ggplot", the 
data that can be directly plotted by ggplot2 will be returned, which is easier for you to make series
plots afterwards.}

\item{plot}{representing the plot type, there are two types, "norm" and "cum", "norm" gives an normal
plot, and "cum" gives a cumulative plot. Default is "norm".}

\item{name}{If \code{output = 'ggplot'}, name has to be assigned to your output, in order to differentiate
different outputs in the later multiplot using \code{plotTS_comb}.}

\item{showNA}{A boolean representing whether the NA values should be marked, default is TRUE.}

\item{x}{label for x axis.}

\item{y}{label for y axis.}

\item{title}{plot title.}

\item{list}{If your input is a list of time series, then use \code{list = your time sereis list}}
}
\value{
A plot of the input time series.
}
\description{
plot time series, with marks on missing value.
}
\details{
If your input has more than one time series, the program will only plot the common period of 
different time series.
}
\examples{
plotTS(testdl[[1]])
plotTS(testdl[[1]], x = 'xxx', y = 'yyy', title = 'aaa')

# If input is a datalist
plotTS(list = testdl)

# Or if you want to input time series one by one
# If plot = 'cum' then cumulative curve will  be plotted.
plotTS(testdl[[1]], testdl[[2]], plot = 'cum')

# You can also directly plot multicolumn dataframe
dataframe <- list2Dataframe(extractPeriod(testdl, commonPeriod = TRUE))
plotTS(dataframe, plot = 'cum')

# Sometimes you may want to process the dataframe and compare with the original one
dataframe1 <- dataframe
dataframe1[, 2:4] <- dataframe1[, 2:4] + 3
plotTS(dataframe, dataframe1, plot = 'cum')
# But note, if your input is a multi column dataframe, it's better to plot one using plotTS,
# and compare them using plotTS_comb. If all data are in one plot, there might be too messy.


# More examples can be found in the user manual on https://yuanchao-xu.github.io/hyfo/

}
\references{
\itemize{
\item H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
}
}
