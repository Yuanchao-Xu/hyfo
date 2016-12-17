## Welcome 

hyfo is an **R package**, initially designed for the European Project EUPORIAS, and cooperated with DHI Denmark, which was then extended to other uses in hydrology, hydraulics and climate. 

This package mainly focuses on data processing and visulization in hydrology and climate forecasting. Main function includes NetCDF file processing, data extraction, data downscaling, data resampling, gap filler of precipitation, bias correction of forecasting data, flexible time series plot, and spatial map generation. It is a good pre-processing and post-processing tool for hydrological and hydraulic modellers.

## User manual

pdf version [here](http://media.wix.com/ugd/199c69_f3c30d4625a545908b5c0f13e17f0b33.pdf), easier to download and looks better.

html version [here](http://rpubs.com/rankthefirst/hyfo), easier to copy paste codes.

## Installation

For released version:
```{r}
install.packages("hyfo")
```

For development version:
```{r}
  install.packages('devtools')
  # Ignore the warning that Rtool is not installed, unless you want other function from devtools.
  # If "devtools" is already installed in your computer, you can directly run the following code.
  devtools::install_github('Yuanchao-Xu/hyfo')
```

If you meet with some error during the installation of development version, e.g., 
```{r}
  cannot remove prior installation of package 'XXX'
  or
  error in installation of XXX package.
```
If so, just use `install.pakcages('xxx')` to reinstall XXX package. And then reinstall hyfo again. For other errors can be solved by directly reinstall hyfo.


You can also directly download ".tar.gz" file from the link above, and install in an R IDE e.g. Rstudio. **Or** use the **Simpler** way to install the latest version. Remember，your R should be at least R 3.1.0.


## Updates 

Version 1.3.6 was released

- transfer from ncdf to ncdf4
- grepAndMatch created, for capturing dimension names.
- minor bug fixed about the loadNcdf, when no dimension found, it will give an error indicating.
- change most of the match function into grepAndMatch, in order to deal with different dimension names.
- add name attributes to gridfile$xyCoords$x,y, when writeNcdf, the dim names will be taken from that attribute, which can be exactly the same with the original. 
- bug fixed for nc files without members.

For historical releases and the introduction of updates about each version, please click [here](https://github.com/Yuanchao-Xu/hyfo/releases) 

## Feedback

Any bugs and suggestions are welcomed, please leave a comment [here](https://github.com/Yuanchao-Xu/hyfo/issues)