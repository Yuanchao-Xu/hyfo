

#' Biascorrect the input timeseries or hyfo dataset
#' 
#' Biascorrect the input time series or dataset, the input time series or dataset should consist of observation, hindcast, and forecast.
#' observation and hindcast should belong to the same period, in order to calibrate. Then the modified forecast
#' will be returned. If the input is a time series, first column should be date column and rest columns should be 
#' the value column. If the input is a hyfo dataset, the dataset should be the result of \code{loadNcdf}, or a list
#' file with the same format.
#' 
#' @param frc a hyfo grid data output or a dataframe (time series) consists of Date column and one or more value columns, 
#' representing the forecast to be calibrated.
#' @param hindcast a hyfo grid data output or a dataframe(time series) consists of Date column and one or more value columns, 
#' representing the hindcast data. This data will be used in the calibration of the forecast, so it's better to have the same date period as
#' observation data. Check details for more information.
#' @param obs a hyfo grid data output or a dataframe (time series) consists of Date column and one or more value columns, 
#' representing the observation data.
#' @param method bias correct method, including 'delta', 'scaling'...
#' @param scaleType only when the method "scaling" is chosen, scaleType will be available. Two different types
#' of scaling method, 'add' and 'mult', which means additive and multiplicative scaling method. More info check 
#' details.
#' @param input If input is a time series, \code{input = 'TS'} needs to be assigned, or hyfo will take it as 
#' an hyfo output grid file. Default is time series input, where in most of the cases we prefer. If your input
#' is a hyfo output file, \code{input = 'hyfo'}.
#' @param preci If the precipitation is biascorrected, then you have to assign \code{preci = TRUE}. Since for
#' precipitation, some biascorrect methods may not apply to, or some methods are specially for precipitation. 
#' Default is FALSE.
#' @details 
#' 
#' Since climate forecast is based on global condition, when downscaling to different regions, it may include
#' some bias, biascorrection is used then to fix the bias.
#' 
#' \strong{Hindcast}
#' 
#' In order to bias correct, we need to pick up some
#' data from the forecast to train with the observation, which is called hindcast in this function. Hindcast
#' should have \strong{EVERY} attributes that forecast has.
#' 
#' Hindcast is also called re-forecast, is the forecast of the past. E.g. you have a forecast from year 2000-2010, assuming now you are in 2005. So from 2000-2005, this period
#' is the hindcast period, and 2005-2010, this period is the forecast period.
#'
#' Hindcast can be the same as forecast, i.e., you can use forecast itself as hindcast to train the bias correction.
#'
#'
#' \strong{How it works}
#' 
#' Forecast product has to be calibrated, usually the system is doing forecast in real time. So, e.g., if the 
#' forecast starts from year 2000, assuming you are in year 2003, then you will have 3 years' hindcast 
#' data (year 2000 - 2003), which can be used to calibrate. And your forecast period is (2003-2004)
#' 
#' E.g. you have observation from 2001 - 2002, this is your input obs. Then you can take the same 
#' period (2001-2002) from the forecast, which is the hindcast period. For forecast, you can take any period.
#' The program will evaluate the obs and hindcast, to get the modification of the forecast, and then add the 
#' modification to the forecast data.
#' 
#' \strong{method}
#' 
#' Different methods used in the bias correction.
#' 
#' \strong{delta}
#' 
#' This method consists on adding to the observations the mean change signal (delta method). 
#' This method is applicable to any kind of variable but it is preferable to avoid it for bounded variables
#'  (e.g. precipitation, wind speed, etc.) because values out of the variable range could be obtained 
#'  (e.g. negative wind speeds...)
#'  
#'  \strong{scaling}
#'  
#' This method consists on scaling the simulation  with the difference (additive) or quotient (multiplicative) 
#' between the observed and simulated means in the train period. The \code{additive} or \code{multiplicative}
#' correction is defined by parameter \code{scaling.type} (default is \code{additive}).
#' The additive version is preferably applicable to unbounded variables (e.g. temperature) 
#' and the multiplicative to variables with a lower bound (e.g. precipitation, because it also preserves the frequency). 
#'  
#' 
#' @examples 
#' 
#' # Use testdl as an example, we take frc, hindcast and obs from testdl.
#' data(testdl)
#' 
#' # common period has to be extracted in order to better train the forecast.
#' 
#' datalist <- extractPeriod(testdl, startDate = '1994-1-1', endDate = '1995-10-1')
#' 
#' frc <- datalist[[1]]
#' hindcast <- datalist[[2]]
#' obs <- datalist[[3]]
#' 
#' # default method is delta
#' frc_new <- biasCorrect(frc, hindcast, obs)
#' 
#' # If the variable is precipitation, it cannot be negative value, so use multi scale method
#' frc_new <- biasCorrect(frc, hindcast, obs, method = 'scaling', scaleType = 'multi')
#' 
#' # If the forecasts you extracted only has incontinuous data for certain months and years, e.g.,
#' # for seasonal forecasting, forecasts only provide 3-6 months data, so the case can be 
#' # for example Dec, Jan and Feb of every year from year 1999-2005.
#' # In such case, you need to extract certain months and years from observed time series.
#' # extractPeriod() can be then used.
#'   
#' 
#' 
#' @references 
#' Bias correction methods come from \code{biasCorrection} from \code{dowscaleR}
#' 
#' Santander Meteorology Group (2015). downscaleR: Climate data manipulation and statistical downscaling. R
#' package version 0.6-0. https://github.com/SantanderMetGroup/downscaleR/wiki
#' @export

biasCorrect <- function(frc, hindcast, obs, method = 'delta', scaleType = 'multi', input = 'TS', preci = FALSE){
  
  if (input == 'TS') {
    # First check if the first column is Date
    if (!grepl('-|/', obs[1, 1]) | !grepl('-|/', hindcast[1, 1]) | !grepl('-|/', frc[1, 1])) {
      stop('First column is not date or Wrong Date formate, check the format in ?as.Date{base} 
           and use as.Date to convert.If your input is a hyfo dataset, put input = "hyfo" as an
           argument, check help for more info.')
    } 
    
    
    # change to date type is easier, but in case in future the flood part is added, Date type doesn't have
    # hour, min and sec, so, it's better to convert it into POSIxlt.
    
    # if condition only accepts one condition, for list comparison, there are a lot of conditions, better
    # further process it, like using any.
    if (any(as.POSIXlt(hindcast[, 1]) != as.POSIXlt(obs[, 1]))) {
      warning('time of obs and time of hindcast are not the same, which may cause inaccuracy in 
              the calibration.')
    }
    
    if (ncol(frc) == 2) {
      frc_data <- biasCorrect_core(frc[, 2], hindcast[, 2], obs[, 2], method = method, 
                                   scaleType = scaleType, preci = preci)
    } else if (ncol(frc) > 2) {
      # In this case more than one value columns exist in the dataset, both frc and hindcast.
      
      n <- ncol(frc)
      
      # For every column, it's biascorrected respectively.
      frc_data <- lapply(2:n, function(x) biasCorrect_core(frc[, x], hindcast[, x], obs[, 2], method = method,
                                                           scaleType = scaleType, preci = preci))
      frc_data <- do.call('cbind', frc_data)
      
    } else stop('Wrong TS input, check your TS dimension.')
    
    
  } else if (input == 'hyfo') {
    print('Under development...')
  }

  names <- colnames(frc)
  frc <- data.frame(frc[, 1], frc_data)
  colnames(frc) <- names
  
  return(frc)
}


# this is only used to calculate the value column, 
biasCorrect_core <- function(frc, hindcast, obs, method = 'delta', scaleType = 'multi', preci = FALSE){
  

  # default is the simplest method in biascorrection, just do simple addition and subtraction.
  if (method == 'delta') {
    # comes from downscaleR biascorrection method
    frcMean <- mean(obs, na.rm = TRUE)
    hindcastMean <- mean(hindcast, na.rm = TRUE)
    frc <- obs - hindcastMean + frcMean
    
  } else if (method == 'scaling') {
    obsMean <- mean(obs, na.rm = TRUE)
    hindcastMean <- mean(hindcast, na.rm = TRUE)
    
    if (scaleType == 'multi') {
      frc <- frc / hindcastMean * obsMean
      
    } else if (scaleType == 'add') {
      frc <- frc - hindcastMean + obsMean
    }
    
    
  } else if (method == 'eqm') {
    
  # To be added, right now too complicated and not so much use.
    
  }
  
  
  return(frc)
}