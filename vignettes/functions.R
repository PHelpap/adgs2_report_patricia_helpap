#functions used in the vignettes are stored here:

#used in Phenology Modelling:
#-----------Growing degree day model optimization
gdd_model <- function(temp, par) { #put the function into the R file for submission
  # split out parameters from a simple
  # vector of parameter values
  temp_threshold <- par[1]
  gdd_crit <- par[2]

  # accumulate growing degree days for
  # temperature data
  gdd <- cumsum(ifelse(temp > temp_threshold, temp - temp_threshold, 0))

  # figure out when the number of growing
  # degree days exceeds the minimum value
  # required for leaf development, only
  # return the first value
  doy <- unlist(which(gdd >= gdd_crit)[1])

  return(doy)
}

#-------Phenology Model Calibration
# run model and compare to true values
# returns the RMSE
rmse_gdd <- function(par, data) { #also put in functions folder for submission

  # split out data
  drivers <- data$drivers
  validation <- data$validation

  # calculate phenology predictions
  # and put in a data frame
  predictions <- drivers |>
    group_by(year) |>
    summarise(
      predictions = gdd_model(
        temp = tmean,
        par = par
      )
    )

  predictions <- left_join(predictions, validation, by = "year")

  rmse <- predictions |>
    summarise(
      rmse = sqrt(mean((predictions - doy)^2, na.rm = TRUE))
    ) |>
    pull(rmse)

  # return rmse value
  return(rmse)
}
