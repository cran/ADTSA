Cal_Cross_Corr <-
function(ts1, ts2, max_lag) {
  # Calculation of cross-correlation
  cc_result <- ccf(ts1, ts2, lag.max = max_lag, plot = FALSE)

  # Extracting the results
  lags <- cc_result$lag
  correlations <- cc_result$acf

  # Return the results as a data frame
  return(data.frame(lag = lags, correlation = correlations))
}
