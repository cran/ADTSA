Estimate_Acps <-
function(ts, method = "periodogram") {
  # Calculating autocorrelation
  acf_values <- acf(ts, plot = FALSE)$acf

  # Estimation of power spectrum based on periodogram method 
    spectrum <- spectrum(acf_values, plot = FALSE)
    return(spectrum$spec)
}
