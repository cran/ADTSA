Astimate_Acf_Band <-
function(ts, confidence_level = 0.95) {
  
  n <- length(ts)
  acf_values <- acf(ts, plot = FALSE)$acf
  se_acf <- sqrt((1 + 2*sum(acf_values^2)) / n) 

  # Confidence intervals for autocorrelation
  ci_upper <- acf_values + qnorm(confidence_level)*se_acf
  ci_lower <- acf_values - qnorm(confidence_level)*se_acf

  # Calculate bandwidth
  bandwidth <- which(ci_lower | ci_upper < 0)
  if (length(bandwidth) > 0) {
    return(min(bandwidth))
  } else {
    return(NA) # If there is no intersection
  }
}
