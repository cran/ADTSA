Analyze_Fre_Acf <-
function(ts, max_frequency = 0.5) { 
  acf_values <- acf(ts, plot = FALSE)$acf
  fft_values <- fft(acf_values)
  frequencies <- seq(0, max_frequency, length.out = length(fft_values))

  return(data.frame(frequency = frequencies, fft_value = Mod(fft_values)))
}
