Period_ts <- function(ts)
{
	specvalues <- spec.pgram(ts, taper=0, log='no', plot = FALSE)
	ind <- which.max(specvalues$spec)
	dd <- specvalues$freq[ind]
	return(1/dd)
}