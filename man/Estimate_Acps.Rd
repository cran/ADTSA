\name{Estimate_Acps}
\alias{Estimate_Acps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of Autocorrelation Power Spectrum (ACPS)
}
\description{
This function is used to estimate the autocorrelation power spectrum (ACPS) of the data.
}
\usage{
Estimate_Acps(ts, method = "periodogram")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a vector of time series data.
}
  \item{method}{
periodogram method for power spectrum estimation.
}
}

\value{
A vector.
}

\author{
Hossein hassani, Masoud yarmohammdi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

\examples{
data(co2)
Estimate_Acps(co2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
