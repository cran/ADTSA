\name{Period_ts}
\alias{Period_ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Time Series Periodicity
}
\description{
One of the uses of spectrum density and periodogram is to detect the periodicity of the series. If the spectral density or periodogram or any estimator has a peak at a frequency, the periodicity of the series can be understood. If we reverse the frequency at which the peak is located, the period of the series is obtained.
}
\usage{
Period_ts(ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a vector of time series data.
}
}

\value{
A number.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(co2)
Period_ts(co2)
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
