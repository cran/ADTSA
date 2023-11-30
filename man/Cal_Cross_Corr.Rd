\name{Cal_Cross_Corr}
\alias{Cal_Cross_Corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Cross-Correlation
}
\description{
This function calculates the cross correlation between two time series.
}
\usage{
Cal_Cross_Corr(ts1, ts2, max_lag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts1}{
a numeric vector of the first time series data.
}
  \item{ts2}{
a numeric vector of the second time series data.
}
  \item{max_lag}{
maximum lag at which to compute cross-correlation.
}
}

\value{
A data frame.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(co2)
data(sunspots)
Cal_Cross_Corr(co2,sunspots,4)
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
