\name{Analyze_Fre_Acf}
\alias{Analyze_Fre_Acf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency Analysis of Autocorrelation in a Time Series.
}
\description{
This function performs autocorrelation frequency analysis in a time series.
}
\usage{
Analyze_Fre_Acf(ts, max_frequency = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a numeric vector of time series data.
}
  \item{max_frequency}{
the maximum frequency under consideration.
}
}

\value{
A data frame.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

\examples{
data(co2)
Analyze_Fre_Acf(co2)
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
