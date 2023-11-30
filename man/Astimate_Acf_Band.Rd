\name{Astimate_Acf_Band}
\alias{Astimate_Acf_Band}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing Bandwidth of Autococorrelation in Time Series
}
\description{
This function calculates the bandwidth of autocorrelation in a time series.
}
\usage{
Astimate_Acf_Band(ts, confidence_level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a numeric vector of time series data.
}
  \item{confidence_level}{
confidence level for bandwidth calculation.
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
Astimate_Acf_Band(co2)
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
