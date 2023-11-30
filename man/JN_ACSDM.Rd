\name{JN_ACSDM}
\alias{JN_ACSDM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Acceleration Constant for the Alternative Data Method
}
\description{
This function calculates the acceleration constant for the alternative data method. This is done by sequentially removing one observation from the sample data points.
}
\usage{
JN_ACSDM(ts, lgmx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a vector of time series data.
}
  \item{lgmx}{
maximum lag at which to compute autocorrelations.
}
}

\value{
A vector of accelerations for autocorrelations up to lagmax.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(co2)
JN_ACSDM(co2,4)
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
