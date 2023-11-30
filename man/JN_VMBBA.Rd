\name{JN_VMBBA}
\alias{JN_VMBBA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Acceleration of the Vectorized Moving Block Bootstrap
}
\description{
This function computes the acceleration constants for the vectorized moving block bootstrap, which is a method used to generate a large number of replications of a time series sample in order to estimate the sampling distribution of a statistic.
}
\usage{
JN_VMBBA(ts, lgmx, bs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
a vector of time series data.
}
  \item{lgmx}{
maximum lag at which to compute autocorrelations.
}
  \item{bs}{
block size of the vectorized moving block bootstrap.
}
}

\value{
a vector of the acceleration constants for autocorrelations up to lagmax.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(co2)
JN_VMBBA(co2, lgmx=5, bs = 2)
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
