\name{pairwise_MBL}
\alias{pairwise_MBL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating Pairs of Entries for the Vectorized Moving Block
}
\description{
This function yields index positions of pairs of observations for the vectorized moving block bootstrap.
}
\usage{
pairwise_MBL(mat, lgmx, l_ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
a matrix giving position indices for observations in each resampled block. Columns represent different blocks.
}
  \item{lgmx}{
maximum lag at which to calculate autocorrelations.
}
  \item{l_ts}{
time series length or the number of time points.
}
}
\details{
Autocorrelation at lag h is a statistical measure that assesses the similarity between observations in a time series that are h lags apart. pairwise_MBL returns indices for both the observations in id.M, as well as those h lags apart.If some indices exceed the length of the time series, then the corresponding pairs of observations are discarded.
}
\value{
A list of pairs of indices for computing autocorrelations up to lagmax, each containing a matrix of indices for autocorrelation at a lag no greater than lagmax.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

\examples{
mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
lgmx <- 2
l_ts <- 5
pairwise_MBL(mat, lgmx, l_ts)
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
