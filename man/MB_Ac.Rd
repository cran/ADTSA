\name{MB_Ac}
\alias{MB_Ac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimating autocorrelations for vectorized moving block
}
\description{
This function calculates autocorrelations for vectorized moving block bootstrap samples based on the position indices of pairs of observations.
}
\usage{
MB_Ac(pair_mat, ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pair_mat}{
a list of position indices of pairs of observations to calculate autocorrelations. An object returned by the function pairwise_MBL.
}
  \item{ts}{
a numeric vector of time series data.
}
}
\details{
If there are missing values, this function estimates autocorrelations assuming complete data. Therefore, in such cases, the result is not valid.
}
\value{
a vector of estimated autocorrelations.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
ts <- 1:6
lgmx <- 2
l_ts <- 5
pair_mat1 = pairwise_MBL(mat, lgmx, l_ts)
MB_Ac(pair_mat1, ts)
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
