\name{get.ahat}
\alias{get.ahat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating the Acceleration
}
\description{
This function calculates the acceleration for the bias and acceleration corrected intervals
}
\usage{
get.ahat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of resampling estimates to compute the acceleration.
}
}

\value{
The accelaration estimate.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x = rnorm(100)
get.ahat(x)
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
