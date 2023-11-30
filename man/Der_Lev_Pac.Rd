\name{Der_Lev_Pac}
\alias{Der_Lev_Pac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utilizing Durbin-Levinson Algorithm for Estimating Partial Autocorrelations
}
\description{
This function calculates partial autocorrelations utilizing the Durbin-Levinson algorithm.
}
\usage{
Der_Lev_Pac(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of autocorrelations.
}
}

\value{
a vector of partial autocorrelations.
}
\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

\examples{
data(co2)
Der_Lev_Pac(co2)
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
