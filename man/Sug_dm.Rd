\name{Sug_dm}
\alias{Sug_dm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Alternative Data Method
}
\description{
This function performs the surrogate data method for testing auto (partial) correlations.
}
\usage{
Sug_dm(ahat, ts, a1, a2, boot, lgmx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ahat}{
a vector of estimated acceleration constants for autocorrelations up to lagmax.
}
  \item{ts}{
a vector of time series data.
}
  \item{a1}{
the percentages for the lower  limits of confidence intervals.
}
  \item{a2}{
the percentages for the upper limits of confidence intervals.
}
  \item{boot}{
number of bootstrap replications.
}
  \item{lgmx}{
maximum lag at which to calculate autocorrelations.
}
}

\value{
A list with two components: acf and pacf.

For acf:

se: standard error estimates for autocorrelations.

CI: a list of estimated confidence intervals for autocorrelations. Contain two elements: per and BCa

For pacf:

se: standard error estimates for partial autocorrelations.

CI: a list of estimated confidence intervals for partial autocorrelations. Contain two elements: per and BCa
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
set.seed(123)
ts <- rnorm(100)

ahat <- list(acf=c() ,pacf=c())
ahat$acf <- acf(ts, lag.max = 19, plot = FALSE)$acf[,,1]
ahat$pacf <- pacf(ts, lag.max = 19, plot = FALSE)$acf[,,1]

a1 <- 0.025
a2 <- 0.975
boot <- 100
lgmx <- 20

Sug_dm(ahat, ts, a1, a2, boot, lgmx)
   
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
