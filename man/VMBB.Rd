\name{VMBB}
\alias{VMBB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The vectorized moving block bootstrap.
}
\description{
The VMB function tests for (partial) autocorrelations using the Vectorized Moving Block (VMB) launcher.
}
\usage{
VMBB(acf.est, pacf.est, ahat, ts, bs, a1, a2, boot, lgmx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf.est}{
a vector of sample autocorrelation estimates up to lagmax.
}
  \item{pacf.est}{
a vector of sample partial autocorrelation estimates up to lagmax.
}
  \item{ahat}{
a vector of estimated acceleration constants for autocorrelations up to lagmax. An object returned by JN_VMBBA.
}
  \item{ts}{
a vector of time series data.
}
  \item{bs}{
block size for the VMBB bootstrap.
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
ts <- rnorm(100)
acf_est <- acf(ts, plot = FALSE)$acf[-21]
pacf_est <- pacf(ts, plot = FALSE)$acf[-21]

ahat <- list(acf=c() ,pacf=c())
ahat$acf <- acf(ts, lag.max = 19, plot = FALSE)$acf[,,1]
ahat$pacf <- pacf(ts, lag.max = 19, plot = FALSE)$acf[,,1]

bs <- 10
a1 <- 0.025
a2 <- 0.975
boot <- 100
lgmx <- 20

VMBB(acf_est, pacf_est, ahat, ts, bs, a1, a2, boot, lgmx)
   
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
