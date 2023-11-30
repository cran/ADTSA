\name{P_CI}
\alias{P_CI}
\alias{B_CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percentile and Bias-corrected and accelerated intervals
}
\description{
The first function P_CI creats percentile intervals. The second function B_CI builds Bias-corrected and accelerated intervals.
}
\usage{
P_CI(e.b,a1,a2)

B_CI(e.b,e,B,ahat,a1,a2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e.b}{
a matrix of bootstrap sample estimates for multiple variables. Each column represents one variable.  
  }
  \item{a1}{
the percentages for the lower  limits of confidence intervals.
}
  \item{a2}{
the percentages for the upper limits of confidence intervals.
}
  \item{e}{
the original sample estimates.  
  }
  \item{B}{
number of replications.
  }
  \item{ahat}{
estimated acceleration constant.
}
}
\value{
a matrix of the obtained confidence intervals for (partial) autocorrelations.
}

\author{
Hossein hassani, Masoud yarmohammadi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
set.seed(123) 
e <- rnorm(1, mean = 5, sd = 1) 
e.b <- matrix(rnorm(1000, mean = 5, sd = 1), ncol = 10) # Bootstrap data

B <- 100 # Number of bootstrap samples
ahat <- 0.4 # Bias correction parameter
a1 <- 0.025 # Low confidence level
a2 <- 0.975 # High level of confidence

P_CI(e.b,a1,a2)
B_CI(e.b, e, B, ahat, a1, a2)

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
