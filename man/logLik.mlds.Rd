\name{logLik.mlds}
\alias{logLik.mlds}
\alias{logLik.mlbs}


\title{ Compute Log-Likelihood for an mlds object}
\description{
  This function provides a method for extracting the log likelihood from an object of class \sQuote{mlds}.
}
\usage{
\method{logLik}{mlds}(object, ...)
\method{logLik}{mlbs}(object, ...)
}

\arguments{
  \item{object}{an object of class \sQuote{mlds} or  \sQuote{mlbs}typically from a call to \code{mlds}}
  \item{\dots}{for passing additional parameters, but is currently not used. }
}

\value{
An object of class \sQuote{logLik} whose value is the logarithm of the likelihood with attribute \code{df} providing the degrees of freedom
}
\author{ Kenneth Knoblauch}

\examples{
data(kk1)
logLik(mlds(SwapOrder(kk1)))
}

\keyword{univar}
