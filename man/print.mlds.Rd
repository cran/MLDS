\name{print.mlds}
\alias{print.mlds}

\title{ Difference Scale default print statement}
\description{
 This is the default print statement for a \sQuote{mlds} object.
 It displays the difference scale as a named vector, with the names corresponding to the stimulus levels and the value of \code{sigma}.
}
\usage{
\method{print}{mlds}(x, digits = max(3, getOption("digits") - 4), ...)
}

\arguments{
  \item{x}{an object of class \sQuote{mlds}, typically from a call to \code{mlds}.}
  \item{digits}{number of digits to display in the output. }
  \item{\dots}{ additional arguments to be passed to the default method. }
}

\author{ Kenneth Knoblauch}

\examples{
data(kk1)
print(mlds(SwapOrder(kk1)))

}

\seealso{\code{\link{mlds}}}
\keyword{print}
