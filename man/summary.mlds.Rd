\name{summary.mlds}
\alias{summary.mlds}
\alias{print.summary.mlds}

\title{Summary for a mlds fit }
\description{
Takes a fitted 'mlds' object and produces a summary of it
}
\usage{
## S3 method for class 'mlds':
summary.mlds(object, digits = max(3, getOption("digits") - 4), ...)

## S3 method for class 'summary.mlds':
print.summary.mlds(x, digits = max(3, getOption("digits") - 4), ...)
}

\arguments{
  \item{object}{ an object of class 'mlds' typically produced by a call to \code{mlds}. }
  \item{digits}{ The number of digits to display.}
  \item{x }{An object of class 'summary.mlds'.}
  \item{\dots}{Addtional arguments passed to the default print method. Only effects the output of \code{pscale}.}
}
\details{
 Displays summary information from a 'mlds' object.
}
\value{
A list of 5 elements
  \item{pscale }{A named vector indicating the difference scale.  The names are the stimulus levels.}
  \item{sigma }{The estimate of the scale parameter.  For method = "glm", this is always 1.}
 \item{logLik }{The logarithm of likelihood. }
 \item{method }{The fitting method used, either "glm" or "optim". }
 \item{link }{The link used for the binomial family. }
 Normally, \code{print.summary.mlds} is not meant to be called directly by the user.
 }


\author{Kenneth Knoblauch}

\seealso{ \code{\link{mlds}}}
\examples{
data(kk1)
kk1.mlds <- mlds(SwapOrder(kk1))
summary(kk1.mlds)

}
\keyword{methods}
\keyword{print}
