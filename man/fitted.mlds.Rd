\name{fitted.mlds}
\alias{fitted.mlds}

\title{ Fitted Responses for a Difference Scale }
\description{
  \code{fitted.mlds} returns the fitted responses from an estimated difference scale obtained by \code{mlds}.
}
\usage{
\method{fitted}{mlds}(object, ...)
}


\arguments{
  \item{object}{object of class 'mlds", typically obtained from the output of \code{mlds}. }
  \item{\dots}{currently ignored}
}

\value{
 A numeric vector contained the fitted probabilities to the responses of the observer for each quadruple.
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5. }
\author{Kenneth Knoblauch}
\seealso{\code{\link{mlds}}, \code{\link{predict.mlds}}}
\examples{
data(kk1)
data(kk2)
data(kk3)
fitted(mlds(SwapOrder(rbind(kk1, kk2, kk3))))
}

\keyword{ methods }
