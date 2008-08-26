\name{pmc}
\alias{pmc}

\title{ Proportion of Misclassifications According to an Estimated MLDS Fit}
\description{
   \code{pmc} calculates the proportion of the observer's responses that are misclassifications on the basis of the estimated MLDS.
}
\usage{
pmc(x, ...)
}

\arguments{
  \item{x}{object of class 'mlds'.}
  \item{\dots}{ currently unused.}
}

\value{
   numeric indicating the proportion of misclassified trials on the basis of the estimated scale.
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5. }
\author{ Kenneth Knoblauch }

\seealso{\code{\link{mlds}}}
\examples{
	data(kk1)
	kk1.mlds <- mlds(SwapOrder(kk1))
	pmc(kk1.mlds)
}

\keyword{univar}
