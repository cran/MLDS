\name{simu.6pt}
\alias{simu.6pt}

\title{Perform Bootstrap Test on 6-point Likelihood for MLDS FIT}
\description{
  Using the \code{fitted} responses (probabilities) to the difference scale, new responses are generated which permit new 6-point likelihoods to be calculated.  The distribution of a large number of such likelihoods can be compared with that obtained from the observed responses to evaluate the internal consistency of the estimated scale.
}
\usage{
simu.6pt(obj, nsim = 1)
}

\arguments{
  \item{obj}{object of class \sQuote{mlds}}
  \item{nsim}{ integer indicating number of bootstrap trials.}
}

\value{
  LIST with 4 components
  \item{boot.samp }{vector of numeric giving the log likelihood for the 6-point test for each simulation.}
  \item{lik6pt }{numeric indicating the log likelihood for the 6-point test on the original data}
  \item{p }{proportion of simulations on which the simulated log likelihood was higher than that obtained from the original sample.}
  \item{N }{numeric indicating the number of simulations.  It should be the length of \code{boot.samp}.}
  }
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5. }
\author{Kenneth Knoblauch and Laurence T. Maloney}

\seealso{\code{\link{mlds}}, \code{\link{lik6pt}}}
\examples{
	data(kk1)
	x.mlds <- mlds(SwapOrder(kk1))
	#nsim should be near 10,000 for stability,
	# but this will take a little time
	simu.6pt(x.mlds, 100)
}
\keyword{ datagen }
\keyword{ misc }
