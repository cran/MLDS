\name{boot.mlds}
\alias{boot.mlds}
\title{ Resampling of an Estimated Difference Scale }
\description{
 Using the \code{fitted} responses (probabilities) to the difference scale, new responses are generated which permit new bootstrap replications of estimated scales to be generated. The mean scale is useful for evaluating bias and the standard deviation for estimating standard errors of the scale values. 
}
\usage{
boot.mlds(x, nsim, ...)
}
\arguments{
  \item{x}{ an object of class `mlds'.}
  \item{nsim}{ an integer, the number of simulations. }
  \item{\dots}{ Additional options passed along to the function \code{mlds}. }
}
\details{
  Either the scale values (from `glm' method) or the scale values and \eqn{\sigma} (from `optim' method) permit the fitted probabilities to be estimated.  These are used to generate new responses to the quadruples using \code{rbinom}.  The new responses are then used with \code{mlds} to estimate a bootstrapped scale.  This is repeated \eqn{N} times and stored in the output with the mean and standard deviation of the bootstrapped scales.
}
\value{
A list of 4 elements:  
  \item{boot.samp }{A \eqn{p} x \eqn{N} matrix of the bootstrap samples of the scale, where p is the number of scale values and N is the number of simulations.  }
  \item{bt.mean }{A vector of length \eqn{p} giving the mean of the bootstrap scales.}
  \item{bt.sd }{A vector of length \eqn{p} giving the standard deviation of the boostrap scales. }
  \item{N }{The number of bootstrap simulations.}
  
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5.  }
\author{Kenneth Knoblauch and Laurence T. Maloney}

\seealso{\code{\link{mlds}}}
\examples{
data(kk1)
	kk1.mlds <- mlds(kk1)
	#nsim should be near 10,000 for stability,
	# but this will take a little time
	boot.mlds(kk1.mlds, 100)
}
\keyword{ datagen }
\keyword{ misc }
