\name{mlds}
\alias{mlds}

\title{Fit Difference Scale by Maximum Likelihood }
\description{
 \code{mlds} fits the results of a difference scaling experiment using either \code{glm} (Generalized Linear Model) or by direct maximization of the likelihood using \code{optim}.
}
\usage{
mlds(data, stimulus, method = "glm", lnk = "probit", 
		opt.meth = "BFGS", opt.init = NULL, 
		control = glm.control(maxit = 50000, epsilon = 1e-14),
		... )
}

\arguments{
  \item{data}{ A data.frame with 5 columns giving the response and the ranks of the stimulus levels for each trial, or an object of class \sQuote{mlds.df} which also contains additional information as attributes .}
  \item{stimulus}{A numeric vector that contains the physical stimulus levels used in the experiment.  If \code{data} is of class \sQuote{mlds.df}, this information is included as an attribute.  If \code{NULL}, a sequence of 1-n is used, where n is the number of stimulus levels, deduced from the highest rank in \code{data}. }
  \item{method}{ Character, taking the value of \dQuote{glm} or \dQuote{optim}.  Default is \dQuote{glm}. }
  \item{lnk}{Character indicating either one of the built-in links for the binomial family or a user defined link of class \sQuote{link-glm}.  See \code{\link{family}} and \code{\link{make.link}}. Default is \dQuote{probit}.}
  \item{opt.meth}{If \code{method} = \dQuote{optim}, the method used by \code{optim} can be specified.  Defaults to \dQuote{BFGS}.}
  \item{opt.init}{Vector of numeric giving initial values which must be provided if you specify the \dQuote{optim} method.}
  \item{control}{ A list of control values for either \code{glm} or \code{optim}.  Since the \code{method} defaults to dQuote{glm}, the default is a glm list but should be changed if the dQuote{optim} method is chosen.}
\item{...}{Additional arguments passed along to \code{glm} or \code{optim.}
	}
}
\details{Observers are presented with pairs of pairs of stimuli, distributed along a physical stimulus axis.  For example, for stimuli \eqn{a, b, c, d} with \eqn{a < b < c < d}, they see the pairs \eqn{(a, b)} and \eqn{(c, d)}. For each trial, they make a judgement as to whether the difference between the elements of pair 1 is greater or not than the difference between the elements of pair 2.  From a large number of trials on different quadruples, \code{mlds} estimates numbers, \eqn{Psi_1,..., Psi_n}, by maximum likelihood such that \eqn{(Psi_d - Psi_c) > (Psi_b - Psi_a)} when the observer chooses pair 2, and pair 1, otherwise.
	
If there are \eqn{p} stimulus levels tested, then \eqn{p - 1} coefficients are estimated.  The \dQuote{glm} method constrains the lowest estimated value, \eqn{Psi_1 = 0}, while the \dQuote{optim} method constrains the lowest and highest values to be 0 and 1, respectively.  The \dQuote{optim} method estimates an additional scale parameter, \code{\sigma}, whereas this value is fixed at 1.0 for the \dQuote{glm} method.  In principle, the scales from the two methods are related by
\deqn{1/\sigma_o = max(Psi_g)}
where \eqn{\sigma_o} is \code{\sigma} estimated with the \dQuote{optim} method and \eqn{Psi_g} corresponds to the perceptual scale values estimated with the \dQuote{glm} method.  The equality may not be exact as the \dQuote{optim} method prevents the selection of values outside of the interval [0, 1] whereas the \dQuote{glm} method does not.
}
\value{
A list of class 'mlds' whose components depend on whether the method was "glm" or "optim", 
  \item{pscale }{A numeric vector of the estimated difference scale.}
  \item{stimulus }{The physical stimulus levels}
   \item{sigma }{The scale estimate, always 1.0 for "glm"}
    \item{method }{The fitting method}
     \item{link }{The binomial link specified, default "probit"}
      \item{obj }{For method "glm", an object of class "glm" resulting from the fit.}
  \item{logLik }{for method "optim", the logarithm of likelihood at convergence}
     \item{hess }{for method "optim", the Hessian matrix at convergence}
      \item{data }{For method "optim", the data.frame or "mlds.df" entered as an argument.}
      \item{conv }{For method "optim", a code indicating whether \code{optim} converged or not.  See \code{\link{optim}}.}
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5. }
\author{Kenneth Knoblauch and Laurence T. Maloney}
\note{ The glm method often generates warnings that fitted probabilities are 0 or 1. This does not usually affect the values of the estimated scale.  However, it may be wise to check the results with the optim method and obtain standard errors from a bootstrap method (see \code{\link{boot.mlds}}. The warnings will often disappear if the link is modified or more data are obtained. 
	}

\seealso{\code{\link{glm}}, \code{\link{optim}} }
\examples{
data(AutumnLab)
#Note the warnings generated by glm method
x.mlds <- mlds(AutumnLab)
summary(x.mlds)
y.mlds <- mlds(AutumnLab, method = "optim", opt.init = c(seq(0, 1, len = 10), 0.16))
summary(y.mlds)
plot(x.mlds)
#How sigma relates the scales obtained by the 2 different methods.
lines(y.mlds$stimulus,  y.mlds$pscale/y.mlds$sigma)
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
