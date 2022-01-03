\name{mlds}
\alias{mlds}
\alias{mlds.mlds.df}
\alias{mlds.mlbs.df}
\alias{mlds.data.frame}
\alias{mlds.formula}

\title{Fit Difference Scale by Maximum Likelihood }
\description{ Generic function  \code{mlds} uses different methods to fit the results of a difference scaling experiment either using \code{glm} (Generalized Linear Model), by direct maximization of the likelihood using \code{optim} or by maximizing the likelihood with respect to a function of the stimulus dimension specified by a one sided formula.
}
\usage{
mlds(x, ...)

\method{mlds}{mlds.df}(x, stimulus = NULL, method = "glm", 
	lnk = "probit", opt.meth = "BFGS", glm.meth = "glm.fit",
	opt.init = NULL, control = glm.control(maxit = 50000, epsilon = 1e-14), 
	... )
	
\method{mlds}{mlbs.df}(x, stimulus = NULL, method = "glm", 
	lnk = "probit",
	control = glm.control(maxit = 50000, epsilon = 1e-14), 
	glm.meth = "glm.fit", 
	... )

\method{mlds}{data.frame}(x, ... )

\method{mlds}{formula}(x, p, data, stimulus = NULL, 
	lnk = "probit", opt.meth = "BFGS", 
	control = list(maxit = 50000, reltol = 1e-14), ... )
}

\arguments{
	\item{x}{For comparisons of two pairs of stimuli, when the \code{method} is specified as \sQuote{glm} or \sQuote{optim} a data frame with 5 columns giving the response and the ranks of the stimulus levels for each trial, or an object of class \sQuote{mlds.df} which also contains additional information as attributes. For comparisons of triples of stimuli, only the method \sQuote{glm} is currently defined.  The object can be a data frame of 4 columns with the first specifying the response and the other 3 the stimulus level ranks, or an object of class \sQuote{mlbs.df}, which contains additional attributes.  It can also be a one-side formula with parameters \code{p} and stimulus variable \code{sx} that gives a parametric formula to fit to the data for the \code{formula} method.}
  \item{data}{ A data frame with 4 or 5 columns giving the response and the ranks of the stimulus levels for each trial, or an object of class \sQuote{mlbs.df} or \sQuote{mlds.df}, respectively, which also contains additional information as attributes, required when the \sQuote{formula} method is used.}
  \item{p}{numeric vector of parameters of length one greater than the number of parameters in the \code{formula} argument that specifies initial values for the parameters.  The extra parameter, specified last, is the initial estimate of \code{sigma}. }
  \item{stimulus}{A numeric vector that contains the physical stimulus levels used in the experiment.  If \code{data} is of class \sQuote{mlds.df}, this information is included as an attribute.  If \code{NULL}, a sequence of \eqn{1:n} is used, where n is the number of stimulus levels, deduced from the highest rank in \code{data}. }
  \item{method}{ character, taking the value of \dQuote{glm} or \dQuote{optim}.  Default is \dQuote{glm}. }
  \item{lnk}{character indicating either one of the built-in links for the binomial family or a user defined link of class \sQuote{link-glm}.  See \code{\link{family}} and \code{\link{make.link}}. Default is \dQuote{probit}.}
  \item{opt.meth}{If \code{method} = \dQuote{optim}, the method used by \code{optim} can be specified.  Defaults to \dQuote{BFGS}.}
  \item{opt.init}{Vector of numeric giving initial values which must be provided if you specify the \dQuote{optim} method.}
  \item{control}{ A list of control values for either \code{glm} or \code{optim}.  Since the \code{method} defaults to \dQuote{glm}, the default is a glm list but should be changed if the \dQuote{optim} method is chosen.}
 \item{glm.meth}{the method to be used in fitting the model, only when \code{method = glm}. The default value is \dQuote{glm.fit}.  See\code{\link{glm}} for further details.
 }
\item{...}{Additional arguments passed along to \code{glm} or \code{optim.}
	}
}
\details{Observers are presented with either triples or pairs of pairs of stimuli, distributed along a physical stimulus axis.  For example, for stimuli \eqn{a, b, c} with \eqn{a < b < c}, they see the triple \eqn{a, b, c}, or for stimuli \eqn{a, b, c, d} with \eqn{a < b < c < d}, they see the pairs \eqn{(a, b)} and \eqn{(c, d)}. For each trial, they make a judgement respectivily as to whether the difference between stimuli 1 and 2 is greater or not that between stimuli 2 and 3 or the elements of pair 1 is greater or not than the difference between the elements of pair 2.  From a large number of trials on different quadruples, \code{mlds} estimates numbers, \eqn{Psi_1,..., Psi_n}, by maximum likelihood such that \eqn{(Psi_d - Psi_c) > (Psi_b - Psi_a)} when the observer chooses pair 2, and pair 1, otherwise.
	
If there are \eqn{p} stimulus levels tested, then \eqn{p - 1} coefficients are estimated.  The \dQuote{glm} method constrains the lowest estimated value, \eqn{Psi_1 = 0}, while the \dQuote{optim} method constrains the lowest and highest values to be 0 and 1, respectively.  The \dQuote{optim} method estimates an additional scale parameter, \code{sigma}, whereas this value is fixed at 1.0 for the \dQuote{glm} method.  In principle, the scales from the two methods are related by
\deqn{1/\sigma_o = max(Psi_g)}
where \eqn{\sigma_o} is \code{sigma} estimated with the \dQuote{optim} method and \eqn{Psi_g} corresponds to the perceptual scale values estimated with the \dQuote{glm} method.  The equality may not be exact as the \dQuote{optim} method prevents the selection of values outside of the interval [0, 1] whereas the \dQuote{glm} method does not.
}
\value{
A list of class \sQuote{mlds} whose components depend on whether the method was specified as \sQuote{glm}, \sQuote{optim} with the default method, or the formula method was used,
  \item{pscale }{A numeric vector of the estimated difference scale.}
  \item{stimulus }{The physical stimulus levels}
   \item{sigma }{The scale estimate, always 1.0 for \sQuote{glm}}
    \item{method }{The fitting method}
     \item{link }{The binomial link specified, default \sQuote{probit}}
      \item{obj }{For method \sQuote{glm}, an object of class \sQuote{glm} resulting from the fit.}
  \item{logLik }{for method \sQuote{optim}, the logarithm of likelihood at convergence}
     \item{hess }{for method \sQuote{optim}, the Hessian matrix at convergence}
      \item{data }{For method\sQuote{optim}, the data.frame or \sQuote{mlds.df} entered as an argument.}
      \item{conv }{For method \sQuote{optim}, a code indicating whether \code{optim} converged or not.  See \code{\link{optim}}.}
   \item{par}{For \sQuote{formula} method, the parameters estimated.}
   \item{formula}{The one-sided formula specified with the \sQuote{method}.}
   \item{func}{For \sQuote{formula} method, a function obtained from the one-sided \code{formula}.}
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \doi{10.1167/3.8.5}. 

Knoblauch, K. and Maloney, L. T. (2008) MLDS:  Maximum likelihood difference scaling in R. 
\emph{Journal of Statistical Software}, \bold{25:2}, 1--26, \doi{10.18637/jss.v025.i02}.
}
\author{Kenneth Knoblauch and Laurence T. Maloney}
\note{ The glm method often generates warnings that fitted probabilities are 0 or 1. This does not usually affect the values of the estimated scale.  However, it may be wise to check the results with the optim method and obtain standard errors from a bootstrap method (see \code{\link{boot.mlds}}). The warnings will often disappear if the link is modified or more data are obtained. 
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

#Example with triads
data(kktriad)
kkt.mlds <- mlds(kktriad)
plot(kkt.mlds, type = "b")

#An example using the formula method
data(kk1)
# with one parameter
kk.frm1 <- mlds(~ sx^p, p = c(3, 0.02), data = kk1)
# with two parameters
kk.frm2 <- mlds(~p[1] * (sx + abs(sx - p[2])) - p[1] * p[2], 
	p = c(0.9, 0.3, 0.2), data = kk1)
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
