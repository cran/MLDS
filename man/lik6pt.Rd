\name{lik6pt}
\alias{lik6pt}

\title{ Compute Log Likelihood for 6-point Test }
\description{
  The 6-point test evaluates the validity of the estimated difference scale.  Given 6 values, a, b, c, a', b', c', on the stimulus scale, if the pair \eqn{(a, b) > (a', b')} and \eqn{(b, c) > (b', c')} then it must be that \eqn{(a, c) > (a', c')}, where the symbol \eqn{>} is taken here to mean \dQuote{is judged more different than}. Given the observer's difference scale and \eqn{\sigma} estimate, the likelihood of the choices made is calculated based on the link function indicated in the \sQuote{mlds} object.
}
\usage{
lik6pt(x, Six.Pts, ...)
}

\arguments{
  \item{x}{an object of class 'mlds', typically created by \code{mlds} }
  \item{Six.Pts}{ a list of 3 data.frames, with names \code{A}, \code{B}, \code{E}. Each data.frame corresponds to a sample from a difference scaling experiment.  The corresponding rows of the three data.frames yield the triples of trials that provide a 6-point test.  The list can be constructed with the function \code{GetSixPts}. }
  \item{\dots}{ currently unused. }
}

\value{
 Returns the likelihood of the observer's responses for all of the 6-point conditions from a given data set. As currently implemented, it returns a 1x1 matrix.
 }
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \url{http://journalofvision.org/3/8/5/}, doi:10.1167/3.8.5. }
\author{Kenneth Knoblauch, based on C code by Laurence T. Maloney and J. N. Yang.}

\seealso{\code{\link{Get6pts}}, \code{\link{mlds}},
	\code{\link{simu.6pt}} }
\examples{

data(kk1)
x.df <- mlds(kk1)
lik6pt(x.df, Get6pts(x.df))

}

\keyword{univar}
