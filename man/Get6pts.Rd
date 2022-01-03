\name{Get6pts}
\alias{Get6pts}

\title{Find All 6-point Conditions in data.frame}
\description{
 \code{Get6pts} enumerates all 6-point conditions from a difference scaling experiment and is used as one of the input arguments for calculating an observer's likelihood for his performance on 6-point conditions.
}
\usage{
Get6pts(x, nrep, ...)
}
\arguments{
  \item{x}{ an object of class \sQuote{mlds}.}
  \item{nrep}{ integer indicating how many sessions are in the data set.}
  \item{\dots}{Possibility of sending additional arguments but currently unused.}
}
\details{
  The 6-point condition is defined on 6-tuples of points, a, b, c, a', b', c', ordered on a physical scale.  The condition requires that if the pair \eqn{(a, b) > (a', b')} and the pair \eqn{(b, c) > (b', c')}, then \eqn{(a, c) > (a', c')}.
}
\value{A list of three same size data.frames with an attribute, \code{indices} that is a three column data.frame with the same number of rows as each of the three data.frames.  Each data.frame is of the format from a difference scaling experiment. Same named rows indicate three trials that form a 6-point condition, i.e., given the 6-tuple of stimuli, a, b, c, a', b', c',
  \item{A}{data.frame indicating the trials (a, b) vs (a', b')}
  \item{B}{data.frame indicating the trials (b, c) vs (b', c')}
  \item{E}{data.frame indicating the trials (a, c) vs (a', c')}
The attribute gives the row numbers from which the trials were obtained from the original data.frame.
}
\references{Maloney, L. T. and Yang, J. N. (2003) Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \doi{10.1167/3.8.5}.}
\author{Kenneth Knoblauch}
\note{It is important that the stimuli are in physical order and not the experimental order. If in experimental order, then \code{SwapOrder} should be applied first.
}
\seealso{ \code{\link{SwapOrder}},  \code{\link{lik6pt}},  \code{\link{simu.6pt}} }
\examples{
data(kk1)
kk.6pt <- Get6pts(mlds(SwapOrder(kk1)), nrep = 1)

}
\keyword{manip}
