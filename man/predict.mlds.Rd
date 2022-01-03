\name{predict.mlds}
\alias{predict.mlds}
\alias{predict.mlbs}

\title{Predict method for MLDS Fits }
\description{
 Predict values based on difference scale fit. 
}
\usage{
\method{predict}{mlds}(object, newdata = NULL, type = "link", ...)

\method{predict}{mlbs}(object, newdata = NULL, type = "link", ...)
}

\arguments{
  \item{object}{object of class \sQuote{mlds} or  \sQuote{mlbs}, typically from the output of \code{mlds}}
  \item{newdata}{A data.frame or object of class \sQuote{mlbs.df} with 4 columns or \sQuote{mlds.df} with 5 columns, corresponding to the response at each trial and the ranks of, respectively, the triple or quadruple of stimuli presented.}
  \item{type}{ character indicating scale on which predictions should be made, \dQuote{link}, the default, on the scale of the linear predictor or \dQuote{response}, on the response scale. }
  \item{\dots}{ When \code{obj$method} = \dQuote{glm}, these are additional arguments passed to \code{predict.glm}. }
}
\details{
  The \code{newdata} argument is needed principally for the 6-point test (see \code{\link{lik6pt}}), to extract the estimated probabilities for the subsets of the original data that form valid 6-point tests.
}
\value{
 A numeric vector of the predicted values either on the scale of the linear predictor or on the response scale.
}
\references{ Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585,  \doi{10.1167/3.8.5}.}
\author{Kenneth Knoblauch}

\seealso{\code{\link{mlds}}, \code{\link{lik6pt}}}
\examples{
data(kk1)
kk1.mlds <- mlds(SwapOrder(kk1))
predict(kk1.mlds, type = "response")
}

\keyword{ methods }
