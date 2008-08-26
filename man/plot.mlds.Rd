\name{plot.mlds}
\alias{plot.mlds}
\alias{lines.mlds}
\alias{points.mlds}

\title{ Plot a mlds Object }
\description{
  Plots the difference scale as a function of stimulus level.
}
\usage{
\method{plot}{mlds}(x, standard.scale = FALSE, ...)
\method{lines}{mlds}(x, standard.scale = FALSE, ...)
\method{points}{mlds}(x, standard.scale = FALSE, ...)
}

\arguments{
  \item{x}{\code{mlds} object, typically result of \code{mlds}}
  \item{standard.scale}{ logical indicating whether the plotted difference scale should be normalized to maximum value = 1}
  \item{\dots}{ other parameters to be passed through to the plotting function }
}

\author{ Kenneth Knoblauch}

\seealso{ \code{\link{mlds}} }
\examples{
data(kk1)
plot(mlds(SwapOrder(kk1)))
lines(mlds(SwapOrder(kk1)))


}

\keyword{ hplot}
