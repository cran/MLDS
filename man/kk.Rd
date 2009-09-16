\name{kk}
\alias{kk}
\alias{kk1}
\alias{kk2}
\alias{kk3}
\alias{kktriad}
\docType{data}
\title{Difference Scale Judgment Data Sets}
\description{
Three data sets for one subject in a sample difference scaling experiment. The stimuli were scatterplots of bivariate Gaussian samples with different correlations. Either the function \code{runQuadExperiment} or \code{runTriadExperiment} was used to collect the data.
}
\usage{
	data(kk1)
	data(kk2)
	data(kk3)
	data(kktriad)}
\format{
  Four data frames, (kk1, kk2, kk3, kktriad) with 330, or in the case of kktriad 165, observations each with the following 5 (4 for kktriad) components.
  \describe{
    \item{\code{resp}}{a numeric vector taking on values 0 and 1 indicating responses of observer}
    \item{\code{S1}}{a numeric vector, rank order of weakest stimulus on the physical scale.}
    \item{\code{S2}}{a numeric vector, rank order of physical stimulus 2.}
    \item{\code{S3}}{a numeric vector, rank order of physical stimulus 3.}
    \item{\code{S4}}{a numeric vector, rank order of strongest stimulus on the physical scale. \code{kktriad} does not contain this component as it is from an experiment with triads rather than quadruples}
  }
}
\details{ The kk1-3 datasets were generated on three separate days using the function
	\code{runQuadExperiment} with \code{DisplayOneQuad} and \code{DefineMyScale} to define the stimuli and display them, anti-respectively.  The experiments were run on a Macintosh Pro with a 15 inch screen.  The observer was seated about 40 cm from the screen. The kktriad data set was generated from one run using the function \code{runTriadExperiment} with \code{DisplayOneTriad} to control the display.
}
\references{
Knoblauch, K. and Maloney, L. T. (2008) MLDS:  Maximum likelihood difference scaling in R. 
\emph{Journal of Statistical Software}, \bold{25:2}, 1--26, \url{http://www.jstatsoft.org/v25/i02}.
}

\examples{
data(kk1)
plot(mlds(SwapOrder(kk1))) 
# Fit and plot difference scale for first data set kk1, 
#  using quadruples of stimuli

data(kktriad)
plot(mlds(kktriad), type = "b") 
# Fit and plot experimental data 
#   using triples of stimuli
}
\keyword{datasets}
