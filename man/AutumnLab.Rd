\name{AutumnLab}
\alias{AutumnLab}
\docType{data}
\title{Difference Scale Judgement Data Set}
\description{
  Judgments and stimulus ranks for one observer during one session from a difference scaling experiment.  On each trial, the observer is presented with 4 stimuli \eqn{(a, b, c, d)} from a physical scale with \eqn{a < b < c < d}.  The stimuli are presented as two pairs \eqn{(a, b)} and \eqn{(c, d)} and the observer judges between which pair the perceived difference is greater.  
}
\usage{data(AutumnLab)}
\format{
  A data frame with 210 observations on the following 5 variables.
  \describe{
    \item{\code{resp}}{an integer vector of 0, 1, indicating the choice of the observer between the pairs (\code{S1}, \code{S2}) vs (\code{S3}, \code{S4})}
    \item{\code{S1}}{an integer vector, rank order of physical stimulus level for the lowest/weakest stimulus}
    \item{\code{S2}}{an integer vector, rank order of physical stimulus 2}
    \item{\code{S3}}{an integer vector, rank order of physical stimulus 3}
    \item{\code{S4}}{an integer vector, rank order of higher/strongest stimulus}
  }
}
\details{
  The difference scaling paradigm was used to estimate changes of image quality with increasing rate of image compression.  Image compression was performed using Vector Quantisation and the standard Lab color space.  In the overall experiment, the compression was performed for several images and in several different color spaces.  Scales were obtained from several observers, as well.  These data are from one observer, for one image with compression performed in the Lab color space.  The 10 compression levels tested  were 1 (no compression), and 6-30\% reduction in file size in steps of 3.  The physical stimulus levels are attached to the data.frame as an attribute, \code{stimulus}.  There is also an attribute, \code{invord}, which is a vector of type logical of length 210, which can be used with \code{SwapOrder} to restore the order of the pairs to correspond to physical position (lower/upper) in the experiment, rather than the way it is stored, stimulus level (lower/higher compression). 
}


\references{
 Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585, \doi{10.1167/3.8.5}.
 
Charrier, C., Maloney, L. T., Cherifi, H. \& Knoblauch, K. (2007) 
Maximum likelihood difference scaling of image quality in 
compression-degraded images, 
\emph{Journal of the Optical Society of America}, \bold{24}, 3418--3426.
}
\examples{
data(AutumnLab)
plot(mlds(AutumnLab))
}
\keyword{datasets}
