\name{Transparency}
\alias{Transparency}
\docType{data}
\title{
Difference Scaling of Transparency
}
\description{
The data set was obtained from an experiment in which observers judged the differences between pairs of image pairs containing transparent pebble-shaped objects. The refractive indice (RI) of the transparent material of each object was varied systematically across the four images.
}
\usage{data(Transparency)}
\format{
  A data frame with 2520 observations on the following 6 variables.
  \describe{
    \item{\code{resp}}{a numeric vector taking on values of 0 and 1 indicating responses of observer.}
    \item{\code{S1}}{a numeric vector, rank order of weakest stimulus on the physical scale for a given trial.}
    \item{\code{S2}}{a numeric vector, rank order of physical stimulus 2 for the trial.}
    \item{\code{S3}}{a numeric vector , rank order of physical stimulus 3 for the trial.}
    \item{\code{S4}}{a numeric vector, rank order of strongest stimulus on the physical scale for a given trial.}
    \item{\code{Obs}}{a factor identifying observers with levels \code{O1} ... \code{O6}.}
  }
}
\details{
The physical scale is RI and the psychophysical scale is estimated using MLDS. Observers were not told to base their judmgents on RI (RI was not mentioned in instructions) but to judge apparent differences. Fleming et al (2007) conjectured that observers used a measure of the distortion of the background as seen through each transparent object in judging differences.
}

\references{Fleming, R.,   J\"akel, F., Maloney, L. T.
 (2007) Visual perception of refractive materials  
\emph{Journal of Vision}  \bold{7}, 561.
}
\examples{
	
Trsp.mlds <- mlds(as.mlds.df(Transparency[, -6], st = attr(Transparency, "stimulus")))
plot(Trsp.mlds, xlab = "Index of Refraction", type = "l",
	ylab = "Difference Scale", ylim = c(0, 20),
	lwd = 3)

Trsp.Obs <- sapply(levels(Transparency$Obs), function(obs)
	mlds(as.mlds.df(subset(Transparency, Obs == obs, 
		select = 1:5), st = attr(Transparency, "stimulus"))),
		simplify = FALSE, USE.NAMES = TRUE)
invisible(sapply(Trsp.Obs, lines, type = "b"))


}
\keyword{datasets}
