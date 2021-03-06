\name{DisplayOneQuad}
\alias{DisplayOneQuad}
\alias{DisplayOneTriad}
\alias{DefineMyScale}
\title{ Helper Functions for Perception of Correlation Difference Scale Experiment }
\description{
 These functions are required by \code{runTriadExperiment} and \code{runQuadExperiment} to define and display trials.  They are provided as examples, and users are expected to define their own functions to define and display stimuli for estimating difference scales along other physical continua.  \code{DefineMyScale} returns a numeric vector providing the physical levels to be tested (here values of \eqn{r}).  \code{DisplayOneQuad} produces a 2 x 2 graphic of four scatterplots. \code{DisplayOneTriad} produces a 1 x 3 graphic of three scatterplots. 
}
\usage{
DisplayOneQuad(rr, PntNum = 100, ptSize = 1, 
	xlim = c(-4, 4), ylim = c(-4, 4))
DisplayOneTriad(rr, PntNum = 100, ptSize = 1, 
	xlim = c(-4, 4), ylim = c(-4, 4))
DefineMyScale(rr = c(seq(0, 0.9, len = 10), 0.98))
}

\arguments{
  \item{rr}{vector of numeric or integer.  For \code{DisplayOneTrial}, a four element vector of integers, providing the indices of the physical scale values to be displayed.  For \code{DefineMyScale}, a numeric vector, providing the physical scale values to be tested.}
  \item{PntNum}{numeric giving the number of points to be displayed in each subgraphic.}
  \item{ptSize}{numeric indicating the size of the points in each graphic, passed to the \code{cex} argument of \code{plot}.}
  \item{xlim, ylim}{2-element numerics to determine range of values plotted on the display.}
}

\value{
\code{DisplayOneTriad} and \code{DisplayOneQuad} are used for their side-effect of producing a stimulus on the display.  \code{DefineMyScale} outputs a numeric vector of the physical scale.  While its use here is quite trivial, in general, it permits the tailoring of the stimulus levels to the particular experiment.  In the present case, one could imagine, for example, modifying it so that successive levels were evenly spaced in \eqn{r^2} rather than \eqn{r}.
  }

\author{Kenneth Knoblauch and Laurence T. Maloney}

\seealso{ \code{\link{runTriadExperiment}}, \code{\link{runQuadExperiment}} }
\examples{

\dontrun{runQuadExperiment(DisplayTrial = "DisplayOneQuad", 
		DefineStimuli = "DefineMyScale")}

}
\keyword{misc}
