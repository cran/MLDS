\name{runQuadExperiment}
\alias{runQuadExperiment}
\alias{runTriadExperiment}
\alias{runSampleExperiment}
\title{Start and run a Difference Scale Experiment }
\description{
 Runs a difference scaling experiment displaying stimuli with the function \code{DiplayTrial} defined by the function \code{DefineStimuli}.
 }
\usage{
runQuadExperiment(DisplayTrial, DefineStimuli, NumTrials = NULL,
 	DisplaySize = 7.5, aspect = 1, ...)
runTriadExperiment(DisplayTrial, DefineStimuli, NumTrials = NULL,
 	DisplaySize = 3.5, aspect = 1, ...)
runSampleExperiment(DisplayTrial, DefineStimuli)
}
\arguments{
  \item{DisplayTrial}{character giving the name of a function to display a trial with triple or quadruple of stimuli.}
  \item{DefineStimuli}{character giving the name of a function that defines the set of stimuli from which triples or quadruples are drawn.}
  \item{DisplaySize}{numeric giving the overall size of the display on the screen. }
  \item{NumTrials}{integer giving the number of trials to display.  If \code{NULL} (the default), the number is computed on the basis of the size of stimulus set designated by \code{DefineStimuli}.}
  \item{\dots}{currently unused}
  \item{aspect}{numeric giving the height/width ratio of the display on the screen.}
}
\details{
 These functions are to demonstrate how to run a difference scaling experiment.  Helper functions \code{DisplayOneQuad} or \code{DisplayOneTriad} and \code{DefineMyScale} permit running the perception of correlation experiment, sample data from which are provided as datasets,
 (see \code{\link{kk}}). 
 \code{runSampleExperiment} is defunct and replaced by these functions.
 }
\value{
 An object of class \sQuote{mlds.df} or \sQuote{mlbs.df}, depending on the experiment run, is returned.  Each inherits from \sQuote{data.frame} and has attributes \sQuote{stimulus} and \sQuote{invord}.
 See \code{\link{kk}} for an example.
 }
\author{ Kenneth Knoblauch and Laurence T. Maloney }

\examples{
##This will start a 330 trial interactive experiment
##  of quadruples
\dontrun{
	runQuadExperiment("DisplayOneQuad", "DefineMyScale")
	}
##This will run 10 trials of an interactive experiment
##  of triads
\dontrun{
	runTriadExperiment("DisplayOneTriad", 
		"DefineMyScale", NumTrials = 10)
	}
}
\keyword{misc}
