\name{runSampleExperiment}
\alias{runSampleExperiment}
\title{Start and run a Difference Scale Experiment }
\description{
 Runs a difference scaling experiment displaying stimuli with the function \code{DiplayTrial} defined by the function \code{DefineStimuli}.
 }
\usage{
runSampleExperiment(DisplayTrial, DefineStimuli, NumTrials = NULL, DisplaySize = 10, ...)
}
\arguments{
  \item{DisplayTrial}{character giving the name of a function to display a trial with quadruple of stimuli.}
  \item{DefineStimuli}{character giving the name of a function that defines the set of stimuli from which quadruples are drawn.}
  \item{DisplaySize}{numeric giving the overall size of the display on the screen. }
  \item{NumTrials}{integer giving the number of trials to display.  If \code{NULL} (the default), the number is computed on the basis of the size of stimulus set designated by \code{DefineStimuli}.}
  \item{\dots}{currently unused}
}
\details{
 This function is to demonstrate how to run a difference scaling experiment.  Two helper functions \code{DisplayOneTrial} and \code{DefineMyScale} permit running the perception of correlation experiment, sample data from which are provided as datasets,
 {see \code{\link{kk}}.}
 }
\value{
 An object of class `mlds.df' is returned.  It inherits from
 `data.frame' and has attributes `stimulus' and `invord'.
 See \code{\link{kk}} for an example.
 }
\author{ Kenneth Knoblauch and Laurence T. Maloney }

\examples{
##This will start a 330 trial interactive experiment
\dontrun{runSampleExperiment("DisplayOneTrial", "DefineMyScale")}
##This will run 10 trials of an interactive experiment
\dontrun{runSampleExperiment("DisplayOneTrial", "DefineMyScale", NumTrials = 10)}
}
\keyword{misc}
