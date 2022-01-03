\name{SimMLDS}
\alias{SimMLDS}

\title{
Simulate Output of MLDS Experiment
}
\description{
Given a block of trials of an MLDS experiment, an underlying response function and the judgment variability, simulate the response of an observer.
}
\usage{
SimMLDS(Trials, Scale, Sigma, n = 1)
}
\arguments{
  \item{Trials}{an N by 4 or 3 matrix or data frame of integers indicating the n trials of an MLDS experiment.  The columns indicate the indices of the stimuli presented on a trial, 4 for an experiment with quadruples and 3 for triads.  A data frame for this argument is most easily generated with the \code{combn} function.
}
  \item{Scale}{a vector of values indicating the underlying responses of the simulated observer for each stimulus level.  The length of this vector should equal the largest integer in \code{Trials}.
}
  \item{Sigma}{a vector of length 1 indicating the judgment standard deviation of the simulated observer.
}
  \item{n}{integer giving number of simulated data sets to return}
}
\details{
	Given a data frame of indices to the responses associated with stimulus levels and the judgment variability, the function returns the results of 1 or multiple MLDS experiments, either with triads or quads, depending on the number of columns in the data frame.
}
\value{
If the argument \code{n} is set to 1 (default), an object of class \sQuote{mlds.df} or \sQuote{mlbs.df} with simulated responses.  If \code{n} is greater than 1, a list of such objects is returned.
}
\references{
Maloney, L. T. and Yang, J. N. (2003). Maximum likelihood difference scaling. \emph{Journal of Vision}, \bold{3(8):5}, 573--585,  \doi{10.1167/3.8.5}. 

Knoblauch, K. and Maloney, L. T. (2008) MLDS:  Maximum likelihood difference scaling in R. 
\emph{Journal of Statistical Software}, \bold{25:2}, 1--26, \doi{10.18637/jss.v025.i02}.
}
\author{
Kenneth Knoblauch and Laurence T. Maloney
}


\seealso{
see also \code{\link{boot.mlds}}
}
\examples{

Tr <- t(combn(10, 4))
Sc <- seq(0, 1, len = 11)^2
Sig <- 0.2
sim.lst <- SimMLDS(Tr, Sc, Sig, n = 10)
sim.res <- sapply(sim.lst, mlds)


}

\keyword{datagen}
