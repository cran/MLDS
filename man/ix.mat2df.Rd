\name{ix.mat2df}
\alias{ix.mat2df}
\title{Transform data.frame back to Raw Difference Scale Format}
\description{
 \code{ix.mat2df} converts a long data.frame that is used as the data argument for \code{mlds} with  method = \dQuote{glm} to the 5 column format, typically from the results of a difference scaling experiment.  The first column is the response and the next four are the ranks of the stimulus levels used in each trial, the 2 pairs.  \code{ix.mat2df} is the inverse of \code{make.ix.mat}.  This form of data.frame is used with the \dQuote{optim} method of \code{mlds}.
}
\usage{
ix.mat2df(d)
}
\arguments{
  \item{d}{ a data.frame with \eqn{p} columns and \eqn{n} rows, where \eqn{p} is the number of stimulus levels and \eqn{n} is the number of trials. The first column is the \code{resp}, 0 or 1, and the next \eqn{p - 1} columns correspond to the incidences of the stimulus levels on each trial, 3 or 4 per row, with the signs \eqn{+ - - +}.  The column corresponding to the lowest level stimulus is deleted, as its coefficient is contrained to equal 0 in the current parameterization of the problem.  Thus, 3 non-zero terms would be of the form \eqn{ - - +}, indicating the presence of the lowest level stimulus in the trial. }
}


\value{
  A 5 column data.frame which could be coerced to interger. 
  \item{resp }{The response, 0 or 1, indicating which pair was chosen}
  \item{S1-S4 }{The rank of the 4 stimulus levels presented on each trial.}
}

\author{Kenneth Knoblauch}
\note{ }
\seealso{\code{\link{make.ix.mat}},  \code{\link{mlds}}}
\examples{
	
data(AutumnLab)
ix.mat <- make.ix.mat(AutumnLab)
#orig.df <- ix.mat2df(ix.mat) # should be the same as original
# better to use as.mlds.df as ix.mat2df is deprecated
orig.df <- as.mlds.df(ix.mat) 

}
\keyword{manip}
