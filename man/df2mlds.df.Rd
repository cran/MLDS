\name{df2mlds.df}
\alias{df2mlds.df}

\title{ Coerces a data.frame to mlds.df }
\description{
  Coerce a data frame from an MLDS experiment to an object of class \code{mlds.df} by adding column names and attributes so that it will be properly treated by methods related to the MLDS functions.
}
\usage{
df2mlds.df(d, st)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{ a 5 column data frame from an MLDS experiment, with one column of responses followed by four indicated the indices of the stimuli from each trial }
  \item{st}{numeric indicating the stimulus levels from the MLDS experiment }
}
\details{
 This function coerces a data frome from an MLDS experiment to an object of class \code{mlds.df} but inheriting from class \code{data.frame}.  It changes the column names to \code{resp} and
 \code{S1}, \code{S2}, \code{S3}, \code{S4} and adds two attributes:  \code{stimulus}, a vector of scale values used in plotting the estimated scale and \code{invord}, a logical vector indicating the order of the presentation of pairs (were the larger scale values on the bottom or not) which is used with the \code{SwapOrder} function.
 }
\value{
 \code{data.frame} of class \code{mlds.df} with \code{stimulus} and \code{invord} attributes.
 }
\author{ Kenneth Knoblauch }
\note{ If the scale starts at 0, then 1 should be added to each scale value because the scale values will be used as indices and \code{R} indices start at 1, not 0.
}
\seealso{\code{\link{mlds}} }
\examples{
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
