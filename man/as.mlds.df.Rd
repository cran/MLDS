\name{as.mlds.df}
\alias{df2mlds.df}
\alias{as.mlds.df}
\alias{as.mlds.df.data.frame}
\alias{as.mlbs.df.data.frame}
\alias{as.mlbs.df}

\title{ Coerces a data.frame to mlds.df }
\description{
  Coerce a data frame from an MLDS experiment to an object of class \code{mlbs.df} or \code{mlds.df} by adding column names and attributes so that it will be properly treated by methods related to the MLDS functions.
}
\usage{
as.mlds.df(d, ...)
as.mlbs.df(d, ...)

\method{as.mlds.df}{data.frame}(d, st, ...)
\method{as.mlbs.df}{data.frame}(d, st, ...)


df2mlds.df(d, st)
}
\arguments{
  \item{d}{ a 4 or 5 column data frame from an MLDS experiment, with one column of responses followed by three or four, respectively, indicating the indices of the stimuli from each trial or a data frame of > 5 columns with the response in the first column and the covariates (signed indicator variables) for the stimuli of each trial in the rest of the columns }
  \item{st}{numeric indicating the stimulus levels from the MLDS experiment }
  \item{...}{additional arguments passed to the methods}
}
\details{
 This function coerces a data frome from an MLDS experiment to an object of class \code{mlbs.df} or \code{mlds.df} but inheriting from class \code{data.frame}.  It changes the column names to \code{resp} and
 \code{S1}, \code{S2}, \code{S3} and if a quad experiment, \code{S4} and adds two attributes:  \code{stimulus}, a vector of scale values used in plotting the estimated scale and \code{invord}, a logical vector indicating the order of the presentation of pairs (were the larger scale values on the bottom or not) which is used with the \code{SwapOrder} function.
 Note that when the argument is in signed indicator form, just a data frame is returned with no special \code{mlds} attributes.
 }
\value{
 \code{data.frame} of class \code{mlbs.df} or \code{mlds.df} with \code{stimulus} and \code{invord} attributes, unless the input is in signed indicator form.  See details.
 }
\author{ Kenneth Knoblauch }
\note{ If the scale starts at 0, then 1 should be added to each scale value because the scale values will be used as indices and \code{R} indices start at 1, not 0.
}
\seealso{\code{\link{mlds}} }

\keyword{manip}
