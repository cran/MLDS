\name{Rbind}
\alias{Rbind}
\title{ Concatenate Objects of Class 'mlds.df' by Row}
\description{
 Concatenate the data.frame of \sQuote{mlds.df} objects by row
and concatenate the \sQuote{invord} attributes, also. 
}
\usage{
Rbind(...)
}

\arguments{
  \item{\dots}{Objects of class \sQuote{mlds.df}.}
}
\details{
  Uses \code{rbind.data.frame} to concatenate the data.frame component of several \sQuote{mlds.df} objects and then concatenates there \code{invord} attributes, as well. 
}
\value{
 An object of class \sQuote{mlds.df} that is composed of data from several experiments. 
}

\author{Kenneth Knoblauch }

\examples{
data(kk1)
data(kk2)
data(kk3)
kk <- Rbind(kk1, kk2, kk3)
nrow(kk1)
nrow(kk)
length(attr(kk1, "invord"))
length(attr(kk, "invord"))
}
\keyword{manip}
