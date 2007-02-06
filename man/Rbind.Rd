\name{Rbind}
\alias{Rbind}
\title{ Concatenate Objects of Class 'mlds.df' by Row}
\description{
 Concatenate the data.frame of 'mlds.df' objects by row
and concatenate the 'invord' attributes, also. 
}
\usage{
Rbind(...)
}

\arguments{
  \item{\dots}{Objects of class mlds.df.}
}
\details{
  Uses rbind.data.frame to concatenate the data.frame component of several mlds.df objects and then concatenates there 'invord' attributes, as well.  It would have been better to have an rbind method but since the objects inherit from class data.frame, this method would be called automatically and the 'invord' attribute would not be updated.
}
\value{
 An object of class 'mlds.df that is composed of data from several experiments. 
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
