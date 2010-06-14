\name{summary.mlds.bt}
\alias{summary.mlds.bt}

\title{
Method to Extract Bootstrap Values for MLDS Scale Values
}
\description{
Extracts the means and standard deviations of the bootstrapped scale values from an MLDS experiment.
}
\usage{
\method{summary}{mlds.bt}(object, standard.scale = TRUE, sigma = FALSE, ...)
}

\arguments{
  \item{object}{object of class \sQuote{mlds.bt}, typically obtained from running \code{boot.mlds} or \code{boot.mlbs} on an object created by \code{mlds}.
}
  \item{standard.scale}{logical, if TRUE (default), the values are returned on the standard scale (0, 1).  Otherwise, the values are returned in unnormalized units.
}
  \item{sigma}{
If TRUE and \code{standard.scale = TRUE}, the bootstrapped \code{sigma} value is printed out and its standard deviation.
}
  \item{...}{additional arguments to summary, currently unused.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A two column matrix is returned of the bootstrap means and standard deviations in columns 1 and 2, respectively.
}

\author{
Kenneth Knoblauch
}


\seealso{
 \code{\link{boot.mlds}} 
}
\examples{
data(kk1)
kk.mlds <- mlds(kk1)
kk.bt <- boot.mlds(kk.mlds, nsim = 10)
summary(kk.bt)

}

\keyword{methods}
