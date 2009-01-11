\name{make.ix.mat}
\alias{make.ix.mat}

\title{Create data.frame for Fitting Difference Scale by glm }
\description{
\code{make.ix.mat} generates a \eqn{n x p} matrix from the \eqn{n x 5} column data.frame storing the results of a difference scaling experiment, where \eqn{p} is the number of stimulus levels tested and \eqn{n} is the number of trials.  The first column is the response (0 or 1), and the \eqn{p - 1} succeeding columns code covariates for all but the first stimulus level, which is contrained to be 0. These columns take the value 0 unless the stimulus level was in the trial, in which case they take, in order, the values, 1, -1, -1, 1.
}
\usage{
make.ix.mat(data, xi = NULL, ...)
}

\arguments{
  \item{data}{a 5 column data.frame.  The first column is the \code{resp} and the next 4 columns the index of the stimulus level, 1 to \eqn{p}, labelled \code{S1-S4}}
  \item{xi}{an integer indicating the number of stimulus levels tested.  If this is NULL, it is determined from the maximum value in \code{data}.}
  \item{\dots}{Other arguments, not used for the moment.}
}
\details{
  To fit a difference scale using \code{mlds} and \code{method} = \dQuote{glm}, each stimulus level is treated as a covariate taking on the values 0, if it was not present in the trial, or -1 or 1, the latter two depending on the ordinal stimulus level within the trial.  This is a helper function to transform the typical 5 column data.frame from a difference scaling experiment, indicating the response and the 4 stimulus levels, to one in the format described above.  Matrices of this form can also be used as \code{newdata} for the predict method.  This is exploited in the function \code{like6pt}.  It is here that the argument \code{xi} is necessary since the data.frame for the first of the 6-point comparisons does not contain the highest level of the scale and so needs to be specified so that the data.frame conforms with that used to generate the \sQuote{mlds} object.
}
\value{
 A data.frame with\eqn{n} rows and \eqn{p} columns.
  \item{resp}{The \code{resp} (0, 1) is coded in the first column.  This could be logical, instead.}
  \item{stim.2--stim.p}{Columns 2 through \eqn{p} code the
  	presence and absence of the stimulus on a trial.  If the stimulus is present, then the value is -1 or 1 as a function of the contribution of the stimulus level to the decision variable.}
}

\author{Kenneth Knoblauch}
\note{ In the current parameterization, the coefficient of the initial stimulus level is constrained to 0.  Thus, the column corresponding to this level is left-out of the data.frame.  For trials in which this stimulus is present, the non-zero elements are (-1, -1, 1), in that order.
}
\seealso{\code{\link{ix.mat2df}}, \code{\link{mlds}}, \code{\link{lik6pt}}, \code{\link{glm}} }
\examples{
data(AutumnLab)
make.ix.mat(AutumnLab)
mlds(AutumnLab, c(1, seq(6, 30, 3)))
}
\keyword{manip}
