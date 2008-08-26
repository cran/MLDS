\name{SwapOrder}
\alias{SwapOrder}

\title{Order Stimuli and Adjust  Responses from Difference Scaling
  data.frame }

\description{ Invert the order of the stimuli on each trial so that they are in physical order and not the presentation order and adjust responses, accordingly to reflect whether the lower or higher stimulus pair was selected or the inverse.  }

\usage{ SwapOrder(data) } 

\arguments{
     \item{data}{data.frame or \sQuote{mlds.df} object with five columns, \code{resp}, \code{S1},
\code{S2}, \code{S3} and \code{S4}.  \code{resp} is the binary response
of the observer (it could be a logical).  The other four indicate the
four stimulus levels presented on a given trial. In principle, this
function can be applied when the order reflects the position of the
pairs and not their physical ordering. } }

\details{ Pairs of stimuli in a difference scaling experiment are often presented on top and bottom and the responses, 0 and 1, refer to bottom and top pair, respectively.  \code{SwapOrder} modifies the data.frame so that the stimuli are ordered by physical level and the responses, 0 and 1, are modified to refer to the lower and upper pair on the physical scale, respectively. If the object inherits also from class \sQuote{mlds.df}, it has an \code{invord} attribute that is a logical vector of length the number of rows in the data.frame.  It indicates the trials on which the higher stimulus levels were presented on the bottom. If this attribute is present, it is used to modify the data.frame.  In this case, the function acts as its own inverse.  }

\value{ A data.frame of the same format as the input data.frame but with the stimulus order modified and the response inverted on those trials on which the higher physical level stimulus were on the bottom. If the order reflects the position, it is modified to reflect the stimulus level and vice versa.
}
\author{Kenneth Knoblauch}
\note{Storing the physical position of the stimuli allows the original configuration not to be lost when re-ordering the stimuli to reflect physical stimulus level.  The \code{invord} attribute could, in principle, be used, also, to test for an influence of or bias related to the physical position.
}

\examples{
data(kk1)
kk1.swo <- SwapOrder(kk1)
}
\keyword{manip}
