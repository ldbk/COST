\name{sampledFO}
\alias{sampledFO}
\alias{sampledFO,csDataCons-method}
\docType{methods}
\title{Sampled haul index}
\description{
Method returning from a 'csDataCons' object an index of sampled hauls for a given species and a given fraction.
}

\usage{
sampledFO(x,species,fraction="LAN",sampPar=TRUE,...)
}

\arguments{
  \item{x}{A \emph{csDataCons} object.}
  \item{species}{Given species for which sampling must be considered.}
  \item{fraction}{Given catch category for which sampling must be considered. To be chosen between "LAN" (default value) and "DIS".}
  \item{sampPar}{logical specifying if given species is considered to be automatically sampled during the sampling process (default value is \code{TRUE}).}
  \item{...}{Further arguments.}
}

\value{A list with 2 vectors of the same length as HH table from input 'csDataCons' object. \code{$sampWt} is the index for weight sampling, \code{$sampLg} is for length sampling (numbers). 
1 means that the haul is sampled/measured and the species is caught in the fraction, 0 means that the haul is sampled/measured but the species is not caught in the fraction (0-values), 
and NA means that the haul is not sampled/measured.
}


\author{Mathieu Merzereaud}

\seealso{\code{\link[COSTcore]{csDataCons}}
}

\examples{
data(sole)
x <- csDataCons(csDataVal(sole.cs))
sampledFO(x,species="Solea solea",fraction="LAN")

}
\keyword{methods}
