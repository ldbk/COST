\name{sampStrDef}
\alias{sampStrDef}
\alias{sampStrDef,csData-method}
\alias{sampStrDef,csDataVal-method}
\docType{methods}
\title{Definition of the sampling strategy}
\description{
This method defines from 'csData' or 'csDataVal' datasets what is the sampling strategy at haul level (one value per \emph{hh} row) and at trip level (one value per \emph{tr} row).    
}

\usage{
sampStrDef(x,species,fraction="LAN",fishAct="foCatEu5",sampPar=TRUE,\dots)
}

\arguments{
  \item{x}{A \emph{csData} or \emph{csDataVal} object.}
  \item{species}{Character specifying species for which sampling strategy must be defined} 
  \item{fraction}{Character specifying catch category for which sampling strategy must be defined. To be chosen between "LAN" (default value) for landings and "DIS" for discards.}
  \item{fishAct}{Character specifying fishing activity field taken into account for sampling strategy definition. To be chosen between "foCatEu5" (default value), "foCatEu6" and "focatNat".}
  \item{sampPar}{Logical. Checks if given species is considered to be automatically sampled (\code{TRUE}, default value) or not (\code{FALSE}) during the sampling process.}
  \item{...}{Further arguments.}
}

\value{Returned elements are \code{$sampStrategyTR} that describes the sampling strategy at trip level, and \code{$sampStrategyHH} for sampling strategy at haul level. Used codification is :
\item{1}{Sampling for length in fishing trips - Unsorted catch}
\item{2}{Sampling for length in fishing trips - Commercial Categories}
\item{3}{Sampling for length in Commercial categories}
\item{4}{Sampling for age in fishing trips - Unsorted catch}
\item{5}{Sampling for age in fishing trips - Commercial Categories}
\item{6}{Sampling for age in commercial categories}
\item{0}{No sampling or undefined strategy}
\item{9}{Several defined strategies (trip level)}
}           


\author{Mathieu Merzereaud}

\seealso{\code{\link[COSTcore]{csData}}, \code{\link[COSTcore]{csDataVal}}
}

\examples{
data(sole)
sampStrDef(sole.cs,species="Solea solea")
}

\keyword{methods}


