\name{lenDisPlot}
\alias{lenDisPlot}
\alias{lenDisPlot,csData-method}
\alias{lenDisPlot,csDataVal-method}
\docType{methods}
\title{Plot of length distribution}
\description{
This method plots from a \emph{csData/csDataVal} object the length distribution of one or several trips, for given species and catch category, at trip level or fishing operation level.
}

\usage{
lenDisPlot(x,species,fraction="LAN",trpCode="all",level="trip",\dots)
}

\arguments{
  \item{x}{A \emph{csData} or \emph{csDataVal} object with \emph{hl} informations.}
  \item{species}{Character string specifying species (e.g \code{"Solea solea"}).}                                                                                                                         
  \item{fraction}{Field specifying catch category (\code{"LAN"}, \code{"DIS"}, or \code{c("LAN","DIS")} for total catch).}
  \item{trpCode}{Character specifying trip codes ("all" means that all trips in \emph{x} object will be considered).}
  \item{level}{Character string specifying the level at which the length distribution is considered (\code{"fo"} for fishing operation-level, \code{"trip"} for trip-level).}
  \item{...}{Further graphical arguments.}
}

\author{Mathieu Merzereaud}

\seealso{\code{\link{edaResult}}, \code{\link{deltCalc}}, \code{\link{plot.edaResult}}
}

\examples{
data(sole)
lenDisPlot(sole.cs,"Solea solea","DIS","DIL1197",level="fo")
}
\keyword{methods}
