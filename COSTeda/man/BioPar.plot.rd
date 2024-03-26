\name{bioPar.plot,csData-method}
\alias{bioPar.plot}
\alias{bioPar.plot,csData-method}
\docType{methods}
\title{Plots of individual biological parameters}
\description{
This method implements a scatter plot of individual biological data. It requires a \emph{csData} object with \emph{ca} table.
}

\usage{
bioPar.plot(object,type="wl",species="all",selection=FALSE,\dots)}

\arguments{
  \item{object}{A \emph{csData} object with \emph{ca} informations.}
  \item{type}{To be chosen between \code{"wl"} (weight at length), \code{"ml"} (maturity at length) or \code{"sl"} (sex at length).}
  \item{species}{A character (vector) specifying species (default value is "all" for all species) .}
  \item{selection}{If \code{TRUE}, outlier(s) identification can be done by clicking on points and corresponding ca table extract is then returned.}
  \item{...}{Further graphical arguments.}
}

\author{Mathieu Merzereaud}                                    

\seealso{\code{\link{bioPar.boxplot}}, \code{\link{csPlot.design}}, \code{\link{identify}}
}

\examples{
data(sole)
bioPar.plot(sole.cs)
}
\keyword{methods}
