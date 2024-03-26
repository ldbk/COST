\name{bioPar.boxplot,csData-method}
\alias{bioPar.boxplot}
\alias{bioPar.boxplot,csData-method}
\docType{methods}
\title{Boxplot of individual biological parameters}
\description{
This method implements a boxplot of individual biological data. It requires a \emph{csData} object with \emph{ca} table.
}

\usage{
bioPar.boxplot(object,type="wl",species="all",\dots)
}

\arguments{
  \item{object}{A \emph{csData} object with \emph{ca} informations.}
  \item{type}{To be chosen between \code{"wl"} (weight at length), \code{"ml"} (maturity at length) or \code{"sl"} (sex at length).}
  \item{species}{A character (vector) specifying species (default value is "all" for all species) .}
  \item{...}{Further graphical arguments.}
}

\author{Mathieu Merzereaud}

\seealso{\code{\link{bioPar.plot}}, \code{\link{csPlot.design}}
}

\examples{
data(sole)
bioPar.boxplot(sole.cs)

#4 graphs on the same page
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
print(bioPar.plot(sole.cs),newpage=FALSE) ; popViewport(1)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
print(bioPar.plot(sole.cs,type="ml"),newpage=FALSE) ; popViewport(1)
pushViewport(viewport(layout.pos.col=1, layout.pos.row=2))
print(bioPar.boxplot(sole.cs,type="sl"),newpage=FALSE) ; popViewport(1)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
print(bioPar.boxplot(sole.cs,type="ml"),newpage=FALSE) ; popViewport(1)

}
\keyword{methods}
