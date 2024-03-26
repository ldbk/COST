\name{disCorrPlot,csDataCons-method}
\alias{disCorrPlot}
\alias{disCorrPlot,csDataCons-method}
\docType{methods}
\title{Scatterplot of discards volume versus an auxiliary variable}
\description{
Plot a scatter plot of discards volume (weight or number) versus an auxiliary variable that can be fishing time or landings volume.
}

\usage{
disCorrPlot(object,species="all",landSpp=as.character(NA),aux="time",val="weight",
            sampPar=TRUE,timeStrata=FALSE,spaceStrata=FALSE,techStrata=FALSE,reg=TRUE,
            show.legend="right",\dots)}

\arguments{
  \item{object}{A \emph{csDataCons} object.}
  \item{species}{Character (vector) specifying considered discarded species (default value is "all" for all species).}
  \item{landSpp}{Character (vector) specifying considered landed species (if NA (default value), species are those described by 'species' parameter). 
  Unused if fishing time is the auxiliary variable.}
  \item{aux}{Character defining auxiliary variable (to be chosen between "time" for fishing time, and "landings" for landings volume).}
  \item{val}{Character specifying considered variable (to be chosen between "weight" and "number").}
  \item{sampPar}{Logical specifying if given species is/are considered to be automatically sampled during the sampling process (default value is \code{TRUE}).}
  \item{timeStrata}{Logical. If TRUE, display is done within time strata.}
  \item{spaceStrata}{Logical. If TRUE, display is done within spatial strata.}
  \item{techStrata}{Logical. If TRUE, display is done within technical strata.}
  \item{reg}{Logical. If TRUE, a simple regression line is drawn in each panel.}
  \item{show.legend}{Logical. If TRUE, a legend is displayed.}    
  \item{...}{Further graphical arguments.}
}

\value{
A dataframe with discards volume (\emph{disVol} field) and an auxiliary variable (\emph{auxVar}) 
values for each sampled haul/trip recorded in \emph{object@hh} table, with time, space and technical strata specification (one row for one plotted point). 
}


\author{Mathieu Merzereaud}

\seealso{\code{\link[COSTcore]{csDataCons}}
}

\examples{
#consolidated object is created for a given stratification
data(sole)
strDef <- strIni(timeStrata="quarter",techStrata="foCatEu5")
object <- csDataCons(csDataVal(sole.cs),strDef)

res <- disCorrPlot(object,aux="landings",techStrata=TRUE,l.col="steelblue",bg="gold",lty=2)
}
\keyword{methods}
