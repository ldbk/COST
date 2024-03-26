\name{landisVol}
\alias{landisVol}
\alias{landisVol,csData,missing-method}
\alias{landisVol,csData,strIni-method}
\docType{methods}
\title{Calculation upon sea-sampled catch weight at FO level, fishing day level, and trip level.}
\description{
This method creates an object of class \emph{edaResult} with \emph{desc="landisVol"} containing volume (weights) informations about sampled and raised catch,
for a given species in a specified catch category. It requires a \emph{csData} object built from \pkg{COSTcore} package.
Only sea sampling data is computed.
}


\usage{
%\S4method{landisVol}{csData,missing}(object,strDef="missing",species,fraction="LAN",sampPar=TRUE,\dots)
%\S4method{landisVol}{csData,strIni}(object,strDef,species,fraction="LAN",sampPar=TRUE,\dots)
landisVol(object,strDef,species,fraction="LAN",sampPar=TRUE,\dots)
}

\arguments{
  \item{object}{A \emph{csData} object with sea-sampling information (\emph{tr}, \emph{hh} and \emph{sl} required).}
  \item{strDef}{A \emph{strIni} object specifying time (e.g \code{"year"}, \code{"quarter"}, \code{"month"},...),
  space (e.g \code{"area"}, \code{"rect"},...) and/or technical stratification  (e.g \code{"foCatNat"}, \code{"foCatEu5"},...).}
  \item{species}{Field specifying species (e.g \code{"Solea solea"}).}
  \item{fraction}{Field specifying catch category (to be chosen between \code{"LAN"} and \code{"DIS"}.).}
  \item{sampPar}{Logical indicating if, during the sampling process, specified species is considered to be automatically sampled or not (therefore, 0-values criterion differs). }
  \item{...}{Further arguments.}
}


\value{An object of class \emph{edaResult} with \emph{desc="landisVol"}.}

\author{Mathieu Merzereaud}

\seealso{\code{\link{edaResult}}, \code{\link{plot.edaResult}}, \code{\link{boxplot.edaResult}}
}

\examples{
data(sole)  
df <- subset(sole.cs,sampType=="S")  #only sea sampling data is kept
obj <- landisVol(df,species="Solea solea")
}

\keyword{methods}
