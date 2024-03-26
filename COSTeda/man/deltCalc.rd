\name{deltCalc}
\alias{deltCalc}
\alias{deltCalc,csData,strIni-method}
\alias{deltCalc,csDataVal,strIni-method}
\docType{methods}
\title{Calculation of Delta index for sampling outliers detection and variance calculation}
\description{
This method implements the calculation of Delta values, derived from the formulation of the variance in landings-at-length.
It requires an input \emph{csData/csDataVal} object built from \pkg{COSTcore} package. Length distribution informations are taken from \emph{hl} table.
}

\usage{
deltCalc(data,strDef,species,fraction="LAN",strategy="metier",indSamp=TRUE,method="delta",\dots)
}

\arguments{
  \item{data}{A \emph{csData/csDataVal} object with \emph{tr}, \emph{hh}, \emph{sl} and \emph{hl} informations.}
  \item{strDef}{A \emph{strIni} object specifying time, space or/and technical stratification.}
  \item{species}{Field specifying species (e.g \code{"Solea solea"}).}
  \item{fraction}{Fate of the catch on which calculation is made. To be chosen between \code{"LAN"}, \code{"DIS"} and \code{"all"} for total catch.}
  \item{strategy}{To be chosen between \code{"metier"} and \code{"cc"} (for commercial categories). Sample definition differs according to chosen strategy.}
  \item{indSamp}{If \code{TRUE}, output is within each sample and is dedicated to outliers detection. If \code{FALSE}, output is within length classes and is dedicated to variance calculation.}
  \item{method}{Method for calculating delta values. If \code{"delta"}, samples are raised to haul/trip level. If \code{"subsample"}, samples are considered at sampling level (no raising process). }
  \item{...}{Further arguments.}
}

\details{For more informations about arguments, see \emph{FishFrame/COST Exchange format specification}.}

\value{An object of class \emph{edaResult} with \emph{desc="sampDeltaCalc"} if \emph{indSamp=TRUE}, 
  or with \emph{desc="varDeltaCalc"} if \emph{indSamp=FALSE}. In the first case, a rough 'condition factor' K is also computed 
  (returned as \emph{$Kid} element) to check for unexpected sample weights : K = sum(lenNum * lenCls^3)/subSampWt, for each sample. 
  }

\references{Vigneau, J. and Mahevas, S. (2007)
\emph{Detecting sampling outliers and sampling heterogeneity when catch-at-length is estimated using the ratio estimator}. Oxford Journals.
}
\author{Mathieu Merzereaud}
\seealso{\code{\link{edaResult}}, \code{\link{plot.edaResult}}, \code{\link{lenDisPlot}}
}

\examples{
data(sole)
strD <- strIni(timeStrata="quarter",techStrata="commCat")
obj <- deltCalc(sole.cs,strD,"Solea solea",strategy="cc")

}
\keyword{methods}
