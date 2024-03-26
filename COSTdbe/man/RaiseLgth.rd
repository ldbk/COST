\name{RaiseLgth}
\alias{RaiseLgth}
\alias{RaiseLgth,dbeOutput,csDataCons,clDataCons-method}
\alias{RaiseLgth,dbeOutput,csDataCons,missing-method}
\docType{methods}
\title{Estimation of total numbers-at-length from market sampling}
\description{
This method calculates total numbers-at-length by strata from market sampling data. 
}

\usage{
RaiseLgth(dbeOutput,csObject,clObject,spp,taxon,sex=as.character(NA),sampPar=TRUE,incl.precision=TRUE,probs=c(0.025,0.975),\dots)
}

\arguments{
  \item{dbeOutput}{A \emph{dbeOutput} object.}
  \item{csObject}{A \emph{csDataCons} object matching 'dbeOutput' specifications.}
  \item{clObject}{A \emph{clDataCons} object matching 'dbeOutput' specifications. If not specified, data is only raised to trips.}
  \item{spp}{Species, if missing this is set to dbeOutput@species}
  \item{taxon}{Taxon, if missing this is set to dbeOutput@species}
  \item{sex}{Sex}
  \item{sampPar}{logical specifying if given species is considered to be automatically sampled during the sampling process (default value is TRUE)}
  \item{incl.precision}{Logical. If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs.}
  \item{probs}{Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}.}
  \item{\dots}{Further arguments}  
}


\value{An updated object of class \emph{dbeOutput}.
Slots nSamp\$len & nMeas\$len with number of samples and measurements,
totalW\$estim with total weight,
lenStruc\$estim with numbers-at-length estimates,
lenVar with the variance of numbers-at-length,
totalN\$estim with total numbers,
totalNvar with the variance of total numbers.
 }


\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbeObject}}, \code{\link{RaiseAge}}, \code{\link{dbeCalc}}, \code{\link[COSTcore]{csDataCons}}, \code{\link[COSTcore]{clDataCons}}
}

\examples{
data(sole)
#stratification
strD <- strIni(timeStrata="quarter",techStrata="commCat")
#only market sampling data and biological parameters are kept
csObject <- csDataCons(csDataVal(subset(sole.cs,sampType\%in\%c("M","V"))),strD)
clObject <- clDataCons(clDataVal(sole.cl),strD)
#initializing the output object
dbeOutput <- dbeObject(species="Solea solea",catchCat="LAN",strataDesc=strD)

# total numbers at length
dbeOutput <- RaiseLgth (dbeOutput, csObject, clObject)

}
\keyword{methods}
