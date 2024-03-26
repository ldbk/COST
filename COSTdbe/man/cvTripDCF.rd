\name{cvTripDCF}
\alias{cvTripDCF}
\alias{cvTripDCF,csDataCons-method}
\docType{methods}
\title{Method for calculating CVs within trips}
\description{This method calculates CVs of numbers-at-length estimates, total number and total weight estimates within each trip 
for each stratum from a \emph{csDataCons} object. It also computes a fourth indicator that is the weighted mean of CVs of number-at-length per stratum, 
and finally the number of trips per stratum.     
}

\usage{           
cvTripDCF(csObject,spp,catchCat="LAN",sampPar=TRUE,\dots)
}


\arguments{
  \item{csObject}{A \emph{csDataCons} object.}
  \item{spp}{Character. The species for which calculation is to be made.}
  \item{catchCat}{Character. Specified catch category. Typically "LAN" or "DIS" or both.}
  \item{sampPar}{Logical. Parameter characterizing the sampling process and the way 0-values have to be considered in the calculating process. See \emph{sampledFO} method.}
  \item{...}{Further arguments}} 
  

\value{A list with one element per species. Each element is a list with 5 elements : \emph{$nbT} is the number of trips per stratum, 
\emph{$D} is the CVs of numbers-at-length, \emph{$N} the CVs of the total numbers, and \emph{$W} the CVs of the total weights (along with the means and the SDs), 
and \emph{DCF} is the weighted mean of the CVs of the numbers-at-length, per stratum)
}
                                      

\author{Mathieu Merzereaud}
\seealso{\code{\link{sampledFO}}, \code{\link[COSTcore]{csDataCons}}}

\examples{
data(sole)
#stratification
strD <- strIni(timeStrata="quarter",techStrata="commCat")
#only market sampling data and biological parameters are kept
csObject <- csDataCons(csDataVal(subset(sole.cs,sampType\%in\%c("M","V"))),strD)

out <- cvTripDCF(csObject,"Solea solea") 
}
\keyword{methods}
