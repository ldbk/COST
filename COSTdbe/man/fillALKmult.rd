\name{fillALKmult}
\alias{fillALKmult}
\alias{fillALKmult,csDataCons-method}
\docType{methods}
\title{Method for filling empty length classes in stratified ALK}
\description{This method allows to fill empty length classes in ALKs from CA table that are recorded in HL table. A multinomial model adjusted from CA information is used to fill the gaps.     
}

\usage{           
fillALKmult(object,spp,p=10,trace=TRUE,\dots)
}


\arguments{
  \item{object}{A \emph{csDataCons} object.}
  \item{spp}{Character. The species for which filling process is wished (only one species).}
  \item{p}{Number of (virtual) individuals to be added for each empty length class.}
  \item{trace}{Logical. Switch for tracing optimization.}
  \item{...}{Further arguments}
} 
  

\value{An updated \emph{csDataCons} object. CA table is filled out with new individuals according to multinomial projection and \emph{p} parameter. These new individuals are indexed with a negative \emph{fishId}. }


\author{Mathieu Merzereaud}
\seealso{\code{\link{alkLgthRec}}, \code{\link[COSTcore]{csDataCons}}}

\examples{
data(sole)
#stratification
strD <- strIni(timeStrata="quarter")
csObject <- csDataCons(csDataVal(sole.cs),strD)
newObject <- fillALKmult(csObject,"Solea solea",p=15)
newObject@ca[newObject@ca$fishId<0,]
}

\keyword{methods}
