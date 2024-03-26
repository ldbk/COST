\name{totVolume}
\alias{totVolume}
\alias{totVolume,dbeOutput,csDataCons,ceDataCons,missing-method}
\alias{totVolume,dbeOutput,csDataCons,ceDataCons,clDataCons-method}
\docType{methods}
\title{Estimation of total volume of discards or/and landings (weight, number & number-at-length)}
\description{
Generic function to estimate total volume of discards or/and landings (weight, number & number-at-length) based on various raising methods.
}

\usage{
totVolume(dbeOutput,csObject,ceObject,clObject,\dots)
}

\arguments{
  \item{dbeOutput}{A \emph{dbeOutput} object. All necessary information for calculation process are taken in the first slots (species, catch category,...). See \emph{dbeObject} method for object initialization.}
  \item{csObject}{A \emph{csDataCons} object matching 'dbeOutput' specifications.}
  \item{ceObject}{A \emph{ceDataCons} object matching 'dbeOutput' specifications.}
  \item{clObject}{An optionnal \emph{clDataCons} object matching 'dbeOutput' specifications. If specified, raising is made with ratio-to-landings method.}
  \item{...}{Further arguments such as:\\
\tabular{ll}{
  \bold{type} \tab Specification of the raising method : \code{"trip"} (default value) for raising by trip, \code{"fo"} for raising by fishing operations, 
\code{"fd"} for raising by fishing days,\code{"landings"} for ratio-to-total landings raising method, and \code{"time"} for ratio-to-fishing duration 
raising method. \cr
  \bold{landSpp} \tab Character vector describing the species considered in the 'volume of landings' variable if chosen raising method is ratio-to-landings (see 'clObject' description).\cr 
%  \bold{val} \tab Estimated parameter. To be chosen between \code{"weight"} (default value), \code{"number"} and \code{"nAtLength"}.\cr
  \bold{sampPar} \tab Logical specifying if given species is considered to be automatically sampled during the sampling process (default value is \code{TRUE}).\cr 
  \bold{incl.precision} \tab Logical. If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs.\cr
  \bold{probs} \tab Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}.\cr

}
}
}


\value{An updated object of class \emph{dbeOutput}.}

\references{Vigneau, J. (2006)          
\emph{Raising procedures for discards : Sampling theory (Toward agreed methodologies for calculating precision in the discard programmes)}. Working document in support of PGCCDBS (Rostock, 2006).
}

\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbeObject}}, \code{\link{dbeCalc}}, \code{\link[COSTcore]{csDataCons}}, \code{\link[COSTcore]{ceDataCons}}, \code{\link[COSTcore]{clDataCons}}
}

\examples{

data(sole)

#consolidated datasets are built 
strDef <- strIni(timeStrata="quarter",techStrata="foCatEu5")
csObject <- csDataCons(csDataVal(sole.cs),strDef)
clObject <- clDataCons(clDataVal(sole.cl),strDef)
ceObject <- ceDataCons(ceDataVal(sole.ce),strDef)

#dbeOutput initial object
obj <- dbeObject(desc="My object",species="Solea solea",catchCat="DIS",strataDesc=strDef,
                 methodDesc="analytical")

##raising by trip
newObj <- totVolume(obj,csObject,ceObject)
newObj

}
\keyword{methods}
