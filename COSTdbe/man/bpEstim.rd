\name{bpEstim}
\alias{bpEstim}
\alias{bpEstim,dbeOutput,csDataCons,missing-method}
\alias{bpEstim,dbeOutput,csDataCons,dbeOutput-method}
\docType{methods}
\title{Analytical estimates of biological parameters}
\description{
This method implements analytical estimates of empirical weight-at-length/age (dbeOutput$param="weight"), maturity-at-length/age (dbeOutput$param="maturity"), 
sex-ratio-at-length/age (dbeOutput$param="sex"), individual length-at-age (dbeOutput$param="length"), and variances. 
The needed parameters are from input 'dbeOutput' slots. Estimates-at-age can be calculated by injecting the length distribution resultin from 3 different sampling data :
\emph{ca} table or \emph{hl} table from input consolidated object, or total number-at-length from an input \emph{dbeOuput} object. 
In the latter case, \emph{totalN} and \emph{totalW} slots are also inserted in output \emph{dbeOutput} object.
}                                                                                                                                           

\usage{
bpEstim(dbeOutput,object,dbeLD,adjust=TRUE,immature.scale=1,incl.precision=TRUE,probs=c(0.025,0.975),\dots)
}
                                                        
\arguments{
  \item{dbeOutput}{A \emph{dbeOutput} object.}
  \item{object}{A \emph{csDataCons} object.}
  \item{dbeLD}{Optionnal. A \emph{dbeOutput} object with total numbers-at-length data (possibly resulting from \emph{RaiseLgth} method). 
  If missing, injected length distribution for estimates-at-age calculation is created within input consolidated object. In that case, 
  the source table is specified with \emph{adjust} parameter.}
  \item{adjust}{Logical. Only useful if \emph{dbeLD} parameter is missing. If FALSE, length distribution in \emph{object}'s CA table is supposed to be representative of the catch 
  (all calculations are made within CA). If TRUE (default value), estimates-at-age are calculated by injecting \emph{object}'s HL information.}
  \item{immature.scale}{Numeric or character. Specifies the value(s) in \emph{matStage} field (from ca table in \emph{object}) for which the individuals are defined as immature.}
  \item{incl.precision}{Logical. If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs.}
  \item{probs}{Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}.}
  \item{...}{Further arguments.}
}
                                     

\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput},\link{dbeCalc},\link[COSTcore]{csDataCons}}
}

\examples{
data(sole)
#stratification object
strDef <- strIni(timeStrata="quarter",spaceStrata="area")
#consolidated object
object <- csDataCons(csDataVal(sole.cs),strDef)
#dbeOutput initial object with needed parameters
dbeOutput <- dbeObject(desc="My object",species="Solea solea",param="weight",
                       strataDesc=strDef,methodDesc="analytical")

lWeight <- bpEstim(dbeOutput,object)

}
\keyword{methods}
