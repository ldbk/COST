\name{dbeCalc}
\alias{dbeCalc}
\alias{dbeCalc,dbeOutput-method}
\docType{methods}                       
\title{CI and CV calculation} 
\description{
Method for calculating coefficients of variation or confidence intervals from 'dbeOutput' object estimates. Input object can optionally be updated with resulting table.
}

\usage{
dbeCalc(object,type="CI",vrbl="l",probs=c(0.025,0.975),replicates=FALSE,update=TRUE,\dots)
}

\arguments{
  \item{object}{A \emph{dbeOutput} object.}
  \item{type}{Character. "CI" for confidence interval calculation, or "CV" for coefficients of variation.}
  \item{vrbl}{Character specifying 'dbeOutput' estimates on which calculation is applied : "l" for length structure, "a" for age structure, "n" for total number estimates, "w" for total weight estimates.}
  \item{probs}{Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{type="CI"}). See \emph{quantile}.}
  \item{replicates}{Logical. If \code{TRUE}, calculation is made from @...\$rep elements ; if \code{FALSE}, @...\$estim and @...Var 'dbeOutput' data are used.}
  \item{update}{Logical. If \code{TRUE}, updated 'dbeOutput' object is returned ; if \code{FALSE}, only resulting dataframe is returned}
  \item{...}{Further arguments used as '\emph{quantile} method input parameter (if \code{type="CI"} and besides \code{probs} parameter).}
}

\details{
If calculation is made from replicates (see \emph{replicates} parameter), confidence interval is estimated using \emph{quantile} fonction with \emph{probs} and \dots parameters.
If calculation is made from estimates, normal distribution of total estimates is assumed to compute confidence intervals. 
Possible resulting negative bounds are automatically replaced by 0 in output object. 
}


\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbePlot}}, \code{\link{quantile}}
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
dbeOutput <- RaiseLgth (dbeOutput, csObject, clObject,incl.precision=FALSE)
#object is updated with cv-at-length 
dbeOutput <- dbeCalc(dbeOutput,type="CV")

}
\keyword{methods}
