\name{bpBoot}
\alias{bpBoot}
\alias{bpBoot,dbeOutput,csDataCons-method}
\docType{methods}
\title{Bootstrap estimates of biological parameters}
\description{
 This method implements a non-parametric bootstrap procedure to estimate empirical estimates of weight-at-length, maturity-at-length, 
 sex-ratio-at-length, weight-at-age, maturity-at-age, sex-ratio-at-age, length-at-age and their associated variances. It requires a csDataCons object and a dbeOutput object.}

\usage{
bpBoot(dbeOutput,object,mat.scale=list(immature=c(0,1),mature=c(2:8)),
       sample.boot=FALSE,nboot=1000,\dots)          % modif 2:10 <-> 2:8
}

\arguments{
  \item{dbeOutput}{   A \emph{dbeOutput} object.}
  \item{object}{      A \emph{csDataCons} object.}
  \item{mat.scale}{   List containing the maturity stages for immature and mature individuals. The codes must match with maturity scale of \emph{csDataCons} object.} 
  \item{sample.boot}{ Logical value. If FALSE, the resampling unit is the individual. If TRUE, the resampling unit is the sample.}
  \item{nboot}{       Single positive integer indicating the number of bootstrap replicates.}
  \item{\dots}{       Any additional arguments.}
}

\details{
This method uses the non-parametric bootstrap techniques to estimate the 
variation in biological parameters (mean weight, maturity and sex-ratio)
based on the original data provided at the slot ca of a consolidated COSTobject.

It is assumed that the biological sampling is representative of the catches. 
If the number of samples by defined strata is lower than ten, variance estimates should not be considered reliable. 

The results of this method are included as components of the object of class "dbeObject". 
}

\references{B. Efron and R. Tibshirani (1993). \emph{An Introduction to the Bootstrap}. Chapman & Hall.}

\author{Paz Sampedro}
\seealso{\code{\link{bpEstim},\link{dbeOutput},\link[COSTcore]{csDataCons},\link{dbePlot}}}

\examples{
## Mean weight-at-length/age and variance for IFREMER sole data
## Estimates are made by quarter and area 
#
#data (sole)
#sole.str <- strIni(timeStrata="quarter",spaceStrata="area")
#sole.cons <- csDataCons(csDataVal(sole.cs),sole.str)
#sole.dbeOutput <- dbeObject(desc="Results of design based estimates",species="Solea solea",
#                            param="weight",strataDesc=sole.str,methodDesc="Bootstrap")
#sole.Weight <- bpBoot(sole.dbeOutput,sole.cons, sample.boot=TRUE)         
}                                                             % modif T <-> TRUE

\keyword{design}%bootstrap}     %modif MM

