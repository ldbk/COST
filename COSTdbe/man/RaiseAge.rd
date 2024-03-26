\name{RaiseAge}
\alias{RaiseAge}
\alias{RaiseAge,dbeOutput,csDataCons,clDataCons-method}
\alias{RaiseAge,dbeOutput,csDataCons,missing-method}
\docType{methods}
\title{Estimation of total numbers-at-age from market sampling}
\description{Estimation of total numbers-at-age from market sampling    
}

\usage{           
RaiseAge(dbeOutput,csObject,clObject,\dots)
}


\arguments{
  \item{dbeOutput}{A \emph{dbeOutput} object.}
  \item{csObject}{A \emph{csDataCons} object matching 'dbeOutput' specifications.}
  \item{clObject}{Optionnal. A \emph{clDataCons} object matching 'dbeOutput' specifications (only required if type=\code{"direct"}).}
  \item{...}{Further arguments such as:\\
\tabular{ll}{ 
    \bold{type} \tab Character. Specification of the raising method : \code{"p"} (default value), \code{"fixedK"}, \code{"propK"}, \code{"agesK"}, or \code{"direct"}.\cr
    \bold{sex} \tab Character. Sex specification, basically \code{"M"}, \code{"F"} or \code{as.character(NA)} (default value) for no restriction. \cr
    \bold{incl.precision} \tab Logical (default value = TRUE). If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs. \cr
    \bold{probs} \tab Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}. \cr

}
}} 

\references{D.K. Kimura, Statistical assessment of the age-length key, J. Fish. Res. Board Can. 34 (1977)}

\details{Method used for estimates and variance calculation of total numbers at age is specified by \emph{type} input parameter. 
If \code{"direct"}, calculations are based on a stratified simple random sampling (see \emph{Type1SimpleRandomSampling.doc} file in COSTdbe package's doc folder for methodology), 
and raising process is made within \emph{clObject} input data.
If \code{"p"}, \code{"fixedK"}, \code{"propK"} or \code{"agesK"}, estimates are similarly computed following Kimura (see \emph{references} section).
In these cases, as calculation requires total numbers-at-length estimates, \emph{RaiseLgth} method must be run beforehand to update input \emph{dbeOutput} object with needed information.
Contrary to estimates, variances computation differs between these 4 methods. \code{"fixedK"} is computing within an "age subsample fixed" strategy expectation, 
\code{"propK"} within an "age subsample random" strategy, \code{"agesK"} within a "random sample with ages only" strategy (see Kimura for those methodologies), 
and finally, \code{"p"} default method is based on "random sample with ages and lengths" strategy.}


\value{An updated object of class \emph{dbeOutput}.
Slots nSamp\$age & nMeas\$age with number of samples and measurements,
ageStruc\$estim with numbers-at-age estimates,
ageVar with the variance of numbers-at-age .}


\author{Mathieu Merzereaud & Marcel Machiels}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbeObject}}, \code{\link{RaiseAgeBoot}}, \code{\link{RaiseLgth}}, \code{\link{dbeCalc}}, \code{\link[COSTcore]{csDataCons}}, \code{\link[COSTcore]{clDataCons}}}

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

# total numbers at age
dbeOutput <- RaiseAge (dbeOutput, csObject)


}
\keyword{methods}
