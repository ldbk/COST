\name{stratAggreg}
\alias{stratAggreg}
\alias{stratAggreg,dbeOutput-method}
\docType{methods}
\title{Aggregation of 'dbeOutput' table}
\description{
This method is aggregating (summing in case of number or weight estimates, or averaging as to mean parameters) input 'dbeOutput' object tables over chosen stratification(s). 
}

\usage{
stratAggreg(object,timeStrata=TRUE,spaceStrata=TRUE,techStrata=FALSE,wt="totalW",incl.precision=TRUE,probs=c(0.025,0.975),\dots)
}

\arguments{
  \item{object}{A \emph{dbeOutput} object.}
  \item{timeStrata}{Logical. If \code{TRUE}, aggregation is made over time strata.}
  \item{spaceStrata}{Logical. If \code{TRUE}, aggregation is made over space strata.}
  \item{techStrata}{Logical. If \code{TRUE}, aggregation is made over technical strata.}
  \item{wt}{Character. Input \emph{object} slot with which a weighted mean over strata is computed (only if \emph{param} slot is "weight", "maturity" or "sex"). 
            Possible values are "totalW" or "totalN". Check that specified slot is not empty, otherwise resulting table of estimates will also be empty.}
  \item{incl.precision}{Logical. If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs.}
  \item{probs}{Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}.}
  \item{\dots}{Further arguments}  
}


\value{An updated object of class \emph{dbeOutput}.}
\details{
Aggregating process is only applied to these tables : \code{nSamp}, \code{nMeas}, \code{lenVar}, \code{ageVar}, \code{totalNvar}, \code{totalWvar}, and all \code{estim} and \code{rep} 
list elements.
Other tables in output object are empty (so, for example, 'dbeCalc' method must be called to calculate and insert \emph{cv} table in this new object). 
WARNING : summing variances requires some strong probability assumptions within strata, such as uncorrelated variables. In the 'variance at age' case, this can be problematic 
since the same age-length key is used for every technical strata.   
}

\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbeObject}}
}


\keyword{methods}
