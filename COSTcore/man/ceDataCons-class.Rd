\name{ceDataCons-class}
\docType{class}
\alias{ceDataCons}
\alias{ceDataCons-class}
\alias{ceDataCons-methods}
\alias{ceDataCons,ceDataVal-method}
\alias{ceDataCons,ceDataVal,strIni-method}
\alias{ceDataCons,ceDataVal,missing-method}
\alias{ceDataCons,missing,missing-method}
\alias{ceDataCons,missing-method}
\title{Class "ceDataCons"}
\description{The ceDataCons-class is similar to \link{ceDataVal} but stores consolidated commercial effort data, i.e. that which is aggregated over spatial, temporal and technical (e.g. gear type) strata.  The expectation is that this post-stratified data will be the basis for the application of statistical methods.}
\section{Objects from the Class}{
The creator function "ceDataCons" can be called to create objects from this class.
}
\section{Slots}{
\describe{
    \item{\code{desc}:}{object description}
    \item{\code{ce}:}{effort information}
  }
\tabular{lrll}{
\bold{slot} \tab \bold{columns} \tab \bold{class} \tab \bold{description} \cr
%\bold{\code{desc}} \tab \tab \code{character} \tab object description \cr
%\bold{\code{ce}} \tab \tab \code{data.frame} \tab effort information \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr 
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab trpNum \tab \code{numeric} \tab number of trips \cr
 \tab foNum \tab \code{numeric} \tab number of fishing operations \cr
 \tab foDur \tab \code{numeric} \tab duration of fishing operations \cr
 \tab effKwDays \tab \code{numeric} \tab effort in kw days \cr
 \tab effGtDays \tab \code{numeric} \tab effort in T days \cr
 \tab daysAtSea \tab \code{numeric} \tab effort in days at sea \cr	
}
}
\section{Methods}{
  \describe{
    \item{ce}{\code{signature(object = "ceDataCons")}: \code{ce} slot accessor.}
    \item{desc}{\code{signature(object = "ceDataCons")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "ceDataCons")}: dimensions of \code{ce} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "ceDataCons")}: first rows of the \code{ce} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "ceDataCons", y = "ceDataCons")}: bind two \code{ceDataCons} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "ceDataCons")}: subset a \code{ceDataCons} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "ceDataCons")}: summary a \code{ceDataCons} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "ceData")}: last rows of the \code{ce} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("ceDataCons")
}
\keyword{classes}

