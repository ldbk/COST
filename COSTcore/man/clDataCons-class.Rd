\name{clDataCons-class}
\docType{class}
\alias{clDataCons}
\alias{clDataCons-class}
\alias{clDataCons-methods}
\alias{clDataCons,clDataVal-method}
\alias{clDataCons,clDataVal,strIni-method}
\alias{clDataCons,clDataVal,missing-method}
\alias{clDataCons,missing,missing-method}
\alias{clDataCons,missing-method}
\title{Class "clDataCons"}
\description{The clDataCons-class is similar to \link{clDataVal} but stores consolidated commercial landings data, i.e. that which is aggregated over spatial, temporal and technical (e.g. gear type) strata.  The expectation is that this post-stratified data will be the basis for the application of statistical methods.}
\section{Objects from the Class}{
The creator function "clDataCons" can be called to create objects from this class.
}
\section{Slots}{
\describe{
    \item{\code{desc}:}{object description}
    \item{\code{cl}:}{landings information}
  }
\tabular{lrll}{
\bold{slot} \tab \bold{columns} \tab \bold{class} \tab \bold{description} \cr
%\bold{\code{desc}} \tab \tab \code{character} \tab object description \cr
%\bold{\code{cl}} \tab \tab \code{data.frame} \tab landings information \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab time \tab \code{numeric} \tab time strata \cr
 \tab space \tab \code{numeric} \tab space strata \cr 
 \tab technocal \tab \code{numeric} \tab technical strata \cr
 \tab spp \tab \code{factor} \tab species \cr 
 \tab landCat \tab \code{factor} \tab landing category \cr 
 \tab commCatScl \tab \code{factor} \tab commercial category scale \cr
 \tab commCat \tab \code{factor} \tab commercial category \cr
 \tab unallocCatchWt \tab \code{numeric} \tab unallocated catch weight \cr
 \tab misRepCatchWt \tab \code{numeric} \tab area misreported catch weight \cr
 \tab landWt \tab \code{numeric} \tab official landings weight \cr
 \tab landMult \tab \code{numeric} \tab landings multiplier \cr
 \tab landValue \tab \code{numeric} \tab official landings value \cr
}
}
\section{Methods}{
  \describe{
    \item{cl}{\code{signature(object = "clData")}: \code{cl} slot accessor.}
    \item{desc}{\code{signature(object = "clDataCons")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "clDataCons")}: dimensions of \code{cl} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "clDataCons")}: first rows of the \code{cl} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "clDataCons", y = "clDataCons")}: bind two \code{clDataCons} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "clDataCons")}: subset a \code{clDataCons} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "clDataCons")}: summary a \code{clDataCons} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "clDataCons")}: last rows of the \code{cl} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("clDataCons")
}
\keyword{classes}

