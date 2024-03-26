\name{csDataCons-class}
\docType{class}
\alias{csDataCons}
\alias{csDataCons-class}
\alias{csDataCons-methods}
\alias{csDataCons,csDataVal-method}
\alias{csDataCons,csDataVal,strIni-method}
\alias{csDataCons,csDataVal,missing-method}
\alias{csDataCons,missing,missing-method}
\alias{csDataCons,missing-method}
\title{Class "csDataCons"}
\description{The csDataCons-class is similar to \link{csDataVal} but stores consolidated commercial sampling data, i.e. that which is aggregated over spatial, temporal and technical (e.g. gear type) strata.  The expectation is that this post-stratified data will be the basis for the application of statistical methods.}
\section{Objects from the Class}{
The creator function "csDataCons" can be called to create objects from this class.
}
\section{Slots}{
\describe{
    \item{\code{desc}:}{object description}
    \item{\code{tr}:}{trip information}
    \item{\code{hh}:}{haul information}
    \item{\code{sl}:}{species list}
    \item{\code{hl}:}{length frequencies}
    \item{\code{ca}:}{growth and maturity}
  }
\tabular{lrll}{
\bold{slot} \tab \bold{columns} \tab \bold{class} \tab \bold{description} \cr
%\bold{\code{desc}} \tab \tab \code{character} \tab object description \cr
% \tab \tab \tab \cr
\bold{\code{tr}} \tab \tab \code{data.frame} \tab  trip information \cr
 \tab PSUid \tab \code{numeric} \tab primary sampling unit id \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab sampType \tab \code{factor} \tab sampling type \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab proj \tab \code{factor} \tab project \cr
 \tab trpCode \tab \code{factor} \tab trip code \cr
 \tab foNum \tab \code{numeric} \tab number of fishing operations \cr 
 \tab daysAtSea \tab \code{numeric} \tab effort in days at sea \cr 
 \tab vslId \tab \code{numeric} \tab vessel identifier \cr 
 \tab sampCtry \tab \code{factor} \tab sampling country \cr 
 \tab sampMeth \tab \code{factor} \tab sampling method \cr
 \tab \tab \tab \cr
\bold{\code{hh}} \tab \tab \code{data.frame} \tab haul information \cr
 \tab PSUid \tab \code{numeric} \tab primary sampling unit id \cr
 \tab SSUid \tab \code{numeric} \tab secondary sampling unit id \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr 
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab sampType \tab \code{factor} \tab sampling type \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab proj \tab \code{factor} \tab project \cr
 \tab trpCode \tab \code{factor} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab foVal \tab \code{factor} \tab fishing operation validity \cr
 \tab catReg \tab \code{character} \tab parts (landings/discards) of the catch registered \cr
 \tab sppReg \tab \code{character} \tab species of the catch registered \cr
 \tab aggLev \tab \code{factor} \tab aggregation level \cr
 \tab foDur \tab \code{numeric} \tab fishing operation duration \cr
 \tab latIni \tab \code{numeric} \tab latitude of shooting position in decimal degrees \cr
 \tab lonIni \tab \code{numeric} \tab longitude of shooting position in decimal degrees \cr
 \tab latFin \tab \code{numeric} \tab latitude of hauling position in decimal degrees \cr
 \tab lonFin \tab \code{numeric} \tab longitude of hauling position in decimal degrees \cr
 \tab foDep \tab \code{numeric} \tab fishing operation depth \cr
 \tab \tab \tab \cr
\bold{\code{sl}} \tab \tab \code{data.frame} \tab species list \cr
 \tab PSUid \tab \code{numeric} \tab primary sampling unit id \cr
 \tab SSUid \tab \code{numeric} \tab secondary sampling unit id \cr
 \tab TSUid \tab \code{numeric} \tab terciary sampling unit id \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr 
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab sort \tab \code{factor} \tab sorting strata \cr
 \tab sampType \tab \code{factor} \tab sampling type \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab proj \tab \code{factor} \tab project \cr
 \tab trpCode \tab \code{factor} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab spp \tab \code{factor} \tab species \cr 
 \tab valCode \tab \code{factor} \tab validity code \cr 
 \tab wt \tab \code{numeric} \tab weight \cr 
 \tab subSampWt \tab \code{numeric} \tab subsample weight \cr 
 \tab lenCode \tab \code{factor} \tab length code \cr
 \tab \tab \tab \cr
\bold{\code{hl}} \tab \tab \code{data.frame} \tab length frequencies \cr
 \tab PSUid \tab \code{numeric} \tab primary sampling unit id \cr
 \tab SSUid \tab \code{numeric} \tab secondary sampling unit id \cr
 \tab TSUid \tab \code{numeric} \tab terciary sampling unit id \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr 
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab sort \tab \code{factor} \tab sorting strata \cr
 \tab sampType \tab \code{factor} \tab sampling type \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab proj \tab \code{factor} \tab project \cr
 \tab trpCode \tab \code{factor} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab spp \tab \code{factor} \tab species \cr 
 \tab sex \tab \code{factor} \tab gender \cr 
 \tab lenCls \tab \code{numeric} \tab length class \cr
 \tab lenNum \tab \code{numeric} \tab frequency at length \cr
 \tab \tab \tab \cr
\bold{\code{ca}} \tab \tab \code{data.frame} \tab growth and maturity \cr
 \tab PSUid \tab \code{numeric} \tab primary sampling unit id \cr
 \tab SSUid \tab \code{numeric} \tab secondary sampling unit id \cr
 \tab time \tab \code{factor} \tab time strata \cr
 \tab space \tab \code{factor} \tab space strata \cr 
 \tab technical \tab \code{factor} \tab technical strata \cr
 \tab sort \tab \code{factor} \tab sorting strata \cr
 \tab sampType \tab \code{factor} \tab sampling type \cr
 \tab landCtry \tab \code{factor} \tab landing country \cr
 \tab vslFlgCtry \tab \code{factor} \tab vessel flag country \cr
 \tab proj \tab \code{factor} \tab project \cr
 \tab trpCode \tab \code{factor} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab spp \tab \code{factor} \tab species \cr 
 \tab sex \tab \code{factor} \tab gender \cr 
 \tab stock \tab \code{factor} \tab stock \cr
 \tab lenCls \tab \code{numeric} \tab length class \cr
 \tab age \tab \code{numeric} \tab age \cr
 \tab fishId \tab \code{numeric} \tab individual identification \cr
 \tab lenCode \tab \code{factor} \tab length code \cr
 \tab plusGrp \tab \code{factor} \tab plus group \cr
 \tab otoWt \tab \code{numeric} \tab otolith weight \cr
 \tab otoSide \tab \code{factor} \tab otolith side \cr
 \tab indWt \tab \code{numeric} \tab individual weight \cr
 \tab matScale \tab \code{factor} \tab maturity scale \cr
 \tab matStage \tab \code{factor} \tab maturity stage \cr
}
}
\section{Methods}{
  \describe{
    \item{ca}{\code{signature(object = "csDataCons")}: \code{ca} slot accessor.}
    \item{desc}{\code{signature(object = "csDataCons")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "csDataCons")}: dimensions of slots (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "csDataCons")}: first rows of the each slot (see \code{\link{head}}).}
    \item{hh}{\code{signature(object = "csDataCons")}: \code{hh} slot accessor.}
    \item{hl}{\code{signature(object = "csDataCons")}: \code{hl} slot accessor.}
    \item{rbind2}{\code{signature(x = "csDataCons", y = "csDataCons")}: bind two \code{csDataCons} objects by adding rows (see \code{\link{rbind2}}).}
    \item{sl}{\code{signature(object = "csDataCons")}: \code{sl} slot accessor.}
    \item{subset}{\code{signature(x = "csDataCons")}: subset a \code{ceData} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "csDataCons")}: summary a \code{ceData} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "csDataCons")}: last rows of each slot (see \code{\link{tail}}).}
    \item{tr}{\code{signature(object = "csDataCons")}: \code{tr} slot accessor.}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("csDataCons")
}
\keyword{classes}

