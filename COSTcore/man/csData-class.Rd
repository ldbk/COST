\name{csData-class}
\docType{class}
\alias{csData}
\alias{csData-class}
\alias{csData-methods}
\alias{csData,missing,missing,missing,missing,missing-method}
\alias{csData,data.frame,missing,missing,missing,missing-method}
\alias{csData,data.frame,data.frame,missing,missing,missing-method}
\alias{csData,data.frame,data.frame,data.frame,missing,missing-method}
\alias{csData,data.frame,data.frame,data.frame,data.frame,missing-method}
\alias{csData,data.frame,data.frame,data.frame,data.frame,data.frame-method}
\alias{csData,data.frame,missing,missing,missing,data.frame-method}
\alias{csData,character,character,character,character,missing-method}
\alias{csData,character,character,character,character,character-method}
\alias{csData,character,character,character,missing,missing-method}
\alias{csData,character,character,missing,missing,missing-method}
\alias{csData,character,missing,missing,missing,missing-method}
\alias{csData,character,missing,missing,missing,character-method}

\title{Class "csData"}
\description{The csData class stores commercial fisheries sampling data and maps the CS tables in Fish Frame version 5.}
\section{Objects from the Class}{
The creator function "csData" can be called to create objects from this class.
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
 \tab sampType \tab \code{character} \tab sampling type \cr
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab proj \tab \code{character} \tab project \cr
 \tab trpCode \tab \code{character} \tab trip code \cr
 \tab vslLen \tab \code{numeric} \tab vessel length-over-all \cr 
 \tab vslPwr \tab \code{numeric} \tab vessel power (kw)\cr 
 \tab vslSize \tab \code{numeric} \tab vessel size (GRT)\cr 
 \tab vslType \tab \code{character} \tab vessel type \cr 
 \tab harbour \tab \code{character} \tab landing harbour \cr                    %added 03/12/2008 MM 
 \tab foNum \tab \code{numeric} \tab number of fishing operations \cr 
 \tab daysAtSea \tab \code{numeric} \tab effort in days at sea \cr 
 \tab vslId \tab \code{numeric} \tab vessel identifier \cr 
 \tab sampCtry \tab \code{character} \tab sampling country \cr 
 \tab sampMeth \tab \code{character} \tab sampling method \cr
 \tab \tab \tab \cr
\bold{\code{hh}} \tab \tab \code{data.frame} \tab haul information \cr
 \tab sampType \tab \code{character} \tab sampling type \cr
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab proj \tab \code{character} \tab project \cr
 \tab trpCode \tab \code{character} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab foVal \tab \code{character} \tab fishing operation validity \cr
 \tab aggLev \tab \code{character} \tab aggregation level \cr
 \tab catReg \tab \code{character} \tab parts (landings/discards) of the catch registered \cr
 \tab sppReg \tab \code{character} \tab species of the catch registered \cr
 \tab date \tab \code{character} \tab date \cr
 \tab time \tab \code{character} \tab time \cr
 \tab foDur \tab \code{numeric} \tab fishing operation duration \cr
 \tab latIni \tab \code{numeric} \tab latitude of shooting position in decimal degrees \cr
 \tab lonIni \tab \code{numeric} \tab longitude of shooting position in decimal degrees \cr
 \tab latFin \tab \code{numeric} \tab latitude of hauling position in decimal degrees \cr
 \tab lonFin \tab \code{numeric} \tab longitude of hauling position in decimal degrees \cr
 \tab area \tab \code{character} \tab area \cr
 \tab rect \tab \code{character} \tab statistical rectangle \cr
 \tab subRect \tab \code{character} \tab statistical subrectangle \cr           %added 03/12/2008 MM
 \tab foDep \tab \code{numeric} \tab fishing operation depth \cr
 \tab waterDep \tab \code{numeric} \tab water depth \cr
 \tab foCatNat \tab \code{character} \tab fishing operations category, national \cr
 \tab foCatEu5 \tab \code{character} \tab fishing operations category, european level 5 \cr
 \tab foCatEu6 \tab \code{character} \tab fishing operations category, european level 6 \cr
% \tab gear \tab \code{character} \tab gear \cr                                 %modified 03/12/2008 MM
 \tab meshSize \tab \code{numeric} \tab mesh size \cr
 \tab selDev \tab \code{character} \tab selectivity device \cr
 \tab meshSizeSelDev \tab \code{numeric} \tab mesh size in selectivity device \cr
 \tab \tab \tab \cr
\bold{\code{sl}} \tab \tab \code{data.frame} \tab species list \cr
 \tab sampType \tab \code{character} \tab sampling type \cr
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab proj \tab \code{character} \tab project \cr
 \tab trpCode \tab \code{character} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab spp \tab \code{character} \tab species \cr 
% \tab sex \tab \code{character} \tab gender \cr                                 %added 03/12/2008 MM
 \tab catchCat \tab \code{character} \tab catch category \cr 
 \tab landCat \tab \code{character} \tab landing category \cr 
 \tab commCatScl \tab \code{character} \tab commercial category scale \cr
 \tab commCat \tab \code{character} \tab commercial category \cr
 \tab subSampCat \tab \code{character} \tab subsampling category \cr
 \tab sex \tab \code{character} \tab gender \cr                                 %added 03/12/2008 MM
% \tab valCode \tab \code{character} \tab validity code \cr 
 \tab wt \tab \code{numeric} \tab weight \cr 
 \tab subSampWt \tab \code{numeric} \tab subsample weight \cr 
 \tab lenCode \tab \code{character} \tab length code \cr
 \tab \tab \tab \cr
\bold{\code{hl}} \tab \tab \code{data.frame} \tab length frequencies \cr
 \tab sampType \tab \code{character} \tab sampling type \cr
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab proj \tab \code{character} \tab project \cr
 \tab trpCode \tab \code{character} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab spp \tab \code{character} \tab species \cr 
% \tab sex \tab \code{character} \tab gender \cr 
 \tab catchCat \tab \code{character} \tab catch category \cr 
 \tab landCat \tab \code{character} \tab landing category \cr 
 \tab commCatScl \tab \code{character} \tab commercial category scale \cr
 \tab commCat \tab \code{character} \tab commercial category \cr
 \tab subSampCat \tab \code{character} \tab subsampling category \cr
 \tab sex \tab \code{character} \tab gender \cr                                 %added 03/12/2008 MM
 \tab lenCls \tab \code{numeric} \tab length class \cr
 \tab lenNum \tab \code{numeric} \tab frequency at length \cr
 \tab \tab \tab \cr
\bold{\code{ca}} \tab \tab \code{data.frame} \tab growth and maturity \cr
 \tab sampType \tab \code{character} \tab sampling type \cr
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab proj \tab \code{character} \tab project \cr
 \tab trpCode \tab \code{character} \tab trip code \cr
 \tab staNum \tab \code{numeric} \tab station number \cr
 \tab quarter \tab \code{numeric} \tab quarter \cr
 \tab month \tab \code{numeric} \tab month \cr
 \tab spp \tab \code{character} \tab species \cr 
 \tab sex \tab \code{character} \tab gender \cr 
 \tab catchCat \tab \code{character} \tab catch category \cr 
 \tab landCat \tab \code{character} \tab landing category \cr 
 \tab commCatScl \tab \code{character} \tab commercial category scale \cr
 \tab commCat \tab \code{character} \tab commercial category \cr
 \tab stock \tab \code{character} \tab stock \cr
 \tab area \tab \code{character} \tab area \cr 
 \tab rect \tab \code{character} \tab statistical rectangle \cr
 \tab subRect \tab \code{character} \tab statistical subrectangle \cr                         %added 03/12/2008 MM
 \tab lenCls \tab \code{numeric} \tab length class \cr
 \tab age \tab \code{numeric} \tab age \cr
 \tab fishId \tab \code{numeric} \tab individual identification \cr
 \tab lenCode \tab \code{character} \tab length code \cr
 \tab ageMeth \tab \code{character} \tab methodology for estimating the age \cr               %added 03/12/2008 MM
 \tab plusGrp \tab \code{character} \tab plus group \cr
 \tab otoWt \tab \code{numeric} \tab otolith weight \cr
 \tab otoSide \tab \code{character} \tab otolith side \cr
 \tab indWt \tab \code{numeric} \tab individual weight \cr
 \tab matMeth \tab \code{character} \tab methodology for estimating the maturity stage \cr    %added 03/12/2008 MM
 \tab matScale \tab \code{character} \tab maturity scale \cr
 \tab matStage \tab \code{character} \tab maturity stage \cr
}
}
\section{Methods}{
  \describe{
    \item{ca}{\code{signature(object = "csData")}: \code{ca} slot accessor.}
    \item{csDataVal}{\code{signature(object = "csData")}: creator of Validated data class.}
    \item{desc}{\code{signature(object = "csData")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "csData")}: dimensions of slots (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "csData")}: first rows of the each slot (see \code{\link{head}}).}
    \item{hh}{\code{signature(object = "csData")}: \code{hh} slot accessor.}
    \item{hl}{\code{signature(object = "csData")}: \code{hl} slot accessor.}
    \item{rbind2}{\code{signature(x = "csData", y = "csData")}: bind two \code{csData} objects by adding rows (see \code{\link{rbind2}}).}
    \item{sl}{\code{signature(object = "csData")}: \code{sl} slot accessor.}
    \item{subset}{\code{signature(x = "csData")}: subset a \code{ceData} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "csData")}: summary a \code{ceData} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "csData")}: last rows of each slot (see \code{\link{tail}}).}
    \item{tr}{\code{signature(object = "csData")}: \code{tr} slot accessor.}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("csData")
}
\keyword{classes}
