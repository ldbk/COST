\name{clData-class}
\docType{class}
\alias{clData}
\alias{clData-class}
\alias{clData-methods}
\alias{clData,data.frame-method}
\alias{clData,matrix-method}
\alias{clData,character-method}
\alias{clData,missing-method}
\title{Class "clData"}
\description{The clData class stores commercial fisheries landings data and maps the format of the CE tables in Fish Frame version 5.}
\section{Objects from the Class}{
The creator function "clData" can be called to create objects from this class.
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
 \tab landCtry \tab \code{character} \tab landing country \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab quarter \tab \code{numeric} \tab quarter \cr 
 \tab month \tab \code{numeric} \tab month \cr
 \tab area \tab \code{character} \tab area \cr
 \tab rect \tab \code{character} \tab statistical rectangle \cr 
 \tab subRect \tab \code{character} \tab statistical subrectangle \cr           %added 03/12/2008 MM
 \tab taxon \tab \code{character} \tab scientific name \cr                      %modified 03/12/2008 MM
 \tab landCat \tab \code{character} \tab landing category \cr 
 \tab commCatScl \tab \code{character} \tab commercial category scale \cr
 \tab commCat \tab \code{character} \tab commercial category \cr
 \tab foCatNat \tab \code{character} \tab fishing operations category, national \cr
 \tab foCatEu5 \tab \code{character} \tab fishing operations category, european level 5 \cr
 \tab foCatEu6 \tab \code{character} \tab fishing operations category, european level 6 \cr
 \tab harbour \tab \code{character} \tab landing harbour \cr                    %added 03/12/2008 MM 
 \tab vslLenCat \tab \code{character} \tab vessel length categories \cr         %added 03/12/2008 MM
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
    \item{clDataVal}{\code{signature(object = "clData")}: creator of Validated data class.}
    \item{desc}{\code{signature(object = "clData")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "clData")}: dimensions of \code{cl} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "clData")}: first rows of the \code{cl} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "clData", y = "clData")}: bind two \code{clData} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "clData")}: subset a \code{clData} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "clData")}: summary a \code{clData} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "clData")}: last rows of the \code{cl} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("clData")
}
\keyword{classes}

