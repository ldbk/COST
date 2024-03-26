\name{clDataVal-class}
\docType{class}
\alias{clDataVal}
\alias{clDataVal-class}
\alias{clDataVal-methods}
\alias{clDataVal,clData-method}
\alias{clDataVal,missing-method}
\title{Class "clDataVal"}
\description{TThe clDataVal-class is equivalent in structure to \link{clData} but stores a validated form of the commercial landings data contained in the \link{clData}class. The validation process consists of checking the compatibility of entries of variables of type character against the permissible code lists, the checking of numeric variable types, the checking of numeric variable ranges, and the identification of outliers. Data of "clDataVal" is considered suitable for analysis and is additionally a prerequisite to aggregation over strata to form data of class "clDataCons".}
\section{Objects from the Class}{
The creator function "clDataVal" can be called to create objects from this class.
}
\section{Slots}{
\tabular{lrll}{
\bold{slot} \tab \bold{columns} \tab \bold{class} \tab \bold{description} \cr
\bold{\code{desc}} \tab \tab \code{character} \tab object description \cr
\bold{\code{cl}} \tab \tab \code{data.frame} \tab landings information \cr
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
    \item{clDataCons}{\code{signature(object = "clDataVal")}: creator of Consolidated data class.}
    \item{desc}{\code{signature(object = "clDataVal")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "clDataVal")}: dimensions of \code{cl} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "clDataVal")}: first rows of the \code{cl} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "clDataVal", y = "clDataVal")}: bind two \code{clDataVal} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "clDataVal")}: subset a \code{clDataVal} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "clDataVal")}: summary a \code{clDataVal} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "clDataVal")}: last rows of the \code{cl} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("clDataVal")
}
\keyword{classes}

