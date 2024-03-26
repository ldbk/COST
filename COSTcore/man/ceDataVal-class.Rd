\name{ceDataVal-class}
\docType{class}
\alias{ceDataVal}
\alias{ceDataVal-class}
\alias{ceDataVal-methods}
\alias{ceDataVal,ceData-method}
\alias{ceDataVal,missing-method}
\title{Class "ceDataVal"}
\description{The ceDataVal-class is equivalent in structure to \link{ceData} but stores a validated form of the commercial effort data contained in the \link{ceData}class. The validation process consists of checking the compatibility of entries of variables of type character against the permissible code lists, the checking of numeric variable types, the checking of numeric variable ranges, and the identification of outliers. Data of "ceDataVal" is considered suitable for analysis and is additionally a prerequisite to aggregation over strata to form data of class \emph{ceDataCons}.}
\section{Objects from the Class}{
The creator function "ceDataVal" can be called to create objects from this class.
}
\section{Slots}{
\tabular{lrll}{
\bold{slot} \tab \bold{columns} \tab \bold{class} \tab \bold{description} \cr
\bold{\code{desc}} \tab \tab \code{character} \tab object description \cr
\bold{\code{ce}} \tab \tab \code{data.frame} \tab effort information \cr
 \tab vslFlgCtry \tab \code{character} \tab vessel flag country \cr
 \tab year \tab \code{numeric} \tab year \cr
 \tab quarter \tab \code{numeric} \tab quarter \cr 
 \tab month \tab \code{numeric} \tab month \cr
 \tab area \tab \code{character} \tab area \cr
 \tab rect \tab \code{character} \tab statistical rectangle \cr 
 \tab subRect \tab \code{character} \tab statistical subrectangle \cr           %added 03/12/2008 MM
 \tab foCatNat \tab \code{character} \tab fishing operations category, national \cr
 \tab foCatEu5 \tab \code{character} \tab fishing operations category, european level 5 \cr
 \tab foCatEu6 \tab \code{character} \tab fishing operations category, european level 6 \cr
 \tab harbour \tab \code{character} \tab landing harbour \cr                    %added 03/12/2008 MM 
 \tab vslLenCat \tab \code{character} \tab vessel length categories \cr         %added 03/12/2008 MM
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
    \item{ce}{\code{signature(object = "ceDataVal")}: \code{ce} slot accessor.}
    \item{ceDataCons}{\code{signature(object = "ceDataVal")}: creator of Consolidated data class.}
    \item{desc}{\code{signature(object = "ceDataVal")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "ceDataVal")}: dimensions of \code{ce} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "ceDataVal")}: first rows of the \code{ce} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "ceDataVal", y = "ceDataVal")}: bind two \code{ceDataVal} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "ceDataVal")}: subset a \code{ceDataVal} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "ceDataVal")}: summary a \code{ceDataVal} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "ceDataVal")}: last rows of the \code{ce} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("ceDataVal")
}
\keyword{classes}

