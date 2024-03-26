\name{ceData-class}
\docType{class}
\alias{ceData}
\alias{ceData-class}
\alias{ceData-methods}
\alias{ceData,data.frame-method}
\alias{ceData,matrix-method}
\alias{ceData,character-method}
\alias{ceData,missing-method}
\title{Class "ceData"}
\description{The ceData class stores commercial fisheries effort data and maps the format of the CE tables in Fish Frame version 5.}
\section{Objects from the Class}{
The creator function "ceData" can be called to create objects from this class.
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
    \item{ce}{\code{signature(object = "ceData")}: \code{ce} slot accessor.}
    \item{ceDataVal}{\code{signature(object = "ceData")}: creator of Validated data class.}
    \item{desc}{\code{signature(object = "ceData")}: \code{desc} slot accessor.}
    \item{dim}{\code{signature(x = "ceData")}: dimensions of \code{ce} slot (see \code{\link{dim}}).}
    \item{head}{\code{signature(x = "ceData")}: first rows of the \code{ce} slot (see \code{\link{head}}).}
    \item{rbind2}{\code{signature(x = "ceData", y = "ceData")}: bind two \code{ceData} objects by adding rows (see \code{\link{rbind2}}).}
    \item{subset}{\code{signature(x = "ceData")}: subset a \code{ceData} object (see \code{\link{subset}}).}
    \item{summary}{\code{signature(object = "ceData")}: summary a \code{ceData} object (see \code{\link{summary}}).}
    \item{tail}{\code{signature(x = "ceData")}: last rows of the \code{ce} slot (see \code{\link{tail}}).}
	 }
}
\author{Ernesto Jardim <ernesto@ipimar.pt>}
\examples{
showClass("ceData")
}
\keyword{classes}

