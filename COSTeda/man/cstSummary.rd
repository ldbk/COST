\name{cstSummary-methods}
\docType{methods}
\alias{cstSummary}
\alias{cstSummary,csData-method}
\alias{cstSummary,clData-method}
\alias{cstSummary,ceData-method}
\alias{cstSummary,csDataVal-method}
\alias{cstSummary,clDataVal-method}
\alias{cstSummary,ceDataVal-method}
\alias{cstSummary,csDataCons-method}
\alias{cstSummary,clDataCons-method}
\alias{cstSummary,ceDataCons-method}
\alias{cstSummary-methods}
\title{Summary "plus" procedure for "COST" objects.}
\description{
These methods implements a special \emph{summary} procedure for objects of class \emph{csData/csDataVal/csDataCons}, \emph{clData/clDataVal/clDataCons} and \emph{ceData/ceDataVal/ceDataCons}.
}

\usage{
\S4method{cstSummary}{csData}(object,tab="tr",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{csDataVal}(object,tab="tr",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{csDataCons}(object,tab="tr",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{clData}(object,tab="missing",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{clDataVal}(object,tab="missing",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{clDataCons}(object,tab="missing",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{ceData}(object,tab="missing",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{ceDataVal}(object,tab="missing",sizeMax=20,except=NULL,\dots)
\S4method{cstSummary}{ceDataCons}(object,tab="missing",sizeMax=20,except=NULL,\dots)
}

\arguments{
  \item{object}{A \emph{COST} object.}
  \item{tab}{Character specifying one slot of a COST \emph{cs} object (ie "tr", "hh", "sl", "hl" or "ca").}
  \item{sizeMax}{Numeric value specifying the number of rows displayed.}
  \item{except}{Character specifying fields to omit.}
  \item{...}{Further parameters.}
}

\section{Methods}{
\describe{
	\item{cstSummary}{\code{signature(csData)}: summary for a \emph{csData} object.}
	\item{cstSummary}{\code{signature(csDataVal)}: summary for a \emph{csDataVal} object.}
	\item{cstSummary}{\code{signature(csDataCons)}: summary for a \emph{csDataCons} object.}
	\item{cstSummary}{\code{signature(clData)}: summary for a \emph{clData} object.}
	\item{cstSummary}{\code{signature(clDataVal)}: summary for a \emph{clDataVal} object.}
	\item{cstSummary}{\code{signature(clDataCons)}: summary for a \emph{clDataCons} object.}  
  \item{cstSummary}{\code{signature(ceData)}: summary for a \emph{ceData} object.}
  \item{cstSummary}{\code{signature(ceDataVal)}: summary for a \emph{ceDataVal} object.}
  \item{cstSummary}{\code{signature(ceDataCons)}: summary for a \emph{ceDataCons} object.}
}}


\author{Mathieu Merzereaud}

\seealso{\code{\link{summary}}
}

\examples{
data(sole)
cstSummary(sole.cs,tab="ca")
cstSummary(sole.cs,tab="ca",sizeMax=28)
cstSummary(sole.cs,tab="ca",except="trpCode")
}

\keyword{manip}