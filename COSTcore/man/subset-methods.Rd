\name{subset-methods}
\docType{methods}
\alias{subset,ceData-method}
\alias{subset,ceDataVal-method}
\alias{subset,ceDataCons-method}
\alias{subset,clData-method}
\alias{subset,clDataVal-method}
\alias{subset,clDataCons-method}
\alias{subset,csData-method}
\alias{subset,csDataVal-method}
\alias{subset,csDataCons-method}
\title{subset for `COSTcore' classes}
\description{This method implements subsetting for the classes provided by COSTcore.}
\section{Methods}{
\describe{
\item{}{\code{signature(x = "ceData")}}
\item{}{\code{signature(x = "ceDataVal")}}
\item{}{\code{signature(x = "ceDataCons")}}
\item{}{\code{signature(x = "clData")}}
\item{}{\code{signature(x = "clDataVal")}}
\item{}{\code{signature(x = "clDataCons")}}
\item{}{\code{signature(x = "csData")}}
\item{}{\code{signature(x = "csDataVal")}}
\item{}{\code{signature(x = "csDataCons")}}
}}

\details{Subset methods for \emph{csData} and \emph{csDataVal} objects are quite specific. An additional "table" parameter (with default value \code{table="tr"}) 
allows to specify which table will be subset. According to the subset table, the impact on other tables will be various. A subset on \emph{tr} will impact on all the datasets. 
A subset on \emph{hh}, \emph{sl} or \emph{hl} will potentially affect all the tables except \emph{ca}. Finally, a subset \emph{ca} will impact on \emph{tr}, and then incidentally 
on the other datasets. Another "link" logical parameter (with default value \code{link=FALSE}) allows to subset afterwards on \emph{ca}. For example, considering a subset on "area" field 
in \emph{hh} table : if "link" is false, only \emph{ca} part that is linked to \emph{hh} will be subset accordingly. On the other hand, if "link" is true, \emph{ca} will also be subset on "area" field. 
Another subsetting method proceeding on \emph{sl} table with more restrictive effects is also available (see \code{subsetSpp}).}

\seealso{\code{\link{subsetSpp}}
}      

\keyword{methods}

