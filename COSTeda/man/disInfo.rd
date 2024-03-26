\name{disInfo-methods}
\docType{methods}
\alias{disInfo}
\alias{disInfo,csDataVal-method}
\alias{disInfo,clDataVal-method}
\alias{disInfo,ceDataVal-method}
\alias{disInfo,csDataCons-method}
\alias{disInfo,clDataCons-method}
\alias{disInfo,ceDataCons-method}
\alias{disInfo-methods}
\title{Display 'Tapply' function call output applied to "COST" objects. }
\description{
Tapply-like methods to display information from validated and consolidated objects on .txt file. 
'cs' tables are merged one by one from 'tr' to 'hl' until all required fields are in resulting table.
}

\usage{
\S4method{disInfo}{csDataVal}(object,path,field,by,fun,...,biopar=FALSE,transpose=FALSE,title="",append=TRUE)
\S4method{disInfo}{csDataCons}(object,path,field,by,fun,...,biopar=FALSE,transpose=FALSE,title="",append=TRUE)
\S4method{disInfo}{clDataVal}(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE)
\S4method{disInfo}{clDataCons}(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE)
\S4method{disInfo}{ceDataVal}(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE)
\S4method{disInfo}{ceDataCons}(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE)
}

\arguments{
  \item{object}{A \emph{COST} object.}
  \item{path}{Output .txt file path (with .txt extension).}
  \item{field}{Character specifying field on which function is applied. If more than one field is specified (only for cs table), fields are internally concatenated before calculation.}
  \item{by}{Vector with characters specifying factor(s) within which function is applied. For consolidated objects, only \code{"time", "space" & "technical"} are allowed.}
  \item{fun}{Function to be applied.}
  \item{...}{Further arguments to function to be applied.}
  \item{biopar}{If \code{TRUE}, calculation is made upon 'ca' table. If \code{FALSE}, calculation is done upon 'tr', 'hh', 'sl' and 'hl' table.}
  \item{transpose}{If \code{TRUE}, output array is transposed.}
  \item{title}{Title to be written on .txt output file.}
  \item{append}{If \code{TRUE}, output will be appended to .txt file. If \code{FALSE}, file will be overwritten. If \code{NA}, result is not exported, but an output list with 'title' 
  and 'result' elements is returned.}
}

\section{Methods}{
\describe{
	\item{disInfo}{\code{signature(csDataVal)}: disInfo for a \emph{csDataVal} object.}
	\item{disInfo}{\code{signature(csDataCons)}: disInfo for a \emph{csDataCons} object.}
	\item{disInfo}{\code{signature(clDataVal)}: disInfo for a \emph{clDataVal} object.}
	\item{disInfo}{\code{signature(clDataCons)}: disInfo for a \emph{clDataCons} object.}  
  \item{disInfo}{\code{signature(ceDataVal)}: disInfo for a \emph{ceDataVal} object.}
  \item{disInfo}{\code{signature(ceDataCons)}: disInfo for a \emph{ceDataCons} object.}
}}


\author{Mathieu Merzereaud}

\seealso{\code{\link{tapply}}
}

\examples{
data(sole)
sole.cs.val <- csDataVal(sole.cs)

##Change path before run
#
#Path <- "C:/draft.txt"
#disInfo(sole.cs.val,Path,"lenNum","lenCls",sum,na.rm=TRUE,
#        title="Measured numbers at length",append=FALSE)
#disInfo(sole.cs.val,Path,"wt",c("quarter","foCatEu5"),sum,na.rm=TRUE,
#        title="Total sampled weights by quarter and metier")
#disInfo(sole.cs.val,Path,"wt","foCatEu5",sum,na.rm=TRUE,
#        title="Total sampled weights by metier")
#disInfo(sole.cs.val,Path,"subSampWt",c("quarter","foCatEu5","area"),sum,na.rm=TRUE,
#        title="Measured weights by quarter, metier and area")
#disInfo(sole.cs.val,Path,c("trpCode","staNum"),c("quarter"),
#        function(x) length(unique(x)),title="Total FO numbers of sampled trips by quarter")
#disInfo(sole.cs.val,Path,c("trpCode","staNum","landCat"),c("quarter"),
#        function(x) length(unique(x)),title="Sampled FO numbers by quarter")
#disInfo(sole.cs.val,Path,"lenNum",c("quarter","commCat"),sum,na.rm=TRUE,
#        title="Number of measured fish by quarter and commercial category")
##information from CA table
#disInfo(sole.cs.val,Path,"indWt",c("quarter","sex"),mean,na.rm=TRUE,biopar=TRUE,
#        title="Mean individual weight by quarter and sex")
}

\keyword{manip}