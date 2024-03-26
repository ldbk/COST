\name{tkFillGaps}
\alias{tkFillGaps}
\alias{tkFillGaps,csDataCons-method}
\docType{methods}
\title{Addition of virtual individuals in age-length keys}
\description{
This method displays stratified ALKs from a \emph{csDataCons} object in a GUI table that allows to add 'virtual' individuals. 
Input object is then updated and returned (see\emph{Value} section).  
}

\usage{
tkFillGaps(object)
}
                           
\arguments{
  \item{object}{A \emph{csDataCons} object with ca information.}
}

\details{
See \emph{viewGapsAlkCons} for a description of the codification used in displayed ALKs. 
Interface has been computed with \emph{tcltk} package.
}

\value{An updated \emph{csDataCons} object. CA table is updated with new individuals (one per line). 
New \emph{PSUid} and \emph{trpCode} values are then created, and \emph{tr} table is also updated with those new 'virtual trips'.   
}

\author{Mathieu Merzereaud}        

\seealso{\code{\link[COSTcore]{csDataCons}}, \code{\link{viewGapsAlkCons}} 
}

\examples{
data(sole)
  #subset to "27.7.d" & "27.7.e" areas
csSub <- subset(sole.cs,area\%in\%c("27.7.d","27.7.e"),table="hh")
object <- csDataCons(csDataVal(csSub),strIni(spaceStrata="area"))
#obj <- tkFillGaps(object)
#tail(obj)
}
\keyword{methods}
