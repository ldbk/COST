\name{viewGapsAlkCons}
\alias{viewGapsAlkCons}
\alias{viewGapsAlkCons,csDataCons-method}
\docType{methods}
\title{Calculation and display of age-length keys with gaps underlining}
\description{
This method calculates and underlines empty length classes in an age-length key from a given 'csDataCons' object (ca table). 
'.' shows length classes that are not recorded in hl for the specified stratum.
'-' shows length classes that are recorded in hl but not in ca, for the specified stratum.
}

\usage{
viewGapsAlkCons(object,...)
}

\arguments{           
  \item{object}{A \emph{csDataCons} object.}
  \item{...}{Further arguments.}
}



\author{Mathieu Merzereaud}

\seealso{\code{\link{alkLgthRec}}, \code{\link{propMissLgthCons}}, \code{\link[COSTcore]{csDataCons}}
}

\examples{

data(sole)
  #subset to "27.7.d" & "27.7.e" areas
csSub <- subset(sole.cs,area\%in\%c("27.7.d","27.7.e"),table="hh")
conSole.cs <- csDataCons(csDataVal(csSub),strIni(spaceStrata="area"))

viewGapsAlkCons(conSole.cs)  
 
}
\keyword{methods}
