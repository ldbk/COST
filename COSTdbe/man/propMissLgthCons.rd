\name{propMissLgthCons}
\alias{propMissLgthCons}
\alias{propMissLgthCons,csDataCons-method}
\docType{methods}
\title{Proportions of empty length classes in an age-length key}
\description{
This method calculates empty length classes proportion in an age-length key obtained from a 'csDataCons' object (ca table). 
'pEmpty' element describes the proportion of missing LC per alk within hl table. 
'pEmptyExtr' submits the proportion of extrema missing LC (among all missing LC) per alk within hl table.
}

\usage{
propMissLgthCons(object,...)
}

\arguments{
  \item{object}{A \emph{csDataCons} object.}
  \item{...}{Further arguments.}
}



\author{Mathieu Merzereaud}

\seealso{\code{\link{alkLgthRec}}, \code{\link{viewGapsAlkCons}}, \code{\link[COSTcore]{csDataCons}}
}

\examples{

data(sole)                          
  #subset to "27.7.d" & "27.7.e" areas
csSub <- subset(sole.cs,area\%in\%c("27.7.d","27.7.e"),table="hh")
conSole.cs <- csDataCons(csDataVal(csSub),strIni(spaceStrata="area"))

propMissLgthCons(conSole.cs)  
 
}
\keyword{methods}
