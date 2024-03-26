\name{dbePlot}
\alias{dbePlot}
\alias{dbePlot,dbeOutput-method}
\docType{methods}
\title{Graphical display of 'dbeOutput' final estimates}
\description{
Method for plotting final estimates from an input 'dbeOutput' object.
}

\usage{
dbePlot(object,elmt,type="bar",Xstratum=NULL,step=NA,dispKey=TRUE,indScale=FALSE,\dots)
}
                                     
\arguments{
  \item{object}{A \emph{dbeOutput} object.}
  \item{elmt}{Character specifying an element (a dataframe) of \emph{dbeOutput} 'object'. For example, "lenStruc\$estim", "ageVar" or "totalNnum\$cv". 'rep' elements are not accepted ; see \emph{dbePlotRep}.}
  \item{type}{Character specifying the type of the drawn plot. To be chosen between "bar" (default value), "point" and "line".}
  \item{Xstratum}{Stratum displayed on x-axis if 'elmt' doesn't point at length or age structure information. To be chosen between "time", "space", "technical" and \code{NULL} (default value).}
  \item{step}{Numeric. If given, empty length or age classes will be considered and displayed, according to specified value.}
  \item{dispKey}{Logical. If \code{TRUE}, a describing key is displayed}
  \item{indScale}{Logical. If \code{TRUE}, y-axis scale is specific to each panel. If \code{FALSE}, the same limits are used for every panel.}
  \item{...}{Further graphical arguments such as \emph{col, lwd, lty, pch, cex, font, rot,}\dots}
}


\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}, \code{\link{dbePlotRep}}
}

\examples{
data(sole)

#stratification object
strDef <- strIni(timeStrata="quarter",spaceStrata="area")
#consolidated object
object <- csDataCons(csDataVal(sole.cs),strDef)
#dbeOutput initial object with needed parameters
dbeOutput <- dbeObject(desc="My object",species="Solea solea",param="weight",
                       strataDesc=strDef,methodDesc="analytical")

lW <- bpEstim(dbeOutput,object)

dbePlot(lW,elmt="ageStruc$estim",step=1,ylab="Mean weight (g)")

}
\keyword{methods}
