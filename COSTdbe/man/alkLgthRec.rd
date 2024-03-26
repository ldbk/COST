\name{alkLgthRec}
\alias{alkLgthRec}
\alias{alkLgthRec,csDataCons-method}
\docType{methods}
\title{Method for managing gaps in age-length keys}
\description{
This function proposes various methods to solve alk gaps problems inherent to a 'csDataCons' object. 
Input object is updated according to chosen method(s) (grouped/recoded length classes, addition of 'virtual' individuals,...)  
}

\usage{
alkLgthRec(object,type="stepIncr",value,preview=FALSE,postview=TRUE,update=FALSE,\dots)
}
                           
\arguments{
  \item{object}{A \emph{csDataCons} object with ca information.}
  \item{type}{Character for chosen method. Values are :\\
\tabular{ll}{
    \bold{"stepIncr"} \tab Default parameter. Length class step is increased to specified \emph{value} parameter (default value=10) \cr
    \bold{"fillMiss"} \tab All gaps (with size <= value) are filled out with the sum of surrounding recorded classes (default value=1) \cr
%    \item{"sExtrGrp"}{The 'value' first classes are grouped (default value=1)}
%    \item{"lExtrGrp"}{The 'value' last classes are grouped (default value=1)}
    \bold{"sFillMiss"} \tab The 'value' empty classe(s) prior to first recorded length class is filled out with the latter (default value=1) \cr
    \bold{"lFillMiss"} \tab The 'value' empty classe(s) following last recorded length class is filled out with the latter (default value=1) \cr 
%    \item{"sFillAge"}{The 'value' empty classe(s) prior to first recorded length class is filled out with 1 individual of minimal age (default value=1)}
}} 
  \item{value}{Numerical parameter for chosen method (see 'type').}
  \item{preview}{Logical. If \code{TRUE}, original age length key is displayed.}
  \item{postview}{Logical. If \code{TRUE}, new age length key is displayed.}
  \item{update}{Logical. If \code{TRUE}, 'csDataCons' object is updated in accordance with chosen method, and then returned. 
  If \code{FALSE}, descriptive elements about updated alk are returned (see 'values'), but input object remains unchanged.}
  \item{...}{Further arguments, and particularly a \emph{start} numerical parameter specifying the first considered length class when recoding (only useful for 'type="stepIncr"'). 
  Default value is the minimum aged length class in \emph{ca} table.}
}

\value{If \code{update=FALSE}, returned elements are : \code{$alk} is the raw resulting age-length key, \code{$propMiss} are short statistics about gaps (see 'propMissLgthCons' method), 
\code{$lgthCls} is a description of length classes recoding for 'stepIncr', 'sExtrGrp' and 'lExtrGrp' methods and \code{$addIndTab} is a description of added virtual individuals 
for other methods.
}

\author{Mathieu Merzereaud}        

\seealso{\code{\link{viewGapsAlkCons}}, \code{\link{propMissLgthCons}}
}

\examples{

data(sole)
  #restriction to "27.7.d" & "27.7.e" areas
csRaw <- subset(sole.cs,area\%in\%c("27.7.d","27.7.e"),table="hh")
  #consolidation process
conSole.cs <- csDataCons(csDataVal(csRaw),strIni(timeStrata="quarter",spaceStrata="area"))

res1 <- alkLgthRec(conSole.cs,type="stepIncr",value=20)
names(res1)

res1$missProp #updated statistics about missing length classes

res1$lgthCls #updated Length Classes
#if it's allright, consolidated object can be updated --> conSole.cs1
conSole.cs1 <- alkLgthRec(conSole.cs,type="stepIncr",value=20,postview=FALSE,update=TRUE)

}
\keyword{methods}
