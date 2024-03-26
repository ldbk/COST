\name{relativeValue}
\alias{relativeValue}
\alias{relativeValue,csDataVal,missing-method}
\alias{relativeValue,csDataVal,strIni-method}
\alias{relativeValue,clDataVal,missing-method}
\alias{relativeValue,clDataVal,strIni-method}
\alias{relativeValue,ceDataVal,missing-method}
\alias{relativeValue,ceDataVal,strIni-method}
\alias{relativeValue,csDataCons,missing-method}
\alias{relativeValue,clDataCons,missing-method}
\alias{relativeValue,ceDataCons,missing-method}
\docType{methods}
\title{Calculation of relative values of a numerical field within time, space and/or technical stratification}
\description{
This method calculates relative values of a population level variable (if input object class is \emph{clDataVal/clDataCons/ceDataVal/ceDataCons}),
or a sampling level variable (if input object class is \emph{csDataVal/csDataCons}).
Calculation can be done within time, space and/or technical stratification. This stratification (as well as an optionnal strata recoding process) 
is defined by \emph{strDef} parameter for input validated objects (see \emph{strIni}). 
Output is an \emph{edaResult} object with \emph{desc=}"csRelativeValue" or "clceRelativeValue".
An exploratory graphic to compare two objects can be made by applying \emph{plot} function. (see plot.edaResult)
}

\usage{
\S4method{relativeValue}{csDataVal,missing}(data,field="lenNum",\dots)
\S4method{relativeValue}{csDataVal,strIni}(data,strDef,field="lenNum",\dots)
\S4method{relativeValue}{clDataVal,missing}(data,field="landWt",\dots)
\S4method{relativeValue}{clDataVal,strIni}(data,strDef,field="landWt",\dots)
\S4method{relativeValue}{ceDataVal,missing}(data,field="trpNum",\dots)
\S4method{relativeValue}{ceDataVal,strIni}(data,strDef,field="trpNum",\dots)
\S4method{relativeValue}{csDataCons,missing}(data,field="lenNum",\dots)
\S4method{relativeValue}{clDataCons,missing}(data,field="landWt",\dots)
\S4method{relativeValue}{ceDataCons,missing}(data,field="trpNum",\dots)
}

\arguments{
  \item{data}{A \emph{csDataVal/clDataVal/ceDataVal/csDataCons/clDatacons/ceDataCons} object.}
  \item{strDef}{Optionnal. A \emph{strIni} object describing stratification and recoding parameters. Specified stratification must match with 'data' parameter.}
  \item{field}{A numeric field from \emph{data} (e.g "lenNum", "wt", "subSampWt" or "nbSamp" (number of samples in sl table) and "nbInd" (number of individuals in ca table) for 'cs', 
  "landWt" for 'cl', or "trpNum" for 'ce').}
  \item{...}{Further arguments.}
}


\author{Mathieu Merzereaud}

\seealso{\code{\link[COSTcore]{strIni}}}

\examples{
data(sole)
sole.cs.val <- csDataVal(sole.cs)
sole.cl.val <- clDataVal(sole.cl) 
strD <- strIni(timeStrata="month",spaceStrata="area",techStrata="commCat")

CS <- relativeValue(sole.cs.val,strD,"nbSamp")
CL <- relativeValue(sole.cl.val,strD)
}

\keyword{methods}