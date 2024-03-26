\name{strIni-class}
\docType{class}
\alias{strIni}
\alias{strIni-class}
\alias{strIni-methods}
\title{Class "strIni"}
\description{The strIni class stores stratification and recoding information that is required for consolidated objects creation process.}
\section{Objects from the Class}{
The creator function "strIni" can be called to create objects from this class. Default values for slots are NAs, that means no stratification or no recoding)
}
\section{Slots}{
\describe{
    \item{\code{timeStrata}:}{time stratification (e.g "quarter", "month",...)}
    \item{\code{spaceStrata}:}{space stratification (e.g "area",...}
    \item{\code{techStrata}:}{technical stratification (e.g "commCat", "foCatEu5",...}
    \item{\code{tpRec}:}{list for time strata recoding (e.g \code{list(from="1",to="2")})}
    \item{\code{spRec}:}{list for space strata recoding (e.g \code{list(from=c("7D","7D1"),to=c("27.7.d","27.7.d"))})}
    \item{\code{tcRec}:}{list for technical strata recoding (e.g \code{list(from=c("OTB-DEF","OTB-MOL"),to=c("OTB","OTB"))})}
  }
}

\section{Methods}{

  \describe{
    \item{csDataCons}{\code{signature(object="csDataVal",objStrat="strIni")}:\code{csDataCons} class creator.}
    \item{clDataCons}{\code{signature(object="clDataVal",objStrat="strIni")}:\code{clDataCons} class creator.}
    \item{ceDataCons}{\code{signature(object="ceDataVal",objStrat="strIni")}:\code{ceDataCons} class creator.}
	 }
}
\author{Mathieu Merzereaud <Mathieu.Merzereaud@ifremer.fr>}
\examples{
showClass("strIni")
}
\keyword{classes}

