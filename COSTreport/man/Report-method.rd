\name{Report}

\alias{Report}

\alias{Report,strIni,csDataVal,missing,missing-method}
\alias{Report,strIni,clDataVal,missing,missing-method}
\alias{Report,strIni,ceDataVal,missing,missing-method}
\alias{Report,strIni,csDataVal,clDataVal,missing-method}
\alias{Report,strIni,csDataVal,ceDataVal,missing-method}
\alias{Report,strIni,clDataVal,ceDataVal,missing-method}
\alias{Report,strIni,csDataVal,clDataVal,ceDataVal-method}

\alias{Report,strIni,csDataCons,missing,missing-method}
\alias{Report,strIni,clDataCons,missing,missing-method}
\alias{Report,strIni,ceDataCons,missing,missing-method}
\alias{Report,strIni,csDataCons,clDataCons,missing-method}
\alias{Report,strIni,csDataCons,ceDataCons,missing-method}
\alias{Report,strIni,clDataCons,ceDataCons,missing-method}
\alias{Report,strIni,csDataCons,clDataCons,ceDataCons-method}

\alias{Report,dbeOutput,missing,missing,missing-method}

\alias{Report,character,missing,missing,missing-method}

\docType{methods}
\title{Reporting method for CS, CL, CE, DBE objects, and .R scripts}
\description{
ToDo 
}

\usage{
Report(w,x,y,z,language="FR",logo=as.character(NA),\dots)
}

\arguments{
\item{w}{A "strIni" or a "dbeOutput" object, or a character. See below for further description.}
\item{x}{Optionnally  a "csDataVal", "clDataVal", "ceDataVal", "csDataCons", "clDataCons", or "ceDataCons" object. See below for further description.}          
\item{y}{Optionnally  a "clDataVal", "ceDataVal", "clDataCons", or "ceDataCons" object. See below for further description.}
\item{z}{Optionnally  a "ceDataVal" or "ceDataCons" object. See below for further description.}
\item{language}{Character. Language of output report. To be chosen between "FR" (french) or "EN" (english).}
\item{logo}{Character. Path of a logo image to be inserted in the report on the front page.}
\item{\dots}{Further arguments}
}


\section{Methods}{
\describe{
	\item{Report}{\code{signature("strIni","csDataVal","missing","missing")}: Reporting method applied to a "csDataVal" object. For validated objects, "strIni" object is needed to specify chosen stratification.}
	\item{Report}{\code{signature("strIni","clDataVal","missing","missing")}: Reporting method applied to a "clDataVal" object.}
	\item{Report}{\code{signature("strIni","ceDataVal","missing","missing")}: Reporting method applied to a "ceDataVal" object.}
	\item{Report}{\code{signature("strIni","csDataVal","clDataVal","missing")}: Reporting method applied to a "csDataVal" and a "clDataVal" object.}
	\item{Report}{\code{signature("strIni","csDataVal","ceDataVal","missing")}: Reporting method applied to a "csDataVal" and a "ceDataVal" object.}
	\item{Report}{\code{signature("strIni","clDataVal","ceDataVal","missing")}: Reporting method applied to a "clDataVal" and a "ceDataVal" object.}
	\item{Report}{\code{signature("strIni","csDataVal","clDataVal","ceDataVal")}: Reporting method applied to a "csDataVal", a "clDataVal" and a "ceDataVal" object.}
              	
	\item{Report}{\code{signature("strIni","csDataCons","missing","missing")}: Reporting method applied to a "csDataCons" object. For consolidated objects, "strIni" object is needed to describe time, space and technical stratification.}
	\item{Report}{\code{signature("strIni","clDataCons","missing","missing")}: Reporting method applied to a "clDataCons" object.}
	\item{Report}{\code{signature("strIni","ceDataCons","missing","missing")}: Reporting method applied to a "ceDataCons" object.}
	\item{Report}{\code{signature("strIni","csDataCons","clDataCons","missing")}: Reporting method applied to a "csDataCons" and a "clDataCons" object.}
	\item{Report}{\code{signature("strIni","csDataCons","ceDataCons","missing")}: Reporting method applied to a "csDataCons" and a "ceDataCons" object.}
	\item{Report}{\code{signature("strIni","clDataCons","ceDataCons","missing")}: Reporting method applied to a "clDataCons" and a "ceDataCons" object.}
	\item{Report}{\code{signature("strIni","csDataCons","clDataCons","ceDataCons")}: Reporting method applied to a "csDataCons", a "clDataCons" and a "ceDataCons" object.}

	\item{Report}{\code{signature("dbeOutput","missing","missing","missing")}: Reporting method applied to a "dbeOutput" object.}
		
	\item{Report}{\code{signature("character","missing","missing","missing")}: Reporting method applied to a .R script file. Input parameter is the path of the script.}
}}



\value{
A report as a .pdf file called "mainVal.pdf", "mainCons.pdf", "mainDbe.pdf" or "mainScript.pdf" created in working directory.
 }


\author{Tian}
\seealso{\code{\link[COSTdbe]{dbeOutput}}, \code{\link[COSTcore]{csDataVal}}, \code{\link[COSTcore]{clDataVal}}, \code{\link[COSTcore]{ceDataVal}}, 
         \code{\link[COSTcore]{csDataCons}}, \code{\link[COSTcore]{clDataCons}}, \code{\link[COSTcore]{ceDataCons}}
}


\keyword{methods}
