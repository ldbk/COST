\name{controlMap}
\alias{controlMap}
\alias{controlMap,csData,clData,ceData-method}
\alias{controlMap,csData,clData,missing-method}
\alias{controlMap,csData,ceData,missing-method}
\alias{controlMap,csData,missing,missing-method}

\docType{methods}
\title{Method to check consistency between COST objects}
\description{
Quick comparative analysis between main fields occurences of an input raw CS object, within other raw input objects (CL, or CE, or both)
}

\usage{
\S4method{controlMap}{csData,clData,ceData}(csObject,secObject,thiObject,\dots)
\S4method{controlMap}{csData,clData,missing}(csObject,secObject,thiObject,\dots)
\S4method{controlMap}{csData,ceData,missing}(csObject,secObject,thiObject,\dots)
\S4method{controlMap}{csData,missing,missing}(csObject,secObject,thiObject,\dots)
}

\arguments{
  \item{csObject}{A \emph{csData} object.}
  \item{secObject}{Optionnal (not used in 4th case). A \emph{clData} or \emph{ceData} object.}
  \item{thiObject}{Optionnal (used only in 1st case). A \emph{clData} or \emph{ceData} object (depending on \code{secObject} class).}
  \item{...}{Further arguments.}
}

\value{
CA table, CL & CE objects keyfields are compared to CS object's (except CA). 
A 'tcltk' interface displays informations and helps the user during the process. 
'tabConsist' column allows to call COSTeda's 'tabConsist' function for the specified field upon the input objects (result is displayed on the R console). 
The following code is used (a colour transcribes the level of warning 'green=OK, red=alert'):
\item{OK}{all occurences in the table are in CS, and all occurences in CS are in the table}
\item{>S}{all occurences in CS are in the table}
\item{<S}{all occurences in the table are in CS}
\item{!=S}{some occurences in the table are not in CS, and some occurences in CS are not in the table}
\item{NA-NA}{all the field is NA in the table and in CS}
\item{NA-X}{all the field is NA in the table}
\item{X-NA}{all the field is NA in CS} 
\item{-}{the field is not in the table}
'lenCls' is a special field as the displayed value is the proportion of different length classes in CS that are recorded in CA table (green for >90\%, red for <75\%)
}

\author{Mathieu Merzereaud}

\seealso{\code{\link{tabConsist}}, \code{\link[COSTcore]{csData}}, \code{\link[COSTcore]{clData}}, \code{\link[COSTcore]{ceData}}}

\examples{

#data(sole)
#out <- controlMap(sole.cs,sole.cl,sole.ce)

}

\keyword{methods}