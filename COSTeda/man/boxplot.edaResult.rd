\name{boxplot.edaResult}
\docType{methods}
\alias{boxplot,edaResult-method}
\alias{boxplot.edaResult}
\title{Boxplot of "edaResult" object}
\description{
This method is a generic function for producing boxplots from an object of class \emph{edaResult}, resulting from a \emph{COSTeda} package function.
}


\section{Methods}{
\describe{
	\item{boxplot}{\code{signature("edaResult")}: plotting procedure of an object of class \emph{edaResult} with \emph{desc} slot equal to "landisVol".}
}}

\section{Usage}{
\tabular{llll}{
\bold{desc} \tab \bold{parameter} \tab \bold{default} \tab \bold{description} \cr
\bold{\code{"landisVol"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="landisVol"} \cr
    \tab \tab \tab (see \code{landisVol} method). \cr
 \tab type \tab \code{"FD"} \tab Character. If type="FO", boxplot of catch weights per \cr
                  \tab \tab \tab fishing operation, for each fishing day of each trip \cr
                  \tab \tab \tab will be displayed. \cr
 \tab \tab \tab Otherwise (type="FD"), boxplot of catch weights per \cr
 \tab \tab \tab fishing day for each trip will be displayed. \cr
 \tab \dots \tab \tab Further graphical parameters. \cr
}
}
        
\author{Mathieu Merzereaud}

\seealso{\code{\link{edaResult}}, \code{\link{landisVol}}, \code{\link{plot.edaResult}}, \code{\link{boxplot}}
}

\examples{

#desc="landisVol"
data(sole)
ldV <- landisVol(sole.cs,species="Solea solea")

boxplot(ldV)
boxplot(ldV,type="FO")

}

\keyword{dplot}
