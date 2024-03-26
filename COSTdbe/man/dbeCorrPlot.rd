\name{dbeCorrPlot}
\alias{dbeCorrPlot}
\alias{dbeCorrPlot,dbeOutput-method}
\docType{methods}
\title{Levelplot of a correlation matrix between estimates at age (replicates)}
\description{
Method for plotting an image of the correlation matrix between estimates at age, from \emph{dbeOutput} 'ageStruc' slot replicates.
}

\usage{
dbeCorrPlot(object,dispKey=TRUE,\dots)
}

\arguments{
  \item{object}{A \emph{dbeOutput} object with replicates information in 'ageStruc' slot.}
  \item{dispKey}{Logical. If \code{TRUE}, a legend is drawn}
  \item{...}{Further graphical arguments.}
}

\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}
}

%\examples{
%to do 
%}
\keyword{methods}
