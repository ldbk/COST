\name{tabConsist}
\alias{tabConsist}

\title{Field composition over COST objects}

\description{
This function gives a description of a specified field contents in various COST objects. 
}

\usage{
tabConsist(lTab,field,nb=FALSE)
}

\arguments{
  \item{lTab}{ A list of COST objects (within the same class).}
  \item{field}{ Character specifying the field to describe (\code{"month"} and \code{"quarter"} informations are also available from \emph{hh} table in object of class \emph{cs}).}
  \item{nb}{Logical. If \code{TRUE}, number of occurrence of each factor level is displayed.}
}


\author{Mathieu Merzereaud}

\seealso{\code{\link{dfApply}}}
\examples{

data(sole)
tabConsist(list(sole.cs,sole.ce,sole.cl),"area")

}

\keyword{manip}

