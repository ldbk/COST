\name{dbeObject}
\alias{dbeObject}
\docType{methods}
\title{Initialized 'dbeOutput' object creator}
\description{
This function returns an initialized \emph{dbeOutput} object that can be used as input object for 'COSTdbe' package methods.
}

\usage{
dbeObject(desc, species, catchCat, param, strataDesc, methodDesc, ...)
}

\arguments{
  \item{desc}{'desc' slot of returned 'dbeOutput' object.}
  \item{species}{'species' slot of returned 'dbeOutput' object.}
  \item{catchCat}{'catchCat' slot of returned 'dbeOutput' object.}
  \item{param}{'param' slot of returned 'dbeOutput' object.}
  \item{strataDesc}{'strataDesc' slot of returned 'dbeOutput' object.}
  \item{methodDesc}{'methodDesc' slot of returned 'dbeOutput' object.}
  \item{...}{Further arguments.}
}


\author{Mathieu Merzereaud}

\seealso{\code{\link{dbeOutput}}
}

\examples{
obj <- dbeObject(desc="My object",species="Solea solea",catchCat="DIS",methodDesc="analytical")
}
\keyword{methods}
