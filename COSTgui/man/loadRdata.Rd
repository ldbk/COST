\name{loadRdata}
\alias{loadRdata}
\title{
Partial import from Rdata archive
}
\description{
perform a selective importation from Rdata elements
}
\usage{
loadRdata(file.rdata, liste)
}

\arguments{
  \item{rdata}{
a .rdata file 
}
  \item{liste}{
a character vector : the objects to be imported.
}
}

\author{
Vincent Guyader
}


\seealso{\code{\link[stats]{save}}, \code{\link[stats]{load}}}


\examples{
\dontrun{
a<-2
b<-5
save(a,b,file='myRdata.rdata')
rm(a,b)
loadRdata('myRdata.rdata',c('a'))
ls() # a is loaded not b
}
}
