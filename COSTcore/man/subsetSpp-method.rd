\name{subsetSpp}
\alias{subsetSpp}
\alias{subsetSpp,csData-method}
\alias{subsetSpp,csDataVal-method}
\alias{subsetSpp,csDataCons-method}
\docType{methods}
\title{Specific subsetting function applying on SL table from COST objects}
\description{
This method implements subsetting for the raw, the validated and the consolidated CS objects provided by COSTcore, proceeding specifically on SL table. This subset only impacts on HL table, 
and preserves the other tables.  
}

\usage{
subsetSpp(x,subset,link=FALSE,\dots)
}

\arguments{
  \item{x}{A \emph{csData}, \emph{csDataVal} or \emph{csDataCons} object.}
  \item{subset}{Logical expression specifying the subsetting to be applied on sl table.}
  \item{link}{Logical. If TRUE, subset is also applied on ca table.}
  \item{...}{Further arguments.}
}

\seealso{\code{\link{subset,csData-method}}
}

\keyword{methods}
