\name{rbind2-dbeOutput}
\docType{methods}
\alias{rbind2,dbeOutput,dbeOutput-method}
\title{rbind2 for `dbeOutput' objects}
\description{This method implements rbind2 for the 'dbeOutput' class provided by COSTdbe package.}
\section{Methods}{
\describe{
\item{}{\code{signature(x = "dbeOutput", y = "dbeOutput")}}
}}
\details{
Only dataframes are combined in the output object. Its descriptive slots are the same as \emph{x}'s, and \emph{dcrCVIndicator} elements are set to NA. 
}
\keyword{methods}

