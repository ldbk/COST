\name{ageLenMulti,csDataVal-method}
\alias{ageLenMulti}
\alias{ageLenMulti,csDataVal,missing-method}
\alias{ageLenMulti,csDataVal,strIni-method}
\docType{methods}
\title{Multinomial modelisation applied to fisheries age-at-length data}
\description{
This method implements a multinomial analysis of age-at-length data, for specified time, space and technical stratification as defined in input \emph{strIni} object. 
It requires a \emph{csDataVal} object that can be built by \code{csDataVal} method from available \pkg{COSTcore} package. 
All information is taken from \emph{ca} table.
}
                                 
\usage{
ageLenMulti(data,strDef,elmts=list(tp="all",sp="all",tc="all"),age.plus=-1,\dots)
}

\arguments{
  \item{data}{A \emph{csDataVal} object with \emph{ca} informations.}
  \item{strDef}{Optionnal. A \emph{strIni} object. Specified stratification must match with ca fields.}
  \item{elmts}{List of strata occurrence(s) to be kept for modelisation. \emph{tp}, \emph{sp} and \emph{tc} are character vectors. \code{"all"} includes all occurrences for the strata in ca table.}
  \item{age.plus}{Threshold for grouping age (numeric value). (-1)-value means no grouping.}
  \item{...}{Further arguments.}
}

\details{
Output object can then be called by \emph{plot} method to display fitted result (see \code{plot.edaResult}). 
Furthermore, response matrix (\emph{outPut\$age} element) and predictors (in \emph{outPut\$dat} table) internally called by \emph{multinom} formula expression are provided. 
Considering the output as the full multinomial log-linear model, various strata effects studies can then be achieved from it using multinomial nested models or/and contrasts redefinition.    
}

\references{Gerritsen, H.D., McGrath, D., and Lordan, C. (2006)
\emph{A simple method for comparing age-length keys reveals significant regional 
differences within a single stock of haddock (Melanogrammus aeglefinus)}. ICES Journal of Marine Science, 63: 1096-1100.
}

\author{Mathieu Merzereaud}
\seealso{\code{\link{edaResult}}, \code{\link{plot.edaResult}}, \code{\link[nnet]{multinom}}, \code{\link[COSTcore]{strIni}}}

\examples{
data(sole) 
sole.cs.val <- csDataVal(sole.cs) 
ageLenMulti(sole.cs.val,age.plus=6)
}
\keyword{methods}
