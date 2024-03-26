\name{dfApply}
\alias{dfApply}

\title{Aggregate-like function with a two-dimensional result }

\description{
This function is an aggregate-like method that operates over grouping variables whose one constitutes field names of the resulting dataframe. 
}

\usage{
dfApply(tab,valueField,rowFields,colField,fun,...)
}

\arguments{
  \item{tab}{ an R data.frame.}
  \item{valueField}{ character specifying the field on which operates the function (only one).}
  \item{rowFields}{ character specifying the grouping element(s) within rows.}
  \item{colField}{ character specifying the grouping element within columns (only one).}
  \item{fun}{ function to be applied.}
  \item{...}{ further arguments.}
}


\author{Mathieu Merzereaud}

\seealso{\code{\link{tabConsist}}}
\examples{

data(sole)
dfApply(sole.cs@sl,"wt",c("catchCat","commCat"),"sampType",sum,na.rm=TRUE)

}

\keyword{manip}

