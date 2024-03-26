\name{\%>=\%}
\alias{\%>=\%}

\title{
the >= operator without NA
}
\description{
perform a strict inferior comparaison and remplace the NA value with FALSE
}
\usage{
x %>=% y
}

\arguments{
  \item{x}{
vector : the values to be matched.
}
  \item{y}{
numeric : the value to be matched against.
}
}


\author{
Vincent Guyader
}




\seealso{  
\code{\link{\%in\%}}
\code{\link{\%<\%}}
\code{\link{\%>\%}}
\code{\link{\%<=\%}}
\code{\link{\%>=\%}}
}

\examples{
\dontrun{
c(1,NA,3,4,5) \%>=\% 3
c(1,NA,3,4,5) \%<=\% 3
c(1,NA,3,4,5) \%>\% 3
c(1,NA,3,4,5) \%<\% 3
c(1,NA,3,4,5) \%in\% 3
}}


