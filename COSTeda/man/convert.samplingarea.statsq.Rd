\name{convert.samplingarea.statsq}
\alias{convert.samplingarea.statsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts FRS sampling area codes to ICES statistical rectangles}
\description{
Given FRS sampling area codes this function returns the constituent ICES statistical rectangles. 
}
\usage{
convert.samplingarea.statsq(areacode,samplingarea)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{areacode}{A character or numeric vector giving the names of strata within the sampling area e.g. c(1:10) for Demersal. }
  \item{samplingarea}{A character vector giving the names of sampling area; one of "Demersal", "DemersalMet", "Mackerel", "Herring", "Scallops" and "Edible crab". The default is "Demersal".}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list containing:
  \item{statsq}{The ICES statistical rectangles within the strata}
  \item{parentarea}{The strata}
  \item{parentname}{The name of the strata}
}

\author{ Alastair Pout \email{a.pout@marlab.ac.uk}}
\seealso{\code{\link{convert.statsq.lat.lon}} for obtaining the coordinates of statistical rectangles.}
\examples{

# Demersal sampling areas 1 to 13
doarea <-c(1:13)
convert.samplingarea.statsq(doarea)

# Nephrops sampling areas
doarea <-c("AR","CL","FD","FF","FL","IL","IS","MF","NM","NO","OC","ON","OW","SM")
convert.samplingarea.statsq(doarea,samplingarea="Nephrops")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
