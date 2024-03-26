\name{GraphsPar}
\alias{GraphsPar} 
\alias{gp}
\docType{data}
\title{Graphical parameters for plotting procedures from "COSTeda" package.}
\description{      
List of graphical parameters used for \pkg{COSTeda} plotting procedures.
}
\usage{GraphsPar}
\format{
Components of \emph{gp} graphical parameters list:
\tabular{rll}{
l.col \tab character \tab Color(s) of lines \cr
l.lwd \tab numeric \tab Heaviness of lines \cr
lty \tab numeric \tab Type of lines \cr
pch \tab numeric, character \tab Type of points \cr
p.col \tab character \tab Color(s) of points \cr
p.bg \tab character \tab Filling color(s) of points \cr
p.cex \tab numeric \tab Size of points \cr
p.lwd \tab numeric \tab Heaviness of points \cr
cex.lab \tab numeric \tab Size of labels \cr
cex.axis \tab numeric \tab Size of axis annotations \cr
cex.main \tab numeric \tab Size of main title \cr
cex.sub \tab numeric \tab Size of subtitle \cr
col.lab \tab character \tab Color of labels \cr
col.axis \tab character \tab Color of axis annotations \cr
col.main \tab character \tab Color of main title \cr
col.sub \tab character \tab Color of subtitle \cr
font \tab numeric \tab General font \cr
font.lab \tab numeric \tab Font of labels \cr
font.axis \tab numeric \tab Font of axis annotations \cr
font.main \tab numeric \tab Font of main title \cr
font.sub \tab numeric \tab font of subtitle \cr
col \tab character \tab General color(s) \cr
rot \tab numeric \tab Rotation of x-axis annotations \cr
bg \tab character \tab General background color
}}

\details{
According to plotting procedures, some of these parameters turn out to be ineffective.
}

\examples{
data(GraphsPar)
names(gp)
}
\keyword{datasets}
