\name{lengthHist}
\alias{lengthHist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots a length frequency histogram from csData}
\description{
Plots histograms of the length frequency data from the hl table of csData objects. 
Optionally can plot by a grouping variable, and specific factor levels within the grouping variable. The discarded and landed fractions of the catch can be plotted separately or together.     
}
\usage{
lengthHist(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ an object of class csData or csDataVal }
\item{by}{ the character name of a grouping variable. 
}
\item{level}{ the level within the grouping variable, the default is "all".}
\item{fraction}{ the fraction of the catch to plot. DIS for discards, LAN for landings. The default is to plot both if present}
\item{title}{logical. adds a title to the outer margin}
\item{\dots}{other arguments, particularly those to \code{\link{hist}}.
}

}
\details{
The possible options for the grouping variable are those within the 
amalgamated hl table produced by \code{\link{mergecsData}} and include for time: "year", "month" and "quarter";  
for space: "area", and "rect";
for technical: "foCatNat","foCatEu5" and "foCatEu6". 
Other options include "proj", "trpCode", "commCat"  and "sex".  
The default is to plot by "spp" so, for a single species data set, this will plot all length frequencies.

For plotting selected levels within the grouping variable the names of those levels can be passed as a vector to \code{level}, e.g just to plot data from the first quarter then set \code{by ="quarter"} and \code{level=1}. Grouping variables that are numeric, such as months and quarters, are specified as numerics e.g. 
\code{level=c(1,3)}. Grouping variables that are characters are specified as character strings e.g. \code{level=c("OTB_DEM")}

The arguments that can be passed as \code{\dots} includes: 
\code{col} for setting the colour or the bars;
\code{border} for setting the colour of the borders of the bars;
\code{add=TRUE} for adding to an existing plot;
\code{angle} for setting the angle of shading lines;
\code{density} for setting the density of the shading lines.
The argument \code{freq=FASLE} will plot a density histogram.  
If \code{axes=FALSE} no axes will be plotted. 
\code{ylim} sets the limits for the y axes (only applicable for frequency plots) and 
\code{xlab} and \code{ylab} are for axes labels. 
Other graphical parameters include \code{main}, \code{sub}, \code{cex.main}, \code{cex.asis}, \code{cex.lab} e.t.c see \code{\link{hist}} and \code{\link{par}} for more details. 

The outer margin default title is \"Length distribution of \emph{species} by \emph{grouping variable}\". This can be turned off if \code{title=FALSE}.
The figure titles default to the argument passed as \code{by} and, if specified, \code{level}. This can be overwritten by a call to \code{main}. \code{main=""} will result in no figure title.   

\code{par(mfrow=c(nrow,ncol))} can be used to adjust the number of plots per page to accommodate the multiple figures generated when the grouping variable has more than one level. 
                                                                                   
% ~~ If necessary, more details than the description above ~~
}
\value{
%  ~Describe the value returned
% If it is a LIST, use
A named list of the grouping variable and levels plotted, each component of which 
is itself a list of class "histogram" with components breaks, counts, density, 
intensities, mids xname and equidist. See \code{\link{hist}} for details. 

}
%\references{ ~put references to the literature/web site here ~ }
\author{Alastair Pout \email{a.pout@marlab.ac.uk} }
%\note{ ~~further notes~~ 


\seealso{\code{\link{lenDisPlot}} which plots length distributions within trips.}
\examples{
%library(COSTdata)
data(cod2004cs)
# Plotting the length distribution in the hl table 
lengthHist(cod2004cs,col=2)

# and now grouped by the commercial category
lengthHist(cod2004cs,by="commCat",level=1,col=2,density=30,main="")
for(i in 2:6)lengthHist(cod2004cs,by="commCat",level=i,col=(i+1),density=30,main="",add=TRUE,title=FALSE)

# plotting by quarter
par(mfrow=c(2,2))
lengthHist(cod2004cs,by="quarter",col=2,freq=TRUE)
par(mfrow=c(1,1))
}

\keyword{hplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
