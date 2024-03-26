\name{agelenPlot}
\alias{agelenPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots age given length from csData}
\description{
Plots age given length from the ca table of csData objects.  
Optionally can plot by a grouping variable, and specific factor levels within the grouping variable. The discarded and landed fractions of the catch can be plotted separately or together.     
}
\usage{
agelenPlot(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,supsmu=FALSE,jitter=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{ An object of class csData or csDataVal }
\item{by}{ The character name of a grouping variable. }          
\item{level}{ The level within the grouping variable, the default is "all".}
\item{fraction}{ The fraction of the catch to plot. DIS for discards, LAN for landings. The default is to plot both if present}
\item{title}{Logical. adds a title to the outer margin}
\item{supsmu}{ Logical. Adds a running line smoother to the plotted points.}
\item{jitter}{ Logical. If "TRUE" jitters the plotted points, the default is false.}
\item{\dots}{other arguments, particularly those to \code{\link{plot}} and \code{\link{lines}}.}

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

Setting \code{supsmu=TRUE} fits a running line smoother, "super smoother" \code{\link{supsmu}}, 
to the plotted points. The \code{span} argument to this function, which takes values in the range (0-1) can be passed as an argument to \code{\dots}.  

If \code{jitter=TRUE} age class values will be jittered, see \code{\link{jitter}}.  

Other arguments that can be passed as \code{\dots} include 
\code{pch} for setting the plotting character and \code{col} for setting the colour or the plotted points. 
If \code{supsmu==TRUE} then the colour of the fitted lines can be set using 
\code{col.line}, \code{lty} sets the line type and \code{lwd} the line width. 
\code{pch}, \code{col}, \code{col.line}, \code{lwd} and \code{lty} can all be passed a vector as arguments to enable the differetiation between levels of a grouping variable. 
Setting \code{add=TRUE} enables overlaying of multiple plots and for example, can be used to compare the fitted lines for a number of different levels of a grouping variable. 
If \code{axes=FALSE} no axes will be plotted. 
\code{ylim} sets the limits for the y axes and 
\code{xlab} and \code{ylab} are for axes labels. 
Other graphical parameters include \code{main}, \code{sub}, \code{cex.main}, \code{cex.asis}, \code{cex.lab} e.t.c see \code{\link{par}} for more details. 

The outer margin default title is \"Length distribution of \emph{species} by \emph{grouping variable}\". This can be turned off if \code{title=FALSE}.
The figure titles default to the argument passed as \code{by} and, if specified, \code{level}. This can be overwritten by a call to \code{main}. \code{main=""} will result in no figure title.   

\code{par(mfrow=c(nrow,ncol))} can be used to adjust the number of plots per page to accommodate the multiple figures generated when the grouping variable has more than one level. 
                                                                                   
% ~~ If necessary, more details than the description above ~~
}
\value{
%  ~Describe the value returned
% If it is a LIST, use

A named list of the grouping variable and levels plotted, each component of which 
is itself a list with components x and y, the coordinates of the fitted SuperSmoother. 
See \code{\link{supsmu}} for details. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alastair Pout \email{a.pout@marlab.ac.uk} }
%\note{ ~~further notes~~ 


\seealso{\code{\link{ageLenMulti}} which plots age proportions at length.}
\examples{
%library(COSTdata)
data(cod2004cs)
# Plotting the age and length from the ca table for the cod data set 
agelenPlot(cod2004cs)

# and now grouped by area and adding a smooth to the plotted points 
par(mfrow=c(3,2))
agelenPlot(cod2004cs,by="area",col=2,supsmu=TRUE,col.line=3,lwd=2,jitter=TRUE)

# and on the same plot 
par(mfrow=c(1,1))
areas <-1:6
agelenPlot(cod2004cs,by="area",col=areas,pch=areas,supsmu=TRUE,col.line=areas,lwd=2,add=TRUE)
}

\keyword{hplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
