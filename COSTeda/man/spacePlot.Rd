\name{spacePlot}
\alias{spacePlot,csData,character,character,function,ANY,ANY-method}
\alias{spacePlot,csData,character,character,function,character,ANY-method}
\alias{spacePlot,csData,character,character,function,character,character-method}
\alias{spacePlot,csData,character,character,function,missing,character-method}

\alias{spacePlot,clData,character,character,function,ANY,ANY-method}
\alias{spacePlot,clData,character,character,function,character,ANY-method}
\alias{spacePlot,clData,character,character,function,character,character-method}
\alias{spacePlot,clData,character,character,function,missing,character-method}
\alias{spacePlot,clDataCons,character,character,function,ANY,ANY-method}

\alias{spacePlot,ceData,character,character,function,ANY,ANY-method}
\alias{spacePlot,ceData,character,character,function,character,ANY-method}
\alias{spacePlot,ceData,character,character,function,character,character-method}
\alias{spacePlot,ceData,character,character,function,missing,character-method}
\alias{spacePlot,ceDataCons,character,character,function,ANY,ANY-method}


\alias{spacePlot,clDataCons,character,character,function,character,ANY-method}
\alias{spacePlot,clDataCons,character,character,function,character,character-method}
\alias{spacePlot,clDataCons,character,character,function,missing,character-method}
\alias{spacePlot,ceDataCons,character,character,function,character,ANY-method}
\alias{spacePlot,ceDataCons,character,character,function,character,character-method}
\alias{spacePlot,ceDataCons,character,character,function,missing,character-method}
\alias{spacePlot,csDataCons,character,character,function,ANY,ANY-method}
\alias{spacePlot,csDataCons,character,character,function,character,ANY-method}
\alias{spacePlot,csDataCons,character,character,function,character,character-method}
\alias{spacePlot,csDataCons,character,character,function,missing,character-method}


\alias{spacePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spatial plotting function}
\description{
Function that maps a numeric variable over spatial strata, grouped by a function. 
The possible spatial strata are ICES statistical rectangles, ICES areas, FAO areas 
and GSA management areas within the Mediterranean.
The grouping function can be any that can be passed as the \code{func} argument to tapply.  
If passed a COSTobject this function will plot any numeric variable within a specified object optionally by time and technical stratification. 
The geographic area covered extends to approximately to -50 degrees West, 70 degrees East longitude and 
 between 20 and 85 degrees of latitude.  
}
\usage{
spacePlot(costobj, variable, SpaceStrata, func, TimeStrata, TechStrata,\dots)  
%xlim = NULL, ylim = NULL, zlim = NULL, 
%xlab = NULL, ylab = NULL, breaks = NULL, maptype = "image", plotmap = TRUE, 
%overlay = FALSE, ices.divs = FALSE, depths = FALSE, statrects = FALSE, 
%fcoast = FALSE, landmass = FALSE, pch = 1, colour = TRUE, col.coast = "blue", 
%col.cont = "grey", col.pch = "red", col.rect = "grey", col.land = "snow2", 
%col.depth = "grey", col.text = 1, scale = FALSE, scale.title = "", 
%scale.cex = 0.6, scaleplace = "bottomright", scale.box="o",cex.max.bubble = 2, 
%threshold = 0, digits.text = 0, cex.text = 1, \dots)   %modif MM
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{costobj}{Optionally the name of a COST object of class either 'csData', 'clData' or 'ceData' or their validated and consolidated forms.}
  \item{variable}{The name of the numeric variable within the specified COST object to be plotted specified as a character string, e.g. "age" . 
  \cr If a COST object is missing a vector of the numeric variable to be plotted.}
  \item{SpaceStrata}{The name of the spatial strata over which \code{variable} is to be grouped specified as a character string. 
  Options are ICES statistical rectangle "rect", or ICES and FAO areas "area" if a COST object is specified.
  \cr If a COST object is missing a character vector of the same length as \code{variable} specifying the spatial strata. 
  \cr ICES rectangles have the form "34D2" 
  \cr ICES areas in have the form "IVa"   
  \cr FAO areas have the form "27.4.a" (FOA area 27 corresponds to ICES areas).
  \cr GSA areas within the Mediterranean are numbered "G3701" to "G3730". 
  \cr FRS sampling areas are prefixed with a six letter code the first three being FRS the second three specifying the sampling shceme:
\cr    FRSDMM demersal metier sampling area two and three digit codes e.g. "FRSDMM4O" "FRSDMM4aO"
  \cr    FRSDMS demersal sampling area two digit codes e.g. "FRSDMS05" 
  \cr    FRSMAC mackerel sampling area two digit codes e.g. "FRSMAC01"   
  \cr    FRSHER herring sampling area two digit codes e.g. "FRSHER01"
  \cr    FRSNEP Nephrops sampling area two letter codes e.g. "FRSNEPFL"
  \cr    FRSSCA scallop sampling area two letter codes e.g. "FRSSCAEC"
  
  }
  \item{func}{The name of the grouping function to be applied to \code{variable} over \code{SpaceStrata}. This can be any 
  function that can be passed as the \code{func} argument to tapply.
  }
  \item{TimeStrata}{When a COST object is specified the name of the temporal strata over which \code{variable} is to be grouped specified as a character string. 
  \cr The available options are "year", "quarter", and "month".}
  \item{TechStrata}{When a COST object is specified the name of the technical strata over which \code{variable} is to be grouped specified as a character string. 
  \cr The available options are "foCatEu5", "foCatEu6", (level 5 and level 6 respectively of the DCR matrix) and "foCatNat" a national gear specification.}   
  \item{\dots}{Additional arguments such as those to \code{plot}}}            %modif MM
\section{Other arguments}{
Other arguments are :                                                  %
  \itemize{                                                                   %
  \item{xlim}{Vector of c(min, max) for the longitude limits of the plotting area in decimal degrees. 
  Longitudes to the west of 0 degrees are expressed as negatives.}
  \item{ylim}{Vector of c(min, max) for the latitude limits of the plotting area in decimal degrees. }
  \item{zlim}{Vector of c(min, max) for the limits to \code{variable}}
  \item{xlab}{Label for the x axis}
  \item{ylab}{Label for the y axis}
  \item{breaks}{The breaks by which to split \code{variable}. The default is NULL which results in seq(zmin, zmax, length.out=8) being used.}
  \item{maptype}{One of "image", "contour", "bubble" or "values". The default is "image". 
  \cr "image" plots \code{variable} grouped by \code{func} as an image plot over the chosen \code{SpaceStrata}.
  \cr "contour" plots \code{variable} grouped by \code{func} as contour lines over the chosen \code{SpaceStrata}. 
  \cr "bubble" plots \code{variable} grouped by \code{func} as a scaled plotting character at the (approximate) 
  centre of the chosen \code{SpaceStrata}.
  \cr "values" plots \code{variable} grouped by \code{func} as numeric values at the (approximate) centre of the chosen \code{SpaceStrata}.}
  
  \item{plotmap}{Logical. If FALSE plots the map area but omits the "image", "contour", "bubble", or "values".}
  \item{overlay}{Logical. If TRUE adds to the proceeding plot.}
  \item{squarmap}{Logical. If TRUE produces a square plot area with side length equal to the longer of the x or y limits. If FALSE produces a maximum plot area map with x and y limits as specified. The default is FALSE}
  \item{area.lines}{Logical. If TRUE adds the boundary lines of ICES or FRS demsersal sampling areas.}
  \item{depths}{Vector of the form c(50,100,200,500). Adds the specified depth contour.}
  \item{statrects}{Logical. If TRUE adds ICES statistical rectangle division lines.}
  \item{fcoast}{Logical. If TRUE adds 1:250,000 scale coastline. Not available when landmass=TRUE.}
  \item{landmass}{Logical. If TRUE adds landmass polygons.}
  \item{pch}{Plotting character used for maptype="bubble", the default is pch=1.}
  \item{colour}{Logical. If TRUE the colours used for image plots are rev(heatcolors(n-1)) where n is the 
  length of breaks. If FALSE uses grey(rev(seq(0.1, 0.9, length.out =(n-1)))). }
  \item{col.coast}{Colour to be used for plotting the coastline, the default is "blue".}
  \item{col.cont}{Colour to be used for the contour lines, the default is "grey".}
  \item{col.pch}{Colour to be used for the plotting character, the default is "red".}
  \item{col.rect}{Colour to be used for statistical rectangle lines, the default is "grey".}
  \item{col.land}{Colour to be used for the landmass polygons, the default is "white".}
  \item{col.depth}{Colour to be used for the depth contour lines, the default is "grey".}
  \item{col.text}{Colour to be used for values when maptype="values", the default is 1 (black).}
  \item{scale}{Logical. If TRUE a scale is added to the plot.}
  \item{scale.title}{Title for the scale, the default is no title.}
  \item{scale.cex}{Cex to be used for the size of the scale box.}
  \item{scaleplace}{Location of the scale, default is "bottomright", available options are "bottomright", "bottom", "bottomleft", "left",
   "topleft", "top", "topright", "right" and "center".}
  \item{scale.box}{The type of box to be drawn around the scale, "o" the default or "n" none.}
  \item{cex.max.bubble}{Maximum size of the plotting character when maptype="bubble", the default is 2.}
  \item{threshold}{Only relevant when maptype="values". The value below which points are shown with a "+" 
  symbol rather than their actual value. The default is zero.}
  \item{digits.text}{Number of decimal places to print when maptype="values", the default is 0.}
  \item{cex.text}{Cex to be used for printed values when maptype="values". The default is 1.}
  }}                                                                        %modif MM
  %\item{\dots}{Additional arguments such as those to \code{plot}}          %
  %}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}

%\references{ ~put references to the literature/web site here ~ }
\author{Alastair Pout  \email{a.pout@marlab.ac.uk}, Liz Clarke  \email{e.d.clarke@marlab.ac.uk}}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{\code{\link{strataSpacePlot}} for the plotting of COST objects by spatial strata}
\examples{
#====================================================

# The default
spacePlot()


#  Plotting numeric variables and spatial strata....
#  by ICES statistical rectangle,  

rects <-c("45E2","44E3","46E4","44E0","41E1")
values <-c(234,358,127,530,442)

spacePlot(values,rects,mean,maptype="values",statrects=TRUE,landmass=TRUE,
col.land="snow2",col.coast="blue",col.text=2,)

# and by ICES area. 

areas <-c("IVa","IVb","IVc","VIa","VIIa")
values <-c(234,358,127,530,442)

spacePlot(values,areas,mean,maptype="image",statrects=TRUE,landmass=TRUE,
col.land="snow2",col.coast="blue",col.text=2,scale=T,scale.title="Values by ICES area")


#===================================================
# plotting COST objects  

%library(COSTdata)                        %modif MM
data(cod2004ce)
# Effort by Scottish vessels for cod in 2004: 
#   number of trips by ICES statistical rectangle  
spacePlot(cod2004ce,"trpNum","rect",sum,statrects=TRUE,scale.title="Number of trips")
title("Effort by Scottish vessels for cod in 2004")


# Landings by Scottish vessels of cod in 2004: 
#   landed weight by ICES statistical rectangle as a bubble plot   
data(cod2004cl)
spacePlot(cod2004cl,"landWt","rect",sum,maptype="bubble",statrects=TRUE,
          scale.title="Landed weight (kg)")
title("Landings by Scottish vessels of cod in 2004") 



# A comparison of landed weight and number of fish measured by ICES rectangle
data(cod2004cl)
data(cod2004cs)

# first plotting out landings to get the full spatial extent.
spacePlot(cod2004cl,"landWt","rect",sum,maptype="bubble",col.pch=1,cex.max.bubble=4
,scaleplace="topleft",scale=TRUE,scale.box="n",scale.title="Landed wgt (kg)")

# then overlay numbers of measured fish
spacePlot(cod2004cs,"lenNum","rect",sum,maptype="bubble",pch=16,
overlay=TRUE,cex.max.bubble=4,scaleplace="bottomleft",
scale=TRUE,scale.box="n",scale.title="Measured fish")

# redoing the landings plot to show landings < sampling  

spacePlot(cod2004cl,"landWt","rect",sum,maptype="bubble",
col.pch=1,cex.max.bubble=4,scaleplace="topleft",scale=TRUE,
overlay=TRUE,scale.box="n")

title(sub="Landed weight and number of fish measured, cod 2004") 

#===================================================
# plotting COST objects by time and temporal stratification 

# FRS market sampled cod: maximum age by rect and quarter 
spacePlot(cod2004cs,"age","rect",max,TimeStrata="quarter",
statrects=TRUE,nplots=4,maptype="image",landmass=TRUE,col.land="lightgrey",col.text=2)
 
%# CEFAS gear codes: landed weight by ICES rectangle                            %modif MM
%par(ask=TRUE)                                                                  %
%data(CEFAS2006cl)                                                              %
%spacePlot(CEFAS2006cl,"landWt","rect",mean,TechStrata="foCatNat",nplots=6)     %
%par(ask=FALSE)                                                                 %

%# IFREMER effort for sole:  number of trips by gear code (DCR level 5) and quarter by FAO area 
%par(ask=TRUE)
%data(sole)
%spacePlot(sole.ce,"trpNum","area",sum,TimeStrata="quarter",TechStrata="foCatEu5",nplots=4)
%par(ask=FALSE)

}
\keyword{hplot}

