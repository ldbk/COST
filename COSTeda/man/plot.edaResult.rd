\name{plot.edaResult}
\docType{methods}
\alias{plot,edaResult,missing-method}
\alias{plot,edaResult,edaResult-method}
\alias{plot.edaResult}
\title{Plot "edaResult" object}
\description{
This method is a generic function for plotting \emph{edaResult} objects. These objects result from a \emph{COSTeda} package function call.
}


\section{Methods}{
\describe{
	\item{plot}{\code{signature("edaResult","missing")}: plotting procedure of an object of class \emph{edaResult} with \emph{desc} slot equal to "csRelativeValue",
  "clceRelativeValue", "sampDeltaCalc", "sampDeltaId", "landisVol" or "alMulti".}
  \item{plot}{\code{signature("edaResult","edaResult")}: plotting procedure of two objects of class \emph{edaResult} with \emph{desc} slot equal to "csRelativeValue" or
  "clceRelativeValue".}

}}

\section{Usage of plot.edaResult method according to 'desc' slot in input object :}{
\tabular{llll}{
\bold{desc} \tab \bold{parameters} \tab \bold{default value} \tab \bold{description} \cr
\bold{\code{"csRelativeValue"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="csRelativeValue"} \cr
    \tab \tab \tab (see \code{relativeValue} method). \cr
 \tab \dots \tab  \tab Further graphical parameters. \cr
\bold{\code{"clceRelativeValue"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="clceRelativeValue"} \cr
    \tab \tab \tab (see \code{relativeValue} method). \cr
 \tab \dots \tab  \tab Further graphical parameters. \cr
\bold{\code{"sampDeltaCalc"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="sampDeltaCalc"} \cr
    \tab \tab \tab (see \code{deltCalc} method). \cr
 \tab elmts \tab list(tp="all", \tab List of levels for specified stratification to be  \cr
       \tab \tab sp="all",tc="all") \tab displayed on the graph. \cr
 \tab strat1 \tab \tab Optionnal. To be chosen between \code{"timeStrata"}, \cr
        \tab \tab \tab \code{"spaceStrata"} and \code{"techStrata"}. Primary  \cr
        \tab \tab \tab stratification for graphical display.\cr
 \tab strat2 \tab \code{"NULL"} \tab Optionnal. To be chosen between \code{"timeStrata"}, \cr
   \tab \tab \tab \code{"spaceStrata"} and \code{"techStrata"}. Secondary \cr
   \tab \tab \tab stratification for graphical display.\cr   
 \tab selection \tab \code{FALSE} \tab If \code{TRUE}, outliers identification is made, and an \cr
                        \tab \tab \tab \emph{edaResult} object with \code{desc="sampDeltaId"} is \cr 
                        \tab \tab \tab returned. Displayed values during identification \cr
                        \tab \tab \tab process are taken from 'SampNum' field from \cr
                        \tab \tab \tab \code{sampId} returned data.frame.\cr
 \tab show.legend \tab \code{"right"} \tab Display the legend (\code{""} means "no legend").\cr
 \tab shift \tab \code{FALSE} \tab If \code{TRUE}, displayed text is shifted.\cr
 \tab Kplot \tab \code{FALSE} \tab If \code{TRUE}, condition factors 'K' are plotted instead of delta values.\cr 
 \tab \dots \tab \tab Further graphical parameters. \cr
\bold{\code{"sampDeltaId"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="sampDeltaId"} \cr
    \tab \tab \tab (see \code{plot.edaResult} method for \cr
    \tab \tab \tab \emph{desc="sampDeltaCalc"}). \cr
 \tab smpNum \tab \code{"all"} \tab Character specifying sample Id(s) as displayed \cr
                     \tab \tab \tab during outliers identification process ("all" is \cr
                     \tab \tab \tab a shortcut to display all identified samples). \cr
 \tab show.legend \tab \code{"right"} \tab Display the legend (\code{""} means "no legend").\cr
 \tab \dots \tab \tab Further graphical parameters. \cr
\bold{\code{"landisVol"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="landisVol"} \cr
    \tab \tab \tab (see \code{landisVol} method). \cr
 \tab type \tab \code{"FD"} \tab Character. If type="FO", mean FO-catch weight \cr
                  \tab \tab \tab for each fishing day of each trip will be \cr
                  \tab \tab \tab displayed. Otherwise (i.e type="FD"), mean \cr
                  \tab \tab \tab fishing-day-catch weight by trip will be displayed. \cr
 \tab groups \tab \code{NULL} \tab Only for \code{type="FD"}. Character specifying \cr
                    \tab \tab \tab intra-graph stratification (to be chosen between \cr
                    \tab \tab \tab \code{"timeStrata"}, \code{"techStrata"}, \code{"spaceStrata"} \cr
                    \tab \tab \tab and \code{NULL}). \cr
 \tab \dots \tab \tab Further graphical parameters. \cr
\bold{\code{"alMulti"}} \tab \tab \tab  \cr
 \tab x \tab  \tab \emph{edaResult} object with \emph{desc="alMulti"} \cr
    \tab \tab \tab (see \code{ageLenMulti} method). \cr
 \tab grps \tab \code{NULL} \tab Character or NULL. Strata to be differentiated in \cr 
                  \tab \tab \tab each panel. \code{NULL} means one graph per crossed \cr
                  \tab \tab \tab strata (to be chosen between "timeStrata", \cr
                  \tab \tab \tab "spaceStrata", "techStrata" and NULL).\cr
 \tab show.legend \tab \code{"right"} \tab Display the legend (\code{""} means "no legend").\cr
 \tab \dots \tab \tab Further graphical parameters. \cr
}
}
        
\author{Mathieu Merzereaud}

\seealso{\code{\link{edaResult}}, \code{\link{relativeValue}}, \code{\link{deltCalc}}, \code{\link{landisVol}}, \code{\link{ageLenMulti}}, \code{\link{plot}}, \code{\link{boxplot.edaResult}}, \code{\link{GraphsPar}}
}

\examples{
#desc="csRelativeValue" or/and "clceRelativeValue"
data(sole)
sole.cs.val <- csDataVal(sole.cs)
sole.cl.val <- clDataVal(sole.cl) 
strD <- strIni(timeStrata="month",spaceStrata="area",techStrata="commCat")
CS <- relativeValue(sole.cs.val,strD,"nbSamp")
CL <- relativeValue(sole.cl.val,strD)

plot(CS)
plot(CS,CL)


#desc="sampDeltaCalc"
strD <- strIni(timeStrata="quarter",techStrata="commCat")
dlt <- deltCalc(sole.cs,strD,"Solea solea",strategy="cc",indSamp=TRUE)

plot(dlt,strat1="techStrata",strat2="timeStrata")

  
##desc="sampDeltaId"
#sl <- plot(dlt,strat1="techStrata",strat2="timeStrata",selection=TRUE)  
#plot(sl)


#desc="landisVol"
df <- subset(sole.cs,sampType=="S")  #only sea sampling data is kept
ldV <- landisVol(df,strIni(techStrata="foCatEu5",timeStrata="quarter"),
                 species="Solea solea")

plot(ldV,rot=20,cex.lab=0.8)
plot(ldV,groups="techStrata")
plot(ldV,type="FO")


#desc="alMulti"
strD <- strIni(timeStrata="quarter",spaceStrata="area")
aLM <- ageLenMulti(sole.cs.val,strD,elmts=list(tp=c("2","3"),sp="all",tc="all"),age.plus=6)

plot(aLM,grps="timeStrata",l.col=c("steelblue","violetred2"))

}

\keyword{dplot}
