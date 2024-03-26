\name{edaResult-class}
\docType{class}
\alias{edaResult}
\alias{edaResult-class}
\title{Class "edaResult"}
\description{Any object of this class is created by a procedure from \emph{COSTeda} package. Its content depends on \emph{desc} slot that refers to mother function. 
Most often it can be called by a plotting procedure (e.g \emph{plot} or \emph{boxplot}).}

\section{Slots}{
\describe{
    \item{\code{desc}:}{Object of class \code{"character"}}
    \item{\code{outPut}:}{Object of class \code{"ANY"}}
  }
\tabular{lllll}{
\bold{slot} \tab \bold{desc} \tab \bold{elements} \tab \bold{class} \tab \bold{description} \cr
\bold{\code{desc}} \tab \tab \tab \code{character} \tab Distinctive reference to mother function \cr
\bold{\code{outPut}} \tab \tab \tab \code{ANY} \tab Information stored in the object. It depends \cr
                                \tab \tab \tab \tab on \emph{desc} slot. \cr
 
\tab csRelativeValue \tab \tab \code{data.frame} \tab Stratified relative values calculated with \cr
                                  \tab \tab \tab \tab \emph{relativeValue} procedure applied to 'cs'.\cr
 
\tab clceRelativeValue \tab \tab \code{data.frame} \tab Stratified relative values calculated with \cr 
                               \tab \tab \tab \tab \emph{relativeValue} procedure applied to 'cl' or 'ce'.\cr

\tab sampDeltaCalc \tab \tab \code{list} \tab Result from \emph{deltCalc} procedure.\cr
 \tab \tab species \tab \code{character} \tab Specified species. \cr
 \tab \tab fraction \tab \code{character} \tab Catch category. \cr
 \tab \tab strategy \tab \code{character} \tab Chosen strategy for Delta calculation. \cr
 \tab \tab timeStrata \tab \code{character} \tab Specified time stratification. \cr
 \tab \tab spaceStrata \tab \code{character} \tab Specified space stratification. \cr
 \tab \tab techStrata \tab \code{character} \tab Specified technical stratification. \cr
 \tab \tab DeltaMatrix \tab \code{array} \tab Sum of squared delta values of each sample \cr
                              \tab \tab \tab \tab within each strata and length class (only if \cr 
                              \tab \tab \tab \tab \code{indSamp=FALSE} in \code{deltCalc} function call).\cr
 \tab \tab NkMatrix \tab \code{array} \tab Number of samples within each strata (only \cr 
                           \tab \tab \tab \tab if \code{indSamp=FALSE} in \code{deltCalc} function call). \cr
 \tab \tab WkMatrix \tab \code{array} \tab Sampled weight within each strata in grams \cr
                           \tab \tab \tab \tab (only if \code{indSamp=FALSE} in \code{deltCalc} \cr
                           \tab \tab \tab \tab function call). \cr
 \tab \tab SampDeltaMat \tab \code{data.frame} \tab Data.frame of delta values within each  \cr 
                               \tab \tab \tab \tab sample (only if \code{indSamp=TRUE} in \cr 
                               \tab \tab \tab \tab \code{deltCalc} function call). \cr
 \tab \tab tab \tab \code{data.frame} \tab Data.frame resulting from treatment (only if \cr 
                      \tab \tab \tab \tab \code{indSamp=TRUE} in \code{deltCalc} function call). \cr
 \tab \tab DFsamp \tab \code{data.frame} \tab Informations about each sample (only if \cr
                         \tab \tab \tab \tab \code{indSamp=TRUE} in \code{deltCalc} function call). \cr

\tab sampDeltaId \tab \tab \code{list} \tab Result from identification process of \cr 
                   \tab \tab \tab \tab \emph{edaResult} object with \emph{desc="sampDeltaCalc"} \cr
                   \tab \tab \tab \tab plotting procedure.\cr
 \tab \tab species \tab \code{character} \tab Specified species. \cr
 \tab \tab fraction \tab \code{character} \tab Catch category. \cr
 \tab \tab sampId \tab \code{data.frame} \tab Specification of identified samples. \cr
 \tab \tab tabId \tab \code{data.frame} \tab Length distribution data from identified \cr
                         \tab \tab \tab \tab samples. \cr
 \tab \tab tab \tab \code{data.frame} \tab Resulting data from \code{deltCalc} procedure.\cr   
 
\tab landisVol \tab \tab \code{list} \tab Result from \emph{landisVol} procedure.\cr
 \tab \tab species \tab \code{character} \tab Specified species. \cr
 \tab \tab fraction \tab \code{character} \tab Catch category. \cr
 \tab \tab strategy \tab \code{character} \tab Chosen strategy for Delta calculation. \cr
 \tab \tab timeStrata \tab \code{character} \tab Specified time stratification. \cr
 \tab \tab spaceStrata \tab \code{character} \tab Specified space stratification. \cr
 \tab \tab techStrata \tab \code{character} \tab Specified technical stratification. \cr
 \tab \tab \code{VolFO_FDTR} \tab \code{list} \tab Catch weight by FO for each fishing day of \cr
                               \tab \tab \tab \tab each trip. \cr
 \tab \tab \code{MeanFO_FDTR} \tab \code{numeric} \tab Mean FO-catch weight for each fishing day \cr
                                   \tab \tab \tab \tab of each trip. \cr
 \tab \tab \code{VolFD_TR} \tab \code{list} \tab Catch weight by fishing day for each trip and \cr
                             \tab \tab \tab \tab each strata, raised by numbers of FO. \cr
 \tab \tab \code{MeanFD_TR} \tab \code{numeric} \tab Mean FD-raised catch weight by trip and \cr
                                 \tab \tab \tab \tab strata. \cr
 
\tab alMulti \tab \tab \code{list} \tab Result from \emph{ageLenMulti} procedure.\cr
 \tab \tab timeStrata \tab \code{character} \tab Time stratification field. \cr
 \tab \tab spaceStrata \tab \code{character} \tab Space stratification field. \cr
 \tab \tab techStrata \tab \code{character} \tab Technical stratification field. \cr
 \tab \tab Mm \tab \code{multinomial} \tab A \pkg{nnet} package object resulting from \cr
                       \tab \tab \tab \tab \emph{multinom} procedure. \cr
 \tab \tab dat \tab \code{data.frame} \tab Age-at-Length data for specified stratification.  \cr
                       \tab \tab \tab \tab Predictors used in model formula expression \cr
                       \tab \tab \tab \tab were taken from this table. \cr
 \tab \tab age \tab \code{matrix} \tab Extract of \emph{dat} table. Response matrix used in \cr
                    \tab \tab \tab \tab model formula expression. \cr
}}

\author{Mathieu Merzereaud}

\seealso{\code{\link{plot.edaResult}}, \code{\link{boxplot.edaResult}}, \code{\link{relativeValue}}, \code{\link{deltCalc}}, \code{\link{landisVol}}, \code{\link{ageLenMulti}}
}

\examples{
showClass("edaResult")
}

\keyword{classes}

