\name{vesselRaise}
\alias{vesselRaise}
\alias{Vessel raising}
\alias{vesselRaise.boot}
\alias{vesselRaise.an}
\alias{vesselRaise.boot,csDataCons,clDataCons,dbeOutput-method}                             %modif MM
\alias{vesselRaise.an,csDataCons,clDataCons,dbeOutput-method}                               %modif MM


\title{Estimate numbers-at-age and numbers-at-length}
\description{
Estimate numbers-at-age and numbers-at-length from market sampling data with
 Sampling strategy: Length + ALK, Commercial categories within Trips
 Sampling source:  Auction/Market/Harbour Landed fraction
vesselRaise.boot provides variance estimates by bootstrapping the age and length samples. vesselRaise.an provides analytical variance estimates derived from the proportions in the ALK and variation in numbers per kg in the length samples.
}

\usage{
vesselRaise.an(csObject, clObject, dbeOutp, age.plus = -1, incl.precision=TRUE, probs=c(0.025,0.975))                                   
vesselRaise.boot(csObject, clObject, dbeOutp, B, age.plus = -1, bootMethod = "samples", incl.precision=TRUE, probs=c(0.025,0.975))
}

\arguments{
  \item{csObject}{A \emph{csDataCons} object of sampling data}
  \item{clObject}{A \emph{clDataCons} object of landings data}
  \item{dbeOutp}{A \emph{dbeOutput} object that provides sample information and stores the function's results.}
  \item{B}{numeric, length=1. Number of bootstrap iterations}
  \item{age.plus}{numeric, length=1. All ages greater than \code{age.plus} are set to \code{age.plus} to form a plus group}
  \item{bootMethod}{only acceptable value is "samples" to bootstrap samples.}
  \item{incl.precision}{Logical. If TRUE, 'dbeCalc' method function is internally called to compute CVs and CIs.}
  \item{probs}{Numeric vector of probabilities with values in [0,1]. Defines CI bounds (relevant only if \code{incl.precision=TRUE}). See \emph{dbeCalc}.}
}
\details{
 Estimates by length are provided for each combination of the space, time and technical strata specified in dbeOutp. Estimates by age are provided for each combination of the space and time strata because the biological data (ca table) does not include technical stratification.
 Estimation for mixed species landings is not implemented; the species in the biological data must match the taxon field in the landings data.
 Similarly, estimates are not given by sex if unsexed length data and sexed age data are used. Either use combined-sex length and age data or single-sex length and age data.
 If clDataCons includes adjustments to the landed weight these are applied as:
Total Landings = Official Landings * Multiplier + Unallocated Catch + Misallocated Catch
If the multiplier field is NA it is assumed to be one, if unallocated catch and misallocated catch are NA they are assumed to be zero.
}

\value{
  The following slots of dbeOutp are filled:
  \item{methodDesc}{Method of estimation}
  \item{nSamp}{Number of length samples by technical strata and number of age samples where technical is output as "AgeSamples"}
  \item{nMes}{Number of length measurements by technical strata and number of age measurements where technical is output as "Otoliths"}
  \item{lenStruc}{Numbers-at-length. For vesselRaise.boot bootstrap replicates are also given and iter=0 contains estimates from the original data.}
  \item{lenVar}{Variance of numbers-at-length.}
  \item{ageStruc}{Numbers-at-age. For vesselRaise.boot bootstrap replicates are also given and iter=0 contains estimates from the original data.}
  \item{ageVar}{Variance of numbers-at-age.}
  \item{totalN}{Total numbers. For vesselRaise.boot bootstrap replicates are also given and iter=0 contains estimates from the original data.}
  \item{totalNvar}{Variance of total numbers.}
  \item{totalW}{Total landed weight (kg)}
}
                                              

\references{ \url{http://wwz.ifremer.fr/cost} }
\author{ David Maxwell }
\note{
You should check the number of samples and age-length data are suitable before calculating estimates. vesselRaise.an stops if there are length classes with no age information. vesselRaise.boot fills in gaps that are 1 or 2 length groups wide with ages from surrounding length groups, see alkLgthRec(type="fillMiss", value=2).
 %~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ \code{\link{dbeOutput}, \link[COSTcore]{csDataCons}, \link[COSTcore]{clDataCons}, \link{dbeCalc}, \link{alkLgthRec} }}
\examples{
# load example data set
data("LEMexample")
# Setup objects for output
# LEM.strat was defined when creating consolidated object
LEM.dbeOut = dbeObject(desc="Example",species="Microstomus kitt",param="landings",
                       strataDesc=LEM.strat, catchCat="LAN",methodDesc="bootstrap samples")
LEM.dbeOut.an = dbeObject(desc="Example",species="Microstomus kitt",param="landings",
                          strataDesc=LEM.strat, catchCat="LAN",methodDesc="analytical")

# Run vesselRaise.boot function. B set to a very low number of iterations for demonstration only.
# example data set is very small so it generates warnings about the number of samples.
LEM.dbeOut = vesselRaise.boot (csObject = LEM.CScon, clObject = LEM.CLcon, 
                               dbeOutp = LEM.dbeOut, B = 3 )
dbePlotRep(LEM.dbeOut,"lenStruc") # outputs to 2 pages turn graph history on, to see the first page

# Run vesselRaise.an function
LEM.dbeOut.an = vesselRaise.an (csObject = LEM.CScon, clObject = LEM.CLcon, dbeOutp = LEM.dbeOut.an)
# CV (see function dbeCalc for full implementation of calculating CV and confidence intervals)
100*sqrt(LEM.dbeOut.an@ageVar$value)/LEM.dbeOut.an@ageStruc$estim$value

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}


