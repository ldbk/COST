\name{InterCatch_output}
\alias{makeICdf}
\alias{makeICfile}

\title{Create InterCatch Exchange Format file}
\description{
Converts a COST dbeOutput object of numbers at length or age into InterCatch Exchange Format Version 1.4 file
}


\usage{
makeICdf(dbeOutput, filename=NA, output.df = TRUE, append = FALSE,CANUMtype = "length", 
         Country, Species = NA, ReportingCategory, Usage = NA, DataToFrom = NA, 
         SamplesOrigin, InfoFleet = NA, InfoStockCoordinator = NA, InfoGeneral = NA, 
         Sex = "N", PlusGroup = -9, unitMeanWeight = "g", unitCANUM = "n", Maturity = NA)

makeICfile(ICdfs, filename, append=FALSE)
}

\arguments{
  \item{dbeOutput}{A COST dbeOutput object of numbers by length and/or age}
  \item{ICdfs}{list of 3 dataframes corresponding to InterCatch HI, SI and SD record types}
  \item{filename}{filename for output, if NA no file is written}
  \item{output.df}{logical, should list of dataframes be output}
  \item{append}{logical, should output be appended to existing file}
  \item{CANUMtype}{"age" for numbers at age, "lngt" or "length" for numbers at length}
  \item{Country}{The remaining arguments are character elements that match the InterCatch format description and are transferred directly to the output file}
  \item{Species}{If species is NA then it is taken from the dbeOutput, but note this is likely to be scientific name and InterCatch expects FAO code}
  \item{ReportingCategory}{ToDo}
  \item{Usage}{ToDo}
  \item{DataToFrom}{ToDo}
  \item{SamplesOrigin}{ToDo}
  \item{InfoFleet}{ToDo}
  \item{InfoStockCoordinator}{ToDo}
  \item{InfoGeneral}{ToDo}
  \item{Sex}{ToDo}
  \item{PlusGroup}{ToDo}
  \item{unitMeanWeight}{ToDo}
  \item{unitCANUM}{ToDo}
  \item{Maturity}{ToDo}

}
\details{
 makeICdf creates a list of 3 dataframes corresponding to InterCatch HI, SI and SD record types
 then if filename is not NA makeICfile is called and a CSV file output to filename.

 The process can be split into two parts by using makeICdf first, storing the list of dataframes
 and then calling makeICfile on the list.

 We expect the function to develop as InterCatch develops and expect more of the fields to be automatically filled in future versions.
}
\value{
A list of 3 dataframes if output.df = TRUE
A csv file if filename is not NA
}

\references{
InterCatch Exchange Format Version 1.4
\url{http://www.ices.dk/datacentre/InterCatch/IC-ExchFormat1-0\%20Doc1-4.pdf}
} 
 

\author{David Maxwell}


\seealso{\code{\link{dbeOutput}}
}
\examples{

# load example data set
data("LEMexample")
# Setup object for output
LEM.dbeOut.an = dbeObject(desc="Example",species="Microstomus kitt",param="landings",
                          strataDesc=LEM.strat, catchCat="LAN",methodDesc="analytical")

## Run vesselRaise function
LEM.dbeOut.an = vesselRaise.an (csObject = LEM.CScon, clObject = LEM.CLcon, dbeOutp = LEM.dbeOut.an)

# In two steps
# Create list with HI, SI, SD dataframes
LEM.ICdf <- makeICdf (LEM.dbeOut.an, Country = "UKE", Species = "LEM", ReportingCategory = "R", 
                      SamplesOrigin = "M")
# edit dataframes in LEM.ICdf if required then write InterCatch file
# Not run
makeICfile (LEM.ICdf, filename ="IC_LEM.csv")

# In one step, straight to file. output.df=F stops dataframes being saved within R
# Not run
makeICdf (LEM.dbeOut.an, filename ="IC_LEM.csv", output.df = FALSE, Country = "UKE", 
          Species = "LEM", ReportingCategory = "R", SamplesOrigin = "M")

}

\keyword{attribute}


