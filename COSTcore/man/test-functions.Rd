\name{Tests}
\alias{checkTrue}
\alias{checkFalse}
\alias{checkRun}
\alias{checkFail}
\alias{checkIdentical}
\alias{checkEqual}
\alias{startTest}
\alias{finishTest}
\alias{tagTest}
\alias{setCon}
\title{Functions to be used in tests.}
\description{These functions are used to set up unit tests that are similar to those implemented by the package \pkg{RUnit}, the aim being to implement methods that follow sQuote{Extreme Programming} best practices. The objectives of these functions are to test the functionality of the software and establish consistent debugging routines throughout the development process. For more about sQuote{Extreme Programming} check Wikipedia's page \url{http://en.wikipedia.org/wiki/Extreme_Programming}.}
\usage{
setCon(con="zz")
startTest(file="testReport.txt")
tagTest(tag="My tag !", con=getOption("con"))
checkTrue(x, con = getOption("con"))
checkFalse(x, con = getOption("con"))
checkIdentical(x, y, con = getOption("con"))
checkRun(x, con = getOption("con"))
checkFail(x, con = getOption("con"))
checkEqual(x, y, con = getOption("con"))
finishTest(con=getOption("con"))
}
\arguments{
  \item{x}{R object to be used in test. It maybe an expression for \code{checkTrue}, \code{checkFalse}, \code{checkRun} or \code{checkFail} or an object to compared to \code{y} with \code{checkIdentical} or \code{checkEqual}.}
  \item{y}{R object to be compared to \code{x} in tests \code{checkIdentical} or \code{checkEqual}.}
  \item{con}{Name of communication.}
  \item{file}{Report file name.}
  \item{tag}{A tag.}
}
\details{
\describe{
  \item{setCon}{Sets the name of connection to a file.}
  \item{startTest}{Starts test by creating the report file and opening the connection to the file as defined by sQuote{con}.}
  \item{tagTest}{Tags the test for future reference. Automatically includes date and time.}
  \item{checkTrue}{Checks \sQuote{x} expression is \code{TRUE} and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{checkFalse}{Checks \sQuote{x} expression is \code{FALSE} and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{checkIdentical}{Checks \sQuote{x} and \sQuote{y} are \code{identical} and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{checkRun}{Checks \sQuote{x} expression runs successfully and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{checkFail}{Checks \sQuote{x} expression fails to run and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{checkEqual}{Checks \sQuote{x} and \sQuote{y} are \code{equal} and reports to the file opened by the connection defined in \sQuote{con}.}
  \item{finishTest}{Closes the connection defined by \sQuote{con}.}
}}
\value{A report with the results of the tests.}
\references{These functions were originally developed for FLR (\url{http://flr-project.org}).}
\author{FLCore Team and Ernesto Jardim \email{ernesto@ipimar.pt}}
\note{A simple way to implement these tests is to set the result to \code{TRUE}, so that any \code{FALSE} results are immediately apparent. 

The test must be set up in a single file that is to be used during \code{R CMD check} and should be placed on directory \sQuote{tests}. The procedure to set up a test is: 
  \enumerate{
     \item{1}{Set up the connection with \code{setCon()}. The name of the connection can be changed but has to be consistent in all functions.}
     \item{2}{Start the test, for example: \code{zz <- startTest("clDataTest.txt")}}
     \item{3}{Tag the test for later reference: \code{tagTest("1, 2, testing ...")}}
     \item{4}{Test expressions and comparisons with the \code{check} functions, \emph{e.g.} \code{checkRun(mm <- rnorm(100))}}
     \item{5}{To finish test close the connection to the report file with \code{finishTest()}}
  }
}
\keyword{programming}
\keyword{error}
\keyword{utilities}
\keyword{debugging}

