\name{check.fields}
\alias{check.fields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks fields of csData clData and ceData COST objects}
\description{
   This function is designed to perform variable checks on the fields of csData, ceData and clData COST objects. 
   It compares character variables against those specified in \emph{code.list} and the range 
   of numeric values against those specified in \emph{numeric.list}. 
   Error messages specifying any discrepancies are printed to the screen and optionally to a log file. 
   
}
\usage{
check.fields(costobj,logfile=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{costobj}{An object of class csData, ceData or clData  }
  \item{logfile}{Logical, if TRUE creates a logfile in the current working directory, the default is FALSE.}
}
%\details{ 
%to do 
%\cr csData objects consist of 5 nested data frames: tr, hh, sl, hl and ca 
%\cr ceData objects consist of a single data frames: ce. 
%\cr clData objects consist of a single data frames: cl.  
%\cr Each of the variables within these data frames are checked according to type (i.e. character or numeric) against the 
%appropriate entry in \emph{code.list} for character variables or \emph{numeric.list} for numeric variables. 
%The available options can thus be determined by a call to \emph{code.list\$variable} or the numeric range by 
%\emph{numeric.list\$variable} where variable is the name of the variable in question. The type assigned to 
%a variable is that specified in \emp{variable.list\$type}.
%\cr There are no penalties for incompatible entries, this function just highlights what they are. 
%\cr The COST data exchange format is based on the 25th January 2008 FishFrame 5.0 exchange format.  
%}
\value{
%  ~Describe the value returned
% If it is a LIST, use
Prints to the screen a summary of the entries in each field within the component data frames. 
Optionally prints a log file to the current working directory.  
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alastair Pout \email{a.pout@marlab.ac.uk} }
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with 
%\section{Warning }{The returned csobj is no longer in csData format.}
%}
\seealso{\code{\link{code.list}} for the expected character variables, \code{\link{numeric.list}} for the ranges of numeric variables, and 
\code{\link{variable.list}} for details of variable names, types and a brief description of each.}
\examples{
%library(COSTdata)    %modif MM
%data(cod2004cs)
%check.fields(cod2004cs)
%check.fields(cod2004cs,logfile=TRUE)
data(sole)
check.fields(sole.cs)
check.fields(sole.cs,logfile=TRUE)
}

\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
