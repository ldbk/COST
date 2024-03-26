\name{accessor-methods}
\docType{methods}
\alias{tr}
\alias{tr-methods}
\alias{tr,csData-method}
\alias{tr,csDataCons-method}
\alias{hh}
\alias{hh-methods}
\alias{hh,csData-method}
\alias{hh,csDataCons-method}
\alias{sl}
\alias{sl-methods}
\alias{sl,csData-method}
\alias{sl,csDataCons-method}
\alias{hl}
\alias{hl-methods}
\alias{hl,csData-method}
\alias{hl,csDataCons-method}
\alias{ca}
\alias{ca-methods}
\alias{ca,csData-method}
\alias{ca,csDataCons-method}
\alias{ce}
\alias{ce-methods}
\alias{ce,ceData-method}
\alias{ce,ceDataCons-method}
\alias{cl}
\alias{cl-methods}
\alias{cl,clData-method}
\alias{cl,clDataCons-method}
\alias{desc}
\alias{desc-methods}
\alias{desc,csData-method}
\alias{desc,clData-method}
\alias{desc,ceData-method}
\alias{desc,csDataCons-method}
\alias{desc,clDataCons-method}
\alias{desc,ceDataCons-method}
\title{Accessor methods in COSTcore}
\description{These accessor methods can be used to obtain specific slots from a COST object and are designed to implement "encapsulation" within S4 classes. Users are strongly advised to apply these functions when accessing their data to ensure compatibility with any future changes to the class definition.}
\section{Methods}{
  \describe{
	\item{tr}{\code{signature(object = csData or csDataVal or csDataCons)}: \code{tr} slot accessor.}
	\item{hh}{\code{signature(object = csData or csDataVal or csDataCons)}: \code{hh} slot accessor.}
	\item{sl}{\code{signature(object = csData or csDataVal or csDataCons)}: \code{sl} slot accessor.}
	\item{hl}{\code{signature(object = csData or csDataVal or csDataCons)}: \code{hl} slot accessor.}
	\item{ca}{\code{signature(object = csData or csDataVal or csDataCons)}: \code{ca} slot accessor.}
	\item{ce}{\code{signature(object = ceData or ceDataVal or ceDataCons)}: \code{ce} slot accessor.}
	\item{cl}{\code{signature(object = clData or clDataVal or clDataCons)}: \code{cl} slot accessor.}
	\item{desc}{\code{signature(object = csData or csDataVal or csDataCons)}: description slot accessor.}
}}
\examples{
obj <- csData()
tr(obj)
hh(obj)
sl(obj)
hl(obj)
ca(obj)
obj <- ceData()
ce(obj)
obj <- clData()
cl(obj)
}
\keyword{methods}

