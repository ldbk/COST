\name{check-methods}
\docType{methods}
\alias{checkNms}
\alias{checkNms-methods}
\alias{checkNms,data.frame,character-method}
\alias{checkTys}
\alias{checkTys-methods}
\alias{checkTys,data.frame,list-method}
\alias{checkTRnms}
\alias{checkTRnms-methods}
\alias{checkTRnms,data.frame-method}
\alias{checkHHnms}
\alias{checkHHnms-methods}
\alias{checkHHnms,data.frame-method}
\alias{checkSLnms}
\alias{checkSLnms-methods}
\alias{checkSLnms,data.frame-method}
\alias{checkHLnms}
\alias{checkHLnms-methods}
\alias{checkHLnms,data.frame-method}
\alias{checkCAnms}
\alias{checkCAnms-methods}
\alias{checkCAnms,data.frame-method}
\alias{checkCLnms}
\alias{checkCLnms-methods}
\alias{checkCLnms,data.frame-method}
\alias{checkCEnms}
\alias{checkCEnms-methods}
\alias{checkCEnms,data.frame-method}
\alias{checkTRpk}
\alias{checkTRpk-methods}
\alias{checkTRpk,data.frame-method}
\alias{checkHHpk}
\alias{checkHHpk-methods}
\alias{checkHHpk,data.frame-method}
\alias{checkSLpk}
\alias{checkSLpk-methods}
\alias{checkSLpk,data.frame-method}
\alias{checkHLpk}
\alias{checkHLpk-methods}
\alias{checkHLpk,data.frame-method}
\alias{checkCApk}
\alias{checkCApk-methods}
\alias{checkCApk,data.frame-method}
\alias{checkCEpk}
\alias{checkCEpk-methods}
\alias{checkCEpk,data.frame-method}
\alias{checkCLpk}
\alias{checkCLpk-methods}
\alias{checkCLpk,data.frame-method}
\title{Check methods in COSTcore}
\description{Check methods are used to validate S4 classes. These functions are for internal usage only and are not intended to be called directly by users.}
\section{Methods}{
\describe{
	\item{checkNms}{\code{signature(object, names)}: Checks the names in a data.frame against a character vector.}
	\item{checkTys}{\code{signature(object, tys)}: Checks the column types in a data.frame against a list of class names.}
	\item{checkTRnms}{\code{signature(object)}: Wrapper for TR checkNms.}
	\item{checkHHnms}{\code{signature(object)}: Wrapper for HH checkNms.}
	\item{checkSLnms}{\code{signature(object)}: Wrapper for SL checkNms.}
	\item{checkHLnms}{\code{signature(object)}: Wrapper for HL checkNms.}
	\item{checkCAnms}{\code{signature(object)}: Wrapper for CA checkNms.}
	\item{checkCLnms}{\code{signature(object)}: Wrapper for CL checkNms.}
	\item{checkCEnms}{\code{signature(object)}: Wrapper for CE checkNms.}
	\item{checkTRpk}{\code{signature(object)}: Checks the TR primary key.}
	\item{checkHHpk}{\code{signature(object)}: Checks the HH primary key.}
	\item{checkSLpk}{\code{signature(object)}: Checks the SL primary key.}
	\item{checkHLpk}{\code{signature(object)}: Checks the HL primary key.}
	\item{checkCLpk}{\code{signature(object)}: Checks the CL primary key.}
	\item{checkCEpk}{\code{signature(object)}: Checks the CE primary key.}
}}
\keyword{methods}

