# Check tools

startTest <- function(file="testReport.txt"){
	file(file, open="w")
}

finishTest <- function(con=getOption("con")){
	if(is.character(con)) con <- get(con)
	close(con)
}

tagTest <- function(tag="My tag !", con=getOption("con")){
	if(is.character(con)) con <- get(con)
	cat(tag, "\n", file=con)
	cat(date(), "\n", file=con)
	cat("========================\n\n", file=con)
}

setCon <- function(con="zz"){
	options(con=con)	
}

checkIdentical <- function(x, y, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkIdentical: ", identical(x,y), "; call: ", CALL, "\n", file=con)
}

checkEqual <- function(x, y, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkEqual: ", all.equal(x,y), "; call: ", CALL, "\n", file=con)
}

checkTrue <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkTrue: ", isTRUE(x), "; call: ", CALL, "\n", file=con)
}

checkFalse <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkTrue: ", !isTRUE(x), "; call: ", CALL, "\n", file=con)
}

checkFail <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	te <- try(x, TRUE)
	cat("+ checkFail: ", identical(is(te),"try-error"), "; call: ", CALL, "\n", file=con)
}

checkRun <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	te <- try(x, TRUE)
	cat("+ checkRun: ", !identical(is(te),"try-error"), "; call: ", CALL, "\n", file=con)
}

