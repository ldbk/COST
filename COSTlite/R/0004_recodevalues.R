#' Map old values to new values for a vector
#'
#' @param x a vector to be mapped
#' @param from a vector of the old values
#' @param to a vector of the new values
#' @param warn_missing a boolean
#' @return a vector newly mapped
#'
#' @export
mapvalues<-function (x, from, to, warn_missing = FALSE) 
{
	if (length(from) != length(to)) {
		stop("`from` and `to` vectors are not the same length.")
    	}
    	if (!is.atomic(x)) {
	         stop("`x` must be an atomic vector.")
        }
        if (is.factor(x)) {
		levels(x) <- mapvalues(levels(x), from, to, warn_missing)
	        return(x)
	}
        mapidx <- match(x, from)
	mapidxNA <- is.na(mapidx)
	from_found <- sort(unique(mapidx))
	if (warn_missing && length(from_found) != length(from)) {
		message("The following `from` values were not present in `x`: ", 
		paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
	}
	x[!mapidxNA] <- to[mapidx[!mapidxNA]]
	x
}

#' Internal recoding function
#'
#' @param df dataframe
#' @param field truc
#' @param rec bidule
#' @return df
#' @export 
recFun <- function(df,field,rec) {
	Typ <- class(df[,field]) 
	fc <- factor(df[,field]) 
	df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)
	eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
	return(df)
}


