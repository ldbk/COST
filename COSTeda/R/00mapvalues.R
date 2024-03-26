
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
test00<-function(){
	aa<-c("a","a","a1","b","b","b2","c")
	rec<-data.frame(from=c("a","a","a1","b","b2","c"),to=c("a","a","a","b","b","c"))
	rec<-data.frame(from=c("a","a1","b","b2","c"),to=c("a","a","b","b","c"))
	rec<-data.frame(from=c("a1","b","b2","c"),to=c("a","b","b","c"))
	factor(aa,levels=rec$from,labels=rec$to)
	mapvalues(factor(aa),from=rec$from,to=rec$to)
	sort(unique(match(aa,rec$from)))
	rec$to
	aa<-factor(aa)
	levels(aa)
}

