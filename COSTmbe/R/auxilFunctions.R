# Auxiliary R/Splus-functions that run both on R and Splus. 
# See auxilFunctionsR.r and auxilFunctionsS.r for language specific functions

paste0 <- function(...) paste(...,sep="")
catn <- function(...) cat(..., fill=T)

sep.find <- function(filename, skip=1){
# finds the field separator- and decimal character by reading the second line
# of <filename> and counting the occurences of ";", ",", and "."
# the most frequent is taken to be field separator character and the second 
# most decimal character
    x <- scann(filename, what="", sep="\n", n=1, skip=skip)
    if (calledFromR){
        splitt <- function(...) strsplit(...)
    } else {
        splitt <- function(...) unpaste(...)
        which.max <- function(x) (1:length(x))[x==max(x)][1]
    }
    n1 <- length(unlist(splitt(x,";")))			        # 1
    n2 <- length(unlist(splitt(x,",")))			        # 40
    n3 <- length(unlist(splitt(x,"\\.")))			# 7
    which.sep <- which.max(c(n1,n2,n3))				# 2
    which.dec <- which.max(c(n1,n2,n3)[-(1:which.sep)])		# 2
    sep <- c(";",",",".")[which.sep]				# ,
    dec <- c(";",",",".")[-(1:which.sep)][which.dec]		# .
    list(sep=sep, dec=dec)
}
