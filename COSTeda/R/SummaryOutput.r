#####################
##                 ##
##  Summary(plus)  ##
##                 ##
## MM  07/02/2008  ##
#####################



setGeneric("cstSummary", function(object,
                                  tab="tr",
                                  sizeMax=20,
                                  except=NULL,
                                  ...){
	standardGeneric("cstSummary")}
)


################################################################################
################################################################################


proc.cstSummary <- function(dat,
                            sizeMax,
                            except=NULL,
                            indFactor,...) {   #subprocedure

#'indFactor' fields as factors
sapply(indFactor,function(x) dat[,x] <<- factor(as.character(dat[,x])))
dat <- dat[,!names(dat)%in%except]
classes <- sapply(1:ncol(dat),function(x) class(dat[,x]))
vsum1 <- summary(dat,maxsum=nrow(dat)+2)
vsum2 <- summary(dat,maxsum=sizeMax)
index <- (1:ncol(vsum2))[classes=="factor"]

#sorting process
ord <- function(x) {val <- as.numeric(sapply(x,function(x) strsplit(x,":")[[1]][2]))
                    val[val==0] <- x[val==0] <- NA
                    indNA <- grep("NA's",x)
                    indOth <- grep("(Other)",x)
                    val[indNA] <- val[indOth] <- 0
                    indic <- order(val,decreasing=TRUE)
                    return(x[indic])}
                    
invisible(sapply(index,function(x) vsum1[,x] <<- ord(vsum1[,x])))
invisible(sapply(index,function(x) vsum2[,x] <<- ord(vsum2[,x])))

#take off useless lines
test1 <- apply(vsum1,1,function(x) all(is.na(x)))
test2 <- apply(vsum2,1,function(x) all(is.na(x)))
vsum1 <- as.table(vsum1[!test1,])
vsum2 <- as.table(vsum2[!test2,])

#according to output table size
if (nrow(vsum1)<=sizeMax) 
  return(vsum1) 
else {
  warning(paste("Complete summary tab size exceeds sizeMax!! (",as.character(nrow(vsum1))," rows) ",sep=""))
  return(vsum2)}
}


################################################################################
################################################################################



setMethod("cstSummary", signature(object="csData"), function(object,
                                                             tab="tr",
                                                             sizeMax=20,
                                                             except=NULL,
                                                             ...){
eval(parse('',text=paste("dat <- object@",tab,sep="")))
listFact <- list(tr=1:16,hh=c(1:14,19:29),sl=c(1:14,17),hl=1:15,ca=c(1:21,23:25,27,29:31))
proc.cstSummary(dat,sizeMax,except,listFact[[tab]])                                       
})



setMethod("cstSummary", signature(object="csDataVal"), function(object,
                                                                tab="tr",
                                                                sizeMax=20,
                                                                except=NULL,
                                                                ...){
eval(parse('',text=paste("dat <- object@",tab,sep="")))
listFact <- list(tr=1:16,hh=c(1:14,19:29),sl=c(1:14,17),hl=1:15,ca=c(1:21,23:25,27,29:31))
proc.cstSummary(dat,sizeMax,except,listFact[[tab]])                                       
})



setMethod("cstSummary", signature(object="csDataCons"), function(object,
                                                                 tab="tr",
                                                                 sizeMax=20,
                                                                 except=NULL,
                                                                 ...){
eval(parse('',text=paste("dat <- object@",tab,sep="")))
listFact <- list(tr=2:14,hh=3:16,sl=c(4:15,18),hl=4:16,ca=c(3:17,19:21,23,25:27))
proc.cstSummary(dat,sizeMax,except,listFact[[tab]])                                       
})






setMethod("cstSummary", signature(object="clData"), function(object,
                                                             tab="missing",
                                                             sizeMax=20,
                                                             except=NULL,
                                                             ...){
dat <- object@cl
proc.cstSummary(dat,sizeMax,except,1:17)
})



setMethod("cstSummary", signature(object="clDataVal"), function(object,
                                                                tab="missing",
                                                                sizeMax=20,
                                                                except=NULL,
                                                                ...){
dat <- object@cl
proc.cstSummary(dat,sizeMax,except,1:17)
})



setMethod("cstSummary", signature(object="clDataCons"), function(object,
                                                                 tab="missing",
                                                                 sizeMax=20,
                                                                 except=NULL,...){
dat <- object@cl
proc.cstSummary(dat,sizeMax,except,1:9)
})





setMethod("cstSummary", signature(object="ceData"), function(object,
                                                             tab="missing",
                                                             sizeMax=20,
                                                             except=NULL,
                                                             ...){
dat <- object@ce
proc.cstSummary(dat,sizeMax,except,1:12)
})



setMethod("cstSummary", signature(object="ceDataVal"), function(object,
                                                                tab="missing",
                                                                sizeMax=20,
                                                                except=NULL,
                                                                ...){
dat <- object@ce
proc.cstSummary(dat,sizeMax,except,1:12)
})



setMethod("cstSummary", signature(object="ceDataCons"), function(object,
                                                                 tab="missing",
                                                                 sizeMax=20,
                                                                 except=NULL,
                                                                 ...){
dat <- object@ce
proc.cstSummary(dat,sizeMax,except,1:4)
})




################################################################################################################################
# tapply-like methods to display information from csDataVal, clDataVal, ceDataVal, csDataCons, clDataCons & ceDataCons objects #
################################################################################################################################


setGeneric("disInfo", function(object,...){
	standardGeneric("disInfo")
	}
)


################################
################################
##  Validated data structure  ##
################################
################################



setMethod("disInfo", signature("csDataVal"), function(object,path,field,by,fun,...,biopar=FALSE,transpose=FALSE,title="",append=TRUE) {

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)==0) stop("Missing 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")

allFields <- c(field,by)
if (biopar) {
  TAB <- ca(object)
  if (any(!allFields%in%names(TAB))) stop("some parameters don't match with CA table!!") 
} else {
#"month", "quarter" & "semester" are put in HH
  HH <- hh(object)
  HH$month <- as.numeric(sapply(HH$date,function(x) strsplit(as.character(x),"-")[[1]][2]))
  HH$quarter <- ceiling(HH$month/3) ; HH$semester <- ceiling(HH$month/6)
#step by step, we merge all the tables from tr to hl until all specified fields are in
  go <- TRUE
#1st step
  tab <- tr(object)
  if (all(allFields%in%names(tab))) go <- FALSE
#2nd step
  if (go) {
    tab <- tab[tab$sampType!="V",]
    tab <- merge(HH,tab,by=c("sampType","landCtry","vslFlgCtry","year","proj","trpCode"),sort=FALSE,all=TRUE)
  }
  if (all(allFields%in%names(tab))) go <- FALSE
#3rd step
  if (go) tab <- merge(sl(object),tab,by=c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum"),sort=FALSE,all.x=TRUE)
  if (all(allFields%in%names(tab))) go <- FALSE
#4th step
  if (go) tab <- merge(slSex(sl(object),hl(object)),tab,by=c("sampType","landCtry","vslFlgCtry","year","proj","trpCode",
                                           "staNum","spp","catchCat","landCat","commCatScl","commCat","subSampCat","sex"),sort=FALSE,all.x=TRUE)
  TAB <- tab
  if (any(!allFields%in%names(TAB))) stop("some parameters don't match with tables!!") 
  }

if (length(field)==1) {
  eval(parse('',text=paste("calc <- tapply(TAB$",field,",list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))
} else {
  Field <- apply(TAB[,field],1,paste,collapse=":-:")
  eval(parse('',text=paste("calc <- tapply(Field,list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))
}

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",paste(field,collapse=", ",sep=""),"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)






setMethod("disInfo", signature("clDataVal"), function(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE) {

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)!=1) stop("One field must be specified in 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")

tab <- cl(object)
allFields <- c(field,by)
if (!all(allFields%in%names(tab))) stop("Fields don't match with 'cl' format!!")
eval(parse('',text=paste("calc <- tapply(tab$",field,",list(",paste("tab$",by,collapse=",",sep=""),"),fun,...)",sep="")))

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",field,"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)







setMethod("disInfo", signature("ceDataVal"), function(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE) {

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)!=1) stop("One field must be specified in 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")

tab <- ce(object)
allFields <- c(field,by)
if (!all(allFields%in%names(tab))) stop("Fields don't match with 'ce' format!!")
eval(parse('',text=paste("calc <- tapply(tab$",field,",list(",paste("tab$",by,collapse=",",sep=""),"),fun,...)",sep="")))

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",field,"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)









###################################
###################################
##  Consolidated data structure  ##
###################################
###################################



setMethod("disInfo", signature("csDataCons"), function(object,path,field,by,fun,...,biopar=FALSE,transpose=FALSE,title="",append=TRUE) {   #by only contains "time","space" or/and "technical

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)==0) stop("Missing 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
by <- by[by%in%c("time","space","technical")]
if (length(by)==0) stop("'by' must contain \"time\",\"space\" or/and \"technical\"!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")

if (biopar) {
  TAB <- ca(object)
  if (any(!field%in%names(TAB))) stop("'field' parameter doesn't match with CA table!!") 
} else {
  TAB <- tr(object)
  if (any(!field%in%names(TAB))) {
    TAB <- hh(object)
    if (any(!field%in%names(TAB))) {
      TAB <- sl(object)
      if (any(!field%in%names(TAB))) {
        TAB <- hl(object)
        if (any(!field%in%names(TAB))) stop("'field' parameter doesn't match with tables!!")
      }
    }
  }
}

if (length(field)==1) {
  eval(parse('',text=paste("calc <- tapply(TAB$",field,",list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))
} else {
  Field <- apply(TAB[,field],1,paste,collapse=":-:")
  eval(parse('',text=paste("calc <- tapply(Field,list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))
}

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",paste(field,collapse=", ",sep=""),"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)







setMethod("disInfo", signature("clDataCons"), function(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE) {   #by only contains "time","space" or/and "technical

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)==0) stop("Missing 'field' parameter!!")
if (length(field)>1) stop("Only one field name in 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
by <- by[by%in%c("time","space","technical")]
if (length(by)==0) stop("'by' must contain \"time\",\"space\" or/and \"technical\"!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")


TAB <- cl(object)
if (!field%in%names(TAB)) stop("'field' parameter doesn't match with CL table!!") 

eval(parse('',text=paste("calc <- tapply(TAB$",field,",list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",field,"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)





setMethod("disInfo", signature("ceDataCons"), function(object,path,field,by,fun,...,transpose=FALSE,title="",append=TRUE) {   #by only contains "time","space" or/and "technical

if (missing(path)) stop("Missing 'path' parameter!!")
if (missing(field)) stop("Missing 'field' parameter!!")
if (length(field)==0) stop("Missing 'field' parameter!!")
if (length(field)>1) stop("Only one field name in 'field' parameter!!")
if (missing(by)) stop("Missing 'by' parameter!!")
by <- by[by%in%c("time","space","technical")]
if (length(by)==0) stop("'by' must contain \"time\",\"space\" or/and \"technical\"!!")
if (missing(fun)) stop("Missing 'fun' parameter!!")


TAB <- ce(object)
if (!field%in%names(TAB)) stop("'field' parameter doesn't match with CE table!!") 

eval(parse('',text=paste("calc <- tapply(TAB$",field,",list(",paste("TAB$",by,collapse=",",sep=""),"),fun,...)",sep="")))

if (transpose) calc <- t(calc)

if (!is.na(append)){
#Report
sink(file=path,append=append)
    cat("\n") 		
    cat(title,"\n")
    eval(parse('',text="cat(paste(rep(\"-\",nchar(title)),collapse=\"\",sep=\"\"),\"\n\")"[nchar(title)>0]))
		cat("\n")
    cat("Field -> ",field,"\n")
		cat("By    -> ",paste(by,collapse=", ",sep=""),"\n")
		cat("FUN   -> ",deparse(substitute(fun)),"\n","\n")
    print(calc)
		sink()
invisible(calc)
} else {
#no Report, only output
return(list(title=title,result=calc))
}}
)





# CS/CL/CE consistency (presence of values of a given field in each specified table)

tabConsist <- function(lTab,field,nb=FALSE) {                 #lTab is a list containing various COST objects

timeField <- function(tab) {
month <- sapply(as.character(tab$date),function(x) as.numeric(strsplit(x,"-")[[1]][2]))
quarter <- ceiling(month/3)
semester <- ceiling(quarter/2)
return(data.frame(month=month,quarter=quarter,semester=semester))
}
                             
newLtab <- NULL

#test on objects classes
ind <- FALSE
clas <- unlist(lapply(lTab,class))
if (all(clas%in%c("csData","ceData","clData"))) ind <- TRUE
if (all(clas%in%c("csDataVal","ceDataVal","clDataVal"))) ind <- TRUE
if (all(clas%in%c("csDataCons","ceDataCons","clDataCons"))) ind <- TRUE
if (!ind) stop("objects classes don't match!!")

#names for each table
nam <- rep(c("CS","CL","CE"),each=3) ; reit <- rep(c(5,1,1),each=3)
names(reit) <- names(nam) <- c("csData","csDataVal","csDataCons","clData","clDataVal","clDataCons","ceData","ceDataVal","ceDataCons")
NAM <- nam[clas] ; IND <- rep(1,length(NAM))
if (length(NAM)>1) invisible(sapply(2:length(NAM),function(x) IND[x] <<- sum(NAM[1:x]%in%NAM[x])))
NAM <- paste(NAM,IND,sep="")
namTab <- namT <- unlist(do.call("list",lapply(lTab,function(x) slotNames(x)[-1])))
at <- rep("_",length(namTab))
at[namTab%in%c("cl","ce")] <- ""
namTab[namTab%in%c("cl","ce")] <- ""
NAMES <- paste(rep(NAM,reit[clas]),at,namTab,sep="")

#tables in lTab are split and put in a list
eval(parse('',text=paste("newLtab <- list(",paste(NAMES,"=lTab[[",rep(1:length(NAM),reit[clas]),"]]@",namT,collapse=",",sep=""),")",sep="")))
#and time fields are added
#first, time field are added in hh table
invisible(sapply(1:length(newLtab), function(x) if ("date"%in%names(newLtab[[x]])) newLtab[[x]] <<- cbind.data.frame(newLtab[[x]],timeField(newLtab[[x]]))))   #added 19/01/2009

#test the presence of specified field in the tables, and remove the table if no
n <- length(newLtab)
invisible(sapply(n:1,function(x) if (!field%in%names(newLtab[[x]])) newLtab[[x]] <<- NULL))
if (length(newLtab)==0) stop("specified field cannot be found in input objects!!")

#all values of specified field (colNames of the resulting table)
lValues <- do.call("list",lapply(newLtab,function(x) unique(x[,field])))
lValues <- sort(na.omit(unique(unlist(lValues))))

#... presence of each value in each table for the specified field (Y/N)
tab <- TAB <- do.call("rbind",lapply(1:length(newLtab),function(x) lValues%in%newLtab[[x]][,field]))
#... or number of occurence of each value in each table                                                                    #added 19/01/2009
if (nb) tab <- do.call("rbind",lapply(1:length(newLtab),function(x) table(factor(newLtab[[x]][,field],levels=lValues))))   #                                                                                                                #

dimnames(tab) <- list(names(newLtab),lValues)
if (!nb) tab[tab] <- "x" ; tab[TAB=="FALSE"] <- ""                                                                         #

return(as.table(tab))
}





#mix between 'aggregate' & 'tapply'
dfApply <- function(tab,valueField,rowFields,colField,fun,...){
if (length(colField)!=1) stop("wrong 'rowField' parameter!!")
if (length(valueField)!=1) stop("wrong 'valueField' parameter!!")

#rowFields are concatenated and then tapply function is used
mat <- tapply(tab[,valueField],list(apply(tab[,rowFields,drop=FALSE],1,paste,collapse=":-:"),tab[,colField]),fun,...)
mat1 <- do.call("rbind",lapply(rownames(mat),function(x) strsplit(x,":-:")[[1]]))
df1 <- as.data.frame(mat1) ; names(df1) <- rowFields
df2 <- as.data.frame(mat) ; rownames(df2) <- NULL
dfInt <- data.frame(rep("|",nrow(df1))) ; names(dfInt)[1] <- "|"
df <- cbind.data.frame(df1,dfInt,df2)
return(df)
}




###############
##  ctrlMap  ##
###############



#on teste 2 champs 
testTk <- function(csField,othField) {

  if (is.null(csField) | is.null(othField)) {
  
    return("-")
  
  } else {

    ref <- as.character(csField)
    comp <- as.character(othField)

    allNA <- all(is.na(comp)) & all(is.na(ref))
    naS <- !all(is.na(comp)) & all(is.na(ref))
    nA <- all(is.na(comp)) & !all(is.na(ref))

    if (!(allNA | naS | nA)) {

      greater <- all(ref%in%comp)
      smaller <- all(comp%in%ref)
      equal <- greater & smaller
      if (equal) {
        greater <- smaller <- different <- FALSE
      } else {
        different <- (!greater) & (!smaller)
      }
    
    } else {

      greater <- smaller <- equal <- different <- FALSE
    }

  tst <- c("OK","<S",">S","!=S","NA-NA","X-NA","NA-X")[c(equal,smaller,greater,different,allNA,naS,nA)]
  if (length(tst)>1) stop("!!!!")

  return(tst)

}

}



ctrlMap <- function(cs,ca,cl,ce) {

#il faut commencer par inserer la donnee temporelle dans les tables
cs@hh$Ymonth <- sapply(as.character(cs@hh$date),function(x) as.numeric(strsplit(x,"-")[[1]][2])) 
cs@hh$Yquarter <- ceiling(cs@hh$Ymonth/3)
cs@hh$Ysemester <- ceiling(cs@hh$Ymonth/6)
cs@ca$semester <- ceiling(as.numeric(as.character(cs@ca$quarter))/2)
#on colle l'annee au champ trimestre, semestre et mois
cs@hh$Ymonth <- paste(as.character(cs@hh$year),as.character(cs@hh$Ymonth),sep=" - ")
cs@hh$Yquarter <- paste(as.character(cs@hh$year),as.character(cs@hh$Yquarter),sep=" - ")
cs@hh$Ysemester <- paste(as.character(cs@hh$year),as.character(cs@hh$Ysemester),sep=" - ")
cs@ca$month <- paste(as.character(cs@ca$year),as.character(cs@ca$month),sep=" - ")
cs@ca$quarter <- paste(as.character(cs@ca$year),as.character(cs@ca$quarter),sep=" - ")
cs@ca$semester <- paste(as.character(cs@ca$year),as.character(cs@ca$semester),sep=" - ")

clInd <- !is.null(cl) ; ceInd <- !is.null(ce) ; caInd <- (nrow(ca)>1)
if ((!clInd) & (!ceInd) & (!caInd)) stop("Nothing to check!!")

if (clInd) {
  cl@cl$semester <- ceiling(as.numeric(as.character(cl@cl$quarter))/2)
  cl@cl$month <- paste(as.character(cl@cl$year),as.character(cl@cl$month),sep=" - ")
  cl@cl$quarter <- paste(as.character(cl@cl$year),as.character(cl@cl$quarter),sep=" - ")
  cl@cl$semester <- paste(as.character(cl@cl$year),as.character(cl@cl$semester),sep=" - ")
}

if (ceInd) {
  ce@ce$semester <- ceiling(as.numeric(as.character(ce@ce$quarter))/2)
  ce@ce$month <- paste(as.character(ce@ce$year),as.character(ce@ce$month),sep=" - ")
  ce@ce$quarter <- paste(as.character(ce@ce$year),as.character(ce@ce$quarter),sep=" - ")
  ce@ce$semester <- paste(as.character(ce@ce$year),as.character(ce@ce$semester),sep=" - ")
}

outp <- matrix("",ncol=3,nrow=14)

#les champs a traiter sont :
CSfield <- c("@sl$spp","@hh$year","@hh$Ysemester","@hh$Yquarter","@hh$Ymonth","@hh$area","@hh$rect","@hh$subRect",
                "@hh$foCatNat","@hh$foCatEu5","@hh$foCatEu6","@sl$commCat","@sl$lenCode")
CAfield <- c("@ca$spp","@ca$year","@ca$semester","@ca$quarter","@ca$month","@ca$area","@ca$rect","@ca$subRect",
                "@ca$foCatNat","@ca$foCatEu5","@ca$foCatEu6","@ca$commCat","@ca$lenCode")
CLfield <- c("@cl$taxon","@cl$year","@cl$semester","@cl$quarter","@cl$month","@cl$area","@cl$rect","@cl$subRect",
                "@cl$foCatNat","@cl$foCatEu5","@cl$foCatEu6","@cl$commCat","@cl$lenCode")
CEfield <- c("@ce$spp","@ce$year","@ce$semester","@ce$quarter","@ce$month","@ce$area","@ce$rect","@ce$subRect",
                "@ce$foCatNat","@ce$foCatEu5","@ce$foCatEu6","@ce$commCat","@ce$lenCode")
fields <- c("spp-taxon","year","semester","quarter","month","area","rect","subRect","foCatNat","foCatEu5","foCatEu6","commCat","lenCode","lenCls")





require(tcltk)
require(tcltk2)
ttc <- tktoplevel()
tkfocus(ttc) 
tkwm.deiconify(ttc)
tkgrab.set(ttc)
tkwm.title(ttc, "Check fields consistency")
fontHeading <- tkfont.create(family="times",size=14,weight="bold")
fontSymb <- tkfont.create(family="times",size=13)
eval(parse('',text=paste("
tkgrid(tklabel(ttc,text=\"     \"), tklabel(ttc,text=\"     \"),","tklabel(ttc,text=\"     CA~CS     \",font=fontHeading),"[caInd],"tklabel(ttc,text=\"     CL~CS     \",font=fontHeading),"[clInd],
                    "tklabel(ttc,text=\"     CE~CS     \",font=fontHeading),"[ceInd],"tklabel(ttc,text=\"     tabConsist     \",font=fontHeading))",sep="")))
tkgrid(tklabel(ttc,text=""))
tkgrid(tklabel(ttc,text=""))
colFont <- c("green4","red","green4","red","black","black","black","black")
colFont2 <- c("green4","black","green4","black","black","black","black","black")
names(colFont) <- names(colFont2) <- c("OK","<S",">S","!=S","NA-NA","X-NA","NA-X","-")                    
cbTab <- character(14)    
tkfocus(ttc)   
for (i in 1:(length(cbTab)-1)) {
  eval(parse('',text=paste("cbF",i," <- tkcheckbutton(ttc)",sep="")))
  eval(parse('',text=paste("cbValueF",i," <- tclVar(\"0\")",sep="")))
  eval(parse('',text=paste("tkconfigure(cbF",i,",variable=cbValueF",i,")",sep="")))
  if (caInd) eval(parse('',text=paste("res1 <- outp[i,1] <- testTk(cs",CSfield[i],",cs",CAfield[i],")",sep="")))
  if (clInd) eval(parse('',text=paste("res2 <- outp[i,2] <- testTk(cs",CSfield[i],",cl",CLfield[i],")",sep="")))
  if (ceInd) eval(parse('',text=paste("res3 <- outp[i,3] <- testTk(cs",CSfield[i],",ce",CEfield[i],")",sep="")))
  if (i>1) {
    eval(parse('',text=paste("tkgrid(tklabel(ttc,text=fields[",i,"],font=fontHeading),tklabel(ttc,text=\"\"),","tklabel(ttc,text=res1,foreground=colFont[res1],font=fontSymb),"[caInd],"tklabel(ttc,text=res2,foreground=colFont[res2],font=fontSymb),"[clInd],"tklabel(ttc,text=res3,foreground=colFont[res3],font=fontSymb),"[ceInd],"cbF",i,")",sep="")))
  } else {
    eval(parse('',text=paste("tkgrid(tklabel(ttc,text=fields[",i,"],font=fontHeading),tklabel(ttc,text=\"\"),","tklabel(ttc,text=res1,foreground=colFont[res1],font=fontSymb),"[caInd],"tklabel(ttc,text=res2,foreground=colFont2[res2],font=fontSymb),"[clInd],"tklabel(ttc,text=res3,font=fontSymb),"[ceInd],"cbF",i,")",sep="")))
  }
  if (i %in% c(1,5,8,11,12)) {
    tkgrid(tklabel(ttc,text=""))
    tkgrid(tklabel(ttc,text=""))
  }
}
   
#la derniere ligne est specifique : analyse des classes de tailles (proportion de classes de taille dans CS presentes dans CA
propLC <- unique(cs@hl$lenCls)%in%unique(ca$lenCls)
prop <- round(100*sum(propLC)/length(propLC),1)

outp[14,] <- c(paste(prop,"%",sep=""),"-","-") 
cbF14 <- tkcheckbutton(ttc) 
cbValueF14 <- tclVar("0")
tkconfigure(cbF14,variable=cbValueF14)
eval(parse('',text=paste("tkgrid(tklabel(ttc,text=\"lenCls\",font=fontHeading),tklabel(ttc,text=\"\"),",
      "tklabel(ttc,text=paste(prop,\"%\",sep=\"\"),foreground=c(\"green4\",\"black\",\"red\")[c(prop>=90,(prop<90 & prop>=75),prop<75)],font=fontSymb),"[caInd],
      "tklabel(ttc,text=\"-\",font=fontSymb),"[clInd],"tklabel(ttc,text=\"-\",font=fontSymb),"[ceInd],"cbF14)",sep="")))
 

dimnames(outp) <- list(fields,c("CA~CS","CL~CS","CE~CS")) 

 
   
OnOK <- function()
{
    cs@hh$month <- cs@hh$Ymonth 
    cs@hh$quarter <- cs@hh$Yquarter
    cs@hh$semester <- cs@hh$Ysemester
    fields[1] <- "spp"
    for (i in 1:length(cbTab)) {
      eval(parse('',text=paste("cbTab[",i,"] <<- as.character(tclvalue(cbValueF",i,"))",sep="")))
      if (cbTab[i]=="1") {
      print(fields[i])
      try(eval(parse('',text=paste("tabC <- tabConsist(list(cs",",cl"[clInd],",ce"[ceInd],"),\"",fields[i],"\")",sep=""))),silent=TRUE) 
      try(print(tabC),silent=TRUE)
      tabC <- NA
      cat("\n\n")
      }
    }

    tkgrab.release(ttc)
    tkdestroy(ttc)
}

OnCancel <- function() {

    cbTab <<- rep("0",length(cbTab))
    tkgrab.release(ttc)
    tkdestroy(ttc)
    
    }
    
OK.but <- tkbutton(ttc, text = "   OK   ", command = OnOK)
Cancel.but <- tkbutton(ttc, text = " Cancel ", command = OnCancel)
tkgrid(tklabel(ttc,text=""))
tkgrid(tklabel(ttc,text=""),tklabel(ttc,text=""),tklabel(ttc,text=""), tklabel(ttc,text=""),OK.but, Cancel.but)
tkgrid(tklabel(ttc, text = "    "))
tkfocus(ttc)  

invisible(as.data.frame(outp)) 
}  





setGeneric("controlMap", function(csObject,
                                 secObject,
                                 thiObject,
                                 ...){     #COSTeda:::tabConsist 's parameters                                                  
	standardGeneric("controlMap")}
)



setMethod("controlMap", signature(csObject="csData",secObject="clData",thiObject="ceData"), function(csObject,
                                                                                                              secObject,
                                                                                                              thiObject,
                                                                                                              ...){

                                                                                                 
ctrlMap(csObject,ca(csObject),secObject,thiObject,...)

})

setMethod("controlMap", signature(csObject="csData",secObject="clData",thiObject="missing"), function(csObject,
                                                                                                              secObject,
                                                                                                              ...){

                                                                                                 
ctrlMap(csObject,ca(csObject),secObject,NULL,...)

})


setMethod("controlMap", signature(csObject="csData",secObject="ceData",thiObject="missing"), function(csObject,
                                                                                                              secObject,
                                                                                                              ...){

                                                                                                 
ctrlMap(csObject,ca(csObject),NULL,secObject,...)

})

setMethod("controlMap", signature(csObject="csData",secObject="missing",thiObject="missing"), function(csObject,
                                                                                                              ...){

                                                                                                 
ctrlMap(csObject,ca(csObject),NULL,NULL,...)

})




 
##exemple
# data(sole)
# gg <- controlMap(sole.cs,sole.cl,sole.ce)








