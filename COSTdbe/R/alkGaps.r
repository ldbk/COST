
#library(COSTcore)
#
#====================================================================
# strLChl : * presence of length classes in hl by strata (consolidated object as input)
#====================================================================

strLChl <- function(object) {
ca <- ca(object)
hl <- hl(object)
#is strata information available?
Time <- factor(ca$time,levels=as.character(unique(sort(object@hl$time))))
Space <- factor(ca$space,levels=as.character(unique(sort(object@hl$space))))
indNAtime <- all(is.na(Time))
indNAspace <- all(is.na(Space))
if (indNAtime & indNAspace) stop("no defined stratification in ca or no common strata between hl and ca!!")
#length class step
BY <- switch(as.character(ca$lenCode[1]),"mm"=1,"2mm"=2,"3mm"=3,"scm"=5,"cm"=10,"20mm"=20,"25mm"=25,"30mm"=30,"50mm"=50)
#according to available strata, alk is computed (length classes are defined according to hl table)
eval(parse('',text=paste("strLC <- tapply(object@hl$lenNum,list(factor(object@hl$lenCls,levels=seq(from=min(object@hl$lenCls,na.rm=TRUE),to=max(object@hl$lenCls,na.rm=TRUE),by=BY)),",
                         paste(c("hl$time"[!indNAtime],"hl$space"[!indNAspace]),collapse=","),"),function(x) sum(x,na.rm=TRUE)>0)",sep="")))
#NAs --> FALSE (= not in hl)
strLC[is.na(strLC)] <- FALSE
return(strLC)
}



#====================================================================
# viewGapsAlk : * displaying procedure for alks that underline empty length classes (gaps)   (input alks with 0s)
#   '.' are LC that are not recorded in hl for the specified stratum
#   '-' are LC that are recorded in hl but not in ca, for the specified stratum
#====================================================================

viewGapsAlk <- function(alk,strLC) {            #strLC is a stratified array of length classes presence in hl
#index of empty length classes
dime <- 1:length(dim(alk))
index <- apply(alk,dime[-2],function(x) rep(all(x==0),dim(alk)[2]))
test <- aperm(index,replace(dime,1:2,2:1))
View <- alk ; View[test] <- "-"
if (!missing(strLC)) {
  tst <- array(TRUE,dim=dim(View),dimnames=dimnames(View))
  tst[] <- apply(strLC,2:length(dim(strLC)),rep,ncol(View))
  tst0 <- View%in%c("0","-")
  View[!tst & tst0] <- "."
}
print(as.table(View))
}


#====================================================================
# propMissLgth : * function that computes the proportion of missing length class in a single alk (dim=2), and the rate of missing length classes at the extrema
#                  (if strLC, then proportions are computed according to LC that are recorded in hl)
#====================================================================

propMissLgth <- function(alk,strLC) {         #strLC is a logical vector of presence of length classes in hl for specified stratum
if (!missing(strLC)) alk <- alk[strLC,,drop=FALSE] 

if (nrow(alk)==0) {
  
  pEmpty <- 0 ; pEmptyExtr <- 0/0

} else {

#index of empty length classes
index <- apply(alk,1,function(x) all(x==0))
#index of empty length class at the extrema
if (all(index)) {
  indExtr <- index
} else {
  #small classes
  ind1 <- index
  ind1[match(FALSE,index):length(index)] <- FALSE
  #large classes
  ind2 <- rev(index)
  ind2[match(FALSE,ind2):length(index)] <- FALSE
  ind2 <- rev(ind2)  
  #so, index is finally...
  indExtr <- ind1|ind2 }

#proportion of empty length classes
pEmpty <- sum(index)/nrow(alk)
#proportion of extreme empty classes within empty classes
pEmptyExtr <- sum(indExtr)/sum(index)
}

return(list(pEmpty=pEmpty,pEmptyExtr=pEmptyExtr))
}




#====================================================================
# gapsRm : * function with various methods to remove gaps from a single ALK
#====================================================================

  #====================================================================
  # stepIncrFun : function increasing the steps of length classes to 'value' in a given alk
  #====================================================================


stepIncrFun <- function(alk,value=10,start=as.numeric(rownames(alk))[1],...) { #value is the new length class step (in mm)  --> authorized values : 1, 2, 3, 5, 10, 20, 30, 50
if (!value%in%c(1,2,3,5,10,20,30,50)) stop("wrong 'value' parameter!!")
if (nrow(alk)<2) stop("'alk' object has wrong dimensions!!")
ALK <- alk
#new length classes definition
lc <- as.numeric(rownames(alk))
#value must be >= alk's step
if (value<(lc[2]-lc[1])) stop("'value' mustn't be smaller than alk's step!!")
#'start' value must be in ] lc[1]-value ; lc[1] ]
if (start>lc[1] | start<=lc[1]-value) stop("wrong 'start' parameter!!")
nlc <- seq(from=start,to=max(lc),by=value)
#recoding of length classes
NLC <- cut(lc,c(nlc,1000000)-0.01,labels=nlc)
rownames(ALK) <- as.character(NLC)
#sum over length classes
lSums <- by(ALK,rownames(ALK),function(x) apply(x,2,sum))
#new alk
newAlk <- matrix(unlist(lSums),ncol=ncol(ALK),byrow=TRUE,dimnames=list(names(lSums),colnames(ALK)))
#output
nwLgthCls <- data.frame(oldLC=lc,newLC=as.numeric(as.character(NLC)))
return(list(newLgthClas=nwLgthCls,newAlk=newAlk))
}

  #====================================================================
  # fillMissFun : function that fills gaps of maximum size specified by 'value' parameter with the sum of values from surrounding filled classes (or another function as defined by FUN)
  #====================================================================


fillMissFun <- function(alk,value=1,...) {   #'value' is the maximum 'gap size' for which the filling process can be applied 
ALK <- alk
if (value<1) stop("wrong 'value' parameter!!")
#'patterns' of the gaps that are to be filled
pattern <- sapply(1:value,function(x) paste(c(1,rep(0,x),1),collapse="")) 
DAT <- NULL
RN <- rownames(ALK)
#loop
while (length(pattern)>0) {
#index to find gaps to be filled according to 'value' parameter
index <- paste(as.numeric(apply(ALK,1,function(x) !all(x==0))),collapse="")
#matching of the patterns in 'index'
matc <- regexpr(pattern[1],index)[1]
if (matc<0) {
    pattern <- pattern[-1]
} else {
    d.index <- matc + c(0,nchar(pattern[1])-1)     #data index
    g.index <- matc + c(1:(nchar(pattern[1])-2))   #gap index
    valFill <- apply(ALK[d.index,],2,sum,na.rm=TRUE)          #replacement row
    eval(parse('',text=paste("ALK[g.index,] <- rbind(",paste(rep("valFill",length(g.index)),collapse=","),")",sep="")))   #gaps filling process
    dat <- expand.grid(age=colnames(ALK),lenCls=RN[g.index])    #builds a 'virtual' consolidated ca table from 'virtual' individuals
    dat$N <- valFill ; dat <- dat[dat$N>0,]
    Dat <-  as.data.frame(do.call("rbind",lapply(1:nrow(dat),function(x) do.call("cbind",lapply(t(dat[x,]),rep,dat$N[x]))))) #we want as many rows as individuals
    names(Dat) <- names(dat) ; Dat <- Dat[,1:2]
    DAT <- rbind(DAT,Dat)
}}
nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(RN))
#output
return(list(newLgthClas=nwLgthCls,newAlk=ALK,addInd=DAT))
}

  #====================================================================
  # sExtrGrpFun : function that groups the 'value' number of first classes in a given alk
  #====================================================================


#
#sExtrGrpFun <- function(alk,value=2) {    #value is the number of length classes to group at the top of the alk
#ALK <- alk
#if (value<2) stop("wrong 'value' parameter!!")
#if (value>(nrow(alk)-1)) stop("wrong 'value' parameter!!")
#if (nrow(alk)<2) stop("not relevant for 'alk' object!!")
##new Alk with first classes grouped
#newAlk <- rbind(apply(ALK[1:value,],2,sum),ALK[(value+1):nrow(ALK),])
##name of the first class
#RN <- rownames(ALK)
#rownames(newAlk)[1] <- RN[value]
##output with definition of length classes recoding
#rn <- rownames(newAlk) 
#nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(c(rep(rn[1],value),rn[2:length(rn)])))
#return(list(newLgthClas=nwLgthCls,newAlk=newAlk))
#}
#
#
  #====================================================================
  # lExtrGrpFun : function that groups the 'value' number of last classes in a given alk
  #====================================================================

#lExtrGrpFun <- function(alk,value=2) {    #value is the number of length classes to group at the bottom of the alk
#ALK <- alk
#if (value<2) stop("wrong 'value' parameter!!")
#if (value>(nrow(alk)-1)) stop("wrong 'value' parameter!!")
#if (nrow(alk)<2) stop("not relevant for 'alk' object!!")
##new Alk with last classes grouped
#ind <- nrow(ALK)-value
#newAlk <- rbind(ALK[1:ind,],apply(ALK[(ind+1):nrow(ALK),],2,sum))
##name of the last class
#RN <- rownames(ALK)
#rownames(newAlk)[nrow(newAlk)] <- RN[ind+1]
##output with definition of length classes recoding
#rn <- rownames(newAlk) 
#nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(c(rn[1:(length(rn)-1)],rep(rn[length(rn)],value))))
#return(list(newLgthClas=nwLgthCls,newAlk=newAlk))
#}
#
#

  #====================================================================
  # sFillMissFun : function that fills gaps at the extremum (top) of the alks ; 'value' specifies the number of rows to be filled upward with the first filled row
  #====================================================================

sFillMissFun <- function(alk,value=1,...) {    
RN <- rownames(alk)
nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(RN))

if (value<1) stop("wrong 'value' parameter!!")
if (nrow(alk)<2) stop("not relevant for 'alk' object!!")
#empty rows index
ind <- apply(alk,1,function(x) !all(x==0))

first <- (1:nrow(alk))[ind][1]
if (all(!ind)) first <- 1
#if no filled row or if first filled row is 1st
if (first==1) {
 
 return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=NULL))

} else {

#index of rows to be filled according to 'value'
index <- max(c(1,first-value)):(first-1)
invisible(sapply(index,function(x) alk[x,] <<- alk[first,]))

dat <- expand.grid(age=colnames(alk),lenCls=RN[index])    #builds a 'virtual' consolidated ca table from 'virtual' individuals
dat$N <- alk[first,] ; dat <- dat[dat$N>0,]
Dat <-  as.data.frame(do.call("rbind",lapply(1:nrow(dat),function(x) do.call("cbind",lapply(t(dat[x,]),rep,dat$N[x]))))) #we want as many rows as individuals
names(Dat) <- names(dat) ; Dat <- Dat[,1:2]

return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=Dat))}

}



  #====================================================================
  # lFillMissFun : function that fills gaps at the extremum (bottom) of the alks ; 'value' specifies the number of rows to be filled downward with the last filled row
  #====================================================================

lFillMissFun <- function(alk,value=1,...) {    
RN <- rownames(alk)
nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(RN))

if (value<1) stop("wrong 'value' parameter!!")
if (nrow(alk)<2) stop("not relevant for 'alk' object!!")
#empty rows index
ind <- apply(alk,1,function(x) !all(x==0))

#last filled row
last <- (1:nrow(alk))[ind] ; last <- last[length(last)]

if (all(!ind)) last <- nrow(alk)
#if no filled row, or if last filled row is the last
if (last==nrow(alk)) {
 
 return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=NULL))

} else {

#index of rows to be filled according to 'value'
index <- (last+1):min(c(nrow(alk),last+value))
invisible(sapply(index,function(x) alk[x,] <<- alk[last,]))

dat <- expand.grid(age=colnames(alk),lenCls=RN[index])    #builds a 'virtual' consolidated ca table from 'virtual' individuals
dat$N <- alk[last,] ; dat <- dat[dat$N>0,]
Dat <-  as.data.frame(do.call("rbind",lapply(1:nrow(dat),function(x) do.call("cbind",lapply(t(dat[x,]),rep,dat$N[x]))))) #we want as many rows as individuals
names(Dat) <- names(dat) ; Dat <- Dat[,1:2]

return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=Dat))}
}



  #====================================================================
  # sFillAgeFun : function that fills first empty length classes with 1 for first age (first column) ; 'value' specifies the number of rows to be filled upward 
  #====================================================================

#sFillAgeFun <- function(alk,value=1,...) {    
#RN <- rownames(alk)
#nwLgthCls <- data.frame(oldLC=as.numeric(RN),newLC=as.numeric(RN))
#
#if (value<1) stop("wrong 'value' parameter!!")
#if (nrow(alk)<2) stop("not relevant for 'alk' object!!")
##empty rows index
#ind <- apply(alk,1,function(x) !all(x==0))
#
##first filled row
#first <- (1:nrow(alk))[ind][1]
#if (all(!ind)) first <- 1
##if no filled row or if first filled row is 1st
#if (first==1) {
# 
# return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=NULL))
#
#} else {
#
##index of rows to be filled according to 'value'
#index <- max(c(1,first-value)):(first-1)
#invisible(sapply(index,function(x) alk[x,1] <<- 1))
#
#Dat <- data.frame(age=as.numeric(colnames(alk)[1]),lenCls=as.numeric(rownames(alk)[index]))
#
#return(list(newLgthClas=nwLgthCls,newAlk=alk,addInd=Dat))}
#}
#









  #====================================================================
  # gapsRm : method using previous functions and applying them to stratified alks (dim > 2) or single alks (dim=2)
  #====================================================================


gapsRm <- function(alk, 
                   type="stepIncr",        #type= "stepIncr" & value=10  --> case no2   (length step is increased to specified value)
                   value,                  #type= "fillMiss" & value=1   --> case no3   (all gaps of length <= value are filled with the sum of surrounding filled classes)
                   preview=TRUE,           #NO xxxxx type= "sExtrGrp" & value=1   --> case no4.1 (the number of first classes specified by value parameter are grouped)  xxxxx
                   postview=TRUE,...) {    #NO xxxxx type= "lExtrGrp" & value=1   --> case no4.2 (the number of last classes specified by value parameter are grouped)  xxxxx
                                           #type= "sFillMiss" & value=1  --> case no5.1 (the 'value' number of rows prior to first filled length class are filled)
                                           #type= "lFillMiss" & value=1  --> case no5.2 (the 'value' number of rows following last filled length class are filled)
                                           #NO xxxxx type= "sFillAge" & value=1   --> case no6   (the 'value' number of rows prior to first filled length class is filled with 1 for first age value  xxxxx
                                           #'view1' displays input alk
                                           #'view2' displays output alk

ms <- missing(value)

if (preview) viewGapsAlk(alk)
                        
Dim <- length(dim(alk))

#creation of 'Alk' object -> 2 cases, if alk is stratified or not 
if (Dim<3) {     #if 'alk' is not stratified

eval(parse('',text=paste("Alk <- ",type,"Fun(alk",",value=value"[!ms],",...)",collapse="",sep="")))
newDim <- Alk$newLgthClas
#if type%in%c("fillMiss","sFillMiss","lFillMiss"), 'virtual' ca has to be created
if (type%in%c("fillMiss","sFillMiss","lFillMiss")) df <- Alk$addInd
Alk <- Alk$newAlk
Prop <- propMissLgth(Alk)

} else {         #if 'alk' is stratified
val <- NULL
#apply function on each alk for each strata
res <- apply(alk,3:Dim,function(x) {
    eval(parse('',text=paste("val <- ",type,"Fun(x",",value=value"[!ms],",...)",collapse="",sep="")))
    newDim <<- val$newLgthClas
    val$newAlk}
    )
#recreate the stratified alk
Alk <- array(res,dim=c(length(unique(newDim$newLC)),dim(alk)[2:Dim]))
ll <- dimnames(alk)
ll[[1]] <- unique(newDim$newLC)
dimnames(Alk) <- ll

#idem with 'addInd' on each alk for each strata if type%in%c("fillMiss","sFillMiss","lFillMiss")
if (type%in%c("fillMiss","sFillMiss","lFillMiss")) {
strat <- paste("\"",apply(expand.grid(dimnames(alk)[3:Dim]),1,paste,collapse="\",\""),"\"",sep="")
df <- data.frame(do.call("rbind",lapply(strat,function(X) {
                                          eval(parse('',text=paste("val <- ",type,"Fun(alk[,,",X,"]",",value=value"[!ms],",...)$addInd",collapse="",sep="")))
                                          strSpl <- strsplit(X,",")[[1]]
                                          eval(parse('',text=paste("dat <- cbind(val,",paste(strSpl,collapse=","),")",sep="")))
                                          if (is.null(val)) dat <- NULL else names(dat) <- ""    
                                          return(dat)})))
if (nrow(df)>0) names(df) <- c("age","lenCls",paste("str",(3:Dim)-2,sep=""))                                                            ############ modified MM : 03/11/2008
}

Prop <- list()
Prop$pEmpty <- apply(Alk,3:Dim,function(x) propMissLgth(x)$pEmpty)
Prop$pEmptyExtr <- apply(Alk,3:Dim,function(x) propMissLgth(x)$pEmptyExtr)
}

if (preview & postview) {
  cat("\n\n\n\n")
  cat("New alk object created!!!")
  cat("\n\n\n\n")
}

if (postview) viewGapsAlk(Alk)

if (type%in%c("fillMiss","sFillMiss","lFillMiss")) {
  invisible(list(alk=Alk,addIndTab=df,propMiss=Prop))
} else {
  invisible(list(alk=Alk,lgthCls=newDim,propMiss=Prop))
}
}



  #====================================================================
  # Example
  #====================================================================

#library(COSTcore)
#x <- sole.cs
#CA <- ca(x)
#alk <- tapply(CA$age,list(factor(CA$lenCls,levels=seq(from=min(CA$lenCls),to=max(CA$lenCls),by=10)),CA$age,CA$quarter,CA$area),length)
##no NAs in the alk --> 0s
#alk[is.na(alk)] <- 0
#
#
#viewGapsAlk(alk)                                                                    #check empty lines in alk
#alk <- gapsRm(alk,type="stepIncr",value=20,preview=FALSE,postview=TRUE)             #length class step is raised to 20 
#alk <- gapsRm(alk$alk,type="fillMiss",value=2,preview=FALSE,postview=TRUE)          #gaps are filled with available information 
#alk <- gapsRm(alk$alk,type="sExtrGrp",value=4,preview=FALSE,postview=TRUE)          #first length classes are grouped 
#alk <- gapsRm(alk$alk,type="lExtrGrp",value=4,preview=FALSE,postview=TRUE)          #last length classes are grouped 
#



############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#######################################                                                  ###################################################
#######################################              CONSOLIDATED METHODS                ###################################################
#######################################                                                  ###################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################


#====================================================================
# alkLgthRec : * method using 'gapsRm' function by calling directly a consolidated object
#
#     type= "stepIncr" & value=10  --> case no2   (length step is increased to specified value)
#     type= "fillMiss" & value=1   --> case no3   (all gaps of length <= value are filled with the sum of surrounding filled classes)
#     NO xxxxx type= "sExtrGrp" & value=1   --> case no4.1 (the number of first classes specified by value parameter are grouped)  xxxxx
#     NO xxxxx type= "lExtrGrp" & value=1   --> case no4.2 (the number of last classes specified by value parameter are grouped)  xxxxx
#     type= "sFillMiss" & value=1  --> case no5.1 (the 'value' number of rows prior to first filled length class are filled)
#     type= "lFillMiss" & value=1  --> case no5.2 (the 'value' number of rows following last filled length class are filled)
#     NO xxxxx type= "sFillAge" & value=1   --> case no6   (the 'value' number of rows prior to first filled length class is filled with 1 for first age value xxxxx
#
#====================================================================
 
recomposeCA <- function(df,DF,BY,updown="up") {    #updown="down"

tab <- unique(df[,c("lenCls","time","space")])
tab$indic <- 1 ; step <- 0
if (updown=="down") BY <- -BY
inc <- NULL
while(nrow(tab)>0) {
step <- step + BY
tab$lenCls <- factor(as.numeric(as.character(tab$lenCls))+BY)
CA <- merge(DF,tab,all.x=TRUE) ; CA <- CA1 <- CA[!is.na(CA$indic) & !is.na(CA$age),]
#otoWt, indWt and matStage are set to NA
if (nrow(CA)>0) {
  CA$otoWt <- CA$indWt <- CA$matStage <- NA 
  CA$lenCls <- factor(as.numeric(as.character(CA$lenCls))-step)
}
inc <- rbind(inc,CA)
if (nrow(CA)>0) {
#found occurences are removed from tab
occ <- unique(CA1[,c("time","space","lenCls")]) ; occ$ind2 <- 1
tab <- merge(tab,occ,all.x=TRUE) 
tab <- tab[is.na(tab$ind2),c("lenCls","time","space","indic")]}
}
return(inc)
} 
 
       
setGeneric("alkLgthRec", function(object,
                                  type="stepIncr",        
                                  value,                  
                                  preview=FALSE,          
                                  postview=TRUE,
                                  update=FALSE,
                                  ...) {       
	standardGeneric("alkLgthRec")}
)



setMethod("alkLgthRec",signature(object="csDataCons"), function(object,
                                                                type="stepIncr",        
                                                                value,                  
                                                                preview=FALSE,          
                                                                postview=TRUE,
                                                                update=FALSE,
                                                                ...) { 
                                                                
if (!type%in%c("fillMiss","sFillMiss","lFillMiss","stepIncr")) stop("wrong 'type' parameter!!")
ca <- ca(object)
ms <- missing(value)
#is strata information available?
Time <- factor(ca$time,levels=as.character(unique(sort(object@hl$time))))
Space <- factor(ca$space,levels=as.character(unique(sort(object@hl$space))))
indNAtime <- all(is.na(Time))
indNAspace <- all(is.na(Space))
if (indNAtime & indNAspace) stop("no defined stratification in ca or no common strata between hl and ca!!")
#length class step
BY <- switch(as.character(ca$lenCode[1]),"mm"=1,"2mm"=2,"3mm"=3,"scm"=5,"cm"=10,"20mm"=20,"25mm"=25,"30mm"=30,"50mm"=50)
#according to available strata, alk is computed (length classes are defined according to hl table)
eval(parse('',text=paste("alk <- tapply(ca$age,list(factor(ca$lenCls,levels=seq(from=min(object@hl$lenCls,na.rm=TRUE),to=max(object@hl$lenCls,na.rm=TRUE),by=BY)),",
                         paste(c("ca$age","Time"[!indNAtime],"Space"[!indNAspace]),collapse=","),"),length)",sep="")))
#NAs --> 0
alk[is.na(alk)] <- 0
#'gapsRm' function can be applied to 'alk' ; if update=FALSE, output is 'gapsRm' output
#                                            if update=TRUE, output is a consolidated object with updated length classes  
strLC <- strLChl(object)
  if (preview) {
    viewGapsAlk(alk,strLC)
  }

val <- NULL    
  eval(parse('',text=paste("val <- gapsRm(alk,type=type,","value=value,"[!ms],"preview=FALSE,postview=FALSE,...)",sep="")))
  #val$lgthCls defines the new length classes in object@ca & object@hl 

    #if type=="stepIncr", 'lenCode' field must be updated in ca & hl, and length classes need to be recoded
  if (type=="stepIncr") {
    old <- as.character(val$lgthCls$oldLC) ; upd <- as.character(val$lgthCls$newLC)
    object@ca$lenCls <- as.numeric(as.character(factor(object@ca$lenCls,levels=old,labels=upd)))
    object@hl$lenCls <- as.numeric(as.character(factor(object@hl$lenCls,levels=old,labels=upd)))  
    lCod <- switch(as.character(value),"1"="mm","2"="2mm","3"="3mm","5"="scm","10"="cm","20"="20mm","25"="25mm","30"="30mm","50"="50mm")
    object@ca$lenCode <- lCod 
    object@sl$lenCode <- lCod 
  }

 #   if type%in%c("fillMiss","sFillMiss","lFillMiss"), 'addIndTab' object in 'val' must be formatted as ca table, and pasted to object@ca
  if (type%in%c("fillMiss","sFillMiss","lFillMiss")) {
    if (nrow(val$addIndTab)>0) {                                                                                                            ######### modified MM 30/04/2009
    df <- val$addIndTab ; nam <- names(df) ; names(df) <- c("age","lenCls","time"[!indNAtime],"space"[!indNAspace])
    if (any(c(indNAtime,indNAspace))) 
      eval(parse('',text=paste("df$",c("time"[indNAtime],"space"[indNAspace])," <- NA",collapse=";",sep="")))
    #df must be updated by removing stratified length classes that are not in hl                                      
    #index data frame
    indDF <- expand.grid(dimnames(strLC)) ; indDF <- indDF[strLC,]
    names(indDF) <- c("lenCls","time"[!indNAtime],"space"[!indNAspace])
    Df <- merge(indDF,df,all.x=TRUE)
    #df is subset to stratified LC that are in hl
    Df <- Df[!is.na(Df$age),] ; df <- Df[,names(df)] 
    if (type=="fillMiss") DF <- rbind(recomposeCA(df,ca,BY,"up"),recomposeCA(df,ca,BY,"down"))
    if (type=="sFillMiss") DF <- recomposeCA(df,ca,BY,"up")
    if (type=="lFillMiss") DF <- recomposeCA(df,ca,BY,"down")
    rownames(DF) <- nrow(ca) + c(1:nrow(DF))

    #corresponding 'fishId' will be negative to discriminate 'real' individuals and 'virtual' individuals in ca
    start <- min(c(0,ca$fishId),na.rm=TRUE)
    DF$fishId <- start-(1:nrow(DF))
    DF <- DF[,names(ca(csDataCons()))]
    object@ca <- rbind(object@ca,DF) 
    object@ca$age <- as.numeric(as.character(object@ca$age))
    object@ca$lenCls <- as.numeric(as.character(object@ca$lenCls))  
  }}

if (preview & postview) {
  cat("\n\n\n\n")
  cat("New alk object created!!!")
  cat("\n\n\n\n")
}
  
if (postview) {
    
    viewGapsAlkCons(object)

}  


if (update) {
   
  return(object)

} else {
  
  if (type=="stepIncr") {
    ll <- val[1:2]
   } else {
    ll <- val[1]
    names(df) <- nam
    ll$addIndTab <- df
  }
  ll$propMiss <- propMissLgthCons(object,strLC)
  return(ll)
}
})

                 


#====================================================================
# viewGapsAlkCons : * 'viewGapsAlk' method with a consolidated object as input
#====================================================================
 
       
setGeneric("viewGapsAlkCons", function(object,
                                  ...) {       
	standardGeneric("viewGapsAlkCons")}
)



setMethod("viewGapsAlkCons",signature(object="csDataCons"), function(object,
                                                                ...) { 
ca <- ca(object)
#is strata information available?
Time <- factor(ca$time,levels=as.character(unique(sort(object@hl$time))))
Space <- factor(ca$space,levels=as.character(unique(sort(object@hl$space))))
indNAtime <- all(is.na(Time))
indNAspace <- all(is.na(Space))
if (indNAtime & indNAspace) stop("no defined stratification in ca or no common strata between hl and ca!!")
#length class step
BY <- switch(as.character(ca$lenCode[1]),"mm"=1,"2mm"=2,"3mm"=3,"scm"=5,"cm"=10,"20mm"=20,"25mm"=25,"30mm"=30,"50mm"=50)
#according to available strata, alk is computed (length classes are defined according to hl table)
eval(parse('',text=paste("alk <- tapply(ca$age,list(factor(ca$lenCls,levels=seq(from=min(object@hl$lenCls,na.rm=TRUE),to=max(object@hl$lenCls,na.rm=TRUE),by=BY)),",
                         paste(c("ca$age","Time"[!indNAtime],"Space"[!indNAspace]),collapse=","),"),length)",sep="")))
#NAs --> 0
alk[is.na(alk)] <- 0
strLC <- strLChl(object)
viewGapsAlk(alk,strLC)
})


#====================================================================
# propMissLgthCons : * 'propMissLgth' method with a consolidated object as input
#====================================================================
 
       
setGeneric("propMissLgthCons", function(object,
                                  ...) {       
	standardGeneric("propMissLgthCons")}
)



setMethod("propMissLgthCons",signature(object="csDataCons"), function(object,
                                                                ...) { 
ca <- ca(object)
#is strata information available?
Time <- factor(ca$time,levels=as.character(unique(sort(object@hl$time))))
Space <- factor(ca$space,levels=as.character(unique(sort(object@hl$space))))
indNAtime <- all(is.na(Time))
indNAspace <- all(is.na(Space))
if (indNAtime & indNAspace) stop("no defined stratification in ca or no common strata between hl and ca!!")
#length class step
BY <- switch(as.character(ca$lenCode[1]),"mm"=1,"2mm"=2,"3mm"=3,"scm"=5,"cm"=10,"20mm"=20,"25mm"=25,"30mm"=30,"50mm"=50)
#according to available strata, alk is computed (length classes are defined according to hl table)
eval(parse('',text=paste("alk <- tapply(ca$age,list(factor(ca$lenCls,levels=seq(from=min(object@hl$lenCls,na.rm=TRUE),to=max(object@hl$lenCls,na.rm=TRUE),by=BY)),",
                         paste(c("ca$age","Time"[!indNAtime],"Space"[!indNAspace]),collapse=","),"),length)",sep="")))
#NAs --> 0
alk[is.na(alk)] <- 0
strLC <- strLChl(object)

Dim <- length(dim(alk))
if (Dim<3) {
  return(propMissLgth(alk,strLC))
} else {
index <- strLC
index[] <- rep(paste("\"",apply(expand.grid(dimnames(strLC)[2:length(dim(strLC))]),1,paste,collapse="\",\"",sep=""),"\"",sep=""),each=nrow(strLC))
return(list(pEmpty=apply(index,2:(Dim-1),function(x) eval(parse('',text=paste("propMissLgth(alk[,,",x[1],"],strLC[,",x[1],"])$pEmpty",sep="")))),
            pEmptyExtr=apply(index,2:(Dim-1),function(x) eval(parse('',text=paste("propMissLgth(alk[,,",x[1],"],strLC[,",x[1],"])$pEmptyExtr",sep=""))))))
}
})




##  #====================================================================
##  # Example of usage
##  #====================================================================
##
###Example of use of the function filling gaps in ALK
###---------------------------------------------------
##
##object example
#  #restriction to "27.7.d" & "27.7.e"
#csRaw <- subset(sole.cs,area%in%c("27.7.d","27.7.e"),table="hh")
#  #year in ca --> 2006
#csRaw@ca$year <- 2006 ; csRaw@tr$year <- 2006
#  #consolidation process
#conSole.cs <- csDataCons(csDataVal(csRaw),strIni(timeStrata="quarter",spaceStrata="area"))
#head(conSole.cs)
#
##---------------------------
#
##first view of alk information from conSole.cs@ca
#viewGapsAlkCons(conSole.cs)  
#  # . are LC that are not recorded in hl for the specified stratum
#  # - are LC that are recorded in hl but not in ca, for the specified stratum
#  # --> Nothing in CA for Q1 VIId and Q4 VIIe and no information in HL for Q2 VIIe !!
##empty LC statistics
#propMissLgthCons(conSole.cs)
#  #--> pEmpty = proportion of missing class per alk (according to hl's LC)
#  #--> pEmptyExtr = proportion of extrema missing class par alk (= 1 - proportion of 'surrounded' missing class)
#
##---------------------------
#
##1st METHOD : Raising length class step to value in mm  (type="stepIncr")
#res1 <- alkLgthRec(conSole.cs,type="stepIncr",value=20,postview=TRUE,start=110)
#names(res1)
#
#res1$missProp  #updated statistics about missing length
#
#res1$lgthCls #updated Length Classes
##if it's allright, consolidated object can be updated --> conSole.cs1
#conSole.cs1 <- alkLgthRec(conSole.cs,type="stepIncr",value=20,postview=FALSE,update=TRUE)
#unique(sort(conSole.cs@ca$lenCls ))
#unique(sort(conSole.cs1@ca$lenCls ))
#unique(sort(conSole.cs@hl$lenCls ))
#unique(sort(conSole.cs1@hl$lenCls ))
#
##---------------------------
#
##2nd METHOD : small gaps surrounded by info (L-1, L+1) are filled  (type="fillMiss")
##value = max. number of consecutive missing lines
#res2 <- alkLgthRec(conSole.cs,type="fillMiss",value=1,postview=TRUE)
##viewing the result :
#res2$addIndTab
##or more visually, '400' length class has been filled for Q4 & VIId:
#res2$alk[c("390","400","410"),,4,1]
##and the stat is better now (no surrounded empty class anymore)
#propMissLgthCons(conSole.cs)
#res2$propMiss
##consolidated conSole.cs is updated --> conSole.cs2
#conSole.cs2 <- alkLgthRec(conSole.cs,type="fillMiss",value=1,postview=FALSE,update=TRUE)
#tail(conSole.cs2@ca)  # Addition of the 15 ind. with negative fishId (cf res2$addIndTab)
#
##---------------------------
#
####3rd METHOD : the value first length classes are grouped  (type="sExtrGrp")
###res3 <- alkLgthRec(conSole.cs,type="sExtrGrp",value=10,postview=TRUE)
###res3$lgthCls  #updated Length Classes
####consolidated conSole.cs updated --> conSole.cs3
###conSole.cs3 <- alkLgthRec(conSole.cs,type="sExtrGrp",value=10,postview=FALSE,update=TRUE)
###unique(sort(conSole.cs@hl$lenCls ))
###unique(sort(conSole.cs3@hl$lenCls ))
###
##---------------------------
#
####4th METHOD : the value last length classes are grouped  (type="lExtrGrp")
###res4 <- alkLgthRec(conSole.cs,type="lExtrGrp",value=15,postview=TRUE)
###res4$lgthCls
####consolidated conSole.cs updated --> conSole.cs4
###conSole.cs4 <- alkLgthRec(conSole.cs,type="lExtrGrp",value=15,postview=FALSE,update=TRUE)
###unique(sort(conSole.cs@hl$lenCls ))
###unique(sort(conSole.cs4@hl$lenCls ))
###
###---------------------------
#
##5th METHOD : the value first length classes are duplicated  (type="sFillMiss")
#res5 <- alkLgthRec(conSole.cs,type="sFillMiss",value=4,postview=TRUE)
#res5$addIndTab
##consolidated conSole.cs updated --> conSole.cs5
#conSole.cs5 <- alkLgthRec(conSole.cs,type="sFillMiss",value=4,postview=FALSE,update=TRUE)
#tail(conSole.cs5)  # Addition of the 13 ind. with negative fishId (cf res5$addIndTab)
#
##---------------------------
#
##6th METHOD : the value last length classes are duplicated  (type="lFillMiss")
#res6 <- alkLgthRec(conSole.cs,type="lFillMiss",value=9,postview=TRUE)
#res6$addIndTab
##consolidated conSole.cs updated --> conSole.cs6
#conSole.cs6 <- alkLgthRec(conSole.cs,type="lFillMiss",value=9,postview=FALSE,update=TRUE)
#tail(conSole.cs6)  # Addition of the 20 ind. with negative fishId (cf res6$addIndTab)
#
##---------------------------
#
####7th METHOD : each of the value small empty length classes are filled with 1 fish of first age class  (type="sFillAge")
###res7 <- alkLgthRec(conSole.cs,type="sFillAge",value=7,postview=TRUE)
###res7$addIndTab
####consolidated conSole.cs updated --> conSole.cs7
###conSole.cs7 <- alkLgthRec(conSole.cs,type="sFillAge",value=7,postview=FALSE,update=TRUE)
###tail(conSole.cs7)  # Addition of the 10 ind. with negative fishId (cf res7$addIndTab)
###
##
### It"ll be up to the user to make the best use of this!!
## Here for example, I would recode time stratification to Q1+Q2, Q3 & Q4 in VIId to avoid this missing ALK in Q1 because I know the fishery begins mid-March here.
#




