

#library(COSTdbe)                          
#load("C:/CICVenvOrig.RData")              

################################################################
#                                                              #
# CI and CV calculation from 'dbeOutput' data - object updated #
#                                                              #
################################################################



#internal functions calculating CI or CV from generic dataframe, returning formatted table : 2 methods (replicates, analytical results)
                                                                                     
  # 1. Replicates
  ######################
  
    ####################################################################
    #                                                                  #
    # ciRepFun : calculation of stratified ci from boostrap replicates #
    #                                                                  #
    ####################################################################

  #calculation from replicates                                           #object : 'dbeOutput' object 
ciRepFun <- function(object,vrbl="l",probs=c(0.025,0.975),...) {         #vrbl = "l"(length structure), "a"(age structure), "n"(total numbers) or "w"(total weights)
#'probs' parameter must be a numeric with 2 elements                     #... 'quantile' function parameters
if (!is.numeric(probs) | length(probs)!=2) stop("'probs' parameter must be a numeric with 2 elements!!")
if (probs[1]>probs[2]) stop("wrong 'probs' parameter!!")

#relevant data according to 'vrbl' parameter
  #'rep' data
dfRep <- switch(vrbl,
                l=object@lenStruc$rep,
                a=object@ageStruc$rep,
                n=object@totalN$rep,
                w=object@totalW$rep)

  #is 'vrbl' correctly defined ?
if (is.null(dfRep)) stop("wrong 'vrbl' parameter!!")
if (all(is.na(dfRep))) stop("missing data in input object!!")
  #is there any data?
if (nrow(dfRep)==0) stop("no available data!!")

#nb Iter
B <- length(unique(dfRep$iter))              #added MM 15/03/2011

#definition of aggregation fields
exprBy <- paste("length=as.numeric(as.character(dfRep$length)),"["l"%in%vrbl],
                "age=as.numeric(as.character(dfRep$age)),"["a"%in%vrbl],
                "technical=dfRep$technical,"[!all(is.na(dfRep$technical))],
                "space=dfRep$space,"[!all(is.na(dfRep$space))],
                "time=dfRep$time,"[!all(is.na(dfRep$time))],
                sep="",collapse="")
#last character is removed
exprBy <- substr(exprBy,1,nchar(exprBy)-1)
#stratified CI
CIdf1 <- CIdf2 <- NULL

if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("CIdf1 <- aggregate(dfRep$value,list(",exprBy,"),function(x) quantile(x,probs=probs,...)[1])",sep=""))) ; names(CIdf1)[ncol(CIdf1)] <- "inf"         #modified MM 15/03/2011
  eval(parse('',text=paste("CIdf2 <- aggregate(dfRep$value,list(",exprBy,"),function(x) quantile(x,probs=probs,...)[2])",sep=""))) ; names(CIdf2)[ncol(CIdf2)] <- "sup"         #
  eval(parse('',text=paste("CIdf3 <- aggregate(dfRep$value,list(",exprBy,"),mean,na.rm=TRUE)",sep=""))) ; names(CIdf3)[ncol(CIdf3)] <- "value"                   #
} else {
  eval(parse('',text=paste("CIdf1 <- aggregate(dfRep$value,list(",exprBy,"),function(x) quantile(c(x,rep(0,length=B-length(x))),probs=probs,...)[1])",sep=""))) ; names(CIdf1)[ncol(CIdf1)] <- "inf"         #modified MM 15/03/2011
  eval(parse('',text=paste("CIdf2 <- aggregate(dfRep$value,list(",exprBy,"),function(x) quantile(c(x,rep(0,length=B-length(x))),probs=probs,...)[2])",sep=""))) ; names(CIdf2)[ncol(CIdf2)] <- "sup"         #
  eval(parse('',text=paste("CIdf3 <- aggregate(dfRep$value,list(",exprBy,"),function(x) mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))",sep=""))) ; names(CIdf3)[ncol(CIdf3)] <- "value"                   #
}
#result
df <- merge(CIdf1,CIdf2,sort=FALSE)
df <- merge(df,CIdf3,sort=FALSE)

#formatting process
DF <- dfRep[rep(1,nrow(df)),-match(c("iter"),names(dfRep))]
DF$sup <- DF$inf <- DF$value <- 0
invisible(sapply(names(df),function(x) DF[,x] <<- df[,x]))
rownames(DF) <- 1:nrow(DF)

return(DF)
}




    ####################################################################
    #                                                                  #
    # cvRepFun : calculation of stratified cv from boostrap replicates #
    #                                                                  #
    ####################################################################

  #calculation from replicates                      #object : 'dbeOutput' object                      
cvRepFun <- function(object,vrbl="l",...) {         #vrbl = "l"(length structure), "a"(age structure), "n"(total numbers) or "w"(total weights)
                                                    #... 'quantile' function parameters
#relevant data according to 'vrbl' parameter
  #'rep' data
dfRep <- switch(vrbl,
                l=object@lenStruc$rep,
                a=object@ageStruc$rep,
                n=object@totalN$rep,
                w=object@totalW$rep)

  #is 'vrbl' correctly defined ?
if (is.null(dfRep)) stop("wrong 'vrbl' parameter!!")
if (all(is.na(dfRep))) stop("missing data in input object!!")
  #is there any data?
if (nrow(dfRep)==0) stop("no available data!!")

#nb Iter
B <- length(unique(dfRep$iter))              #added MM 15/03/2011

#definition of aggregation fields
exprBy <- paste("length=as.numeric(as.character(dfRep$length)),"["l"%in%vrbl],
                "age=as.numeric(as.character(dfRep$age)),"["a"%in%vrbl],
                "technical=dfRep$technical,"[!all(is.na(dfRep$technical))],
                "space=dfRep$space,"[!all(is.na(dfRep$space))],
                "time=dfRep$time,"[!all(is.na(dfRep$time))],
                sep="",collapse="")
#last character is removed
exprBy <- substr(exprBy,1,nchar(exprBy)-1)
#stratified CV
CVdf <- NULL
if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("CVdf <- aggregate(dfRep$value,list(",exprBy,"),function(x) sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))",sep="")))     #added MM 15/03/2011
} else {
  eval(parse('',text=paste("CVdf <- aggregate(dfRep$value,list(",exprBy,"),function(x) sd(c(x,rep(0,length=B-length(x))),na.rm=TRUE)/mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))",sep="")))     #added MM 15/03/2011
}
names(CVdf)[ncol(CVdf)] <- "value" 

#formatting process
DF <- dfRep[rep(1,nrow(CVdf)),-match(c("iter"),names(dfRep))]
invisible(sapply(names(CVdf),function(x) DF[,x] <<- CVdf[,x]))
rownames(DF) <- 1:nrow(DF)

#dcrCvIndicator calculation
#stratified estimates
ESTdf <- NULL
if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("ESTdf <- aggregate(dfRep$value,list(",exprBy,"),mean,na.rm=TRUE)",sep=""))) 
} else {
  eval(parse('',text=paste("ESTdf <- aggregate(dfRep$value,list(",exprBy,"),function(x) mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))",sep=""))) 
}
names(ESTdf)[ncol(ESTdf)] <- "value" 
dcrInd <- sum(CVdf$value*ESTdf$value/sum(ESTdf$value,na.rm=TRUE),na.rm=TRUE)

return(list(DF=DF,dcrInd=dcrInd))
}





  # 2. Estimates
  ######################



    ##########################################################
    #                                                        #
    # ciEstFun : calculation of stratified ci from estimates #
    #                                                        #
    ##########################################################

  #calculation from estimates                                            #object : 'dbeOutput' object 
ciEstFun <- function(object,vrbl="l",probs=c(0.025,0.975),...) {         #vrbl = "l"(length structure), "a"(age structure), "n"(total numbers) or "w"(total weights)
#'probs' parameter must be a numeric with 2 elements
if (!is.numeric(probs) | length(probs)!=2) stop("'probs' parameter must be a numeric with 2 elements!!")
if (probs[1]>probs[2]) stop("wrong 'probs' parameter!!")

#relevant data according to 'vrbl' parameter
  #"estim" data
dfEstim <- switch(vrbl,
                l=object@lenStruc$estim,
                a=object@ageStruc$estim,
                n=object@totalN$estim,
                w=object@totalW$estim)
               
  #'var' data
dfVar <- switch(vrbl,
                l=object@lenVar,
                a=object@ageVar,
                n=object@totalNvar,
                w=object@totalWvar)

  #is 'vrbl' correctly defined ?
if (is.null(dfEstim) | is.null(dfVar)) stop("wrong 'vrbl' parameter!!")
if (all(is.na(dfEstim)) | all(is.na(dfVar))) stop("missing data in input object!!")
  #is there any data?
if (nrow(dfEstim)==0 | nrow(dfVar)==0) stop("missing data in input object!!")
  #are the tables consistent?
if (!identical(dim(dfEstim),dim(dfVar))) stop("tables are not matching!!") 

#both tables are merged
names(dfVar)[ncol(dfVar)] <- "var"
df <- merge(dfEstim,dfVar,sort=FALSE)
  #is number of rows the same in df than in previous tables?
if (!identical(nrow(dfEstim),nrow(df))) stop("tables are not matching!!") 

#'inf' and 'sup' fields are calulated, and pasted to df
qInd <- qnorm(probs)
dfInfSup <- data.frame(as.vector(df$value)+t(qInd%*%t(sqrt(df$var)))) ; names(dfInfSup) <- c("inf","sup")
df <- cbind(df,dfInfSup)

#problems of negative bounds
#if (any(c(df$inf,df$sup)<0)) warning("negative CI bound(s)!!")                  
#negative values are put to 0
df$inf[df$inf<0] <- 0
#if parameter is maturity ratio or sex ratio, then values > 1 are put to 1
if (object@param%in%c("sex","maturity")) df$sup[df$sup>1] <- 1

#formatting process
DF <- df[,c(names(dfEstim),"inf","sup")]
rownames(DF) <- 1:nrow(DF)

return(DF)
}




    ##########################################################
    #                                                        #
    # cvEstFun : calculation of stratified cv from estimates #
    #                                                        #
    ##########################################################

  #calculation from estimates                       #object : 'dbeOutput' object                    
cvEstFun <- function(object,vrbl="l",...) {         #vrbl = "l"(length structure), "a"(age structure), "n"(total numbers) or "w"(total weights)

#relevant data according to 'vrbl' parameter
  #"estim" data
dfEstim <- switch(vrbl,
                l=object@lenStruc$estim,
                a=object@ageStruc$estim,
                n=object@totalN$estim,
                w=object@totalW$estim)
                
  #'var' data
dfVar <- switch(vrbl,
                l=object@lenVar,
                a=object@ageVar,
                n=object@totalNvar,
                w=object@totalWvar)
                            
  #is 'vrbl' correctly defined ?
if (is.null(dfEstim) | is.null(dfVar)) stop("wrong 'vrbl' parameter!!")
if (all(is.na(dfEstim)) | all(is.na(dfVar))) stop("missing data in input object!!")
  #is there any data?
if (nrow(dfEstim)==0 | nrow(dfVar)==0) stop("missing data in input object!!")
  #are the tables consistent?
if (!identical(dim(dfEstim),dim(dfVar))) stop("tables are not matching!!") 

#both tables are merged
nam <- names(dfEstim)
names(dfEstim)[ncol(dfEstim)] <- "estim"
names(dfVar)[ncol(dfVar)] <- "var"
df <- merge(dfEstim,dfVar,sort=FALSE)
  #is number of rows the same in df than in previous tables?
if (!identical(nrow(dfEstim),nrow(df))) stop("tables are not matching!!") 

#CV is calulated, and inserted in df
df$value <- sqrt(df$var)/df$estim

#formatting process
DF <- df[,nam]
rownames(DF) <- 1:nrow(DF)

#dcrCvIndicator calculation 
dcrInd <- sum(DF$value*dfEstim$estim/sum(dfEstim$estim,na.rm=TRUE),na.rm=TRUE)

return(list(DF=DF,dcrInd=dcrInd))

}



#####################################################################################
#####################################################################################
#####################                                         #######################
#####################                 Methods                 #######################
#####################              dbeCI - dbeCV              #######################
#####################                                         #######################
#####################################################################################
#####################################################################################




        
setGeneric("dbeCalc", function(object,              # 'dbeOutput' object
                               type="CI",           # "CI" for confidence interval, or "CV" for coefficient of variation
                               vrbl="l",            # specifies data on which calculation is applied : "l" for length structure, "a" for age structure, "n" for total number estimates, "w" for total weight estimates
                               probs=c(0.025,0.975),# used only if type="CI", defines bounds
                               replicates=FALSE,    # if TRUE, calculation is made from $rep elements ; if FALSE, $estim and @...Var are used 
                               update=TRUE,         # if TRUE, updated 'dbeOutput' object is returned ; if FALSE, only resulting dataframe is returned
                               ...){
standardGeneric("dbeCalc")
})



 
setMethod("dbeCalc",signature(object="dbeOutput"),function(object,              #'dbeOutput' object 
                                                           type="CI",           # "CI" for confidence interval, or "CV" for coefficient of variation
                                                           vrbl="l",            # specifies data on which calculation is applied : "l" for length structure, "a" for age structure, "n" for total number estimates, "w" for total weight estimates
                                                           probs=c(0.025,0.975),# used only if type="CI", defines bounds
                                                           replicates=FALSE,    # if TRUE, calculation is made from $rep elements ; if FALSE, $estim and @...Var are used 
                                                           update=TRUE,         # if TRUE, updated 'dbeOutput' object is returned ; if FALSE, only resulting dataframe is returned
                                                           ...){                                

outpuT <- NULL
#according to input parameters, one of the previous functions is to be used
lType <- tolower(type)
if (!lType%in%c("ci","cv")) stop("wrong 'type' parameter!!") 
if (!is.logical(replicates)) stop("wrong 'replicates' parameter!!") 
if (!is.logical(update)) stop("wrong 'update' parameter!!")

if (replicates) lData <- "Rep" else lData <- "Est" 

#so, expected table is...
eval(parse('',text=paste("outpuT <- ",lType,lData,"Fun(object=object,vrbl=vrbl,probs=probs,...)",sep=""))) 

if (lType=="cv") 
  {dcrInd <- outpuT$dcrInd ; output <- outpuT$DF
} else {
  output <- outpuT}

#output depends on 'update' parameter
if (update) {
  #slot to update must be identified 
  updSlot <- switch(vrbl,
                    l="lenNum",
                    a="ageNum",
                    n="totalNnum",
                    w="totalWnum")
  #and then, object is updated and returned
  eval(parse('',text=paste("object@",updSlot,"$",lType," <- output",sep="")))
  if (lType=="cv") eval(parse('',text=paste("object@",updSlot,"$DCRcvIndicator <- dcrInd",sep=""))) 
  return(object)
} else {
  return(outpuT)
}

})


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

############ 
# Examples #
############ 
 
#dbeCalc(object) 
#dbeCalc(object,type="CV") 
#dbeCalc(object,type="CI",vrbl="a") 
#dbeCalc(object,type="CI",vrbl="n",probs=c(0.1,0.9)) #80% 
#object <- dbeCalc(object,type="ci",replicates=TRUE,update=TRUE) 
#object <- dbeCalc(object,type="cv",vrbl="a",replicates=TRUE,update=TRUE) 
#object <- dbeCalc(object,type="ci",vrbl="n",update=TRUE) 
#object <- dbeCalc(object,type="cv",vrbl="n",update=TRUE) 
 
 
 
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

## 'stratAgreg' method for aggegating 'dbeOutput' tables (estim & var) : other output tables are empty
## WARNING : summing the variances requires strong probability assumptions
 

setGeneric("stratAggreg", function(object,                  # 'dbeOutput' object
                                  timeStrata=TRUE,         # if TRUE, aggregation is made over time strata
                                  spaceStrata=TRUE,        # if TRUE, aggregation is made over space strata
                                  techStrata=FALSE,        # if TRUE, aggregation is made over technical strata
                                  wt="totalW",             # can be also "totalN" : weights values for weighted mean calculation (param="weight", "maturity" or "sex")
                                  incl.precision=TRUE,
                                  probs=c(0.025,0.975),
                                  ...){
standardGeneric("stratAggreg")
})



 
setMethod("stratAggreg", signature(object="dbeOutput"),function(object,                  # 'dbeOutput' object
                                                               timeStrata=TRUE,         # if TRUE, aggregation is made over time strata
                                                               spaceStrata=TRUE,        # if TRUE, aggregation is made over space strata
                                                               techStrata=FALSE,        # if TRUE, aggregation is made over technical strata
                                                               wt="totalW",             # can be also "totalN" : weights values for weighted mean calculation (param="weight", "maturity" or "sex")
                                                               incl.precision=TRUE,
                                                               probs=c(0.025,0.975),
                                                               ...){

#'strataDesc' field is updated according to input parameters                                                            #
if (timeStrata) {object@strataDesc@timeStrata <- as.character(NA) ; object@strataDesc@tpRec <- list(NA)}                # ADDED MM : 22/04/2009 
if (spaceStrata) {object@strataDesc@spaceStrata <- as.character(NA) ; object@strataDesc@spRec <- list(NA)}              #
if (techStrata) {object@strataDesc@techStrata <- as.character(NA) ; object@strataDesc@tcRec <- list(NA)}                #

wtTab <- slot(object,wt)$estim

#subfunctions applied to each table 

  #reorder the replicates table
ordRep <- function(tab) {

if ("iter"%in%names(tab)){
  tab <- tab[order(tab$iter),]
  rownames(tab) <- 1:nrow(tab)
}

return(tab)
}
 
  #simple sum 
agg <- function(tab,nSampAge=FALSE) {

if (all(is.na(tab))) {
  return(tab)
} else {
  if (timeStrata) tab$time <- "all"
  if (spaceStrata) tab$space <- "all"
  if (!nSampAge & techStrata) tab$technical <- "all"
  newTab <- aggregate(tab$value,as.list(tab[,rev((1:ncol(tab))[-match("value",names(tab))])]),sum,na.rm=TRUE)
  names(newTab)[ncol(newTab)] <- "value"
  return(ordRep(newTab[,names(tab)]))
}}


  #weighted mean of estimates
aggWt <- function(tab,Wt,void) {
names(Wt)[ncol(Wt)] <- "wt"
if (all(is.na(tab)) | all(is.na(Wt))) {
  return(void)
} else {  
TAB <- merge(tab,Wt,all.x=TRUE,sort=FALSE) 
TAB$value[is.na(TAB$wt)] <- NA ; TAB$wt[is.na(TAB$value)] <- NA 
TAB$value <- TAB$value*TAB$wt
  if (timeStrata) TAB$time <- "all" 
  if (spaceStrata) TAB$space <- "all" 
  if (techStrata) TAB$technical <- "all"
  newTab <- aggregate(cbind(TAB$value,TAB$wt),as.list(TAB[,rev((1:ncol(TAB))[-match(c("value","wt"),names(TAB))])]),sum,na.rm=TRUE)
  names(newTab)[ncol(newTab)-c(1:0)] <- c("value","wt")
  newTab$value <- newTab$value/newTab$wt ; newTab$value[is.nan(newTab$value)] <- NA
  return(ordRep(newTab[,names(tab)]))
}}


  #weighted 'sum' of variances (assuming that variables are iid)  : Var((a.w1 + b.w2)/(w1+w2)) = (w1^2*Var(a) + w2^2*Var(b))/(w1+w2)^2
aggWtVar <- function(tab,Wt,void) {
names(Wt)[ncol(Wt)] <- "wt"
if (all(is.na(tab)) | all(is.na(Wt))) {
  return(void)
} else {
TAB <- merge(tab,Wt,all.x=TRUE,sort=FALSE) 
TAB$value[is.na(TAB$wt)] <- NA ; TAB$wt[is.na(TAB$value)] <- NA 
TAB$value <- TAB$value*TAB$wt*TAB$wt
  if (timeStrata) TAB$time <- "all" 
  if (spaceStrata) TAB$space <- "all" 
  if (techStrata) TAB$technical <- "all"
  newTab <- aggregate(cbind(TAB$value,TAB$wt),as.list(TAB[,rev((1:ncol(TAB))[-match(c("value","wt"),names(TAB))])]),sum,na.rm=TRUE)
  names(newTab)[ncol(newTab)-c(1:0)] <- c("value","wt")
  newTab$value <- newTab$value/(newTab$wt^2) ; newTab$value[is.nan(newTab$value)] <- NA
  return(ordRep(newTab[,names(tab)]))
}}


  object@nSamp$len <- agg(object@nSamp$len)
  object@nSamp$age <- agg(object@nSamp$age,nSampAge=TRUE)
  object@nMeas$len <- agg(object@nMeas$len)
  object@nMeas$age <- agg(object@nMeas$age,nSampAge=TRUE)

  #object@lenStruc$rep <- new("dbeOutput")@lenStruc$rep
  object@lenNum$ci <- new("dbeOutput")@lenNum$ci
  object@lenNum$cv <- new("dbeOutput")@lenNum$cv
  object@lenNum$DCRcvIndicator <- new("dbeOutput")@lenNum$DCRcvIndicator

  #object@ageStruc$rep <- new("dbeOutput")@ageStruc$rep
  object@ageNum$ci <- new("dbeOutput")@ageNum$ci
  object@ageNum$cv <- new("dbeOutput")@ageNum$cv
  object@ageNum$DCRcvIndicator <- new("dbeOutput")@ageNum$DCRcvIndicator

  object@totalN$estim <- agg(object@totalN$estim)
  object@totalN$rep <- agg(object@totalN$rep)
  object@totalNvar <- agg(object@totalNvar)
  object@totalNnum$ci <- new("dbeOutput")@totalNnum$ci
  object@totalNnum$cv <- new("dbeOutput")@totalNnum$cv
  object@totalNnum$DCRcvIndicator <- new("dbeOutput")@totalNnum$DCRcvIndicator

  object@totalW$estim <- agg(object@totalW$estim)
  object@totalW$rep <- agg(object@totalW$rep)
  object@totalWvar <- agg(object@totalWvar)
  object@totalWnum$ci <- new("dbeOutput")@totalWnum$ci
  object@totalWnum$cv <- new("dbeOutput")@totalWnum$cv
  object@totalWnum$DCRcvIndicator <- new("dbeOutput")@totalWnum$DCRcvIndicator

if (object@param%in%c("weight","maturity","sex")) {

  object@lenStruc$estim <- aggWt(object@lenStruc$estim,wtTab,new("dbeOutput")@lenStruc$estim)
  object@lenStruc$rep <- aggWt(object@lenStruc$rep,wtTab,new("dbeOutput")@lenStruc$rep)
  object@lenVar <- aggWtVar(object@lenVar,wtTab,new("dbeOutput")@lenVar)

  object@ageStruc$estim <- aggWt(object@ageStruc$estim,wtTab,new("dbeOutput")@ageStruc$estim)
  object@ageStruc$rep <- aggWt(object@ageStruc$rep,wtTab,new("dbeOutput")@ageStruc$rep)
  object@ageVar <- aggWtVar(object@ageVar,wtTab,new("dbeOutput")@ageVar)

} else {

  object@lenStruc$estim <- agg(object@lenStruc$estim)
  object@lenStruc$rep <- agg(object@lenStruc$rep)
  object@lenVar <- agg(object@lenVar)

  object@ageStruc$estim <- agg(object@ageStruc$estim)
  object@ageStruc$rep <- agg(object@ageStruc$rep)
  object@ageVar <- agg(object@ageVar)
}

if (incl.precision) {

  if (!all(is.na(object@lenStruc$rep))) {
    
    object <- dbeCalc(object,type="CV",vrbl="l",replicates=TRUE,update=TRUE)
    object <- dbeCalc(object,type="CI",vrbl="l",probs=probs,replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(object@lenStruc$estim)) & !all(is.na(object@lenVar))) {
      object <- dbeCalc(object,type="CV",vrbl="l",replicates=FALSE,update=TRUE)
      object <- dbeCalc(object,type="CI",vrbl="l",probs=probs,replicates=FALSE,update=TRUE)
    }
  }



  if (!all(is.na(object@totalN$rep))) {
  
    object <- dbeCalc(object,type="CV",vrbl="n",replicates=TRUE,update=TRUE)
    object <- dbeCalc(object,type="CI",vrbl="n",probs=probs,replicates=TRUE,update=TRUE)
  
  } else {
  
    if (!all(is.na(object@totalN$estim)) & !all(is.na(object@totalNvar))) {
      object <- dbeCalc(object,type="CV",vrbl="n",replicates=FALSE,update=TRUE)
      object <- dbeCalc(object,type="CI",vrbl="n",probs=probs,replicates=FALSE,update=TRUE)
    }
  }



  if (!all(is.na(object@totalW$rep))) {
  
    object <- dbeCalc(object,type="CV",vrbl="w",replicates=TRUE,update=TRUE)
    object <- dbeCalc(object,type="CI",vrbl="w",probs=probs,replicates=TRUE,update=TRUE)
  
  } else {
  
    if (!all(is.na(object@totalW$estim)) & !all(is.na(object@totalWvar))) {
      object <- dbeCalc(object,type="CV",vrbl="w",replicates=FALSE,update=TRUE)
      object <- dbeCalc(object,type="CI",vrbl="w",probs=probs,replicates=FALSE,update=TRUE)
    }
  }



  if (!all(is.na(object@ageStruc$rep))) {
  
    object <- dbeCalc(object,type="CV",vrbl="a",replicates=TRUE,update=TRUE)
    object <- dbeCalc(object,type="CI",vrbl="a",probs=probs,replicates=TRUE,update=TRUE)
  
  } else {
  
    if (!all(is.na(object@ageStruc$estim)) & !all(is.na(object@ageVar))) {
      object <- dbeCalc(object,type="CV",vrbl="a",replicates=FALSE,update=TRUE)
      object <- dbeCalc(object,type="CI",vrbl="a",probs=probs,replicates=FALSE,update=TRUE)
    }
  }

}

return(object)

})



                                                                   