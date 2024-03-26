
#library(COSTcore)
#
#====================================================================
# dbeObj : outcome object for all COSTdbe package methods ouputs
#====================================================================


setClass("dbeOutput",
	representation(
    desc="character",                        #descriptor
    species="character",                     #recall of SL$spp (+ SL$sex)
		catchCat="character",                    #recall of the catch category (discards/landings)
		param="character",                       #recall of the parameter estimated (N, W, maturity, sex-ratio,...)
		strataDesc="strIni",                     #time, space and technical stratification considered
		methodDesc="character",                  #recall of the method (analytical, bootstrap, bayesian)
		nSamp="list",                            #number of samples
		nMeas="list",                            #number of individual measures
		lenStruc="list",                         #estimates of the length structure (param-at-length)
		lenVar="data.frame",                     #estimates of the variance of '$lenStruc'
		lenNum="list",                           #further numerical data about length structure (ex: ci, cv) 
		ageStruc="list",                         #estimates of the age structure (param-at-age)
		ageVar="data.frame",                     #estimates of the variance of '$ageStruc'
		ageNum="list",                           #further numerical data about age structure (ex: ci, cv) 
		totalN="list",                           #estimates of the total number of the parameters
		totalNvar="data.frame",                  #estimates of the variance of '$totalN'
		totalNnum="list",                        #further numerical data about total numbers (ex: ci, cv) 
		totalW="list",                           #estimates of the total weight of the parameters
		totalWvar="data.frame",                  #estimates of the variance of '$totalW'
		totalWnum="list"                         #further numerical data about total weights (ex: ci, cv) 
	),
	prototype(
    desc="dbeObject",
		species=as.character(NA),
		catchCat=as.character(NA),
		param=as.character(NA),
		strataDesc=strIni(),
		methodDesc=as.character(NA),
		nSamp=list(
      len=data.frame(
        time=as.character(NA),
        space=as.character(NA),
        technical=as.character(NA),
        value=as.numeric(NA)),
      age=data.frame(
        time=as.character(NA),
        space=as.character(NA),
        value=as.numeric(NA))
      ),
		nMeas=list(      
      len=data.frame(
        time=as.character(NA),
        space=as.character(NA),
        technical=as.character(NA),
        value=as.numeric(NA)),
      age=data.frame(
        time=as.character(NA),
        space=as.character(NA),
        value=as.numeric(NA))
      ),
		lenStruc=list(
        estim=data.frame(      
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          length=as.character(NA),
          value=as.numeric(NA)),
        rep=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          length=as.character(NA),
          value=as.numeric(NA),
          iter=as.numeric(NA))
          ),
		lenVar=data.frame(      
      time=as.character(NA),
      space=as.character(NA),
      technical=as.character(NA),
      length=as.character(NA),
      value=as.numeric(NA)),
    lenNum=list(
        ci=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          length=as.character(NA),
          value=as.numeric(NA),
          inf=as.numeric(NA),
          sup=as.numeric(NA)),
        cv=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          length=as.character(NA),
          value=as.numeric(NA)),
        DCRcvIndicator=as.numeric(NA)
        ),    
		ageStruc=list(
        estim=data.frame(      
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          age=as.character(NA),
          value=as.numeric(NA)),
        rep=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          age=as.character(NA),
          value=as.numeric(NA),
          iter=as.numeric(NA))
          ),
		ageVar=data.frame(      
      time=as.character(NA),
      space=as.character(NA),
      technical=as.character(NA),
      age=as.character(NA),
      value=as.numeric(NA)),
    ageNum=list(
        ci=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          age=as.character(NA),
          value=as.numeric(NA),
          inf=as.numeric(NA),
          sup=as.numeric(NA)),
        cv=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          age=as.character(NA),
          value=as.numeric(NA)),
        DCRcvIndicator=as.numeric(NA)
        ),    
		totalN=list(
        estim=data.frame(      
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA)),
        rep=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA),
          iter=as.numeric(NA))
          ),
		totalNvar=data.frame(      
      time=as.character(NA),
      space=as.character(NA),
      technical=as.character(NA),
      value=as.numeric(NA)),
    totalNnum=list(
        ci=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA),
          inf=as.numeric(NA),
          sup=as.numeric(NA)),
        cv=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA)),
        DCRcvIndicator=as.numeric(NA)
        ),    
		totalW=list(
        estim=data.frame(      
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA)),
        rep=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA),
          iter=as.numeric(NA))
          ),
		totalWvar=data.frame(      
      time=as.character(NA),
      space=as.character(NA),
      technical=as.character(NA),
      value=as.numeric(NA)),
    totalWnum=list(
        ci=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA),
          inf=as.numeric(NA),
          sup=as.numeric(NA)),
        cv=data.frame(
          time=as.character(NA),
          space=as.character(NA),
          technical=as.character(NA),
          value=as.numeric(NA)),
        DCRcvIndicator=as.numeric(NA)
        )    
	)
)



#====================================================================
# 'dbeOutput' object constructor (initialization)
#====================================================================

dbeObject <- function(desc, species, catchCat, param, strataDesc, methodDesc, ...){

if (missing(desc)) desc <- as.character(NA)
if (missing(species)|all(is.na(species))) stop("Missing 'species' parameter!!")
if (missing(catchCat)) catchCat <- "LAN"
if ((length(catchCat)>1)|(all(is.na(catchCat)))) catchCat <- "LAN"
if (missing(param)) param <- as.character(NA)
if (missing(strataDesc)) strataDesc <- strIni()
if (missing(methodDesc)) methodDesc <- as.character(NA)

new("dbeOutput",desc=desc,species=species,catchCat=toupper(catchCat),param=param,strataDesc=strataDesc,methodDesc=methodDesc)
 	  }
 	  

#====================================================================
# 'rbind2' method for 'dbeOutput' object 
#====================================================================

 
setMethod("rbind2", signature(x="dbeOutput", y="dbeOutput"), function(x,y){

#subfunction
rBind2 <- function(tab1,tab2){
if (all(is.na(tab1))) {
  res <- tab2
} else {
  if (all(is.na(tab2))) {
    res <- tab1
  } else {
    res <- rbind2(tab1,tab2)
    rownames(res) <- 1:nrow(res)
  }
}
return(res)
}

elt <- c("nSamp$len","nSamp$age","nMeas$len","nMeas$age",
         "lenStruc$estim","lenStruc$rep","lenVar","lenNum$ci","lenNum$cv","lenNum$DCRcvIndicator",
         "ageStruc$estim","ageStruc$rep","ageVar","ageNum$ci","ageNum$cv","ageNum$DCRcvIndicator",
         "totalN$estim","totalN$rep","totalNvar","totalNnum$ci","totalNnum$cv","totalNnum$DCRcvIndicator",
         "totalW$estim","totalW$rep","totalWvar","totalWnum$ci","totalWnum$cv","totalWnum$DCRcvIndicator")

invisible(sapply(elt,function(z) eval(parse('',text=paste("x@",z," <<- rBind2(x@",z,",y@",z,")",sep=""))))) 
#'DCRcvIndicator' elements are set to NA (could be recalculated from x & y : IND = [ INDx * sum(ESTx) + INDy * sum(ESTy) ] / sum (ESTx + ESTy) <<- left to be implemented)
x@lenNum$DCRcvIndicator <- x@ageNum$DCRcvIndicator <- x@totalNnum$DCRcvIndicator <- x@totalWnum$DCRcvIndicator <- NA

return(x)
})


                   
#====================================================================
# '+' method for 'dbeOutput' object 
#====================================================================

	
   
     
setMethod("+", signature(e1="dbeOutput", e2="dbeOutput"), function(e1,e2){

#subfunction
addDBE <- function(tab1,tab2) {                
if (all(is.na(tab1))) {
  res <- tab2
} else {
if (all(is.na(tab2))) {
  res <- tab1
} else {
  TAB <- rbind(tab1,tab2)
  if ("time"%in%names(TAB)) TAB$time <- factor(as.character(TAB$time))                                      #modif MM 15/03/2011
  if ("space"%in%names(TAB)) TAB$space <- factor(as.character(TAB$space))                                   #
  if ("technical"%in%names(TAB)) TAB$technical <- factor(as.character(TAB$technical))                       #
  if ("length"%in%names(TAB)) TAB$length <- factor(as.numeric(as.character(TAB$length)))                    #
  if ("age"%in%names(TAB)) TAB$age <- factor(as.numeric(as.character(TAB$age)))                             #
  res <- aggregate(TAB$value,as.list(TAB[,rev(names(TAB)[-match("value",names(TAB))])]),sum,na.rm=TRUE)
  names(res)[ncol(res)] <- "value"
}}
return(res[,names(tab1)])
} 

eT <- e1
elt <- c("nSamp$len","nSamp$age","nMeas$len","nMeas$age",
         "lenStruc$estim","lenStruc$rep","lenVar",
         "ageStruc$estim","ageStruc$rep","ageVar",
         "totalN$estim","totalN$rep","totalNvar",
         "totalW$estim","totalW$rep","totalWvar")

#invisible(sapply(elt,function(z) eval(parse('',text=paste("eT@",z," <<- addDBE(e1@",z,",e2@",z,")",sep="")))))    #probleme d'ecrasement de 'e1' lors de la premiere iteration (nSamp ecrase??!??)
for (i in elt) eval(parse('',text=paste("eT@",i," <- addDBE(e1@",i,",e2@",i,")",sep="")))


eltNA <- c("lenNum$ci","lenNum$cv","lenNum$DCRcvIndicator",
           "ageNum$ci","ageNum$cv","ageNum$DCRcvIndicator",
           "totalNnum$ci","totalNnum$cv","totalNnum$DCRcvIndicator",
           "totalWnum$ci","totalWnum$cv","totalWnum$DCRcvIndicator")

#invisible(sapply(eltNA,function(z) eval(parse('',text=paste("eT@",z," <<- new(\"dbeOutput\")@",z,sep="")))))      #idem
for (i in eltNA) eval(parse('',text=paste("eT@",i," <- new(\"dbeOutput\")@",i,sep="")))

if (!all(is.na(e1@lenNum$ci)) & !all(is.na(e2@lenNum$ci))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@lenStruc$rep))) {
    
    eT <- dbeCalc(eT,type="CI",vrbl="l",probs=c(0.025,0.975),replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@lenStruc$estim)) & !all(is.na(eT@lenVar))) {

      eT <- dbeCalc(eT,type="CI",vrbl="l",probs=c(0.025,0.975),replicates=FALSE,update=TRUE)
    
    }
  }
}

if (!all(is.na(e1@lenNum$cv)) & !all(is.na(e2@lenNum$cv))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@lenStruc$rep))) {
    
    eT <- dbeCalc(eT,type="CV",vrbl="l",replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@lenStruc$estim)) & !all(is.na(eT@lenVar))) {
      
      eT <- dbeCalc(eT,type="CV",vrbl="l",replicates=FALSE,update=TRUE)

    }
  }
}

if (!all(is.na(e1@ageNum$ci)) & !all(is.na(e2@ageNum$ci))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@ageStruc$rep))) {
    
    eT <- dbeCalc(eT,type="CI",vrbl="a",probs=c(0.025,0.975),replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@ageStruc$estim)) & !all(is.na(eT@ageVar))) {

      eT <- dbeCalc(eT,type="CI",vrbl="a",probs=c(0.025,0.975),replicates=FALSE,update=TRUE)
    
    }
  }
}

if (!all(is.na(e1@ageNum$cv)) & !all(is.na(e2@ageNum$cv))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@ageStruc$rep))) {
    
    eT <- dbeCalc(eT,type="CV",vrbl="a",replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@ageStruc$estim)) & !all(is.na(eT@ageVar))) {
      
      eT <- dbeCalc(eT,type="CV",vrbl="a",replicates=FALSE,update=TRUE)

    }
  }
}

if (!all(is.na(e1@totalNnum$ci)) & !all(is.na(e2@totalNnum$ci))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@totalN$rep))) {
    
    eT <- dbeCalc(eT,type="CI",vrbl="n",probs=c(0.025,0.975),replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@totalN$estim)) & !all(is.na(eT@totalNvar))) {

      eT <- dbeCalc(eT,type="CI",vrbl="n",probs=c(0.025,0.975),replicates=FALSE,update=TRUE)
    
    }
  }
}

if (!all(is.na(e1@totalNnum$cv)) & !all(is.na(e2@totalNnum$cv))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@totalN$rep))) {
    
    eT <- dbeCalc(eT,type="CV",vrbl="n",replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@totalN$estim)) & !all(is.na(eT@totalNvar))) {
      
      eT <- dbeCalc(eT,type="CV",vrbl="n",replicates=FALSE,update=TRUE)

    }
  }
}

if (!all(is.na(e1@totalWnum$ci)) & !all(is.na(e2@totalWnum$ci))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@totalW$rep))) {
    
    eT <- dbeCalc(eT,type="CI",vrbl="w",probs=c(0.025,0.975),replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@totalW$estim)) & !all(is.na(eT@totalWvar))) {

      eT <- dbeCalc(eT,type="CI",vrbl="w",probs=c(0.025,0.975),replicates=FALSE,update=TRUE)
    
    }
  }
}

if (!all(is.na(e1@totalWnum$cv)) & !all(is.na(e2@totalWnum$cv))) { #alors, on procede au calcul!!

  if (!all(is.na(eT@totalW$rep))) {
    
    eT <- dbeCalc(eT,type="CV",vrbl="w",replicates=TRUE,update=TRUE)
    
  } else {
  
    if (!all(is.na(eT@totalW$estim)) & !all(is.na(eT@totalWvar))) {
      
      eT <- dbeCalc(eT,type="CV",vrbl="w",replicates=FALSE,update=TRUE)

    }
  }
}

return(eT)

})



