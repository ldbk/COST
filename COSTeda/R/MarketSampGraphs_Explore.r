


#################################################################################################
##                                                                                             ##
## Plots of volume of landings and number of fish measured by time, technical and space strata ##
##                                                                                             ##
##                                   MM 06/02/2008                                             ##
#################################################################################################


      ##########################
      ##                      ##
      ##  Internal functions  ##
      ##                      ##
      ##########################




#-------------------------------------------------------------------------------
# Calculation from cs datasets of relative values for a specified stratification
#-------------------------------------------------------------------------------


csRelativeValue <- function(df,Var,timeStrata,spaceStrata,techStrata,Cons=FALSE,tpRec=NA,spRec=NA,tcRec=NA){              #modif 20/01/2009
                                                                                                                 #
#recoding procedure                                                                                              #
recFun <- function(df,field,rec) {                                                                               #
  Typ <- class(df[,field])                                                                                       #
  fc <- factor(df[,field])                                                                                       #
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]                                                                     #
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))                                           #
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)                                           #
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))                        #
  return(df)                                                                                                     #
}                                                                                                                #

#if df is a validated object, recoding is made...
if (!Cons) {
  
  if (Var!="nbInd"){
  
  HH <- hh(df) 
  #time stratification fields must be added to hh
  HH$month <- sapply(HH$date,function(x) as.numeric(strsplit(x,"-")[[1]][2]))
  HH$quarter <- ceiling(HH$month/3)
  HH$semester <- ceiling(HH$month/6)

  #all strata information is in hh table, except for technical stratification (it can be also in tr or in sl/hl) 
    #if technical stratification is in tr table, it has to be merged with hh
  if (is.na(techStrata)==FALSE & techStrata%in%c("vslLen","vslPwr","vslSize","vslType")) {
    #tr primary key to link tr and hh
    trKey <- apply(tr(df)[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse="")   
    hhKey <- apply(HH[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse="")
    HH[,techStrata] <- tr(df)[match(hhKey,trKey),techStrata]
  }

  #hh primary key to link hh and sl/hl table
  hhKey <- apply(HH[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum")],1,paste,collapse="")   #hh primary key

   #if Var="lenNum", HH has to be merged with hl table, else it will be merged to sl table
  if (Var=="lenNum") {
    tab <- hl(df)
  } else {
    tab <- sl(df)
    tab$nbSamp <- 1}
  
  tabKey <- apply(tab[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum")],1,paste,collapse="") 
  #hh information is pasted to tab
  tab <- cbind.data.frame(tab,
                          HH[match(tabKey,hhKey),names(HH)%in%c(timeStrata,spaceStrata,techStrata),drop=FALSE]
                         )  

} else {

#Var=="nbInd", so ca table is what we need
tab <- ca(df)
tab$nbInd <- 1

}
if (!is.na(tpRec[1])) tab <- recFun(tab,timeStrata,tpRec)                       #modif 20/01/2009
if (!is.na(spRec[1])) tab <- recFun(tab,spaceStrata,spRec)                      #
if (!is.na(tcRec[1])) tab <- recFun(tab,techStrata,tcRec)                       #

} else {

#if df is a consolidated object...
  if (Var=="nbInd") {
    tab <- ca(df)
    tab$nbInd <- 1                                                              #modif 20/01/2009
  } else {
    if (Var=="lenNum") {
      tab <- hl(df)
    } else {
      tab <- sl(df)
      tab$nbSamp <- 1}
  }
}

#internal procedure to calculate and report relative value within specified stratification
dfRelativeValue <- function(tab,val,field,strat){
if (all(is.na(tab[,field]))) {
  return(NULL)
} else {
  summ <- tapply(tab[,val],list(tab[,field]),sum,na.rm=TRUE)
  return(data.frame(value=summ/sum(summ,na.rm=TRUE),
                    vrbl=val,
                    mod=names(summ),
                    str=field,
                    type=strat))}
}

#for each stratification, calculation is made
result <- NULL
if (is.na(timeStrata)==FALSE & timeStrata%in%names(tab)) 
  result <- rbind.data.frame(result,dfRelativeValue(tab,Var,timeStrata,"time"))
if (is.na(spaceStrata)==FALSE & spaceStrata%in%names(tab)) 
  result <- rbind.data.frame(result,dfRelativeValue(tab,Var,spaceStrata,"space"))
if (is.na(techStrata)==FALSE & techStrata%in%names(tab)) 
  result <- rbind.data.frame(result,dfRelativeValue(tab,Var,techStrata,"technical"))

if (!is.null(result)) {
  if (nrow(result)==0) result <- NULL}
if (!is.null(result)) {
  rownames(result) <- 1:nrow(result) 
#levels of mod field are redefined (ordered) 
  #to prevent wrong numerical testing (ex for 'rect' :"37E9")                 #modif 08/09/2008
  modal <- gsub("E","p",as.character(result$mod))                             #
  modal <- gsub("e","p",modal)                                                #
  modal <- gsub("D","p",modal)                                                #
  modal <- gsub("d","p",modal)                                                #
num <- suppressWarnings(as.numeric(modal))
lev <- c(as.character(unique(sort(num[!is.na(num)]))),         #numerical modalities are sorted numerically
           unique(sort(as.character(result$mod)[is.na(num)])))   #character modalities are sorted
  result$mod <- factor(result$mod,levels=lev)
}

return(new("edaResult",desc="csRelativeValue",outPut=result))
}









#-------------------------------------------------------------------------------
# Calculation from ce/cl datasets of relative values for a specified stratification
#-------------------------------------------------------------------------------


clceRelativeValue <- function(df,Var,timeStrata,spaceStrata,techStrata,Cons=FALSE,tpRec=NA,spRec=NA,tcRec=NA){  #here, df=df@ce or df@cl          #modif 20/01/2009
                                                                                                                              #
#recoding procedure                                                                                                           #
recFun <- function(df,field,rec) {                                                                                            #
  Typ <- class(df[,field])                                                                                                    #
  fc <- factor(df[,field])                                                                                                    #
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]                                                                                  #
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))                                                        #
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)                                           #
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))                                     #
  return(df)                                                                                                                  #
}                                                                                                                             #


if (!Cons) { #recoding process                                                  #modif 20/01/2009

#a time stratification field is missing in cl/ce table, so we add it to df
df$semester <- ceiling(df$quarter/2)

  if (!is.na(tpRec[1])) df <- recFun(df,timeStrata,tpRec)                       #
  if (!is.na(spRec[1])) df <- recFun(df,spaceStrata,spRec)                      #
  if (!is.na(tcRec[1])) df <- recFun(df,techStrata,tcRec)                       #
}                                                                               #

#internal procedure to calculate and report relative volume within specified stratification
dfRelativeValue <- function(tab,val,field,strat){
if (all(is.na(tab[,field]))) {
  return(NULL)
} else {
summ <- tapply(tab[,val],list(tab[,field]),sum,na.rm=TRUE)
return(data.frame(value=summ/sum(summ,na.rm=TRUE),
                  vrbl=val,
                  mod=names(summ),
                  str=field,
                  type=strat))}
}

#for each stratification, calculation is made
result <- NULL
if (is.na(timeStrata)==FALSE & timeStrata%in%names(df)) 
  result <- rbind.data.frame(result,dfRelativeValue(df,Var,timeStrata,"time"))
if (is.na(spaceStrata)==FALSE & spaceStrata%in%names(df)) 
  result <- rbind.data.frame(result,dfRelativeValue(df,Var,spaceStrata,"space"))
if (is.na(techStrata)==FALSE & techStrata%in%names(df)) 
  result <- rbind.data.frame(result,dfRelativeValue(df,Var,techStrata,"technical"))

if (!is.null(result)) {
  if (nrow(result)==0) result <- NULL}
if (!is.null(result)) {
  rownames(result) <- 1:nrow(result) 
  #levels of mod field are redefined (ordered) 
      #to prevent wrong numerical testing (ex for 'rect' :"37E9")                 #modif 08/09/2008
    modal <- gsub("E","p",as.character(result$mod))                             #
    modal <- gsub("e","p",modal)                                                #
    modal <- gsub("D","p",modal)                                                #
    modal <- gsub("d","p",modal)                                                #
  num <- suppressWarnings(as.numeric(modal))
  lev <- c(as.character(unique(sort(num[!is.na(num)]))),         #numerical modalities are sorted numerically
           unique(sort(as.character(result$mod)[is.na(num)])))   #character modalities are sorted
  result$mod <- factor(result$mod,levels=lev)
}

return(new("edaResult",desc="clceRelativeValue",outPut=result))
}








#-------------------------------------------------------------------------------
# Display of relative rates of given variable within a specified sratification
#-------------------------------------------------------------------------------


plotRelativeValue <- function(tabColumns,tabPoints=NULL,...){

if (is.null(tabColumns)) stop("No information in main object!!") 

if (!is.null(tabPoints)) {


  #-----------------------------------------------------------------------------
  # Case n1 : 2 types of information
  #-----------------------------------------------------------------------------
  
  
  #only matching stratifications are kept
  indStrCol <- apply(tabColumns[,c("str","type")],1,paste,collapse="")  #stratification from table n1
  indStrPoi <- apply(tabPoints[,c("str","type")],1,paste,collapse="")   #stratification from table n2
  tabColumns <- tabColumns[indStrCol%in%indStrPoi,]
  if (nrow(tabColumns)==0) stop("Stratifications of input objects are not matching!!")
  tabPoints <- tabPoints[indStrPoi%in%indStrCol,]
  
  #before merging, some fields have to be distinguished
  names(tabPoints)[1:2] <- c("valueA","vrblA")
  tab <- merge(tabColumns,tabPoints,sort=FALSE,all=TRUE)  #ou sort=TRUE??
  #NA values are filled
  tab$vrbl[is.na(tab$vrbl)] <- tabColumns$vrbl[1]
  tab$value[is.na(tab$value)] <- 0
  tab$vrblA[is.na(tab$vrblA)] <- tabPoints$vrblA[1] 
  tab$valueA[is.na(tab$valueA)] <- 0
  tab <- tab[order(tab$type,tab$mod),]
  
} else {

  #-----------------------------------------------------------------------------
  # Case n2 : only 1 type of information
  #-----------------------------------------------------------------------------


  tab <- tabColumns

}  
 
 
  #-----------------------------------------------------------------------------
  # Preparation for graphical display
  #-----------------------------------------------------------------------------
 
 

tab$strip <- apply(tab[,c("type","str")],1,function(x) paste(unique(x),collapse=" = "))
indG <- c(TRUE,!is.null(tab$vrblA))

if (indG[2]) {
  Points <- tab$valueA
  modal <- sort(unique(as.character(tab$type)))
  #names(Points) <- as.character(factor(tab$type,levels=modal,labels=1:length(modal)))
  names(Points) <- as.character(mapvalues(tab$type,from=modal,to=1:length(modal)))
} else {
  Points <- NULL
}                         
 
#modification of levels to sort numerical value correctly
#tab$mod <- factor(tab$mod,levels=unique(tab$mod)) 
 
  #-----------------------------------------------------------------------------
  # Update of graphical parameters
  #-----------------------------------------------------------------------------
 
 
data(GraphsPar,envir=environment())                                                                                                                           
dots <- list(...) 
if (is.null(dots$p.col)) 
  dots$p.col <- "black"
if (is.null(dots$p.bg)) 
  dots$p.bg <- "lightblue"
if (is.null(dots$l.col)) 
  dots$l.col <- "red"
if (is.null(dots$lwd)) 
  dots$lwd <- 2 
if (is.null(dots$pch)) 
  dots$pch <- 20
if (is.null(dots$rot)) 
  dots$rot <- 0  

sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))

if (is.null(dots$xlab)) 
  dots$xlab <- "" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Frequency" 
if (is.null(dots$main)) 
  dots$main <- paste("Relative Rates of total \"",paste(c(as.character(tab$vrbl[1]),as.character(tab$vrblA[1])),collapse="\" and \""),"\" value(s) \nby ",
                     paste(unique(as.character(tab$type)),collapse=", ")," Strata",sep="")


  #-----------------------------------------------------------------------------
  # Graphical display 
  #-----------------------------------------------------------------------------


print(barchart(value~mod|strip,data=tab,scales=list(x=list(relation="free",rot=dots$rot[1],cex=dots$cex.lab[1]),font=dots$font.axis),main=list(dots$main,font=dots$font.main),
               xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),par.strip.text=list(font=dots$font.lab),
               key =list(lines=list(pch=c(15,1)[indG],type=c("p","l")[indG],col=c(dots$p.bg[1],dots$l.col[1])[indG],lwd=c(2,dots$lwd[1])[indG],
                         cex=c(1.2,dots$cex[1])[indG],lty=c(1,dots$lty[1])[indG]),
                         text=list(c(as.character(tab$vrbl)[1],as.character(tab$vrblA)[1])[indG]),font=dots$font.lab,space="right",columns=1,border=TRUE), 
               prepanel=function(x,y,mis=Points,subscripts,...){
                x <- x[,drop=TRUE]
                prepanel.default.bwplot(x,y,...)
               },
               panel = function(x,y,mis=Points,subscripts,...){
                x <- x[,drop=TRUE]
                panel.barchart(x,y,col=dots$p.bg[1],fill=dots$p.bg[1],...)
                panel.lines(type="o",mis[names(mis)==as.character(packet.number())],col=dots$l.col[1],lwd=dots$lwd[1],pch=dots$pch[1],cex=dots$cex[1],lty=dots$lty[1])},
               layout=c(1,length(unique(tab$strip))),ylim=c(0,max(c(tab$value,tab$valueA),na.rm=TRUE)*1.05)))
        
        
  #-----------------------------------------------------------------------------
  # Return displayed information 
  #-----------------------------------------------------------------------------


invisible(tab)     

}        
   
   
   





      #########################
      ##                     ##
      ##  Methods to export  ##
      ##                     ##
      #########################



#-------------------------------------------------------------------------------
# relativeValue
#-------------------------------------------------------------------------------


    #---------------------------------------------------------------------------
    # Validated objects
    #---------------------------------------------------------------------------


                                                          #ex-biasPlot
setGeneric("relativeValue", function(data,                #cs/cl/ceDataVal or cs/cl/ceDataCons 
                                     strDef,              #'strIni' object
                                     ...){

  standardGeneric("relativeValue")

})




setMethod("relativeValue", signature("csDataVal","missing"), function(data,       
                                                                      field="lenNum",    #or "wt", "subSampWt", "nbSamp" (number of samples)
                                                                      ...){

strDef <- strIni(timeStrata="quarter",spaceStrata="area",techStrata="foCatEu5")
csRelativeValue(data,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata)

})         
  
  
  
                                                                     
setMethod("relativeValue", signature("csDataVal","strIni"), function(data,
                                                                     strDef,       
                                                                     field="lenNum",     #or "wt", "subSampWt", "nbSamp" (number of samples)
                                                                     ...){

csRelativeValue(data,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata,tpRec=strDef@tpRec,spRec=strDef@spRec,tcRec=strDef@tcRec)

})
                                                                               
 
   
   
setMethod("relativeValue", signature("clDataVal","missing"), function(data,       
                                                                      field="landWt",    
                                                                      ...){

strDef <- strIni(timeStrata="quarter",spaceStrata="area")
clceRelativeValue(data@cl,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata)

})         
  
  
  
                                                                     
setMethod("relativeValue", signature("clDataVal","strIni"), function(data,
                                                                     strDef,       
                                                                     field="landWt",    
                                                                     ...){

clceRelativeValue(data@cl,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata,tpRec=strDef@tpRec,spRec=strDef@spRec,tcRec=strDef@tcRec)

})   



setMethod("relativeValue", signature("ceDataVal","missing"), function(data,       
                                                                      field="trpNum",    
                                                                      ...){

strDef <- strIni(timeStrata="quarter",spaceStrata="area")
clceRelativeValue(data@ce,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata)

})         
  
  
  
                                                                     
setMethod("relativeValue", signature("ceDataVal","strIni"), function(data,
                                                                     strDef,       
                                                                     field="trpNum",    
                                                                     ...){

clceRelativeValue(data@ce,field,strDef@timeStrata,strDef@spaceStrata,strDef@techStrata,tpRec=strDef@tpRec,spRec=strDef@spRec,tcRec=strDef@tcRec)

})   


     
    #---------------------------------------------------------------------------
    # Consolidated objects
    #---------------------------------------------------------------------------
     
     
     
     
setMethod("relativeValue", signature("csDataCons","missing"), function(data,       
                                                                       field="lenNum",    #or "wt", "subSampWt", "nbSamp" (number of samples)
                                                                       ...){

csRelativeValue(data,field,"time","space","technical",Cons=TRUE)

})         
  
  
   
   
setMethod("relativeValue", signature("clDataCons","missing"), function(data,       
                                                                       field="landWt",    
                                                                       ...){

clceRelativeValue(data@cl,field,"time","space","technical",Cons=TRUE)

})         




setMethod("relativeValue", signature("ceDataCons","missing"), function(data,       
                                                                       field="trpNum",    
                                                                       ...){

clceRelativeValue(data@ce,field,"time","space","technical",Cons=TRUE)

})         
  
  
     
     
#-------------------------------------------------------------------------------
# plot (--> relativeValue)
#-------------------------------------------------------------------------------
     

setMethod("plot",signature("edaResult","missing"), function(x,
                                                            ...){

if (x@desc%in%c("csRelativeValue","clceRelativeValue")) plotRelativeValue(x@outPut,...)
if (x@desc=="sampDeltaCalc") {
      result <- plotDelta(x,...)
      return(invisible(result))}
if (x@desc=="sampDeltaId") {
      result <- plotDeltaId(x,...)
      return(invisible(result))}
if (x@desc=="landisVol") plotVol(x,...) 
if (x@desc=="alMulti") plotAlMulti(x,...)
#...

})    
     
     
setMethod("plot",signature("edaResult","edaResult"), function(x,
                                                              y,
                                                              ...){

if (all(c(x@desc,y@desc)%in%c("csRelativeValue","clceRelativeValue"))) plotRelativeValue(x@outPut,y@outPut,...)
#...

})        

setMethod("boxplot",signature("edaResult"), function(x,
                                                     ...){

if (x@desc=="landisVol") boxplotVol(x,...) 
#...

})        

                     
