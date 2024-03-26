
####################################
##                                ##
##  'edaResult' class definition  ##
##                                ##
####################################

#-------------------------------------------------------------------------------
#definition of 'edaResult' class that is supposed to contain the result of each COSTeda function  
#-------------------------------------------------------------------------------

setClass("edaResult",
	representation(
		desc="character",  #generating function
		outPut="ANY"       #result from the generating function
	),
	prototype(
		desc=as.character(NA),
		outPut=NA
  )
)




################################################
##                                            ##
## Age at Length Analysis (multinomial model) ##
##                                            ##
##       MM 04/02/2008                        ##
################################################


#-------------------------------------------------------------------------------
# Age at Length multinomial analysis procedure
#-------------------------------------------------------------------------------

ageLenMultinomFun <- function(object,          #a csDataVal object with ca table
                              timeStrata=NULL, #ex: "year","quarter","month"
                              spaceStrata=NULL, #ex: "area","rect"
                              techStrata=NULL, #ex: "commCat"
                              elmts=list(tp="all",sp="all",tc="all"),
                              #grps=NULL, #ex: "timeStrata","spaceStrata","techStrata"
                              age.plus=-1, #ex: 6, 8
                              ...){

require(nnet)
tab <- object@ca

if (is.na(timeStrata)) timeStrata <- NULL
if (is.na(spaceStrata)) spaceStrata <- NULL
if (is.na(techStrata)) techStrata <- NULL

#test stratification/fields
strt <- c(timeStrata,spaceStrata,techStrata)
if (length(strt)>0) {
  if (any(!strt%in%names(tab))) stop("stratification and ca table fields must match!!")
}

#test on elmts
if (!all(names(elmts)%in%c("tp","sp","tc"))) stop("'elmts' parameter is not well defined") 

#ages are grouped within specified 'age.plus' parameter
if (age.plus>=0) tab$age[tab$age>=age.plus] <- age.plus

#only needed data is kept
newtab <- tab[,c("age","lenCls",timeStrata,spaceStrata,techStrata)] 

#subsetting within 'elmts' 
tpT <- !is.null(timeStrata)
if (tpT&(!("all"%in%elmts$tp))) newtab <- newtab[as.character(newtab[,timeStrata])%in%elmts$tp,]

spT <- !is.null(spaceStrata) 
if (spT&(!("all"%in%elmts$sp))) newtab <- newtab[as.character(newtab[,spaceStrata])%in%elmts$sp,]

tcT <- !is.null(techStrata) 
if (tcT&(!("all"%in%elmts$tc))) newtab <- newtab[as.character(newtab[,techStrata])%in%elmts$tc,]

#stratification fields are converted to factors for upcoming calculations
if (tpT) newtab[,timeStrata] <- factor(newtab[,timeStrata]) 
if (spT) newtab[,spaceStrata] <- factor(newtab[,spaceStrata]) 
if (tcT) newtab[,techStrata] <- factor(newtab[,techStrata])

#creation of stratified ALKs
alk <- tapply(newtab$age,lapply(c("lenCls","age",timeStrata,spaceStrata,techStrata),function(x) newtab[,x]),length)
#empty cells are filled
alk[is.na(alk)] <- 0 


alkNam <- dimnames(alk)
alkNam[[2]] <- NULL

#index of absent data for given length class/strata
matTest <- apply(alk,seq(from=1,to=length(c(timeStrata,spaceStrata,techStrata))+2)[-2],function(x) all(x==0))
test <- matrix(matTest,ncol=1)
Age <- apply(alk,2,rbind)

#formatted data for 'multinom' procedure --> Tab
Tab <- expand.grid(alkNam)
names(Tab) <- c("Length",timeStrata,spaceStrata,techStrata)

#length data is converted into numerical field
Tab$Length <- as.numeric(as.character(Tab$Length))
Tab <- cbind.data.frame(as.data.frame(Age),Tab)

#empty rows are removed                          
Tab <- Tab[!test,] 
Age <- Age[!test,]

#2 cases to distinguish : stratification or no stratification ('ntest'= FALSE or TRUE)
ntest <- all(is.null(timeStrata),is.null(spaceStrata),is.null(techStrata))

Mm <- NULL
#'multinom' function applied to Tab object
eval(parse('',text=paste("Mm <- multinom(Age~Length",c(paste("*",paste(c(timeStrata,spaceStrata,techStrata),collapse="*"),"-1",sep=""),"")[c(!ntest,ntest)],
                         ",Hess=T,model=T,data=Tab)",sep="")))
                   
result <- list(timeStrata=timeStrata,
               spaceStrata=spaceStrata,
               techStrata=techStrata,
               Mm=Mm,
               dat=Tab,
               age=Age)

return(new("edaResult",desc="alMulti",outPut=result))

}

 
#-------------------------------------------------------------------------------
# Graphical display of 'ageLenMultinomFun' procedure output  
#-------------------------------------------------------------------------------
          
       
plotAlMulti <- function(x,                   #'edaResult' object with desc="alMulti"
                        grps=NULL,           #ex: "timeStrata","spaceStrata", "techStrata", or NULL
                        show.legend="right", #ex: "", "left"
                        ...){

  #-----------------------------------------------------------------------------
  # Update of graphical parameters
  #-----------------------------------------------------------------------------

data(GraphsPar,envir=environment())                                                                                                                            
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length (mm)"
if (is.null(dots$ylab)) 
  dots$ylab <- "Age Proportion at Length" 
if (is.null(dots$main)) 
  dots$main <- "Age-at-length multinomial analysis"
 
timeStrata <- x@outPut$timeStrata
spaceStrata <- x@outPut$spaceStrata 
techStrata <- x@outPut$techStrata  
 
  #-----------------------------------------------------------------------------
  # Data from 'edaResult' object is extracted
  #-----------------------------------------------------------------------------
 
tab <- x@outPut$dat
Mm <- x@outPut$Mm
stratas <- c(x@outPut$timeStrata,x@outPut$spaceStrata,x@outPut$techStrata)

#if no specified stratification, no legend
Ls <- length(stratas)
if (Ls==0) show.legend <- ""

  #-----------------------------------------------------------------------------
  # Creation of a data.frame with all needed information for graphical display
  #-----------------------------------------------------------------------------

SMm <- summary(Mm,corr=FALSE)
#Frequencies at length are changed to proportion at length
lim <- grep("Length",names(tab))
tabAge <- tab[,1:(lim-1)]
QijP <- tabAge/apply(tabAge,1,sum)
QijP[is.na(QijP)] <- 0
QQ <- cbind.data.frame(as.data.frame(QijP),tab[,lim:ncol(tab),drop=FALSE])
FIT <- Mm$fitted.values

#QQ data.frame is expanded (addition of an Age field) --> datat
L1 <- nrow(QQ)
L2 <- ncol(QQ) 
eval(parse('',text=paste("datat <- data.frame(value=matrix(as.matrix(QQ[,1:(lim-1)]),ncol=1),Age=rep(as.numeric(colnames(QQ)[1:(lim-1)]),each=L1),Length=rep(QQ$Length,(lim-1))",
                         ","[Ls!=0],paste(paste(stratas,"=rep(QQ$",stratas,",(lim-1))",sep=""),collapse=",")[Ls!=0],")",sep="")))
datat$source <- "obs"

#'datatfit' is the same table as datat, but with fitted values (from multinomial modelisation) 
eval(parse('',text=paste("datatfit <- data.frame(value=matrix(as.matrix(FIT),ncol=1),Age=rep(as.numeric(colnames(QQ)[1:(lim-1)]),each=L1),Length=rep(QQ$Length,(lim-1))",
                         ","[Ls!=0],paste(paste(stratas,"=rep(QQ$",stratas,",(lim-1))",sep=""),collapse=",")[Ls!=0],")",sep="")))
datatfit$source <- "smod"

#both dfs are pasted
datat <- rbind(datatfit,datat)      

#Age field is converted into factor
datat$Age <- factor(datat$Age)

if (!is.null(grps)) {
  if (!is.null(x@outPut[[grps]])) {
    grps <- x@outPut[[grps]] 
  } else {
    grps <- "nostrata" 
    datat$nostrata <- factor("all")} 
} else {
  grps <- "nostrata" 
  datat$nostrata <- factor("all")
}

#number of displayed levels in each graph
eval(parse('',text=paste("ll <- nlevels(datat$",grps,")",sep="")))      

declStr <- stratas[!stratas%in%grps]
#erase the last page if length(declStr)>=2
lll <- length(declStr)
if (lll>1) { 
  pageNb <- sum(sapply(declStr[2:lll],function(x) eval(parse('',text=paste("length(unique(QQ$",x,"))",sep=""))))) 
} else {
  pageNb <- 1 
}

strip.col <- trellis.par.get("strip.background")$col
gpTest <- grps=="nostrata"

  #-----------------------------------------------------------------------------
  # Graphical display 
  #-----------------------------------------------------------------------------

 
eval(parse('',text=paste("print(xyplot(value~Length|",paste(c("Age",declStr),collapse="*"),",data = datat, groups = interaction(",grps,",source), type = rep(c(\"p\",\"l\"),each=ll),",
"main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),par.strip.text=list(font=dots$font.lab),",
"col=dots$l.col[1:ll],pch=dots$pch[1],cex=dots$p.cex[1],fill=dots$bg,lwd=dots$p.lwd[1],scales=list(font=dots$font.axis),distribute.type = TRUE,",#page=function(n) {dev.copy(x11) ; if (n==pageNb) dev.off()},",
"key=list(points=list(pch=c(rep(dots$pch[1],ll),NA,rep(15,lll+1)),fill=dots$bg,cex=dots$p.cex[1],lwd=dots$p.lwd[1],col=c(dots$l.col[1:ll],NA,strip.col[1:(lll+1)])),",
"text=list(c(levels(datat$",grps,"),\"\",\"age\",declStr)),title=",c(paste("\"",grps,"\"",sep=""),"NULL")[c(!gpTest,gpTest)],
",cex.title=0.8,space=show.legend,font=dots$font.lab,columns=1,border=TRUE)))",sep="")))

}
  


 

      #########################
      ##                     ##
      ##  Methods to export  ##
      ##                     ##
      #########################



#-------------------------------------------------------------------------------
# ageLenMulti
#-------------------------------------------------------------------------------


    #---------------------------------------------------------------------------
    # Validated objects
    #---------------------------------------------------------------------------

       
       
setGeneric("ageLenMulti", function(data,
                                   strDef,
                                   elmts=list(tp="all",sp="all",tc="all"),
                                   age.plus=-1, 
                                   ...){
	standardGeneric("ageLenMulti")}
)



setMethod("ageLenMulti",signature("csDataVal","missing"), function(data,
                                                                   elmts=list(tp="all",sp="all",tc="all"),
                                                                   age.plus=-1,
                                                                   ...){
strDef <- strIni()
ageLenMultinomFun(data,timeStrata=strDef@timeStrata,spaceStrata=strDef@spaceStrata,techStrata=strDef@techStrata,
                  elmts=elmts,age.plus=age.plus,...)

})


setMethod("ageLenMulti",signature("csDataVal","strIni"), function(data,
                                                                  strDef,
                                                                  elmts=list(tp="all",sp="all",tc="all"),
                                                                  age.plus=-1,
                                                                  ...){
                                                                  
ageLenMultinomFun(data,timeStrata=strDef@timeStrata,spaceStrata=strDef@spaceStrata,techStrata=strDef@techStrata,
                  elmts=elmts,age.plus=age.plus,...)

})

#-------------------------------------------------------------------------------
# plot (--> see MarketSampGraphs_Explore.r file)
#-------------------------------------------------------------------------------



