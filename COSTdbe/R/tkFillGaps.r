
gapsAlk <- function(alk,strLC) {          
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
return(View)
}



setGeneric("tkFillGaps", function(object) {
	standardGeneric("tkFillGaps")}
)



setMethod("tkFillGaps",signature(object="csDataCons"), function(object){

#------------
# ALK is created
#------------

ca <- ca(object) ; ca$age <- as.character(ca$age) ; AGE <- as.numeric(ca$age)
#is strata information available?
Time <- factor(ca$time,levels=as.character(unique(sort(object@hl$time))))
Space <- factor(ca$space,levels=as.character(unique(sort(object@hl$space))))
indNAtime <- all(is.na(Time))
indNAspace <- all(is.na(Space))
if (indNAtime | indNAspace) stop("no defined time or space stratification in ca, or no common strata between hl and ca!!")
#length class step
BY <- switch(as.character(ca$lenCode[1]),"mm"=1,"2mm"=2,"3mm"=3,"scm"=5,"cm"=10,"20mm"=20,"25mm"=25,"30mm"=30,"50mm"=50)
#according to available strata, alk is computed (length classes are defined according to hl table)
eval(parse('',text=paste("alk <- tapply(ca$age,list(factor(ca$lenCls,levels=seq(from=min(object@hl$lenCls,na.rm=TRUE),to=max(object@hl$lenCls,na.rm=TRUE),by=BY)),",
                         paste(c("factor(ca$age,levels=seq(from=min(AGE,na.rm=TRUE),to=max(AGE,na.rm=TRUE),by=1))","Time"[!indNAtime],"Space"[!indNAspace]),collapse=","),"),length)",sep="")))
#NAs --> 0
alk[is.na(alk)] <- 0

strLC <- strLChl(object)  
matriX <- gapsAlk(alk,strLC)


#------------
# ALKs are displayed and can be modified ('dataF' is updated each time)
#------------


require(tcltk)
dataF <- NULL

for (iSpace in 1:dim(matriX)[4])
  for (iTime in 1:dim(matriX)[3])   {
  
matrix1 <- matriX[,,iTime,iSpace]


matrix1 <- cbind(rownames(matrix1),matrix1)
matrix1 <- rbind(colnames(matrix1),matrix1) ; matrix1[1,1] <- "ALK"
tim <- dimnames(matriX)[[3]][iTime]
spa <- dimnames(matriX)[[4]][iSpace]

tclArray1 <- tclArray()
#for (k in (1:dim(matrix1)[3]))
#  for (l in (1:dim(matrix1)[4])) {
    for (i in (1:nrow(matrix1)))
      for (j in (1:ncol(matrix1)))  {
    tclArray1[[i-1,j-1]] <- matrix1[i,j]}
    
     tt <- tktoplevel()
  tclRequire("Tktable")
  tkwm.title(tt,paste("STRATUM : time = ",tim,", space = ",spa,sep=""))
  table1 <- tkwidget(tt,"table",rows=nrow(matrix1),cols=ncol(matrix1),titlerows=1,titlecols=1,
                     height=40,width=20,
                     xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")#,slant="italic")

  tkconfigure(table1,variable=tclArray1,background="white",selectmode="extended",font=fontHeading)

tkbind(tt,"<Destroy>", function() {
    for (i in (1:(nrow(matrix1)-1)))
      for (j in (1:(ncol(matrix1)-1)))  {
      if (matrix1[i+1,j+1]%in%c("-",".") & matrix1[i+1,j+1]!=as.character(tclvalue(tclArray1[[i,j]]))) {
        dataF <<- rbind(dataF,c(tim,spa,tclvalue(tclArray1[[i,0]]),tclvalue(tclArray1[[0,j]]),tclvalue(tclArray1[[i,j]])))}
        }
      tkgrab.release(tt)})

tkwait.window(tt)
}


#------------
# output object (dbeOutput) is updated with new individuals and 'fields' list of parameters
#------------

if (!is.null(dataF)){

  tab <- unique(dataF)
  #tab's lines are duplicated (one line per individual)
  tab <- tab[rep(1:nrow(tab),as.numeric(tab[,5])),]
  
  recode <- as.numeric(factor(apply(tab[,1:2],1,paste,collapse="")))            #to create new PSUid and trpCode values
  
  v.PSUid <- max(ca$PSUid,na.rm=TRUE)+recode
  v.SSUid <- 1
  v.sort <- as.character(ca$sort[1])
  v.sampType <- as.character(ca$sampType[1])
  v.landCtry <- as.character(ca$landCtry[1])
  v.vslFlgCtry <- as.character(ca$vslFlgCtry[1])
  v.proj <- as.character(ca$proj[1])
  v.trpCode <- paste("Virtual_",recode,sep="")
  v.staNum <- 999
  v.spp <- as.character(ca$spp[1])
  v.sex <- as.character(NA)
  v.stock <- as.character(ca$stock[1])
  v.lenCode <- as.character(ca$lenCode[1])
  v.ageMeth <- as.character(NA)
  v.plusGrp <- as.character(ca$plusGrp[1])
  v.otoWt <- NA
  v.otoSide <- as.character(NA)
  v.indWt <- NA
  v.matMeth <- as.character(NA)
  v.matScale <- as.character(NA)
  v.matStage <- as.character(NA)
  N <- nrow(tab)

  virtuaTab <- data.frame(PSUid=v.PSUid,SSUid=rep(v.SSUid,length=N),time=tab[,1],space=tab[,2],technical=rep(as.character(NA),length=N),sort=rep(v.sort,length=N),
                          sampType=rep(v.sampType,length=N),landCtry=rep(v.landCtry,length=N),vslFlgCtry=rep(v.vslFlgCtry,length=N),proj=rep(v.proj,length=N),
                          trpCode=v.trpCode,staNum=rep(v.staNum,length=N),spp=rep(v.spp,length=N),sex=rep(v.sex,length=N),stock=rep(v.stock,length=N),
                          lenCls=tab[,3],age=tab[,4],fishId=min(c(0,ca$fishId),na.rm=TRUE)-c(1:N),lenCode=rep(v.lenCode,length=N),ageMeth=rep(v.ageMeth,length=N),
                          plusGrp=rep(v.plusGrp,length=N),otoWt=rep(v.otoWt,length=N),otoSide=rep(v.otoSide,length=N),indWt=rep(v.indWt,length=N),matMeth=rep(v.matMeth,length=N),
                          matScale=rep(v.matScale,length=N),matStage=rep(v.matStage,length=N))

  trTab <- unique(virtuaTab[,c("PSUid","time","space","technical","sampType","landCtry","vslFlgCtry","proj","trpCode")])
  n <- nrow(trTab)
  trTab <- cbind.data.frame(trTab,data.frame(foNum=rep(NA,length=n),daysAtSea=rep(NA,length=n),vslId=rep(NA,length=n),
                                             sampCtry=rep(as.character(NA),length=n),sampMeth=rep(as.character(NA),length=n)))
                                             
  object@tr <- rbind.data.frame(object@tr,trTab)
  object@ca <- rbind.data.frame(object@ca,virtuaTab)

  invisible(object)

} else {

  invisible(object)

}
})







