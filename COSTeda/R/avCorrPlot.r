
#library(COSTcore)
#library(COSTeda)
#library(lattice)

#################################################################################################################
#                                                                                                               #
# need to have 'sampledFO' method from COSTdbe package --> will be inserted in COSTeda package but not exported #
#                                                                                                               #
#################################################################################################################
extCatchCat <- COSTcore:::extCatchCat

#extCatchCat <- function(x) {
#sapply(x, function(x) substring(as.character(x),1,3))
#}
#

setGeneric("sampledFO", function(x,species,fraction="LAN",sampPar=TRUE,...){     #'sampPar' checks if given species is considered as automatically sampled (if TRUE, sppReg=Par <-> sppReg=All, i.e. includes sppReg="Par" in the analysis ) 
	standardGeneric("sampledFO")
	}
)


setMethod("sampledFO", signature(x="csDataCons"), function(x,species,fraction="LAN",sampPar=TRUE,...){

if (missing(species)) stop("Missing species parameter!!")

fraction <- toupper(fraction)                               #
x@sl$sort <- toupper(x@sl$sort)                                   # MM 29/04/2010
x@hl$sort <- toupper(x@hl$sort)                                   #
x@ca$sort <- toupper(x@ca$sort)                                   #

#Values in 'catReg' field are "All", "Lan", "Dis" & "Non", so...
if (fraction=="DIS") frac <- "Dis" else frac <- "Lan"

  #-----------------------------------------------------------------------------
  # Sampled weight index
  #-----------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # A haul is considered as sampled (weights) for a given species and a given fraction if :
    #   1) (catchReg=="All") OR (catchReg==frac)
    #   AND
    #   2) (sppReg=="All") OR (sppReg=="Par" AND (sampPar=TRUE  OR  species recorded in SL table for 'frac' fraction) )
    #---------------------------------------------------------------------------

#hh-index for sampled(1/0)/non sampled(NA) haul (weights) will be the combination of 2 indexes
indexCat <- indexSpp <- rep(0,nrow(x@hh))
#indexCat==1 if catReg=="All" or frac
indexCat[x@hh$catReg%in%c("All",frac)] <- 1
#indexSpp==1 if sppReg=="All" or if sppReg=="Par" & (sampPar==TRUE or species recorded in SL table for 'frac' fraction)
restrSL <- x@sl[x@sl$spp%in%species & extCatchCat(x@sl$sort)%in%fraction,1:3]
if (nrow(restrSL)>0) {#recorded information in SL                                ### added 12/06/2009
restrSL$ind <- 1 ; indSpp <- merge(x@hh,unique(restrSL[,c(1:2,4)]),all.x=TRUE,sort=FALSE)$ind     #indSpp <-> index of hauls with related information in sl for given species and fraction
} else {                                                                         ###
indSpp <- rep(NA,nrow(x@hh))                                                     ###
}                                                                                ###
indexSpp[x@hh$sppReg=="All" | (x@hh$sppReg=="Par" & (sampPar | (!is.na(indSpp)) ))] <- 1      ##corr MM 27/02/2012
#so, Windex = indexCat*indexSpp (sampled haul index)
Windex <- indexCat*indexSpp
indZero <- (Windex==1) & (is.na(indSpp))    #indZero : index of sampled hauls with 0 values 
#finally,...
Windex[Windex==0] <- NA ; Windex[indZero] <- 0


  #-----------------------------------------------------------------------------
  # Sampled numbers index
  #-----------------------------------------------------------------------------

#hh-index for sampled(1/0)/non sampled(NA) hauls (numbers) is Windex, with hauls in sl but not in hl as NA (this means that species was caught in the fraction, but not measured)
#                                                                                                          (O values for weights remain 0 values for numbers)
restrHL <- unique(x@hl[x@hl$spp%in%species & extCatchCat(x@hl$sort)%in%fraction,1:3])
if (nrow(restrHL)>0) {#recorded information in HL                                 ###
#detect FOs that are recorded in SL but not in HL
restrHL$Ind <- 1 ; indMeas <- merge(unique(restrSL),restrHL,all.x=TRUE) ; indMeas$Ind[is.na(indMeas$Ind)] <- 0
#match index with HH
indMeas <- with(indMeas,aggregate(list(Ind=Ind),list(PSUid=PSUid,SSUid=SSUid),max))       ##added MM 13/04/2011  FO sampled for at least one TTSUid
indMs <- merge(x@hh,indMeas[indMeas$Ind==0,c("PSUid","SSUid","Ind")],all.x=TRUE,sort=FALSE)$Ind
#NAs in 'indMS' means that if info is in SL, then it is in HL
#so, Lindex is...
Lindex <- Windex ; Lindex[!is.na(indMs)] <- NA
} else {                                                                          ###
Lindex <- Windex ; Lindex[Windex%in%1]<- NA
}

#result is returned as a list
return(list(sampWt=Windex,sampLg=Lindex))

})

        






###############################################################################
#                                                                             #
# Correlation plots : Volume (weight or number) of discards ~ time | landings #
#                                                                             #
###############################################################################



setGeneric("disCorrPlot", function(object,
                                   species="all",
                                   landSpp=as.character(NA),
                                   aux="time",                    #or "landings"
                                   val="weight",                  #or "number"
                                   sampPar=TRUE,                  #'sampPar' checks if given species is considered as automatically sampled
                                   timeStrata=FALSE,              #
                                   spaceStrata=FALSE,             # stratification
                                   techStrata=FALSE,              #
                                   reg=TRUE,                      #displayed regression line or not
                                   show.legend="right",
                                   ...){                          #further graphical parameters
	standardGeneric("disCorrPlot")}
)



setMethod("disCorrPlot", signature(object="csDataCons"), function(object,
                                                                    species="all",
                                                                    landSpp=as.character(NA),      #NA (ie landSpp=species),"all" or a character vector 
                                                                    aux="time",                    #or "landings"
                                                                    val="weight",                  #or "number"
                                                                    sampPar=TRUE,                  #'sampPar' checks if given species is considered as automatically sampled
                                                                    timeStrata=FALSE,              #
                                                                    spaceStrata=FALSE,             # stratification
                                                                    techStrata=FALSE,              #
                                                                    reg=TRUE,                      #regression line or not
                                                                    show.legend="right",
                                                                    ...){                          #further graphical parameters


object@sl$sort <- toupper(object@sl$sort)                                                 # MM 29/04/2010
object@hl$sort <- toupper(object@hl$sort)                                                 #
object@ca$sort <- toupper(object@ca$sort)                                                 #

if ("all"%in%species) species <- unique(object@sl$spp)
#according to 'val' and available information, hauls are considered as sampled or not for discards
indSam <- sampledFO(object,species=species,fraction="DIS",sampPar=sampPar)
if (val=="weight") indSam <- indSam$sampWt else indSam <- indSam$sampLg

#2 cases : aux="time" or "landings"
#if 'aux="landings"', hauls must also be sampled for landings
if (aux=="landings") {
  if (all(is.na(landSpp))) landSpp <- species
  if ("all"%in%landSpp) landSpp <- unique(object@sl$spp)
  indSamL <- sampledFO(object,species=landSpp,fraction="LAN",sampPar=sampPar)
  if (val=="weight") indSamL <- indSamL$sampWt else indSamL <- indSamL$sampLg
#sampled hauls are...
  sampHH <- object@hh[(!is.na(indSam)) & (!is.na(indSamL)),]
#for each sampled haul, volume of discards is ...
  if (val=="weight") {
    tab <- object@sl[(object@sl$spp%in%species) & (extCatchCat(object@sl$sort)=="DIS"),]
    disVal <- tapply(tab$wt,list(apply(tab[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$disVol <- as.numeric(disVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$disVol[is.na(sampHH$disVol)] <- 0
#and volume of landings is...
    tabL <- object@sl[(object@sl$spp%in%landSpp) & (extCatchCat(object@sl$sort)=="LAN"),]
    lanVal <- tapply(tabL$wt,list(apply(tabL[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$auxVar <- as.numeric(lanVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$auxVar[is.na(sampHH$auxVar)] <- 0
  } else {
    TAB <- merge(slSex(object@sl,object@hl),object@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)     #modif 09/12/2008 object@hl <-> slSex(object@sl,object@hl)
    TAB$nb <- TAB$lenNum*TAB$wt/TAB$subSampWt
    tab <- TAB[(TAB$spp%in%species) & (extCatchCat(TAB$sort)=="DIS"),]
    disVal <- tapply(tab$nb,list(apply(tab[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$disVol <- as.numeric(disVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$disVol[is.na(sampHH$disVol)] <- 0
#and volume of landings is...
    tabL <- TAB[(TAB$spp%in%landSpp) & (extCatchCat(TAB$sort)=="LAN"),]
    lanVal <- tapply(tabL$nb,list(apply(tabL[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$auxVar <- as.numeric(lanVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$auxVar[is.na(sampHH$auxVar)] <- 0
  }

} else {          #if 'aux="time"',...

#sampled hauls are...
  sampHH <- object@hh[(!is.na(indSam)) & (!is.na(object@hh$foDur)),]
#for each sampled haul, volume of discards is ...
  if (val=="weight") {
    tab <- object@sl[(object@sl$spp%in%species) & (extCatchCat(object@sl$sort)=="DIS"),]
    disVal <- tapply(tab$wt,list(apply(tab[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$disVol <- as.numeric(disVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$disVol[is.na(sampHH$disVol)] <- 0
    sampHH$auxVar <- sampHH$foDur
  } else {
    TAB <- merge(slSex(object@sl,object@hl),object@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)      #modif 09/12/2008 
    TAB$nb <- TAB$lenNum*TAB$wt/TAB$subSampWt
    tab <- TAB[(TAB$spp%in%species) & (extCatchCat(TAB$sort)=="DIS"),]
    disVal <- tapply(tab$nb,list(apply(tab[,c("PSUid","SSUid")],1,paste,collapse=":-:")),sum,na.rm=TRUE)
    sampHH$disVol <- as.numeric(disVal[apply(sampHH[,c("PSUid","SSUid")],1,paste,collapse=":-:")])
    sampHH$disVol[is.na(sampHH$disVol)] <- 0
    sampHH$auxVar <- sampHH$foDur
  }
}

#now, we have X (sampHH$auxVar) and Y (sampHH$disVol) with stratification
df <- sampHH[,c("time","space","technical","disVol","auxVar")]



  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------

data(GraphsPar,envir=environment())
dots <- list(...)
sapply(names(gp),function(x)
                  if (is.null(eval(parse('',text=paste("dots$",x,sep="")))))
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
auxLab <- switch(aux,
                 time="Fishing duration (in mn)",
                 landings=paste(c("Volume of landings","(in g)"[val=="weight"],"(nb)"[val!="weight"]),sep="",collapse=" "))

if (is.null(auxLab)) stop("wrong 'aux' parameter!!")

if (is.null(dots$xlab))
  dots$xlab <- auxLab

if (is.null(dots$ylab))
  dots$ylab <- paste(c("Volume of discards","(in g)"[val=="weight"],"(nb)"[val!="weight"]),collapse=" ")

if (is.null(dots$main))
  dots$main <- paste("Scatter plot \nVolume of discards"," (in g)"[val=="weight"]," (nb)"[val!="weight"]," ~ ",auxLab,sep="",collapse="")

strD <- c(timeStrata,spaceStrata,techStrata)
strip.col <- trellis.par.get("strip.background")$col
#xx <- yy <- list()   #xx : list with stratified auxVar values
#                     #yy : list with stratified disVol values

eval(parse('',text=paste("print(xyplot(disVol~auxVar","|"[sum(strD)>0],paste(c("time","space","technical")[strD],collapse="*"),",data = df,",
"prepanel = function(x,y,subscripts,...) {list(xlim = range(c(0,x,1),na.rm=TRUE),ylim = range(c(0,y,1),na.rm=TRUE))},", 
"panel=function(x,y,subscripts,...) {panel.xyplot(x,y,col=dots$col[1],pch=dots$pch[1],cex=dots$p.cex[1],fill=dots$bg,lwd=dots$p.lwd[1]) ;",
#"if (length(x)>0) {xx[[length(xx)+1]] <<- x ; yy[[length(yy)+1]] <<- y ; ",
#"names(xx)[length(xx)] <<- names(yy)[length(yy)] <<- apply(unique(df[subscripts,(1:3)[strD],drop=FALSE]),1,paste,collapse=\":-:\")} ;",
"if (length(unique(x))>1) panel.lmline(x,y,col=dots$l.col,lwd=dots$l.lwd[1],lty=dots$lty[1]) ; "[reg],            #lm can't be used if x is constant 
##"if (length(x)>0) {yy[[length(yy)+1]] <<- suppressWarnings(cor(x,y)) ; names(yy)[length(yy)] <<- apply(unique(df[subscripts,(1:3)[strD],drop=FALSE]),1,paste,collapse=\":-:\")} ;",
##"vv <- NULL ; suppressWarnings(try(vv <- lm(y~x),silent=TRUE)) ;"[reg],
##"if (!is.null(vv)) {xx[[length(xx)+1]] <<- summary(vv) ; names(xx)[length(xx)] <<- apply(unique(df[subscripts,(1:3)[strD],drop=FALSE]),1,paste,collapse=\":-:\")}},"[sum(strD)>0][reg],
##"xx <<- vv},"[sum(strD)==0][reg],
"},",##[!reg],
"main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),par.strip.text=list(font=dots$font.lab),",
",scales=list(x=list(relation=\"free\"),y=list(relation=\"free\"),font=dots$font.axis),distribute.type = TRUE,",
"key=list(points=list(pch=15,fill=dots$bg,cex=1.2,lwd=1,col=strip.col[1:sum(strD)]),"[sum(strD)>0],
"text=list(c(\"time\",\"space\",\"technical\")[strD]),cex.title=0.8,space=show.legend,font=dots$font.lab,columns=1,border=TRUE)"[sum(strD)>0],"))",sep="")))

invisible(df)#list(y=yy,x=xx))

})





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


###########
# Example #
###########

#consolidated datasets are built for testing
#strDef <- strIni(timeStrata="quarter",techStrata="foCatEu5")
#object <- csDataCons(csDataVal(sole.cs),strDef)
#
#res <- disCorrPlot(object,aux="landings",techStrata=TRUE,l.col="steelblue",bg="gold",lty=2)
#
#








