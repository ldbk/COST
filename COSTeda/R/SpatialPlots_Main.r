# SpatialPlots_Main 
# the main spatial plotting functions
# ACP 21/1/09
#
# includes 
#   space.plot
#   strata.space.plot
#   scale.plot

`strataSpacePlot` <-
function(object,variable,SpaceStrat,func,TimeStrat,TechStrat,nplots=1,multiscale.title,multiscale.cex=1,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# strata.space.plot
# function that does stratified spatial plots 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
obj <-missing(object)
vbl <-missing(variable)
spst <-missing(SpaceStrat)
if(missing(TechStrat)) TechStrat <-NULL
if(missing(TimeStrat)) TimeStrat <-NULL
if(any(obj,vbl,spst))stop("function reqiues object variable and SpaceStrat")

dots <-list(...)

if(class(object)%in%c("clData","clDataVal","clDataCons"))
{
dataframe <-object@cl
dataname <-"cl"
clnames <-names(object@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
}
if(class(object)%in%c("ceData","ceDataVal","ceDataCons"))
{
dataframe <-object@ce
dataname <-"ce"
cenames <-names(object@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
}
if(class(object)%in%c("csData","csDataVal","csDataCons"))
{
trnames <-names(object@tr)
hhnames <-names(object@hh)
slnames <-names(object@sl)
hlnames <-names(object@hl)
canames <-names(object@ca)
tablenames <-c(rep("tr",length(trnames)),rep("hh",length(hhnames)),
rep("sl",length(slnames)),rep("hl",length(hlnames)),rep("ca",length(canames)))
varindex <-which(is.finite(match(c(trnames,hhnames,slnames,hlnames,canames),variable)))
vartable <-tablenames[varindex]
if(length(vartable)>1) warning("specified variable occurs in more than one csData table")
if(length(vartable)<1) stop("No such variable in the cost object")
#newcs <-mergecsData(object)
#newcs <-object
#if(!class(object)%in%c("csDataCons","clDataCons","ceDataCons")) 
newcs <-mergecsData(object)
eval(parse(text=paste("dataframe <-newcs@",vartable[1],sep="")))
dataname <-vartable[1]
if(variable=="lenCls") 
{
plottable <-readline(cat("lenCls occurs in both hl length frequency tables and ca age length tables \n
which do you want to plot? hl or ca \n" ))
eval(parse(text=paste("dataframe <-newcs@",plottable,sep="")))
dataname <-plottable
}
}
if(!(variable %in% names(dataframe)))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
#spacename <-paste(substitute(SpaceStrat))
#if(!(spacename %in% c("rect","area")))stop("SpaceStat must be either rect or area")

if(!class(object)%in%c("csDataCons","clDataCons","ceDataCons"))
{
spacename <-paste(substitute(SpaceStrat))
#if(!(spacename %in% c("rect","area")))stop("SpaceStat must be either rect or area")
}
#if(class(object)%in%c("csDataCons","clDataCons","ceDataCons"))
#{
#SpaceStrat <-"space"
spacename <-paste(substitute(SpaceStrat))
#TechStrat <-"technical"
#TimeStrat <-"time"
#}
funcname <-paste(substitute(func))

eval(parse(text=paste("variable <-dataframe$",variable,sep="")))
if(is.numeric(variable)==FALSE) stop("You need to spacify a numeric variable")
eval(parse(text=paste("statsqs <-dataframe$",SpaceStrat,sep="")))
if(unique(statsqs)[1]=="all")stop("Can not do a spatial plot without a spatial stratification, SpaceStrata must be rect or area.")  

default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
#print(default.title)
if(nplots>1&missing(multiscale.title))multiscale.title <-default.title


timeperiods <-techtypes <-1
timeindex <-techindex <-1:dim(dataframe)[1]

if(!is.null(TimeStrat))
{
timeperiods <-switch(TimeStrat,time=sort(unique(dataframe$time)),year=unique(dataframe$year),quarter=c(1:4),month=c(1:12))
if(timeperiods[1]=="all") TimeStrat <-NULL
timestrataname <-paste(toupper(substr(TimeStrat,1,1)),substr(TimeStrat,2,nchar(TimeStrat)),sep="")
}



if(!is.null(TechStrat))
{
techtypes <-switch(TechStrat,technical=unique(dataframe$technical[!is.na(dataframe$technical)])
,foCatEu5=unique(dataframe$foCatEu5[!is.na(dataframe$foCatEu5)])
,foCatEu6=unique(dataframe$foCatEu6[!is.na(dataframe$foCatEu6)])
,foCatNat=unique(dataframe$foCatNat[!is.na(dataframe$foCatNat)])
,commCat=unique(dataframe$commCat[!is.na(dataframe$commCat)])
)
techstrataname <-paste(toupper(substr(TechStrat,1,1)),substr(TechStrat,2,nchar(TechStrat)),sep="")
if(techtypes[1]=="all")TechStrat <-NULL
}

techtitle <-timetitle <-NULL
# finding the maximum value by strata and setting up a common scale 
maxvals <-NULL
nstrata <-0
timeindex <-techindex <-1:dim(dataframe)[1]
for(i in 1:length(timeperiods))
{
for(j in 1:length(techtypes))
{
if(!is.null(TimeStrat)) timeindex <-which(dataframe[[TimeStrat]] %in% timeperiods[i])
if(!is.null(TechStrat)) techindex <-which(dataframe[[TechStrat]] %in% techtypes[j])
index <-intersect(timeindex,techindex)
if(length(index)>0) 
{
maxvals <-append(maxvals,as.vector(tapply(variable[index],statsqs[index],func)))
nstrata <-nstrata+1
}
}
}
maxstratavalue <-max(maxvals,na.rm=T)
commonbreaks <-seq(0,maxstratavalue,length.out=8)
if(is.null(dots$breaks))dots$breaks <-commonbreaks 
if(is.null(dots$breaks)==FALSE)commonbreaks <-dots$breaks 
nstrata1 <-length(timeperiods)*length(techtypes)
pages <-ceiling(nstrata/nplots)
lastpageplot <-nplots*(1:pages)
addtoplot <-c(lastpageplot[lastpageplot<nstrata],nstrata)
addscale <-rep(FALSE,nstrata)
if(!(nplots %in% c(3,5,7,8,10,11)))addscale[addtoplot] <-TRUE


x <-.layout.matrix(nplots)
if(nplots!=1)
{
par(mfrow=c(1,1))
layout(x)
}
# running throught the strat doing a plot for each. 
k <-0
for(i in 1:length(timeperiods))
{
for(j in 1:length(techtypes))
{
#k <-k+1
#index <-1:dim(dataframe)[1]
#plotvars <-space.plot(variable[index],statsqs[index],func,breaks=commonbreaks,plotmap=FALSE,...)
if(!is.null(TimeStrat)) 
{
timeindex <-which(dataframe[[TimeStrat]] %in% timeperiods[i])
timetitle <-paste(timestrataname,"=",ifelse(TimeStrat=="month",month.abb[i],as.character(timeperiods[i])),sep=" ")
}
if(!is.null(TechStrat))
{
techindex <-which(dataframe[[TechStrat]] %in% techtypes[j])
techtitle <-paste(techstrataname,"=",as.character(techtypes[j]),sep=" ")
#techtitle <-paste(TechStrat,"=",as.character(techtypes[j]),sep=" ")
}
index <-intersect(timeindex,techindex)
if(length(index)>0) #plotmap <-NULL
{
k <-k+1
bigindex <-1:dim(dataframe)[1]
#plotvars <-space.plot(variable[bigindex],statsqs[bigindex],func,breaks=commonbreaks,plotmap=FALSE,...)
plotvars <-space.plot(variable[bigindex],statsqs[bigindex],func,plotmap=FALSE,...)
options(warn=-1)
# stops "breaks dont span range of .." warnings when commonbreaks are used
#space.plot(variable[index],statsqs[index],func,overlay=T,breaks=commonbreaks,...)
space.plot(variable[index],statsqs[index],func,overlay=T,...)
title(sub=paste(timetitle,techtitle,sep=" "))

# adding the multiple scale if its the end of the page
if(k %in% lastpageplot&nplots>1)
{
par(pty="m")
default.mar <-par(mar=c(1,1,1,1))
scale.plot(commonbreaks,place="center",scale.box="n",fillcol=plotvars$cols[1:length(plotvars$cols)],scaletype=plotvars$maptype,multiscale.title,scale.cex=multiscale.cex,cex.max.bubble=plotvars$cex.max.bubble)
scaleplot.mar <-par(mar=default.mar)
}
}
}
}
# resetting graphics paramiters if changed
if(nplots>1)
{
layout(matrix(1,1,1))
par(mfrow=c(1,1))
options(warn=0)
}
}
# ---------------------end of strata.space.plot------------------------------------

setGeneric("strataSpacePlot")
#---------------------------------------------------------------------------------- 
`space.plot` <-
function(variable,SpaceStrat,func,
xlim=NULL,ylim=NULL,zlim=NULL,xlab=NULL,ylab=NULL,breaks=NULL,
maptype="image",plotmap=TRUE,overlay=FALSE,squaremap=FALSE,
area.lines=FALSE,depths=FALSE,statrects=FALSE,fcoast=FALSE,landmass=FALSE,
pch=1,colour=TRUE,
col.coast="blue",col.cont="grey",col.pch="red",col.rect="grey",col.land="white",
col.depth="grey",col.text=1,
scale=FALSE,scale.title="",scale.cex=0.6,scaleplace="bottomright",scale.box="o",
cex.max.bubble=2,threshold=0,digits.text=0,cex.text=1,
...)

{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# space.plot
# function that takes a numeric variable, a vecotor of 
# statsqs or ICES areas and plots out the 
# spatial information gruoped by func
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(pty="m")
if(squaremap==TRUE)par(pty="s")
#library(COSTcore)
data(NHcoast)
data(finecoast)
data(alldepths)
data(faoAreas)
data(Europe)
data(landmasses)
data(GSAareas)
data(code.list)
data(ICESAreaRects)
#print("FRSDMM version")
doGSAs <-doFRS <-doICES <-novals<-FALSE
if(all(c(missing(variable),missing(SpaceStrat),missing(func))))
{
novals <-TRUE
SpaceStrat <-"12G3"
variable <-0.1
func <-sum
if(is.null(xlim))
{
xlim <-c(-20,20)
}
if(is.null(ylim))
{
ylim <-c(44,64)
}

}
if(!is.null(breaks)&&substitute(breaks)=="commonbreaks")options(warn=-1)
if(!(maptype %in% c("image","contour","bubble","values"))) 
stop("maptype must be one of: image, contour, bubble, or values")

#if(unique(SpaceStrat)[1]=="all")stop("Can not do a spatial plot without a spatial stratification, SpaceStrata must be rect or area.")  
if(any(is.na(SpaceStrat)))
{
warning("spatial strata included NA's")
newSpaceStrat <-SpaceStrat[!is.na(SpaceStrat)]
variable <-variable[!is.na(SpaceStrat)]
SpaceStrat <-newSpaceStrat
}
if(class(SpaceStrat)!="character")
{
SpaceStrat <-as.character(SpaceStrat)
warning("spatial strata converted to class character")
}

if(any(is.na(variable)))
{
warning("variable included NA's")
newvariable <-variable[!is.na(variable)]
SpaceStrat <-SpaceStrat[!is.na(variable)]
variable <-newvariable
}


if(all(SpaceStrat %in% faoAreas$FAO))
{
#print("areas are FAO")
SpaceStrat <-as.character(faoAreas$ICES[match(SpaceStrat,faoAreas$FAO)])
}
if(all(SpaceStrat %in% as.character(ICESAreaRects$AreaCode)))
{
#print("doing ICES")
doICES <-TRUE
sampareaname <-"ICES"
}
if(all(substr(SpaceStrat,1,3)=="FRS"))
{
#print("doing FRS areas")
doFRS <-TRUE
saname <-substr(SpaceStrat[1],4,6)
sampareaname <-switch(EXPR=saname,"HER"="Herring","MAC"="Mackerel",
"DMS"="Demersal","NEP"="Nephrops","CRA"="Edible Crab","SCA"="Scallops","DMM"="DemersalMet")
SpaceStrat <-substr(SpaceStrat,7,nchar(SpaceStrat))
if(sampareaname %in% c("Herring","Mackerel","Demersal"))SpaceStrat <-as.numeric(SpaceStrat)
}
if(all(SpaceStrat %in% names(GSAareas)))
{
doGSAs <-TRUE
GSAnames <-SpaceStrat
GSAvar <-variable
GSAfunc <-func
SpaceStrat <-"12G3"
variable <-0.1
func <-sum
}

statsqs <-SpaceStrat
#if(!is.statsq(SpaceStrat)) statsqs <-convert.icesarea.statsq(SpaceStrat)$statsq
if(!is.statsq(SpaceStrat)) statsqs <-.convert.area.statsq(SpaceStrat,sampareaname)$statsq
if(!is.statsq(SpaceStrat)&maptype=="contour") stop("Contour plots are not possible for area strata")

# the max plot area
labels <- 0:90
latnames1 <- ifelse(labels < 10, paste("0", labels, sep = ""),as.character(labels))
latnames <-c(rep("-",12),latnames1,rep("-",11))
midlats <-seq(29.5, 86.0, 0.5) + 0.25
lonnames <- c("A0","A1","A2","A3",paste(rep(LETTERS[c(2:8,10:12,13)], rep(10, 11)), rep(0:9,11), sep = ""))
midlons <- (-44:69) + 0.5
 
# The default plotting area covers the limits of the statsq 

latlons <-convert.statsq.lat.lon(statsqs)
navals <-unique(c(which(is.na(latlons$lon)),which(is.na(latlons$lat))))
if(length(navals)>0)
{
latlons$lon <-latlons$lon[-navals]
latlons$lat <-latlons$lat[-navals]
}
# max plot area for GSAs
if(doGSAs)
{
uniGSAs <-unique(GSAnames)
latindex <-grep("lat",names(unlist(GSAareas[uniGSAs])))
lonindex <-grep("lon",names(unlist(GSAareas[uniGSAs])))
lons <-as.vector(unlist(GSAareas[uniGSAs]))[lonindex]
lons <-lons[!is.na(lons)]
lats <-as.vector(unlist(GSAareas[uniGSAs]))[latindex]
lats <-lats[!is.na(lats)]
latlons$lon <-lons
latlons$lat <-lats
#plot(lons,lats,type="n")
}


#latlons <-latlons[-unique(c(which(is.na(latlons$lon)),which(is.na(latlons$lat)))),]
minx <-min(latlons$lon)
maxx <-max(latlons$lon)
miny <-min(latlons$lat)
maxy <-max(latlons$lat)
if(!is.null(xlim))
{
minx <-xlim[1]
maxx <-xlim[2]
}
if(!is.null(ylim))
{
miny <-ylim[1]
maxy <-ylim[2]
}


xlen <-maxx-minx
ylen <-maxy-miny
midx <-minx+xlen/2
midy <-miny+ylen/2
bigside <-max(xlen,ylen*2)

if(squaremap==TRUE)
{
lowx <-midx-bigside/2
hix <-midx+bigside/2
lowy <-midy-bigside/4
hiy <-midy+bigside/4
areax <-c(lowx-0.5,hix+0.5)
areay <-c(lowy-0.25,hiy+0.25)
}
if(squaremap==FALSE)
{
lowx <-midx-xlen/2
hix <-midx+xlen/2
lowy <-midy-ylen/2
hiy <-midy+ylen/2
areax <-c(lowx-0.01,hix+0.01)
areay <-c(lowy-0.01,hiy+0.01)
}
#print(areax)
#print(areay)
if(areax[1]<(-50))stop("min x limit less than -50W")
if(areax[2]>70)stop("max x limit greater than 70E")
if(areay[1]<0)stop("min y limit less than 0")
if(areay[2]>90)stop("max y limit greater than 90")


extremes <-c(-30 <areax[1],33.5>areax[2],
#max(newcoast$lon,na.rm=T)>areax[2],
min(newcoast$lat,na.rm=T)<areay[1],max(newcoast$lat,na.rm=T)>areax[2])
fextremes <-c(-13.5 <areax[1],15>areax[2],45<areay[1],63>areax[2])
if(!landmass) col.land <-"white"
# overriding the default x anyd y labels if not specified

if(is.null(xlab)) xlab <-""
if(is.null(ylab)) ylab <-""
if(!(scale.title=="")) scale <-TRUE
if(overlay) par(new=TRUE)
if(!overlay) plot(midlons,midlats,type="n",xlim=areax,ylim=areay,xlab=xlab,ylab=ylab,...)

# calculate statsq values

rects <-expand.grid(lonnames,latnames)
rects1 <-as.factor(paste(rects[,2],rects[,1],sep=""))
valpercell <-tapply(variable,SpaceStrat,func)
statsqs <-as.character(names(valpercell))
values <-as.vector(valpercell)
if(!is.statsq(SpaceStrat))
#if(all(SpaceStrat %in% faoAreas$ICES))
{
#casout <-convert.icesarea.statsq(names(valpercell))
casout <-.convert.area.statsq(names(valpercell),sampareaname)
statsqs <-casout$statsq
values <-as.vector(valpercell[match(toupper(casout$parentarea),toupper(names(valpercell)))])
}




index <-match(statsqs,rects1)
vals <-rep(NA,length(midlons)^2)
ww <-(1:length(index))[is.finite(index)]
vals[index[ww]] <-(values)[ww]
mat <-matrix(vals,length(midlons),length(midlats))



if(doGSAs)
{
mat <-GSAmat <-tapply(GSAvar,GSAnames,GSAfunc)
uniGSAs <-names(GSAmat)
}

if (missing(zlim)) 
{
zmin <- min(mat, na.rm = TRUE)
zmax <- max(mat, na.rm = TRUE)
zlim <- c(zmin, zmax)
}
else 
{
mat[mat < zlim[1] & !is.na(mat)] <- zlim[1]
mat[mat > zlim[2] & !is.na(mat)] <- zlim[2]
}

if(!missing(breaks))
{
if(breaks[1]>zlim[1])warning("Breaks dont span the full range of z values")
if(breaks[length(breaks)]<zlim[2])warning("Breaks dont span the full range of z values")
cols <-grey(rev(seq(0.1,0.9,length.out=length(breaks)-1)))
if(colour==T)
{
cols <-rev(heat.colors(length(breaks)-1))
}}

if(missing(breaks))
{
breaks <-seq(zlim[1],zlim[2],length=8)
if(zlim[1]==zlim[2])breaks <-seq(zlim[1],zlim[2],length=2)
cols <-grey(rev(seq(0.1,0.9,length.out=length(breaks)-1)))
if(colour==T)
{
cols <-rev(heat.colors(length(breaks)-1))
}
}
# doing the GSA colour values 
if(doGSAs)
{
binval <-1
colval <-rep(cols,length(uniGSAs))
if(length(breaks)>2)
{
binval <-cut(mat,breaks,include.lowest=TRUE)
colval <-cols[match(binval,names(table(cut(mat,breaks,include.lowest=TRUE))))]
}
#cols <-colval
}
# reassigning mat for the instances when doGSA was TRUE
mat <-matrix(vals,length(midlons),length(midlats))

if(!plotmap) scale <-FALSE
# add to the plot

if(maptype=="image"&plotmap)
{
if(all(mat[is.finite(mat)]==0)) cols <-rep("white",length(cols))
image(midlons,midlats,mat,zlim=zlim,add=TRUE,col=cols,breaks=breaks,xlab="",ylab="")
# little fix to plot the 1/2 statsq for area 04  in the FRS demersal scheme 
if(doFRS==TRUE)
{
if(sampareaname=="Demersal")
{
binval <-1
colval <-cols
if(length(breaks)>2)
{
binval <-cut(mat[which(midlons==-1.5),which(midlats==58.25)],breaks,include.lowest=TRUE)
colval <-cols[match(binval,names(table(cut(mat[which(midlons==-1.5),which(midlats==58.25)],breaks,include.lowest=TRUE))))]
}
polygon(c(-2,-2,-2.5,-2.5),c(58,58.5,58.5,58),col=colval,border=colval) 

if(4 %in% SpaceStrat==FALSE) polygon(c(-2,-2,-2.5,-2.5),c(58,58.5,58.5,58),col="white",border="white") 
}
}
if(doGSAs)
{
for(i in 1:length(uniGSAs))
{
polygon(GSAareas[[uniGSAs[i]]],col=colval[i])
}
landmass.polygons(col.land,border=col.coast)
}
}


# adding ices divisions
if(area.lines==TRUE&doICES==TRUE)ices.division.lines()
if(area.lines==TRUE&doFRS==TRUE)
{
if(sampareaname=="Demersal")demersal.sampling.lines()
}



if(maptype=="contour"&plotmap)
{
scale <-F
contour(midlons,midlats,mat,col=col.cont,drawlabels=F,nlevels=6,add=TRUE)
}

if(statrects)
{
abline(h=seq(20,90,0.5)-0.5,lty=3,col=col.rect)
abline(v=seq(-50,80,1)+1,lty=3,col=col.rect)
axis(3,midlons,lonnames,cex.axis=0.5,tick=F,line=-0.5)
axis(4,midlats,latnames,cex.axis=0.5,las=3,tick=F,line=-1)

if(maptype=="bubble")
{
if(!fcoast&all(extremes))
{
polygon(newcoast,col=col.land,border=col.coast)
polygon(landmasses$iceland,col=col.land,border=col.coast)
polygon(landmasses$greenland,col=col.land,border=col.coast)
polygon(landmasses$spit1,col=col.land,border=col.coast)
polygon(landmasses$spit2,col=col.land,border=col.coast)
polygon(landmasses$spit3,col=col.land,border=col.coast)
polygon(landmasses$spit4,col=col.land,border=col.coast)
polygon(landmasses$spit5,col=col.land,border=col.coast)
polygon(landmasses$spit6,col=col.land,border=col.coast)
polygon(landmasses$spit7,col=col.land,border=col.coast)





coverline(col.land)
box()
}
if(!all(extremes))landmass.polygons(col.land,border=col.coast)
}
}
if(!is.statsq(SpaceStrat)) 
#if(!is.statsq(statsqs)) 
{
#ices.divs <-TRUE
if(!fcoast&all(extremes))
{
polygon(newcoast,col=col.land,border=col.coast)
polygon(landmasses$iceland,col=col.land,border=col.coast)
polygon(landmasses$greenland,col=col.land,border=col.coast)
polygon(landmasses$spit1,col=col.land,border=col.coast)
polygon(landmasses$spit2,col=col.land,border=col.coast)
polygon(landmasses$spit3,col=col.land,border=col.coast)
polygon(landmasses$spit4,col=col.land,border=col.coast)
polygon(landmasses$spit5,col=col.land,border=col.coast)
polygon(landmasses$spit6,col=col.land,border=col.coast)
polygon(landmasses$spit7,col=col.land,border=col.coast)

coverline(col.land)
box()
}
if(!all(extremes))landmass.polygons(col.land,border=col.coast)
}

# adding ices divisions moved up a bit 
#if(ices.divs==TRUE&doICES==TRUE)ices.division.lines()
#if(ices.divs==TRUE&doFRS==TRUE&sampareaname=="Demersal")demersal.sampling.lines()
# adding the coastline
if(landmass) fcoast <-FALSE
if(!all(fextremes)) fcoast <-FALSE
if(fcoast)lines(finecoast$lon,finecoast$lat,col=col.coast)
#if(!(fcoast)&all(extremes))
if(!(fcoast)&all(extremes)&(doGSAs==FALSE))
{
lines(newcoast$lon,newcoast$lat,col=col.coast)
coverline(col.land)
lines(landmasses$iceland,col=col.coast)
lines(landmasses$greenland,col=col.coast)
polygon(landmasses$spit1,col=col.land,border=col.coast)
polygon(landmasses$spit2,col=col.land,border=col.coast)
polygon(landmasses$spit3,col=col.land,border=col.coast)
polygon(landmasses$spit4,col=col.land,border=col.coast)
polygon(landmasses$spit5,col=col.land,border=col.coast)
polygon(landmasses$spit6,col=col.land,border=col.coast)
polygon(landmasses$spit7,col=col.land,border=col.coast)

}
if(!all(extremes)|(doGSAs==TRUE))lines(NHcoast$lon,NHcoast$lat,col=col.coast)
# adding depths
if(depths[1])
{
for(i in 1:length(depths))
{
ww <-(1:length(alldepths$lon))[depths[i]==alldepths$depth]
lines(alldepths$lon[ww],alldepths$lat[ww],col=col.depth)
}}
if(landmass)
{
#if(!fcoast&all(extremes))
if(!(fcoast)&all(extremes)&(doGSAs==FALSE))
{
polygon(newcoast,col=col.land,border=col.coast)
polygon(landmasses$iceland,col=col.land,border=col.coast)
polygon(landmasses$greenland,col=col.land,border=col.coast)
polygon(landmasses$spit1,col=col.land,border=col.coast)
polygon(landmasses$spit2,col=col.land,border=col.coast)
polygon(landmasses$spit3,col=col.land,border=col.coast)
polygon(landmasses$spit4,col=col.land,border=col.coast)
polygon(landmasses$spit5,col=col.land,border=col.coast)
polygon(landmasses$spit6,col=col.land,border=col.coast)
polygon(landmasses$spit7,col=col.land,border=col.coast)

coverline(col.land)
box()
}
if(!all(extremes)|(doGSAs==TRUE))landmass.polygons(col.land,border=col.coast)
}


# the bubble and value plotting comes after coastlines, depths etc so it is not overwriten 
if(maptype=="values"&plotmap)
{
#if(!overlay)scale <-F
lls <-convert.statsq.lat.lon(as.factor(names(valpercell)))
if(!is.statsq(SpaceStrat)) lls <-convert.icesarea.lat.lon(as.factor(names(valpercell)))
if(doFRS==TRUE)
{
if(sampareaname=="Demersal")
{
lls <-.convert.samplingarea.lat.lon(as.factor(names(valpercell)))
}}
vals <-as.vector(valpercell)
vals <-round(vals,digits.text)
text(lls$lon,lls$lat,vals,col=col.text,cex=cex.text)
}



if(maptype=="bubble"&plotmap)
{
#print(as.factor(names(valpercell)))
if(is.statsq(SpaceStrat)) lls <-convert.statsq.lat.lon(as.factor(names(valpercell)))
if(!is.statsq(SpaceStrat)) lls <-convert.icesarea.lat.lon(as.factor(names(valpercell)))
if(doFRS==TRUE)
{
if(sampareaname=="Demersal")
{
lls <-.convert.samplingarea.lat.lon(as.factor(names(valpercell)))
}}
bins <-as.numeric(cut(as.vector(valpercell),breaks,include.lowest=TRUE))
vals <-breaks[bins+1]
size <- cex.max.bubble * ((vals - threshold)/(max(vals,na.rm=T) - threshold))
if(!fcoast&all(extremes))
{
polygon(newcoast,col=col.land,border=col.coast)
polygon(landmasses$iceland,col=col.land,border=col.coast)
polygon(landmasses$greenland,col=col.land,border=col.coast)
polygon(landmasses$spit1,col=col.land,border=col.coast)
polygon(landmasses$spit2,col=col.land,border=col.coast)
polygon(landmasses$spit3,col=col.land,border=col.coast)
polygon(landmasses$spit4,col=col.land,border=col.coast)
polygon(landmasses$spit5,col=col.land,border=col.coast)
polygon(landmasses$spit6,col=col.land,border=col.coast)
polygon(landmasses$spit7,col=col.land,border=col.coast)

box()
}
if(!all(extremes))landmass.polygons(col.land,border=col.coast)
points(lls$lon,lls$lat,col=col.pch,cex=size,pch=pch)

}

#if(!all(extremes)|(novals==TRUE))landmass.polygons(col.land,border=col.coast)
if(!all(extremes)|(novals==TRUE))polygon(x=c(13,13,14,14),y=c(41.5,42,42,41.5),col=col.land,border=col.land)

if(scale)
{
valnames <-breaks
leg <-c(paste(floor(valnames[1:length(valnames)-1]),"<=",floor(valnames[2:(length(valnames))]),sep=" ")) 

if(maptype=="bubble")
{
bins <-as.numeric(cut(as.vector(breaks),breaks,include.lowest=TRUE))
vals <-breaks[bins+1]
scalesize <- cex.max.bubble * ((vals - threshold)/(max(vals,na.rm=T) - threshold))
legend(scaleplace,inset=0.02,leg,pch=rep(pch,length(breaks)),pt.cex=scalesize[2:length(scalesize)]
,cex=scale.cex,title=scale.title,col=col.pch,bty=scale.box)
}


if(maptype=="image")
{
legend(scaleplace,inset=0.02,leg,fill=cols[1:length(cols)],cex=scale.cex,title=scale.title,bty=scale.box)
}
if(maptype=="values")
{
legend(scaleplace,inset=0.02,leg,fill=cols[1:length(cols)],cex=scale.cex,title=scale.title,bty=scale.box)
}


}

box()
par(pty="m")
out <-list(lon=midlons,lat=midlats,values=mat,bks=breaks,cols=cols,maptype=maptype,scale.cex=scale.cex,cex.max.bubble=cex.max.bubble)
return(invisible(out))

}

#-----------------------end of space.plot----------------------------------------
coverline <-function(colour)
{
lonfrom <-c(-1.951,30.186,30.186)
lonto <-c(35.0,30.186,35.0)
latfrom <-c(46.67,46.67,59.921)
latto <-c(46.67,59.921,59.921)
segments(lonfrom,latfrom,lonto,latto,col=colour,lwd=3)
}


.convert.area.statsq <-function(areacode,areaname)
{
areanames <-c("ICES","Demersal","Mackerel","Herring","Nephrops","Scallops","Edible Crab","DemersalMet")
if((areaname %in% areanames)!=TRUE)stop("areaname not known")
if(areaname=="ICES")
{
out <-convert.icesarea.statsq(areacode)
}
if(areaname %in% areanames[-1])
{
out <-convert.samplingarea.statsq(areacode,areaname)
}
return(out)
}






`scale.plot` <-
function(bks,place="center",multiscale.title="",scaletype,fillcol,scale.box,cex.max.bubble,scale.cex)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that adds a scale to multiple plots
# produced by strata.space.plot
# needs work as of 21/4/08
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(1, 1, type='n',xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
valnames <-bks
leg <-c(paste(floor(valnames[1:length(valnames)-1]),"<=",floor(valnames[2:(length(valnames))]),sep=" ")) 
if(scaletype=="image")
{
legend(place,inset=0.02,leg,fill=fillcol,title=multiscale.title,ncol=4,bty=scale.box,cex=scale.cex)
}
if(scaletype=="bubble")
{
threshold <-0
col.pch <-"red"
pch <-1
bins <-as.numeric(cut(as.vector(bks),bks,include.lowest=TRUE))
vals <-bks[bins+1]
scalesize <- cex.max.bubble * ((vals - threshold)/(max(vals,na.rm=T) - threshold))
legend(place,inset=0.02,leg,pch=rep(pch,length(bks)),pt.cex=scalesize[2:length(scalesize)]
,title=multiscale.title,col=col.pch,ncol=4,bty=scale.box,cex=scale.cex)
}
}

#----------------end of scale.plot----------------------------------

  
# ----------generic method for spacePlot--------------------------

spacePlot <-function(costobj, variable, SpaceStrata, func, TimeStrata, TechStrata,...)
{
space.plot(costobj, variable, SpaceStrata,...)
}

#------------methods for cl objects-------------------------

spacePlot.cl <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
eval(parse(text=paste("variable <-costobj@cl$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-costobj@cl$",SpaceStrata,sep="")))
space.plot(variable,SpaceStrata,func,...)
}

spacePlot.clcons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
SpaceStrata <-"space"
eval(parse(text=paste("variable <-costobj@cl$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-costobj@cl$",SpaceStrata,sep="")))
if(all(as.character(SpaceStrata)=="all"))stop("Cant do a spatial plot when SpaceStrata is undefined")
SpaceStrata <-as.character(SpaceStrata)
space.plot(variable,SpaceStrata,func,...)
}


spacePlot.stratcl <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}


spacePlot.stratcl2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}

spacePlot.stratcl3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}


spacePlot.stratclcons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}


spacePlot.stratclcons2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TechStrata <-"technical"
TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}

spacePlot.stratclcons3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@cl)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TechStrata <-"technical"
#TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}


#-------------methods for ce objects--------------------------------

spacePlot.ce <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
cenames <-names(costobj@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
eval(parse(text=paste("variable <-costobj@ce$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-costobj@ce$",SpaceStrata,sep="")))
space.plot(variable,SpaceStrata,func,...)
}


spacePlot.cecons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
cenames <-names(costobj@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
SpaceStrata <-"space"
eval(parse(text=paste("variable <-costobj@ce$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-costobj@ce$",SpaceStrata,sep="")))
if(all(as.character(SpaceStrata)=="all"))stop("Cant do a spatial plot when SpaceStrata is undefined")
space.plot(variable,SpaceStrata,func,...)
}


spacePlot.stratce <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
cenames <-names(costobj@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}

spacePlot.stratce2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
cenames <-names(costobj@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}

spacePlot.stratce3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
cenames <-names(costobj@ce)
if(!(variable %in% cenames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}


spacePlot.stratcecons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@ce)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}


spacePlot.stratcecons2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@ce)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TechStrata <-"technical"
TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}

spacePlot.stratcecons3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
clnames <-names(costobj@ce)
if(!(variable %in% clnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TechStrata <-"technical"
TimeStrata <-"time"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}

# ------------methods for cs Objects----------------


spacePlot.cs <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
object <-costobj
variable <-variable
trnames <- names(object@tr)
hhnames <- names(object@hh)
slnames <- names(object@sl)
hlnames <- names(object@hl)
canames <- names(object@ca)
tablenames <- c(rep("tr", length(trnames)), rep("hh",
length(hhnames)), rep("sl", length(slnames)), rep("hl",
length(hlnames)), rep("ca", length(canames)))
varindex <- which(is.finite(match(c(trnames, hhnames,slnames, hlnames, canames), variable)))
vartable <- tablenames[varindex]
if (length(vartable) > 1)warning("specified variable occurs in more than one csData table")
if (length(vartable) < 1)stop("No such variable in the cost object")
newcs <- mergecsData(object)
eval(parse(text = paste("dataframe <-newcs@", vartable[1],sep = "")))
dataname <- vartable[1]
if (variable == "lenCls") 
{
plottable <- readline(cat("lenCls occurs in both hl length frequency tables and ca age length tables \n\nwhich do you want to plot? hl or ca \n"))
eval(parse(text = paste("dataframe <-newcs@", plottable,sep = "")))
dataname <- plottable
}
eval(parse(text=paste("variable <-dataframe$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-dataframe$",SpaceStrata,sep="")))
space.plot(variable,SpaceStrata,func,...)
}


spacePlot.cscons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
object <-costobj
variable <-variable
trnames <- names(object@tr)
hhnames <- names(object@hh)
slnames <- names(object@sl)
hlnames <- names(object@hl)
canames <- names(object@ca)
tablenames <- c(rep("tr", length(trnames)), rep("hh",
length(hhnames)), rep("sl", length(slnames)), rep("hl",
length(hlnames)), rep("ca", length(canames)))
varindex <- which(is.finite(match(c(trnames, hhnames,slnames, hlnames, canames), variable)))
vartable <- tablenames[varindex]
if (length(vartable) > 1)warning("specified variable occurs in more than one csData table")
if (length(vartable) < 1)stop("No such variable in the cost object")
newcs <- mergecsData(object)
eval(parse(text = paste("dataframe <-newcs@", vartable[1],sep = "")))
dataname <- vartable[1]
if (variable == "lenCls") 
{
plottable <- readline(cat("lenCls occurs in both hl length frequency tables and ca age length tables \n\nwhich do you want to plot? hl or ca \n"))
eval(parse(text = paste("dataframe <-newcs@", plottable,sep = "")))
dataname <- plottable
}
eval(parse(text=paste("variable <-dataframe$",variable,sep="")))
eval(parse(text=paste("SpaceStrata <-dataframe$space",sep="")))
space.plot(variable,SpaceStrata,func,...)
}


spacePlot.stratcs <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
#print(default.title)
#if(missing(multiscale.title))multiscale.title <-default.title
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}


spacePlot.stratcs2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}

spacePlot.stratcs3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}


spacePlot.stratcscons <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TimeStrata <-"time"
TechStrata <-"technical"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,multiscale.title=default.title,...)
}


spacePlot.stratcscons2 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TimeStrata <-"time"
TechStrata <-"technical"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrata,TechStrata,multiscale.title=default.title,...)
}


spacePlot.stratcscons3 <-function(costobj, variable, SpaceStrata,func,TimeStrata,TechStrata,...)
{
csnames <-unique(c(names(costobj@tr),names(costobj@hh),names(costobj@sl),names(costobj@hl),
names(costobj@ca)))
if(!(variable %in% csnames))stop("No such variable in the cost object")
varname <-paste(substitute(variable))
funcname <-paste(substitute(func))
spacename <-paste(substitute(SpaceStrata))
default.title <-paste(funcname,"of",varname,"by",spacename,sep=" ")
SpaceStrata <-"space"
TimeStrata <-"time"
TechStrata <-"technical"
strataSpacePlot(costobj,variable,SpaceStrata,func,TimeStrat=NULL,TechStrat=TechStrata,multiscale.title=default.title,...)
}




#----------------setting the methods--------------------------------------
setGeneric("spacePlot")
# -----for cl objects--------------
setMethod("spacePlot",signature(costobj="clData",variable="character",SpaceStrata="character",func="function"),spacePlot.cl)
setMethod("spacePlot",signature(costobj="clDataCons",variable="character",SpaceStrata="character",func="function"),spacePlot.clcons)
setMethod("spacePlot",signature(costobj="clData",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),
spacePlot.stratcl)
setMethod("spacePlot",signature(costobj="clData",variable="character",SpaceStrata="character",func="function",
TimeStrata="character",TechStrata="character"),spacePlot.stratcl2)
setMethod("spacePlot",signature(costobj="clData",variable="character",SpaceStrata="character",func="function",
TimeStrata="missing",TechStrata="character"),spacePlot.stratcl3)

setMethod("spacePlot",signature(costobj="clDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),
spacePlot.stratclcons)
setMethod("spacePlot",signature(costobj="clDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character",
TechStrata="character"),spacePlot.stratclcons2)
setMethod("spacePlot",signature(costobj="clDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="missing",
TechStrata="character"),spacePlot.stratclcons3)

#------------for ce objects------------------------
setMethod("spacePlot",signature(costobj="ceData",variable="character",SpaceStrata="character",func="function"),spacePlot.ce)
setMethod("spacePlot",signature(costobj="ceDataCons",variable="character",SpaceStrata="character",func="function"),spacePlot.cecons)
setMethod("spacePlot",signature(costobj="ceData",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),spacePlot.stratce)
setMethod("spacePlot",signature(costobj="ceData",variable="character",SpaceStrata="character",func="function",
TimeStrata="character",TechStrata="character"),spacePlot.stratce2)
setMethod("spacePlot",signature(costobj="ceData",variable="character",SpaceStrata="character",func="function",
TimeStrata="missing",TechStrata="character"),spacePlot.stratce3)

setMethod("spacePlot",signature(costobj="ceDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),
spacePlot.stratcecons)
setMethod("spacePlot",signature(costobj="ceDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character",
TechStrata="character"),spacePlot.stratcecons2)
setMethod("spacePlot",signature(costobj="ceDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="missing",
TechStrata="character"),spacePlot.stratcecons3)

#---------for cs objects------------------------
setMethod("spacePlot",signature(costobj="csData",variable="character",SpaceStrata="character",func="function"),spacePlot.cs)
setMethod("spacePlot",signature(costobj="csDataCons",variable="character",SpaceStrata="character",func="function"),spacePlot.cscons)
setMethod("spacePlot",signature(costobj="csData",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),spacePlot.stratcs)
setMethod("spacePlot",signature(costobj="csData",variable="character",SpaceStrata="character",func="function",
TimeStrata="character",TechStrata="character"),spacePlot.stratcs2)
setMethod("spacePlot",signature(costobj="csData",variable="character",SpaceStrata="character",func="function",
TimeStrata="missing",TechStrata="character"),spacePlot.stratcs3)

setMethod("spacePlot",signature(costobj="csDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character"),
spacePlot.stratcscons)

setMethod("spacePlot",signature(costobj="csDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="character",
TechStrata="character"),spacePlot.stratcscons2)
setMethod("spacePlot",signature(costobj="csDataCons",variable="character",SpaceStrata="character",func="function",TimeStrata="missing",
TechStrata="character"),spacePlot.stratcscons3)
#--------end of method definitions-------------------

#===================end of SpatialPlots_main================================================