# SpatialPlots_Extras
# functions for spatial plotting 
# ACP 21/1/09
# includes:  
#   mergecsData
#   layout.matrix
#   landmass.polygons
#   is.statsq
#   convert.statsq.lat.lon
#   convert.icesarea.statsq
#   convert.icesarea.lat.lon
#   convert.statsq.icesarea
#   ices.division.lines
#   ices.division.names
#   subSetSpp
#   subSetTrip
#   subSetProj
#   subSetTarget
#   subSetGear
#   convert.samplingarea.statsq 
#   convert.statsq.samplingarea 
#   demersal.sampling.lines 
#   .convert.samplingarea.lat.lon
# lengthHist

`mergecsData` <-
function(csobj)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that merges the tables of a csData object
# so that $rect, $area, $date, $foCatNat, 
# foCatEu5 and foCatEu6 are appended to each of the 
# tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(class(csobj)%in%c("csData","csDataVal","csDataCons")!=TRUE) stop("this function only works on a csData object")


csobj@hh$staNum <-formatC(csobj@hh$staNum,width=3)
csobj@hl$staNum <-formatC(csobj@hl$staNum,width=3)
csobj@sl$staNum <-formatC(csobj@sl$staNum,width=3)
csobj@ca$staNum <-formatC(csobj@ca$staNum,width=3)




ca6colstring <-apply(csobj@ca[,c(1:6)],1,paste,collapse=".")
tr6colstring <-apply(csobj@tr[,c(1:6)],1,paste,collapse=".")
hh6colstring <-apply(csobj@hh[,c(1:6)],1,paste,collapse=".")
hh7colstring <-apply(csobj@hh[,c(1:7)],1,paste,collapse=".")
sl7colstring <-apply(csobj@sl[,c(1:7)],1,paste,collapse=".")
hl7colstring <-apply(csobj@hl[,c(1:7)],1,paste,collapse=".")


hlindex <-match(hl7colstring,hh7colstring)
if(any(is.na(hlindex))) warning("The key fields between hh and hl tables dont all match")
csobj@hl$rect <-csobj@hh$rect[hlindex]
csobj@hl$area <-csobj@hh$area[hlindex]
csobj@hl$date <-csobj@hh$date[hlindex]
csobj@hl$foCatNat <-csobj@hh$foCatNat[hlindex]
csobj@hl$foCatEu5 <-csobj@hh$foCatEu5[hlindex]
csobj@hl$foCatEu6 <-csobj@hh$foCatEu6[hlindex]
csobj@hl$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@hl$date)),2,3))
csobj@hl$month <-as.POSIXlt(csobj@hl$date)$mon+1
csobj@hl$yearfromdate <-as.numeric(substr(csobj@hl$date,1,4))



slindex <-match(sl7colstring,hh7colstring)
if(any(is.na(slindex))) warning("The key fields between hh and sl tables dont all match")
csobj@sl$rect <-csobj@hh$rect[slindex]
csobj@sl$area <-csobj@hh$area[slindex]
csobj@sl$date <-csobj@hh$date[slindex]
csobj@sl$foCatNat <-csobj@hh$foCatNat[slindex]
csobj@sl$foCatEu5 <-csobj@hh$foCatEu5[slindex]
csobj@sl$foCatEu6 <-csobj@hh$foCatEu6[slindex]
csobj@sl$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@sl$date)),2,3))
csobj@sl$month <-as.POSIXlt(csobj@sl$date)$mon+1
csobj@sl$yearfromdate <-as.numeric(substr(csobj@sl$date,1,4))


trindex <-match(tr6colstring,hh6colstring)
if(any(is.na(trindex))) warning("The key fields between tr and hh tables dont all match")
csobj@tr$rect <-csobj@hh$rect[trindex]
csobj@tr$area <-csobj@hh$area[trindex]
csobj@tr$date <-csobj@hh$date[trindex]
csobj@tr$foCatNat <-csobj@hh$foCatNat[trindex]
csobj@tr$foCatEu5 <-csobj@hh$foCatEu5[trindex]
csobj@tr$foCatEu6 <-csobj@hh$foCatEu6[trindex]
csobj@tr$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@tr$date)),2,3))
csobj@tr$month <-as.POSIXlt(csobj@tr$date)$mon+1
csobj@tr$yearfromdate <-as.numeric(substr(csobj@tr$date,1,4))
 
caindex <-match(ca6colstring,tr6colstring)
if(any(is.na(caindex))) warning("The key fields between tr and ca tables dont all match")
#csobj@ca$rect <-csobj@tr$rect[caindex]
#csobj@ca$area <-csobj@tr$area[caindex]
#csobj@ca$date <-csobj@tr$date[caindex]
csobj@ca$foCatNat <-csobj@tr$foCatNat[caindex]
csobj@ca$foCatEu5 <-csobj@tr$foCatEu5[caindex]
csobj@ca$foCatEu6 <-csobj@tr$foCatEu6[caindex]
#csobj@ca$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@ca$date)),2,3))
#csobj@ca$yearfromdate <-as.numeric(substr(csobj@ca$date,1,4))
#csobj@ca$monthfromdate <-as.numeric(substr(csobj@ca$date,6,7))

csobj@hh$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@hh$date)),2,3))
csobj@hh$month <-as.POSIXlt(csobj@hh$date)$mon+1
csobj@hh$yearfromdate <-as.numeric(substr(csobj@hh$date,1,4))

return(csobj)
}
#----------------------------------
setGeneric("mergecsData")

#------------end of mergecsData-------------------------------

`.layout.matrix` <-
function(nplots)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# layout.matrix
# function that sets the parameters for layout depending 
# on the number of plots required per page. 
# Adds one to nplots for the multiple scale to be plotted 
# along the bottom of the page used in strata.space.plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!(nplots %in% c(1,2,4,6,9,12)))stop ("nplots must be one of 1,2,4,6,9,or 12")
if(nplots==4)
{
x <-matrix(0,7,6)
x[1:3,1:3] <-1
x[1:3,4:6] <-2
x[4:6,1:3] <-3
x[4:6,4:6] <-4
x[7,1:6] <-5
}

if(nplots==2)
{
x <-matrix(0,4,6)
x[1:3,1:3] <-1
x[1:3,4:6] <-2
x[4,1:6] <-3
}

if(nplots==1)
{
x <-matrix(1,1,1)
}

if(nplots==6)
{
x <-matrix(0,5,6)
x[1:2,1:2] <-1
x[1:2,3:4] <-2
x[1:2,5:6] <-3
x[3:4,1:2] <-4
x[3:4,3:4] <-5
x[3:4,5:6] <-6
x[5,1:6] <-7
}

if(nplots==9)
{
x <-matrix(0,7,6)
x[1:2,1:2] <-1
x[1:2,3:4] <-2
x[1:2,5:6] <-3
x[3:4,1:2] <-4
x[3:4,3:4] <-5
x[3:4,5:6] <-6
x[5:6,1:2] <-7
x[5:6,3:4] <-8
x[5:6,5:6] <-9
x[7,1:6] <-10
}

if(nplots==12)
{
x <-matrix(0,7,8)
x[1:2,1:2] <-1
x[1:2,3:4] <-2
x[1:2,5:6] <-3
x[1:2,7:8] <-4
x[3:4,1:2] <-5
x[3:4,3:4] <-6
x[3:4,5:6] <-7
x[3:4,7:8] <-8
x[5:6,1:2] <-9
x[5:6,3:4] <-10
x[5:6,5:6] <-11
x[5:6,7:8] <-12
x[7,1:8] <-13
}
return(x)
}

#---------------------end of layout matrix---------------------------

`landmass.polygons` <-
function(colour="lightgrey",border="grey",...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# landmass.polygons
# function that adds landmass polygons
# for the NOAA 1:5000000 coastline using the coordinates
# defined by landmass in the list "landmasses"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data(NHcoast)
data(landmasses)
NHcoast$string[24171:24335] <-722
lon <-lat <-NULL
alllandmasses <-c("africa","andros","anglesey","arran","baliaricsC","baliaricsW","baltic1",
     "baltic2","baltic3","baltic4","bear","cephalonia","chios","corsica",
"crete","cyprus","dutch1","dutch2","dutch3","dutch4",
"eire","euboea","europe",
"far1","far2","far3","far4","far5","far6",
"greenland","greenland1","greenland2","greenland3","greenland4"
,"greenland5","greenland6","greenland7","greenland8","greenland9"
,"greenland10","greenland11","greenland12","greenland13","greenland14","greenland15",
"greenland16","greenland17",
"iceland","islay","jura","lemnos","lesvos",
"lewis","malta","man","mull","naxos","nor1","nor2","nor3","nor4","nor5","nor6",
"nor7","nor8","nor9","nor10","nor11","nor12","nor13",
"nor14","nor15","nor16","nor17","nor18","nor19","nor20","nor21",
"novaya","novaya1","novaya2","ork","ork2",
"peloponnesus","rhodes","sardinia","shet","sicily","skye",
"spit1","spit2","spit3","spit4","spit5","spit6","spit7","uist","ukmainland"
,"white") 
       
for(i in 1:length(alllandmasses))
{
eval(parse(text=paste("lon <-landmasses$",alllandmasses[i],"$lon",sep="")))
eval(parse(text=paste("lat <-landmasses$",alllandmasses[i],"$lat",sep="")))
polygon(lon,lat,col=colour,border=border,...)
}
box()
}

#------------------enf of landmass polygons-------------------------------

`is.statsq` <-
function(x)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# is.statsq
# returns TRUE 
# if x is 4 characters long, 3rd of which in a capital letter 
# and the other 3 values are 0 to 9 characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(class(x)!="character") x <-as.character(x)
numberchars <-as.character(0:9)
all(c(all(nchar(x)==4),
all(substr(x,3,3) %in% LETTERS),
all((substr(x,4,4) %in% numberchars)),
all((substr(x,1,1) %in% numberchars)),
all((substr(x,2,2) %in% numberchars))
))
}
#-------------------------------------------------------

#setGeneric("is.statsq")

#--------------end of is.statsq-------------------------

`convert.statsq.lat.lon` <-
function (statsq) 
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert.statsq.lat.lon
# function that returns the centres of statsqs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    part1 <- substr(statsq, 1, 2)
    part2 <- substr(statsq, 3, 4)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""), 
        as.character(labels))
    latvalues <- seq(35.5, 80.5, 0.5) + 0.25
   lonlabels <- c("A0","A1","A2","A3",paste(rep(LETTERS[c(2:8,10:12,13)], rep(10, 11)), rep(0:9, 
        11), sep = ""))
lonvalues <- (-44:69) + 0.5
    indx <- match(part1, latlabels)
    lat <- latvalues[indx]
    indx <- match(part2, lonlabels)
    lon <- lonvalues[indx]
if (any(is.na(lat)) | any(is.na(lon))) 
warning("Some stat squares have not been recognised.")
out <-list(lon=lon,lat=lat)
    return(out)
}
#--------------------------------------------------------------

#setGeneric("convert.statsq.lat.lon")
#-----------------end of convert.statsq.lat.lon----------------


convert.lon.lat.statsq <-function (lon, lat) 
{
if(any(lat<36)|any(lat>80))stop("lat value out of range")
if(any(lon<(-44))|any(lon>69))stop("lon value out of range")
newlat <- cut(lat, seq(35.5, 80.5, 0.5),right=FALSE)
newlon <- cut(lon, (-44:69),right=FALSE)
labels <- 0:90
latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
as.character(labels))
lonlabels <- c("A0","A1","A2","A3",paste(rep(LETTERS[c(2:8,10:12,13)], rep(10, 11)), rep(0:9,        11), sep = ""))
    y <- latlabels[as.numeric(newlat)]
    x <- lonlabels[as.numeric(newlon)]
    ices <- paste(y, x, sep = "")
    if (any(is.na(x)) | any(is.na(y))) 
        warning("Not all points have been matched to an ICES rectangle.")
    return((ices))
}

#------------end of convert.lon.lat.statssq---------------










`convert.icesarea.statsq` <-
function(areas)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert.icesarea.statsq
# function that returns the constituent statsqs of an  
# of an ICES area 
# needs data frame ICESAreaRects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ICESAreaRects <-NULL
data(ICESAreaRects)
areas <-toupper(as.character(areas))
uniarea <-unique(areas)
a <-ICESAreaRects
a$statsq <-toupper(as.character(a$statsq))
a$division <-toupper(as.character(a$division))
a$subdivision <-toupper(as.character(a$subdivision))
a$subarea <-toupper(as.character(a$subarea))
b <-stack(a,select=c("subarea","division","subdivision"))
b$statsq <-rep(a$statsq,3)
statsqs <-as.character(b$statsq[which(!is.na(match(b$values,areas)))])
parentarea <-as.character(b$values[which(!is.na(match(b$values,areas)))])
out <-list(statsq=statsqs,parentarea=parentarea)
return(out)
}
#-----------------------------------------------------------------------

#setGeneric("convert.icesarea.statsq")
#-----------------end of convert.icesarea.statsq------------------------

`convert.icesarea.lat.lon` <-
function(icesarea)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that returns the approx centre 
# of an ICES area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lons <-c(3,3,2.5,-9,-15,-15,-15,-10.5,-19,3,-5,-11,-10.5,-7,-7,-3.5,0,3,-18,-35,-28,-28,-3.5,
-2,-8,-6,-14.25,-14.25,-10,40,-12,-3.5)
lats <-c(52,55.5,59.5,58,58,53.5,50.5,61.5,67,67,53.75,53.5,50.5,51,49,49.5,50.25,78,72.5,62,54,43,47
,45,46,44,46,40,40,78,61.5,49.75)
areas <-c("IVc","IVb","IVa","VIa","VIb","VIIc","VIIk","Vb","Va","IIa","VIIa","VIIb","VIIj","VIIg",
"VIIh","VIIc","VIId","IIb","XIVa","XIVb","XII","X","VIIIa","VIIIb","VIIId","VIIIc","VIIIe"
,"IXb","IXa","I","Vb1","VIIe")

index <-match(icesarea,areas)
if(any(is.na(index)))warning("some of the ices areas not recognised")
out <-list(lon=lons[index],lat=lats[index])
return(out)
}
#--------------------------------------

#setGeneric("convert.icesarea.lat.lon")
#-----------------end of convert.icesarea.lat.lon------------------------

convert.statsq.icesarea <-function(statsqs)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert.statsq.area
# function that gives the ICES subarea, division and subdivision
# for a given statsq
# needs ICESAreaRects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data(ICESAreaRects)
aa <-ICESAreaRects
if(is.statsq(statsqs)!=TRUE)stop("statistical rectangles not in expected format")
check <-match(statsqs,aa$statsq)
if(any(is.na(check)!=FALSE))warning("some of your statsqs are not recognised")
subdivs <-as.character(aa$subdivision[match(statsqs,aa$statsq)])
divs <-as.character(aa$division[match(statsqs,aa$statsq)])
subarea <-as.character(aa$subarea[match(statsqs,aa$statsq)])
out <-list(subdivs=subdivs,divs=divs,subarea=subarea)
return(out)
}


#-------------------end of convert.statsq.icesarera


`ices.division.lines` <-
function (division = NULL, area = NULL, lty = 1, col = 1, lwd = 1) 
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ices.division.lines
# function that draws ICES division and subarea boundaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    division.list <- c("I","IIa", "IIb", "IIIa","IIIb","IIIc", "IVa", "IVb", "IVc", 
        "Va", "Vb", "VIa", "VIb", "VIIa", "VIIb", "VIIc", "VIId", 
        "VIIe", "VIIf", "VIIg", "VIIh", "VIIj", "VIIk", "VIIIa", 
        "VIIIb", "VIIIc", "VIIId", "VIIIe", "IXa", "IXb", "X", 
        "XII", "XIVa", "XIVb")
    area.list <- c("I","II", "II", "III", "IV", "IV", "IV", "V", 
        "V", "VI", "VI", "VII", "VII", "VII", "VII", "VII", "VII", 
        "VII", "VII", "VII", "VII", "VII", "VIII", "VIII", "VIII", 
        "VIII", "IX", "IX", "X", "XII", "XIV", "XIV")
    if (is.null(division) & is.null(area)) 
        division <- division.list
    if (!is.null(area)) {
        div <- division.list[area.list %in% area]
        if (is.null(division)) 
            division <- div
        else division <- c(division, div)
    }
    missing.division <- division[!(division %in% division.list)]
    if (length(missing.division) > 0) 
        warning(paste("we don't cater for these divisions:", 
            missing.division))
  if("I" %in% division)
{
lat <-c(90,90,77.5,NA,71,72,72,90)
lon <-c(30,69,69,NA,28,28,30,30)
lines(lon, lat, lty = lty, col = col, lwd = lwd)
}  




if ("IIa" %in% division) {
        lat <- c(73.5, 73.5, 72, 72, 71, NA, 62, 62, 63, 63, 
            73.5)
        lon <- c(-11, 30, 30, 28, 28, NA, 5.2, -4, -4, -11, -11)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IIb" %in% division) {
        lat <- c(90, 90, 73.5, 73.5)
        lon <- c(-11, 30, 30, -11)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IIIa" %in% division) {
        lat <- c(58.2, 57.5, 57.5, 57, 57, NA)
        lon <- c(7, 7, 8, 8, 8.2, NA)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IVa" %in% division & !("IVb" %in% division)) {
        lat <- c(62, 62, NA, 58.2, 57.5, 57.5, NA, 58.5, 62)
        lon <- c(-4, 5.2, NA, 7, 7, -1.7, NA, -4, -4)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IVb" %in% division & !("IVa" %in% division)) {
        lat <- c(57.5, 57.5, 57, 57, NA, 53.5, 53.5)
        lon <- c(-1.7, 8, 8, 8.5, NA, 0, 7)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IVa" %in% division & "IVb" %in% division) {
        lat <- c(62, 62, NA, 58, 57.5, NA, 58.5, 62, NA, 57.5, 
            57.5, 57, 57, NA, 53.5, 53.5)
        lon <- c(-4, 5, NA, 7, 7, NA, -4, -4, NA, -1.7, 8, 8, 
            8.5, NA, 0, 7)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IVc" %in% division) {
        lat <- c(53.5, 53.5, NA, 51, 51)
        lon <- c(0, 7, NA, 1, 2)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("Va" %in% division) {
        lat <- c(68, 68, 63, 63, 62, 62)
        lon <- c(-27, -11, -11, -15, -15, -27)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("Vb" %in% division) {
        lat <- c(60, 60, 60.5, 60.5, 63, 63, 60)
        lon <- c(-15, -5, -5, -4, -4, -15, -15)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIa" %in% division) {
        lat <- c(60, 60, 60.5, 60.5, 58.5, NA, 55, 55, NA, 54.5, 
            54.5, 60)
        lon <- c(-12, -5, -5, -4, -4, NA, -5, -6, NA, -8, -12, 
            -12)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIb" %in% division) {
        lat <- c(60, 60, 54.5, 54.5, 60)
        lon <- c(-18, -12, -12, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIa" %in% division) {
        lat <- c(55, 55, NA, 52, 52)
        lon <- c(-6, -5.2, NA, -4.7, -7.5)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIb" %in% division) {
        lat <- c(54.5, 54.5, NA, 52.5, 52.5)
        lon <- c(-12, -8, NA, -9.5, -12)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIc" %in% division) {
        lat <- c(54.5, 54.5, 52.5, 52.5, 54.5)
        lon <- c(-18, -12, -12, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIId" %in% division) {
        lat <- c(51, 51, NA, 49.5, 50.7)
        lon <- c(1, 2, NA, -2, -2)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIe" %in% division) {
        lat <- c(49.5, 50.7, NA, 50, 50, 49.5, 49.5, 48, 48)
        lon <- c(-2, -2, NA, -5.5, -7, -7, -5, -5, -4.5)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIf" %in% division) {
        lat <- c(NA, 51.5, 51, 51, 50.5, 50.5, 50, 50)
        lon <- c(NA, -5, -5, -6, -6, -7, -7, -5.5)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIg" %in% division) {
        lat <- c(52, 52, NA, 51.5, 51, 51, 50.5, 50.5, 50, 50, 
            51.5)
        lon <- c(-7.5, -4.7, NA, -5, -5, -6, -6, -7, -7, -9, 
            -9)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIh" %in% division) {
        lat <- c(50, 50, 49.5, 49.5, 48, 48, 50)
        lon <- c(-9, -7, -7, -5, -5, -9, -9)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIj" %in% division) {
        lat <- c(52.5, 52.5, NA, 51.5, 48, 48, 50)
        lon <- c(-12, -9.5, NA, -9, -9, -12, -12)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIk" %in% division) {
        lat <- c(52.5, 52.5, 48, 48, 52.5)
        lon <- c(-18, -12, -12, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIIa" %in% division) {
        lat <- c(48, 48, 47.5, 47.5, 47, 47, 46, 46)
        lon <- c(-4.5, -8, -8, -6, -6, -5, -5, -0.9)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIIb" %in% division) {
        lat <- c(46, 46, 45.5, 45.5, 44.5, 44.5, 43.5)
        lon <- c(-0.9, -4, -4, -3, -3, -2, -2)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIIc" %in% division) {
        lat <- c(44.5, 44.5, 43.3, NA, 43, 43, 43, 48)
        lon <- c(-11, -2, -2, NA, -9, -11, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIId" %in% division) {
        lat <- c(48, 48, 47.5, 47.5, 47, 47, 46, 46, 45.5, 45.5, 
            44.5, 44.5, 48)
        lon <- c(-11, -8, -8, -6, -6, -5, -5, -4, -4, -3, -3, 
            -11, -11)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("VIIIe" %in% division) {
        lat <- c(48, 48, 43, 43, 48)
        lon <- c(-18, -11, -11, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IXa" %in% division) {
        lat <- c(43, 43, NA, 36, 36, 43)
        lon <- c(-11, -9, NA, -5.4, -11, -11)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("IXb" %in% division) {
        lat <- c(43, 43, 36, 36, 43)
        lon <- c(-18, -11, -11, -18, -18)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("X" %in% division) {
        lat <- c(48, 48, 36, 36,48)
        lon <- c(-42, -18, -18, -42,-42)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("XII" %in% division) {
        lat <- c(62, 62, 60, 60, 48, 48, 59, 59, 62)
        lon <- c(-27, -15, -15, -18, -18, -42, -42, -27, -27)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("XIVa" %in% division) {
        lat <- c(83.5, 90, 90, 68, 68, 68.7)
        lon <- c(-40, -40, -11, -11, -27, -27)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
    }
    if ("XIVb" %in% division) {
        lat <- c(68.7, 59, 59, 60)
        lon <- c(-27, -27, -44, -44)
        lines(lon, lat, lty = lty, col = col, lwd = lwd)
        }
    if("IIIa" %in% division) {
     
lon <-c(10.74488,11.34143)
lat <-c(56.15972,55.93602)
lines(lon,lat,lty = lty, col = col, lwd = lwd)
lines(c(12.43510,12.23625),c(56.20944,56.08516),lty = lty, col = col, lwd = lwd)
         }
if("IIIb" %in% division) {    
lines(c(12.43510,12.78308),c(55.31461,55.36433),lty = lty, col = col, lwd = lwd)        
lines(c(12.43510,12.23625),c(56.20944,56.08516),lty = lty, col = col, lwd = lwd)        
        
        }
if("IIIc" %in% division) {    
lon <-c(10.74488,11.34143)
lat <-c(56.15972,55.93602)
lines(lon,lat,lty = lty, col = col, lwd = lwd)
lines(c(11.88826,11.88826),c(54.54407,54.17123),lty = lty, col = col, lwd = lwd)
 }
    
}

#------------------------------------------------------------------------

#setGeneric("ices.division.lines")
#--------------------------end of ices.division.lines-------------------

ices.division.names <-function(text.cex=1)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ices.division.names
# function that adds ICES area names to a plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(cex=text.cex)
text(c(3,3,2.5,-9,-15,-15,-15,-10.5,-19,3,-5,-11,-10.5,-7,-7,-3.5,0,3,-18,-35,-28,-28,-3.5,
-2,-8,-6,-14.25,-14.25,-10,40),
c(52,55.5,59.5,58,58,53.5,50.5,61.5,67,67,53.75,53.5,50.5,51,49,49.5,50.25,78,72.5,62,54,43,47
,45,46,44,46,40,40,78),
c("IVc","IVb","IVa","VIa","VIb","VIIc","VIIk","Vb","Va","IIa","VIIa","VIIb","VIIj","VIIg",
"VIIh","VIIe","VIId","IIb","XIVa","XIVb","XII","X","VIIIa","VIIIb","VIIId","VIIIc","VIIIe"
,"IXb","IXa","I"))
par(cex=1)
}
#-----------------------------------------------------------------------

#setGeneric("ices.division.names")
#-------------end of ices.division.names----------------------------------

subSetSpp <-function (costobj, spp) 
{
    library(COSTcore)
    data(code.list)
    if (spp %in% code.list$spp$X3A_CODE) {
        index <- which(code.list$spp$X3A_CODE == spp)
        if (length(index) > 1) 
            stop("More than one species for this code, try the scientific name")
        spp <- as.character(code.list$spp$Scientific_name[index])
    }
    if (class(costobj) %in% c("csData", "csDataVal")) {
 
indexca1 <-which(costobj@ca$spp == spp)
indexsl1 <-which(costobj@sl$spp == spp)

tripsca <-unique(costobj@ca$trpCode[indexca1])
tripssl <-unique(costobj@sl$trpCode[indexsl1])
alltrips <-unique(c(tripsca,tripssl))
#if(all(alltrips %in% tripsca)!=TRUE)warning("Some trips in ca have no sl equivelent")
#if(all(alltrips %in% tripssl)!=TRUE)warning("Some trips in sl have no ca equivelent")
indexhh <-which(costobj@hh$trpCode %in% alltrips)
indextr <-which(costobj@tr$trpCode %in% alltrips)
costobj@hh <- costobj@hh[indexhh,] 
costobj@tr <- costobj@tr[indextr,] 
costobj@ca <- costobj@ca[which(costobj@ca$spp == spp),] 
costobj@hl <- costobj@hl[which(costobj@hl$spp == spp),]
costobj@sl <- costobj@sl[which(costobj@sl$spp == spp),] 
        fishname <- as.character(code.list$spp$English_name[code.list$spp$Scientific_name == 
            spp])
        cat("csData subset by species", fishname, spp, "\n")
 cat("New data set consists of", length(costobj@tr$trpCode), 
        "trip records", "\n")
    cat(length(costobj@hl$spp), "length records", "\n")
    cat("and", length(costobj@ca$spp), "age or maturity records", "\n")

    }
    if (class(costobj) %in% c("clData", "clDataVal")) {
        costobj@cl <- costobj@cl[which(costobj@cl$taxon == spp), 
            ]
        fishname <- as.character(code.list$spp$English_name[code.list$spp$Scientific_name == 
            spp])
        cat("clData subset by taxon", fishname, spp, "\n")
        cat("New data set consists of", length(costobj@cl$taxon), 
            "records", "\n")
    }
    return(costobj)
}

#-------end of subSetSpp--------------------------------------------


subSetTrip <-function (costobj, trpCode,silent=FALSE) 
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# subSetTrip
# Function that subsets a csData object by trip
# now works on csDataCons objects
# and has the option of not printing to screen a summary 
# of the resulting subset. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    library(COSTcore)
    if ((class(costobj) %in% c("csData", "csDataVal","csDataCons")) == FALSE) 
        stop("COST object is not of csData class")
    if (all(trpCode %in% costobj@tr$trpCode) == FALSE) {
        stop("Trip code not recognised in the specified cost object")
    }
    costobj@tr <- costobj@tr[which(costobj@tr$trpCode %in% trpCode), 
        ]
    costobj@hh <- costobj@hh[which(costobj@hh$trpCode %in% trpCode), 
        ]
    costobj@sl <- costobj@sl[which(costobj@sl$trpCode %in% trpCode), 
        ]
    costobj@hl <- costobj@hl[which(costobj@hl$trpCode %in% trpCode), 
        ]
    costobj@ca <- costobj@ca[which(costobj@ca$trpCode %in% trpCode), 
        ]
if(silent==FALSE)
{
    cat("csData subset by trpCode", trpCode, "\n")
    cat("New data set consists of", length(costobj@tr$trpCode), 
        "trip records", "\n")
    cat(length(costobj@hl$trpCode), "length records", "\n")
    cat("and", length(costobj@ca$trpCode), "age records", "\n")
} 
   return(costobj)
}



# --------------end of subSetTrip------------------------------

subSetProj <-function(costobj,proj) 
{
    library(COSTcore)
if ((class(costobj) %in% c("csData","csDataVal"))==FALSE)stop("COST object is not of csData class")


if (all(proj %in% costobj@tr$proj)==FALSE)
{
stop("Project not recognised in the specified cost object")
}
costobj@tr <- costobj@tr[which(costobj@tr$proj %in% proj),]
costobj@hh <- costobj@hh[which(costobj@hh$proj %in% proj),]
costobj@sl <- costobj@sl[which(costobj@sl$proj %in% proj),]
costobj@hl <- costobj@hl[which(costobj@hl$proj %in% proj),]
costobj@ca <- costobj@ca[which(costobj@ca$proj %in% proj),]
        cat("csData subset by project", proj, "\n")
        cat("New data set consists of", length(costobj@tr$proj), 
            "trip records", "\n")
        cat(length(costobj@hl$proj), "length records","\n")
        cat("and", length(costobj@ca$proj), "age records","\n")
    
    return(costobj)
}


#--------------end of subSetProj-----------------------


subSetTarget <-function(costobj,assemblage) 
{
library(COSTcore)
if (class(costobj) %in% c("csData","csDataVal"))
{
#stop("COST object is not of csData class")

if (all(is.na(costobj@hh$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@hh$foCatEu5)|costobj@hh$foCatEu5==""))
{
index <-c(which(is.na(costobj@hh$foCatEu5)),which(costobj@hh$foCatEu5==""))
trips <-costobj@hh$trpCode[-index]
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@tr <- costobj@tr[which(costobj@tr$trpCode %in% trips),]
costobj@hh <- costobj@hh[which(costobj@hh$trpCode %in% trips),]
costobj@sl <- costobj@sl[which(costobj@sl$trpCode %in% trips),]
costobj@hl <- costobj@hl[which(costobj@hl$trpCode %in% trips),]
costobj@ca <- costobj@ca[which(costobj@ca$trpCode %in% trips),]
} 


x <-strsplit(costobj@hh$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
tfish <-x2[,2]


if (all(assemblage %in% tfish)==FALSE)
{
stop("Target assemblage not recognised in the specified cost object")
}

trips <-costobj@hh$trpCode[which(tfish %in% assemblage)]

costobj@tr <- costobj@tr[which(costobj@tr$trpCode %in% trips),]
costobj@hh <- costobj@hh[which(costobj@hh$trpCode %in% trips),]
costobj@sl <- costobj@sl[which(costobj@sl$trpCode %in% trips),]
costobj@hl <- costobj@hl[which(costobj@hl$trpCode %in% trips),]
costobj@ca <- costobj@ca[which(costobj@ca$trpCode %in% trips),]
        cat("csData subset by target assemblage", assemblage, "\n")
        cat("New data set consists of", length(costobj@tr$proj), 
            "trip records", "\n")
cat(length(costobj@hh$proj), "haul records","\n")
        cat(length(costobj@hl$proj), "length records","\n")
        cat("and", length(costobj@ca$proj), "age records","\n")
}

if (class(costobj) %in%c("clData","clDataVal"))
{
if (all(is.na(costobj@cl$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@cl$foCatEu5)|costobj@cl$foCatEu5==""))
{
index <-c(which(is.na(costobj@cl$foCatEu5)),which(costobj@cl$foCatEu5==""))
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@cl <- costobj@cl[-index,]
} 
x <-strsplit(costobj@cl$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
tfish <-x2[,2]
index2 <-which(tfish %in% assemblage)
costobj@cl <- costobj@cl[index2,]
        cat("clData subset by target assemblage", assemblage, "\n")
        cat("New data set consists of", length(costobj@cl$foCatEu5), 
            "records", "\n")
}

if (class(costobj) %in%c("ceData","ceDataVal"))
{
if (all(is.na(costobj@ce$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@ce$foCatEu5)|costobj@ce$foCatEu5==""))
{
index <-c(which(is.na(costobj@ce$foCatEu5)),which(costobj@ce$foCatEu5==""))
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@ce <- costobj@ce[-index,]
} 
x <-strsplit(costobj@ce$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
tfish <-x2[,2]
index2 <-which(tfish %in% assemblage)
costobj@ce <- costobj@ce[index2,]
        cat("ceData subset by target assemblage", assemblage, "\n")
        cat("New data set consists of", length(costobj@ce$foCatEu5), 
            "records", "\n")
}    
    return(costobj)
}

#----------------end of subSetTarget--------------------------
subSetGear <-function(costobj,gear) 
{
library(COSTcore)
if (class(costobj) %in% c("csData","csDataVal"))
{
if (all(is.na(costobj@hh$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@hh$foCatEu5)|costobj@hh$foCatEu5==""))
{
index <-c(which(is.na(costobj@hh$foCatEu5)),which(costobj@hh$foCatEu5==""))
trips <-costobj@hh$trpCode[-index]
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@tr <- costobj@tr[which(costobj@tr$trpCode %in% trips),]
costobj@hh <- costobj@hh[which(costobj@hh$trpCode %in% trips),]
costobj@sl <- costobj@sl[which(costobj@sl$trpCode %in% trips),]
costobj@hl <- costobj@hl[which(costobj@hl$trpCode %in% trips),]
costobj@ca <- costobj@ca[which(costobj@ca$trpCode %in% trips),]
} 

x <-strsplit(costobj@hh$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
geartype <-x2[,1]


if (all(gear %in% geartype)==FALSE)
{
stop("Gear type not recognised in the specified cost object")
}

trips <-costobj@hh$trpCode[which(geartype %in% gear)]

costobj@tr <- costobj@tr[which(costobj@tr$trpCode %in% trips),]
costobj@hh <- costobj@hh[which(costobj@hh$trpCode %in% trips),]
costobj@sl <- costobj@sl[which(costobj@sl$trpCode %in% trips),]
costobj@hl <- costobj@hl[which(costobj@hl$trpCode %in% trips),]
costobj@ca <- costobj@ca[which(costobj@ca$trpCode %in% trips),]
        cat("csData subset by gear type", gear, "\n")
        cat("New data set consists of", length(costobj@tr$proj), 
            "trip records", "\n")
cat(length(costobj@hh$proj), "haul records","\n")
        cat(length(costobj@hl$proj), "length records","\n")
        cat("and", length(costobj@ca$proj), "age records","\n")
}

if (class(costobj) %in% c("clData","clDataVal"))
{
if (all(is.na(costobj@cl$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@cl$foCatEu5)|costobj@cl$foCatEu5==""))
{
index <-c(which(is.na(costobj@cl$foCatEu5)),which(costobj@cl$foCatEu5==""))
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@cl <- costobj@cl[-index,]
} 
x <-strsplit(costobj@cl$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
geartype <-x2[,1]
index2 <-which(geartype %in% gear)
costobj@cl <- costobj@cl[index2,]
        cat("clData subset by gear type", gear, "\n")
        cat("New data set consists of", length(costobj@cl$foCatEu5), 
            "records", "\n")
}

if (class(costobj) %in% c("ceData","ceDataVal"))
{
if (all(is.na(costobj@ce$foCatEu5)))stop("Field $foCatEu5 entirely NA")
if (any(is.na(costobj@ce$foCatEu5)|costobj@ce$foCatEu5==""))
{
index <-c(which(is.na(costobj@ce$foCatEu5)),which(costobj@ce$foCatEu5==""))
warning(paste("$foCatEu5 contained",length(index), "missing values or NA's"))
costobj@ce <- costobj@ce[-index,]
} 
x <-strsplit(costobj@ce$foCatEu5, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
geartype <-x2[,1]
index2 <-which(geartype %in% gear)
costobj@ce <- costobj@ce[index2,]
        cat("ceData subset by gear type", gear, "\n")
        cat("New data set consists of", length(costobj@ce$foCatEu5), 
            "records", "\n")
}   
    return(costobj)
}
#-------------------end of subSetGear----------------------



convert.samplingarea.statsq <-function(areacode,samplingarea="Demersal")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns the constituent statsqs of a sampling area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <-areacode
if (class(x) != "character") x <- as.character(areacode)
allareas <-c("Demersal","Mackerel","Herring","Nephrops","Scallops","Edible Crab","DemersalMet")
if (!(samplingarea %in% allareas))stop(cat("Sampling area must be one of",allareas,"\n"))
data(samplingareas)
a <-samplingareas
samparea <-subset(a,a$TypeName==samplingarea)
#uniindex <-match(unique(substr(samparea$StatRect,1,4)),substr(samparea$StatRect,1,4))
uniindex <- match(unique(paste(samparea$AreaCode,substr(samparea$StatRect, 1, 4),sep=".")),
paste(samparea$AreaCode,substr(samparea$StatRect, 1, 4),sep="."))
samparea <-samparea[uniindex,]
#index <- match(as.character(samparea$AreaCode),x)
index <-which(!is.na(match(as.character(samparea$AreaCode),x)))
ss <-as.character(substr(samparea$StatRect[index],1,4))
parentarea <-as.character(samparea$AreaCode[index])
areaname <-as.character(samparea$AreaName[index])
out <-list(statsq=ss,parentarea=parentarea,parentname=areaname)
return(out)
}
#-------------end of convert.samplingarea.statsq-------------------------

convert.statsq.samplingarea <-function(statsq,samplingarea="Demersal")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns the FRS sampling area of a statsq 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <-statsq
if (class(x) != "character") x <- as.character(statsq)
allareas <-c("Demersal","Mackerel","Herring","Nephrops","Scallops","Edible Crab","DemersalMet")
if (!(samplingarea %in% allareas))stop(cat("Sampling area must be one of",allareas,"\n"))
data(samplingareas)
a <-samplingareas
samparea <-subset(a,a$TypeName==samplingarea)
#uniindex <-match(unique(substr(samparea$StatRect,1,4)),substr(samparea$StatRect,1,4))
uniindex <- match(unique(paste(samparea$AreaCode,substr(samparea$StatRect, 1, 4),sep=".")),
paste(samparea$AreaCode,substr(samparea$StatRect, 1, 4),sep="."))


samparea <-samparea[uniindex,]
index <- match(x,as.character(substr(samparea$StatRect,1,4)))
ss <-as.character(substr(samparea$StatRect[index],1,4))
parentarea <-as.character(samparea$AreaCode[index])
areaname <-as.character(samparea$AreaName[index])
out <-list(parentarea=parentarea,parentname=areaname,statsq=ss)
return(out)
}

#-------------end of convert.statsq.samplingarea-------------------------


demersal.sampling.lines <-function(doarea,nos=FALSE,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adds FRS demersal sampling area boundaries to a map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(missing(doarea))doarea <-c(1:13,40:48,50,60:61,70,93:95)
#area 02
lon <-c(-4,1,1,-2,-2,-4,-4)
lat <-c(62,62,59,59,59.5,59.5,62)
if(2 %in% doarea)lines(lon,lat,...)
# area13
lon <-c(1,4,4,1,1)
lat <-c(62,62,59.5,59.5,62)
if(13 %in% doarea)lines(lon,lat,...)
lon <-c(5.3,4,4,5,5,7,7)
lat <-c(62,62,58.5,58.5,57.5,57.5,58.1)
if(13 %in% doarea)lines(lon,lat,...)
#area05
lon <-c(0,1,1,4,4,5,5,2,2,0,0)
lat <-c(59,59,59.5,59.5,58.5,58.5,58,58,57.5,57.5,59)
if(5 %in% doarea)lines(lon,lat,...)
#area06
lon <-c(0,2,2,5,5,3,3,1,1,0,0)
lat <-c(57.5,57.5,58,58,55.5,55.5,55,55,55.5,55.5,57.5)
if(6 %in% doarea)lines(lon,lat,...)
#area07
lon <-c(5,8,8,8.4,NA,8.2,5,5)
lat <-c(57.5,57.5,57,57,NA,55.5,55.5,57)
if(7 %in% doarea)lines(lon,lat,...)
#area10
lon <-c(2,3,3,8.3,NA,7.1,3,3,2,2)
lat <-c(55,55,55.5,55.5,NA,53.5,53.5,54,54,55)
if(10 %in% doarea)lines(lon,lat,...)
#area12
lon <-c(3,7.1,NA,2,2,3,3)
lat <-c(53.5,53.5,NA,51,52,52,53.5)
if(12 %in% doarea)lines(lon,lat,...)
#area 11
lon <-c(0.1,1,1,3,3,2,2,1)
lat <-c(53.5,53.5,54,54,52,52,51,51)
if(11 %in% doarea)lines(lon,lat,...)
# area8
lon <-c(-3,0,0,1,1,2,2,1,1,0)
lat <-c(56,56,55.5,55.5,55,55,54,54,53.5,53.5)
if(8 %in% doarea)lines(lon,lat,...)
# area9
lon <-c(-4,-3,-3,NA,-3,-3,NA,-4,-4)
lat <-c(59.5,59.5,59,NA,58.94,58.64,NA,58.58,59.5)
if(9 %in% doarea)lines(lon,lat,...)
#area4
lon <-c(-3,-2,-2,0,0,-3,NA,-2,-2,-2.5,-2.5,-3,NA,-3,-3,NA,-3,-3)
lat <-c(59.5,59.5,59,59,56,56,NA,57.6,58,58,58.5,58.5,NA,58.64,58.94,NA,59,59.5)
if(4 %in% doarea)lines(lon,lat,...)
#area40
lon <-c(-5,-4,-4,NA,-5,-5)
lat <-c(59.5,59.5,58.5,NA,58.55,59.5)
if(40 %in% doarea)lines(lon,lat,...)
#area41 
lon <-c(-5,-4,-4,-6,-6,-5,-5)
lat <-c(60.5,60.5,59.5,59.5,60,60,60.5)
if(41 %in% doarea)lines(lon,lat,...)
#area60
lon <-c(-15,-4,-4,-5,-5,-15,-15)
lat <-c(63,63,60.5,60.5,60,60,63)
if(60 %in% doarea)lines(lon,lat,...)
#area61
lon <-c(-10,-7,-7,-8,-8,-10,-10)
lat <-c(61.5,61.5,60.5,60.5,60,60,61.5)
if(61 %in% doarea)lines(lon,lat,...)
#area42
lon <-c(-7,-5,-5,-7,-7)
lat <-c(59.5,59.5,58.5,58.5,59.5)
if(62 %in% doarea)lines(lon,lat,...)
#area44
lon <-c(-10,-8,-8,-7,-7,-8,-8,-10,-10)
lat <-c(58.5,58.5,59,59,57,57,56,56,58.5)
if(44 %in% doarea)lines(lon,lat,...)
# area 48
lon <-c(-12,-6,-6,-7,-7,-8,-8,-10,-10,-12,-12)
lat <-c(60,60,59.5,59.5,59,59,58.5,58.5,54.5,54.5,60)
if(48 %in% doarea)lines(lon,lat,...)
#area70
lon <-c(-18,-12,-12,-18,-18)
lat <-c(60,60,54.5,54.5,60)
if(70 %in% doarea)lines(lon,lat,...)
#area94
lon <-c(-18,-12,-12,-18,-18)
lat <-c(54.5,54.5,52.5,52.5,54.5)
if(94 %in% doarea)lines(lon,lat,...)
#area47
lon <-c(-10,-6,-6,NA,-8,-10,-10)
lat <-c(56,56,55,NA,54.5,54.5,56)
if(47 %in% doarea)lines(lon,lat,...)
#area93
lon <-c(-12,-8,NA,-9.5,-12,-12)
lat <-c(54.5,54.5,NA,52.5,52.5,54.5)
if(93 %in% doarea)lines(lon,lat,...)
#area95
lon <-c(-12,-9.5,NA,-7.6,-5,NA,-2,-2,NA,-4.8,-12,-12)
lat <-c(52.5,52.5,NA,52,52,NA,50.6,49.5,NA,48,48,52.5)
if(95 %in% doarea)lines(lon,lat,...)
#area43
lon <-c(-7,-5,NA,-5.8,-7,-7)
lat <-c(58.5,58.5,NA,57.5,57.5,58.5)
if(43 %in% doarea)lines(lon,lat,...)
#area46
lon <-c(-6,-4.8,NA,-5,-6,-6)
lat <-c(56,56,NA,55,55,56)
if(46 %in% doarea)lines(lon,lat,...)
if(nos)
{
lonname <-c(-2,2.5,2,4.6,7,2.5,-1,0.5,5,-3.5,-4.5,-4.6,-6,-11,-8.6,2,4,-7,-5,-6,-6.75,-5.25,-8,
-8.5,-11,-14,-14,-11,-3)
latname <-c(61,61,58.5,59.75,56.5,56.5,57.5,54.5,54.5,59,59,59.85,59,61.5,60.75,53.25,53,51,53.5,58,56.7,
55.5,55.5,57.5,58,57.5,53.5,53.5,58)
strataname <-c("01","02","05","13","07","06","04","08","10","09","40","41","42","60","61","11",
"12","95","50","43","45","46","47","44","48","70","94","93","03")
index <-match(doarea,as.numeric(strataname))
text(lonname[index],latname[index],strataname[index])
}
}

#----------------------------------


.convert.samplingarea.lat.lon <-function(samparea)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that returns the approx centre 
# of an sampling area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
samparea <-as.numeric(as.character(samparea))
lons <-c(-2,2.5,2,4.6,7,2.5,-1,0.5,5,-3.5,-4.5,-4.6,-6,-11,-8.6,2,4,-7,-5,-6,-6.75,-5.25,-8,
-8.5,-11,-14,-14,-11,-3)
lats <-c(61,61,58.5,59.75,56.5,56.5,57.5,54.5,54.5,59,59,59.85,59,61.5,60.75,53.25,53,51,53.5,58,56.7,
55.5,55.5,57.5,58,57.5,53.5,53.5,58)
areas <-c("01","02","05","13","07","06","04","08","10","09","40","41","42","60","61","11",
"12","95","50","43","45","46","47","44","48","70","94","93","03")
areas <-as.numeric(areas)
index <-match(samparea,areas)
if(any(is.na(index)))warning("some of the sampling areas areas not recognised")
out <-list(lon=lons[index],lat=lats[index])
return(out)
}

#-----------------------------------------------------------------------------
lengthHist <-function(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lengthHist
# function that plots histograms 
# of the length frequency data
# in the hl table of csData objects
# Borrows heavily from MM's lenDisPlot code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(COSTeda)
data(code.list)

if(class(x)%in%c("csData","csDataVal")==FALSE)stop("x is not csData")

fraction <- toupper(fraction)                                                   #
x@sl$catchCat <- toupper(x@sl$catchCat)                                         # MM 29/04/2010
x@hl$catchCat <- toupper(x@hl$catchCat)                                         #
x@ca$catchCat <- toupper(x@ca$catchCat)                                         #

spp <-ifelse(length(table(ca(x)$spp))==1,ca(x)$spp[1],"multiple species")
dots <-list(...)
object <-suppressWarnings(mergecsData(x))@hl
if((by %in% names(object))!=TRUE)stop("by not a recognised grouping variable")
#if(all((fraction %in% names(table(object$catchCat)))==FALSE))
if(all(names(table(object$catchCat))=="DIS"))
{
fraction <-"DIS"
warning("Only DIS fraction present in data")
}
if(all(names(table(object$catchCat))=="LAN"))
{
fraction <-"LAN"
warning("Only LAN fraction present in data")
}
if(all(fraction %in% names(table(object$catchCat)))==FALSE)
{
stop(paste(fraction,"fraction not in the data")) 
}

lgthCode <- as.character(sl(x)[,"lenCode"][1])
stepp <- c(1,5,10,25)
names(stepp) <- c("mm","scm","cm","25mm")
ste <- stepp[lgthCode]
varlevs <-as.character(level)
if(level[1]=="all")varlevs <-levels(factor(object[[by]]))
if(any(as.character(varlevs) %in% as.character(object[[by]])==FALSE))
{
stop("level not present in the variable")
}

df <-object 
# a call to hist to get the breaks over the length class range 
alllengths <-rep(df$lenCls,df$lenNum)
vals <-hist(alllengths,plot=F,breaks=seq(min(df$lenCls),max(df$lenCls),by=ste))
# running through the levels to get the ylimits
allcounts <-NULL
for(i in 1:length(varlevs))
{
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(dim(df)[1]==0)next
alllengths <-rep(df$lenCls,df$lenNum)
allcounts <-append(allcounts,hist(alllengths,breaks=seq(min(df$lenCls)
,max(df$lenCls),by=ste),plot=F)$counts)
}
# doing some defaults for the dots argument
addtitle <-FALSE
if(is.null(dots$main))addtitle <-TRUE
if(is.null(dots$axes))dots$axes <-TRUE
if(is.null(dots$add))dots$add <-FALSE
if(is.null(dots$angle))dots$angle <-45
if(is.null(dots$freq))dots$freq <-TRUE
if(is.null(dots$ylim))dots$ylim <-c(min(allcounts),max(allcounts))
if(dots$freq==FALSE)dots$ylim <-NULL
if(is.null(dots$xlab))dots$xlab <-"Length class (mm)"
if(is.null(dots$ylab)&dots$freq==TRUE)dots$ylab <-"Frequency"
if(is.null(dots$ylab)&dots$freq==FALSE)dots$ylab <-"Density"
# and finally plotting out the levels
out <-NULL
for(i in 1:length(varlevs))
{
if(addtitle)dots$main <-paste(by,varlevs[i],sep=" ")
if(addtitle&&by=="month")dots$main <-month.abb[as.numeric(varlevs)][i]
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]

alllengths <-rep(df$lenCls,df$lenNum)
out[[i]] <-hist(alllengths,main=dots$main,sub=dots$sub,xlab=dots$xlab,ylab=dots$ylab,breaks=vals$breaks
,ylim=dots$ylim,freq=dots$freq,col=dots$col,border=dots$border,
density=dots$density,angle=dots$angle,axes=dots$axes,add=dots$add,
cex.main=dots$cex.main
,line=dots$line,cex.axis=dots$cex.axis,cex.lab=dots$cex.lab)
}

# adding the outer margin title

fishname <-spp
if(fishname!="multiple species")
{
fishname <- as.character(code.list$spp$English_name[code.list$spp$Scientific_name == spp])
}
if(title)
{
title(paste("Length distribution for ",fishname," by ",by, 

sep=""),outer=TRUE,line=-1,cex.main=dots$cex.main)
}
names(out) <-varlevs
invisible(out)
}





#------------end of lengthHist------------------------
# methods for lengthHist
setGeneric("lengthHist")

#--------------------------------------------------


agelenPlot <-function(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,supsmu=FALSE,jitter=FALSE,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# agelenPlot
# function that plots age given length 
# of the ca table of csData objects
# Borrows heavily from MM's lenDisPlot code
# AP 22/04/09
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(COSTeda)
data(code.list)

if(class(x)%in%c("csData","csDataVal")==FALSE)stop("x is not csData")

fraction <- toupper(fraction)                                                   #
x@sl$catchCat <- toupper(x@sl$catchCat)                                         # MM 29/04/2010
x@hl$catchCat <- toupper(x@hl$catchCat)                                         #
x@ca$catchCat <- toupper(x@ca$catchCat)                                         #

spp <-ifelse(length(table(ca(x)$spp))==1,ca(x)$spp[1],"multiple species")

dots <-list(...)
object <-suppressWarnings(mergecsData(x))@ca
if((by %in% names(object))!=TRUE)stop("by not a recognised grouping variable")

if(all(names(table(object$catchCat))=="DIS"))
{
fraction <-"DIS"
warning("Only DIS fraction present in data")
}
if(all(names(table(object$catchCat))=="LAN"))
{
fraction <-"LAN"
warning("Only LAN fraction present in data")
}
if(all(fraction %in% names(table(object$catchCat)))==FALSE)
{
stop(paste(fraction,"fraction not in the data")) 
}

lgthCode <- as.character(sl(x)[,"lenCode"][1])
stepp <- c(1,5,10,25)
names(stepp) <- c("mm","scm","cm","25mm")
ste <- stepp[lgthCode]
varlevs <-as.character(level)
if(level[1]=="all")varlevs <-levels(factor(object[[by]]))
if(any(as.character(varlevs) %in% as.character(object[[by]])==FALSE))
{
stop("level not present in the variable")
}

df <-object 
# running through the levels to get the ylimits
allcounts <-allages <-NULL
for(i in 1:length(varlevs))
{
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(dim(df)[1]==0)next
alllengths <-df$lenCls
strataage <-df$age

allcounts <-append(allcounts,alllengths)
allages <-append(allages,strataage)

}
# doing some defaults for the dots argument
addtitle <-FALSE
if(is.null(dots$add))dots$add <-FALSE
if(dots$add==TRUE&&is.null(dots$main)) dots$main <-""
if(is.null(dots$main))addtitle <-TRUE
if(is.null(dots$xlab))dots$xlab <-"Age"
if(is.null(dots$ylab))dots$ylab <-"Length class (mm)"
if(is.null(dots$axes))dots$axes <-TRUE
if(is.null(dots$col))dots$col <-rep(1,length(varlevs))
if(is.null(dots$col.line))dots$col.line <-dots$col
if(!is.null(dots$col)&length(dots$col==1))dots$col <-rep(dots$col,length(varlevs))
if(!is.null(dots$col.line)&length(dots$col.line==1))dots$col.line <-rep(dots$col.line,length(varlevs))


if(is.null(dots$pch))dots$pch <-rep(1,length(varlevs))
if(!is.null(dots$pch)&length(dots$pch==1))dots$pch <-rep(dots$pch,length(varlevs))
if(is.null(dots$lwd))dots$lwd <-rep(1,length(varlevs))
if(!is.null(dots$lwd)&length(dots$lwd==1))dots$lwd <-rep(dots$lwd,length(varlevs))
if(is.null(dots$lty))dots$lty <-rep(1,length(varlevs))
if(!is.null(dots$lty)&length(dots$lty==1))dots$lty <-rep(dots$lty,length(varlevs))



if(is.null(dots$ylim))dots$ylim <-c(min(allcounts,na.rm=T),max(allcounts,na.rm=T))
if(is.null(dots$xlim))dots$xlim <-c(min(allages,na.rm=T),max(allages,na.rm=T))
if(is.null(dots$span))dots$span <-"cv" 
out <-NULL
# and finally plotting out the levels
for(i in 1:length(varlevs))
{

if(addtitle)dots$main <-paste(by,varlevs[i],sep=" ")
if(addtitle&&by=="month")dots$main <-month.abb[as.numeric(varlevs)][i]
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(jitter)jitterage <-jitter(df$age)
#alllengths <-df$lenCls
if(i==1|dots$add==FALSE)
{
plot(df$age,df$lenCls,ylim=dots$ylim,xlim=dots$xlim,main=dots$main,sub=dots$sub,col=dots$col[i],
pch=dots$pch[i],cex.main=dots$cex.main,cex.axis=dots$cex.axis,cex.lab=dots$cex.lab,
axes=dots$axes,xlab=dots$xlab,ylab=dots$ylab,cex=dots$cex)
if(jitter)points(jitterage,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(supsmu)suppressWarnings(lines(out[[i]] <-supsmu(df$age,df$lenCls,span=dots$span)
,col=dots$col.line[i],lwd=dots$lwd[i],lty=dots$lty[i]))
}

if(i>=2&dots$add==TRUE)
{
points(df$age,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(jitter)points(jitterage,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(supsmu)suppressWarnings(lines(out[[i]] <-supsmu(df$age,df$lenCls,span=dots$span)
,col=dots$col.line[i],lwd=dots$lwd[i],lty=dots$lty[i]))


}


}

# adding the outer margin title

fishname <-spp
if(fishname!="multiple species")
{
fishname <- as.character(code.list$spp$English_name[code.list$spp$Scientific_name == spp])
}
if(title)
{
title(paste("Age given Length for ",fishname," by ",by, 

sep=""),outer=TRUE,line=-1,cex.main=dots$cex.main)
}
if(!is.null(out))names(out) <-varlevs
invisible(out)
}

#--------------end of agelenPlot--------------------------------------------
# methods for lengthHist
setGeneric("agelenPlot")

#--------------------------------------------------



