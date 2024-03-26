# dbeVesselRaiseBoot.r
# development version

# updated 30 Jan 2009
# David Maxwell

# Code to reproduce and bootstrap traditional 'vessel raising' approach to estimate
# landed numbers at age from market sampling data

# Requires COST consolidated data objects as input

# In notation of 2nd COST meeting 24-26 June 2008 this is for:
# Sampling strategy:   Length + ALK, Commercial categories within Trips
# Sampling source:     Auction/Market/Harbour Landed fraction

# Data sets may contain information on sex at different levels depending on species:
# Landings totals in CL unsexed
# 1. Unsexed landed weight SL, unsexed length sample HL, unsexed age sample CA
# 2. Unsexed landed weight SL, unsexed length sample HL, sexed age sample CA
# 3. Unsexed landed weight SL, sexed length sample HL, sexed age sample CA
#  - Data in this form cannot currently be handled by subset function as sex is one of the fields used to combine tables
# (4. SURVEYS ONLY Sexed landed weight by haul, sexed length sample, sexed age sample)

# Currently only providing combined sex estimates,
#  i.e. using combined sex length distribution (LD) with combined sex age-length-key (ALK)

# Technical strata is always NA in ca table of consolidated objects so
# LD is processed by technical strata, aggregated across technical strata and
# then combined with the ALK, which is not stratified by technical strata

# Currently not dealing with cases where landings are not identified by species, i.e. only estimates where taxon=spp


# Task list / Possible Enhancements

# Issue Warning/error if no ALK for a strata with LD
# Check handling of NAs in strata definitions
# Check the use of gap filling in ALK
# Option to set.seed for random number generation
# Label plus group in output? Can subsequent functions deal with character value in age vector?
# Implement resampling of otoliths?
# Methods when sex and/or species available in age data but are unsexed and taxon in length data and/or landings.
# This is weighted (by landings) combination of vessels LDs, consider unweighted combination
# Look to reduce duplication by sharing code used here and in other raising functions
# check/improve speed





## Requires hidden functions from COSTcore:
## resample, spdAgreg, extCatchCat, As.num
resample <- COSTcore:::resample
spdAgreg <- COSTcore:::spdAgreg
extCatchCat <- COSTcore:::extCatchCat
As.num <- COSTcore:::As.num
#
# Uses the following hidden functions in COSTdbe, they should be available without assigning them
#propMissLgth <- COSTdbe:::propMissLgth
#fillMissFun <- COSTdbe:::fillMissFun
#gapsRm <- COSTdbe:::gapsRm

############### CALCULATIONS ##################


setGeneric("vesselRaise.boot", function(csObject,        #consolidated CS table
                                clObject,                #consolidated CL table (same stratification as csObject)
                                dbeOutp,                 #'dbeOutput' object with descriptive fields
                                B,                       # number of bootstrap iterations
                                age.plus = -1,           # age of plus group, if < 0 then no plus group applied
                                bootMethod = "samples",   # "samples" to resample age samples
                                incl.precision=TRUE,    ## added MM 26/07/2010
                                probs=c(0.025,0.975)
                                ){
  standardGeneric("vesselRaise.boot")

})



setMethod("vesselRaise.boot", signature(csObject="csDataCons", clObject="clDataCons", dbeOutp="dbeOutput"),

                            function(csObject,       #consolidated CS table
                            clObject,                #consolidated CL table (same stratification as csObject)
                            dbeOutp,                 #'dbeOutput' object with descriptive fields
                            B,                       # number of bootstrap iterations
                            age.plus = -1,           # age of plus group, if < 0 then no plus group applied
                            bootMethod = "samples",   # "samples" to resample age samples,
                            incl.precision=TRUE,    ## added MM 26/07/2010
                            probs=c(0.025,0.975)
                                                     # NOT IMPLEMENTED "otoliths" to resample otoliths within length classes of ALK
                            )  {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

species <- dbeOutp@species
fraction <- dbeOutp@catchCat ; if ("ALL"%in%fraction) fraction <- c("LAN","DIS")
if (dbeOutp@param != "landings") warning("This method is for estimating landings but 'param' slot in 'dbeOutp' is not 'landings'")

## Warn that calculations are not stratified by species
if (all(is.na(species))) stop("no species in 'dbeOutp' object!!")
if (length(species) > 1) { warning ("  More than one species specified, data for these species will be combined in processing.
  To calculate estimates by species, run function for one species at a time")}

CA = ca(csObject)
CA = CA[CA$spp%in%species,]
if (nrow(CA)==0) stop("no biological parameters in consolidated object!!")
# drop CA records with missing ages
CA = CA[!is.na(CA$age),]

HL = hl(csObject)
HL = HL[HL$spp%in%species & extCatchCat(HL$sort)%in%fraction,]
if (nrow(HL)==0) stop("no length records in part of consolidated object specified (species and fraction subset of csObject@hl)")
# drop HL records with missing numbers at length
HL = HL[!is.na(HL$lenNum),]

if (bootMethod != "samples") {warning('only option bootMethod = "samples", for bootstrapping length and age samples is implemented', call. = F)}
if ( dbeOutp@methodDesc != "bootstrap samples" ) {
  warning(paste ("'dbeOutput' object methodDesc slot has been changed from", dbeOutp@methodDesc, "to 'bootstrap samples'"), call. = F)
  dbeOutp@methodDesc = "bootstrap samples" }

#ages are grouped within specified 'age.plus' parameter
if (age.plus>=0) CA$age[CA$age>=age.plus] <- age.plus
# observed ages, last one might be plus group
ageClasses = sort(unique(CA$age))
nageClasses = length(ageClasses)

if (all(is.na(CA$time))) CA$time <- ""
if (all(is.na(CA$space))) CA$space <- ""

# number of age primary sampling units
nAgeSamples = tapply (CA$PSU, list(time = CA$time, space = CA$space), function(x){length(unique(x))} )
nAge.vec = as.vector(nAgeSamples)
nAge.vec = nAge.vec[!is.na(nAge.vec)]
#identify start and end positions for each set strata ids - !Take care, this does not match the order of Strat in ageids!
###start.pos = c(1, (cumsum(nAge.vec)[-length(nAge.vec)]) +1 )
###end.pos = cumsum(nAge.vec)

# PSUids for each strata combination
ageids = unique( CA [,c( "PSUid", "time","space")] )
ageids$Strat = paste (ageids$time, ageids$space, sep=":=:")
start.pos <- tapply(1:nrow(ageids),list(ageids$Strat),min, na.rm=TRUE)     ### <---- avoid the need to use this nAge.vec
end.pos <- tapply(1:nrow(ageids),list(ageids$Strat),max, na.rm=TRUE)
n.PSUid <- (end.pos+1)-start.pos

# tapply to match production of nAge.vec, may be an easier way?
###uStrat = as.vector (tapply(ageids$Strat, list(time = ageids$time, space = ageids$space), function(x){unique(x)} ) )
###uStrat = uStrat[!is.na (uStrat)]
uStrat <- unique(ageids$Strat)   ### <----  To have the full consistency fith the order of the names for all the objects
# sample new set of PSUid for each strata combination - stratified bootstrap resampling - may be able to use boot function instead
bootAgePSUid = matrix (NA, nrow = dim(ageids)[1], ncol=B+1)
dimnames(bootAgePSUid) = list(NULL, c("orig", paste("iter.",1:B,sep="")))
# original sample ids in first column
bootAgePSUid[,1] = ageids$PSUid

# put resampled ids for each strata into vector in relevant places,
# assigning to all columns using size = nAge.vec[i] * B, instead of using loop by iteration 1 to B
  for ( i in 1:length(uStrat) ){
# need to check order of nAge.vec and uStrat matches
###  bootAgePSUid [ start.pos[i]:end.pos[i], -1] = resample (ageids$PSUid [ ageids$Strat == uStrat[i] ], size = nAge.vec[i] * B, replace=T )
  bootAgePSUid [ start.pos[i]:end.pos[i], -1] = resample (ageids$PSUid [ ageids$Strat == uStrat[i] ], size = n.PSUid[i] * B, replace=T )
  }


# length sampling
HH = hh(csObject)

if (all(is.na(HH$time))) HH$time <- ""
if (all(is.na(HH$space))) HH$space <- ""
if (all(is.na(HH$technical))) HH$technical <- ""

# number of length primary sampling units
nLenSamples = tapply (HH$PSU, list(time = HH$time, space = HH$space, technical = HH$technical),
   function(x){length(unique(x))} )
nLen.vec = as.vector(nLenSamples)
nLen.vec = nLen.vec[!is.na(nLen.vec)]
#identify start and end positions for each strata
start.pos = c(1, (cumsum(nLen.vec)[-length(nLen.vec)]) +1 )
end.pos = cumsum(nLen.vec)

# PSUids for each strata combination
lenids = unique( HH [,c( "PSUid", "time","space","technical")] )
lenids$Strat = paste (lenids$time, lenids$space, lenids$technical, sep=":=:")
# may be an easier way?        ### let's do it like for the ages
start.pos <- tapply(1:nrow(lenids),list(lenids$Strat),min, na.rm=TRUE)     ### <---- avoid the need to use this nLen.vec
end.pos <- tapply(1:nrow(lenids),list(lenids$Strat),max, na.rm=TRUE)
n.PSUid <- (end.pos+1)-start.pos

###uStrat = as.vector (tapply(lenids$Strat, list(time = lenids$time, space = lenids$space, technical = lenids$technical),
###       function(x){unique(x)} ) )
###uStrat = uStrat[!is.na (uStrat)]
uStrat <- unique(lenids$Strat)   ### <----  To have the full consistency fith the order of the names for all the objects
# sample new set of PSUid for each strata combination - stratified bootstrap resampling - may be able to use boot function instead
bootLenPSUid = matrix (NA, nrow = dim(lenids)[1], ncol=B+1)
dimnames(bootLenPSUid) = list(NULL, c("orig", paste("iter.",1:B,sep="")))
# original sample ids in first column
bootLenPSUid[,1] = lenids$PSUid

# put resampled ids for each strata into vector in relevant places,
# assigning to all columns using size = nAge.vec[i] * B, instead of using loop by iteration 1 to B
for ( i in 1:length(uStrat) ){
# order of nLen.vec and uStrat needs to match
###  bootLenPSUid [ start.pos[i]:end.pos[i], -1] = resample (lenids$PSUid [ lenids$Strat == uStrat[i] ], size = nLen.vec[i] * B, replace=T )
  bootLenPSUid [ start.pos[i]:end.pos[i], -1] = resample (lenids$PSUid [ lenids$Strat == uStrat[i] ], size = n.PSUid[i] * B, replace=T )

  }


# LENGTH
# see DeltaCalcFun in LengthGraphs_Explore.r for how to do calculations on raw/validated data object

# Raising within vessel. Information that we need is in tables sl and hl
# for market sampling assume sl is always for combined sexes

# combine sexes in hl - this currently has no effect,
# earlier subset command drops records if SL is unsexed and HL is by sex

HL2 = spdAgreg(list(lenNum = HL$lenNum ),
 BY=list( PSUid = HL$PSUid, SSUid = HL$SSUid, TSUid = HL$TSUid,
          time = HL$time, space = HL$space, technical = HL$technical,
          sort = HL$sort, sampType = HL$sampType, landCtry = HL$landCtry,
          vslFlgCtry = HL$vslFlgCtry, proj = HL$proj, trpCode = HL$trpCode,
          staNum = HL$staNum, lenCls = HL$lenCls), sum )

# merge hl and sl, sex field gets picked up from sl
hlsl = merge (HL2, csObject@sl)
# recreate subSampCat *** relies on hlsl sort having 5 elements separated by -  ***
#hlsl$subSampCat = unlist(lapply (strsplit(as.character(hlsl$sort), split="-"), function(x) {x[5]}))
hlsl$subSampCat = unlist(lapply (strsplit(as.character(hlsl$sort), split="-"), function(x) {x[4]}))                    ######### MM 03/04/2009 : 'commCat' is not in 'sort' anymore

#measured numbers are raised to sample-level
hlsl$raisedNum = hlsl$lenNum * hlsl$wt / hlsl$subSampWt

#raised numbers summed across subsamples ('subSampCat' field)  (sex still in variable list but should all be the same value)
varnames = c("PSUid","SSUid","TSUid","time","space","technical", "sampType" ,"landCtry", "vslFlgCtry", "proj",
 "trpCode", "staNum" , "sex", "lenCls", "lenCode" )
tb <- unique(hlsl[,varnames])

tb$raisedNum = spdAgreg(list(raisedNum = hlsl$raisedNum), BY=list(hlsl$PSUid, hlsl$SSUid, hlsl$TSUid, hlsl$time, hlsl$space, hlsl$technical, hlsl$sampType,
 hlsl$landCtry, hlsl$vslFlgCtry, hlsl$proj, hlsl$trpCode, hlsl$staNum, hlsl$spp, hlsl$lenCls, hlsl$lenCode), sum )[,"raisedNum"]

# is this needed here?
# Need to Check handling of NAs in strata definitions
if (all(is.na(tb$time))) tb$time <- ""
if (all(is.na(tb$space))) tb$space <- ""
if (all(is.na(tb$technical))) tb$technical <- ""

# Total Landed Weight All Landings
CL = cl(clObject)
# *** For now assume taxon = spp ***
CL = CL[CL$taxon %in% species,]
tblan = CL

# apply landing multiplier, unallocated and misreported weights to official landings weight following FishFrame definition
# Total Landings = Official Landings * Multiplier + Unallocated Catch + Misallocated Catch
# if values are NA assume multiplier is 1 and amount to add is 0
### THIS SHOULD BE A GENERIC FUNCTION FOR ALL DBE AND MBE

  tblan$landMult[is.na(tblan$landMult)] = 1
  tblan$unallocCatchWt [is.na(tblan$unallocCatchWt)] = 0
  tblan$misRepCatchWt [is.na(tblan$misRepCatchWt)] = 0
  tblan$adjlandWt = tblan$landWt * tblan$landMult + tblan$unallocCatchWt + tblan$misRepCatchWt

# 2) The Landings Value corresponds to the Official Landings, so we should find
#        Total Value = Total Landings * Landings Value / Official Landings 
  tblan$adjlandValue = tblan$adjlandWt * tblan$landValue / tblan$landWt

# aggregrate landed weight (all vessels) by time, space and technical strata
CLagg = spdAgreg(list(adjlwtOfAll = tblan$adjlandWt, adjlvalOfAll = tblan$adjlandValue),
 BY=list( time = tblan$time, space = tblan$space, technical = tblan$technical), sum )


####### START OF BOOTSTRAP LOOP #################
ac.list = vector("list", B+1) # current approach to store outputs by iteration.
ld.list = vector("list", B+1)
alkgaps.list = vector("list", B+1)
alkgaps.counter = 0

# i=1 uses original data, its output is labelled iter=0
for (i in 1:(B+1) ){
print(i-1)
##  select samples corresponding to bootPSUid. Side effect - PSUid gets renamed bootAgePSUid
ca.boot = merge(data.frame(bootAgePSUid = bootAgePSUid[,i]), CA, by.x="bootAgePSUid", by.y="PSUid")

# may need to specify sequence of lenCls levels based on lenCode
alk = tapply(ca.boot$age, list(lenCls = ca.boot$lenCls, age = ca.boot$age, time = ca.boot$time, space = ca.boot$space),length)
alk[is.na(alk)] <- 0

## Fill in missing rows of alk  (Will be quicker to only run this if there are gaps that need filling)
# (all gaps of length <= value are filled with the sum of surrounding filled classes)

# need to have functions in alkGaps.r available
# Disadvantage is that it creates 'virtual' otoliths

gaps.out = gapsRm(alk,type="fillMiss",value=2,preview=FALSE,postview=FALSE)
alk = gaps.out$alk
# records with otoliths added
if ( dim(gaps.out$addIndTab)[1]>0 ){
alkgaps.list[[i]] = cbind(gaps.out$addIndTab, iter=(i-1))
alkgaps.counter = alkgaps.counter + 1 }

## converting alk array to data.frame
# rla number of aged fish at length class l, age a
# ra number of aged fish of age a
# pla proportion of fish aged a for length class l

x.df = data.frame (expand.grid(dimnames(alk)), rla = as.vector(alk))
sumx = apply(alk, c(1,3,4), sum ) # sum for each length class (dimension 2 of alk array)
sumx.df = data.frame (expand.grid(dimnames(sumx)), ra = as.vector(sumx))
alk.df = merge(x.df, sumx.df)
alk.df$pla = alk.df$rla / alk.df$ra

### Length, tb contains numbers at length raised to individual vessel
##  select samples corresponding to bootPSUid. Side effect - PSUid gets renamed bootLenPSUid
tb.boot = merge(data.frame(bootLenPSUid = bootLenPSUid[,i]), tb, by.x="bootLenPSUid", by.y="PSUid")
#tb.boot

# numbers at length summed across all fields except time, space and technical
ld = tapply(tb.boot$raisedNum, list(lenCls = tb.boot$lenCls, time = tb.boot$time, space = tb.boot$space, technical = tb.boot$technical), sum)
#ld
ld.df = data.frame (expand.grid(dimnames(ld)), nl = as.vector(ld))

# LANDED WEIGHT
# think about trpCode and staNum
# if take two genuine (not bootstrap) samples from same landing, may need to only take first occurence of landedwt to avoid double counting?

varnameswt = c("PSUid","SSUid","TSUid","time","space","technical", "sampType" ,"landCtry", "vslFlgCtry", "proj",
 "trpCode", "staNum" , "sex", "wt" )
tbwt <- unique(hlsl[,varnameswt])
##  select samples corresponding to bootPSUid. Side effect - PSUid gets renamed bootLenPSUid
tbwt.boot = merge(data.frame(bootLenPSUid = bootLenPSUid[,i]), tbwt, by.x="bootLenPSUid", by.y="PSUid")
#tbwt.boot

# Total landed weight all sampled trips by time, space and technical
lwt = spdAgreg(list(lwtOfSampled=tbwt.boot$wt),
 BY=list( time = tbwt.boot$time, space = tbwt.boot$space, technical = tbwt.boot$technical), sum )
lwt

# combine landed weight all vessels with landed weight sampled vessels

# For now ignore proj field, may need to aggregate lwt across proj
# For now ignore sampType field and sex fields in lwt
# For now ignore landCat, commCatScl, commCat, landValue in CLagg
#lwtBoth = merge( lwt [, c("time", "space", "technical", "landCtry", "vslFlgCtry", "spp", "lwtOfSampled")],
#      CLagg [, c("landCtry", "vslFlgCtry", "time", "space", "technical", "adjlwtOfAll")] )

lwtBoth = merge (lwt, CLagg)

# convert landed weight of sampled vessels from g to kg to match total landed weight
lwtBoth$lwtOfSampled = lwtBoth$lwtOfSampled / 1000

# combine landed weights with length distribution
ld.df = merge (ld.df, lwtBoth)         # output not in lencls order?
#ld.df$nl [is.na(ld.df$nl)] <- 0
# raise estimated numbers at length (nl) from sampled vessels to numbers (Nl) for total landed weight
ld.df$Nl = ld.df$nl * ld.df$adjlwtOfAll / ld.df$lwtOfSampled

# sum of technical strata for each time, space combination
ld.df2 = spdAgreg(list(Nl=ld.df$Nl),  BY=list( time = ld.df$time, space = ld.df$space, lenCls = ld.df$lenCls), function(x){sum(x, na.rm=T)} )

# combine LD and ALK
ald = merge(alk.df, ld.df2, by = c("lenCls", "time", "space"), all=T)

# Using gapsRm function above should remove most of the gaps in the ALK so that the line below gives <0 rows>
# lengths without age information
if (dim(ald[is.na(ald$pla) & !is.na(ald$Nl),])[1] > 0) {warning(paste("Iteration", i-1, "has gap(s) in ALK, Numbers at age will be underestimated"))}
# alk information without numbers at length, does not stop processing but indicates length samples do not cover landed length range.
if (dim(ald[!is.na(ald$pla) & is.na(ald$Nl),])[1] > 0) {length.range.warn = T }

# for now keep only length classes with alk and length distribution information
ald = merge(alk.df, ld.df2)

# estimated numbers at length and age, raised to landings
ald$Nla = ald$pla * ald$Nl

# Sum across length classes to give estimated Numbers at Age by strata
ac = spdAgreg(list(NatAge = ald$Nla),
    BY=list( time = ald$time, space = ald$space, age = ald$age ),  sum )
ac$age = as.numeric (as.character(ac$age))

# expand output to include zero values for ages in original samples but not in this iteration
# need to check this works with empty strata definitions
# using expand.grid as in Mathieu's formatt function in dbe_Estimates_BioPar.r could be easier,
# although will need to deal with cases where not all strata combinations are present
timespace = unique(paste(ac$time, ac$space, sep=":=:"))
tmpdf = expand.grid( timespace=timespace, ageClasses)
tmpvals = unlist (strsplit(as.character(tmpdf$timespace),":=:") )

all.ages = data.frame (time = tmpvals[seq(1,(length(tmpvals)-1),by=2)], space = tmpvals[seq(2,length(tmpvals),by=2)], age=ageClasses)
ac.out = merge(all.ages, ac, all=T)
ac.out$NatAge[is.na(ac.out$NatAge)] = 0
ac.out$age = as.numeric (as.character(ac.out$age))
ac.out = ac.out[order(ac.out$time, ac.out$space, ac.out$age),] # ensure output is in consistent order for each iteration
dimnames(ac.out)[[1]] = 1:(dim(ac.out)[1])

ac.out$iter = i-1   # iter=0 is original sample
ac.list[[i]] = ac.out

ld.out = ld.df [, c("time", "space", "technical", "lenCls", "Nl") ]
ld.out = ld.out[order(ld.out$time, ld.out$space, ld.out$technical, ld.out$lenCls),] # ensure output is in consistent order for each iteration
dimnames(ld.out)[[1]] = 1:(dim(ld.out)[1])
ld.out$iter = i-1   # iter=0 is original sample
# list of length classes for original data and bootstrap samples already appear to match (needs more testing)
# so if Number at length(Nl) is NA replace it with 0
ld.out$Nl [is.na(ld.out$Nl)] = 0
ld.list[[i]] = ld.out

} #END OF BOOTSTRAP LOOP

##### Output results and warnings #####

# Number of samples
# Only one slot for number of samples so including length and age samples by using technical strata to denote age samples.
#nSampLen = spdAgreg(list (value=lenids$PSUid), BY = list(time=lenids$time, space=lenids$space, technical=lenids$technical), length)
#nSampAge = spdAgreg(list (value=ageids$PSUid), BY = list(time=ageids$time, space=ageids$space, technical=rep("AgeSamples", length(ageids$PSUid)) ), length)
#dbeOutp@nSamp = rbind(nSampLen, nSampAge)

nSampLen = spdAgreg(list (value=lenids$PSUid), BY = list(time=lenids$time, space=lenids$space, technical=lenids$technical), length) ######### MM 03/04/2009
nSampAge = spdAgreg(list (value=ageids$PSUid), BY = list(time=ageids$time, space=ageids$space), length)                             ######### MM 03/04/2009
dbeOutp@nSamp$len = nSampLen                                                                                                        ######### MM 03/04/2009
dbeOutp@nSamp$age = nSampAge                                                                                                        ######### MM 03/04/2009

# Number measured or aged
#nMeasLen = spdAgreg(list (value=HL$lenNum), BY = list(time=HL$time, space=HL$space, technical=HL$technical), sum)
#nMeasAge = spdAgreg(list (value=CA$age), BY = list(time=CA$time, space=CA$space, technical=rep("Otoliths", length(CA$age))), length)
#dbeOutp@nMes = rbind(nMeasLen, nMeasAge)

nMeasLen = spdAgreg(list (value=HL$lenNum), BY = list(time=HL$time, space=HL$space, technical=HL$technical), sum)                   ######### MM 03/04/2009
nMeasAge = spdAgreg(list (value=CA$age), BY = list(time=CA$time, space=CA$space), length)                                           ######### MM 03/04/2009
dbeOutp@nMeas$len = nMeasLen                                                                                                        ######### MM 03/04/2009
dbeOutp@nMeas$age = nMeasAge                                                                                                        ######### MM 03/04/2009




# Age structure
#convert list of age compositions into data.frame matching dbeOutput format
# technical set to NA as it's always NA in CScon@ca
ac.df = dbeOutp@ageStruc$rep = data.frame  (time =  unlist(lapply(ac.list, FUN = function(x) {x[,"time"]})),
                                space =  unlist(lapply(ac.list, FUN = function(x) {x[,"space"]})),
                                technical = NA,
                                age =  unlist(lapply(ac.list, FUN = function(x) {x[,"age"]})),
                                value =  unlist(lapply(ac.list, FUN = function(x) {x[,"NatAge"]})),
                                iter =  unlist(lapply(ac.list, FUN = function(x) {x[,"iter"]}))  )

ac.df = ac.df[ac.df$iter > 0,]
ac.mean = spdAgreg (list (value=ac.df$value), BY = list(time=ac.df$time, space=ac.df$space, age=ac.df$age), mean)
ac.mean$age = As.num(ac.mean$age)
ac.mean = ac.mean [order(ac.mean$time, ac.mean$space, ac.mean$age),]
dimnames(ac.mean)[[1]] = 1:(dim(ac.mean)[1])

ac.var = spdAgreg (list (value=ac.df$value), BY = list(time=ac.df$time, space=ac.df$space, age=ac.df$age), var)
ac.var$age = As.num(ac.var$age)
ac.var = ac.var [order(ac.var$time, ac.var$space, ac.var$age),]
dimnames(ac.var)[[1]] = 1:(dim(ac.var)[1])

dbeOutp@ageStruc$estim = data.frame(ac.mean[,c("time","space")], technical=NA, ac.mean[,c("age","value")] )
dbeOutp@ageVar = data.frame(ac.var[,c("time","space")], technical=NA, ac.var[,c("age","value")] )

# Length structure
#convert list of length distributions into data.frame matching dbeOutput format
# This output is by technical, unlike ageStruc
ld.df = dbeOutp@lenStruc$rep = data.frame  (time =  unlist(lapply(ld.list, FUN = function(x) {x[,"time"]})),
                                space =  unlist(lapply(ld.list, FUN = function(x) {x[,"space"]})),
                                technical = unlist(lapply(ld.list, FUN = function(x) {x[,"technical"]})),
                                length =  unlist(lapply(ld.list, FUN = function(x) {x[,"lenCls"]})),
                                value =  unlist(lapply(ld.list, FUN = function(x) {x[,"Nl"]})),
                                iter =  unlist(lapply(ld.list, FUN = function(x) {x[,"iter"]}))  )

# sum over length classes to give estimate of total N for each iteration
ld.sumiter = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, iter=ld.df$iter), sum)
# reorder columns
ld.sumiter = ld.sumiter[, c("time","space","technical","value","iter")]
# if want to sort rows differently
# ld.sumiter = ld.sumiter [order(ld.sumiter$iter, ld.sumiter$time, ld.sumiter$space, ld.sumiter$technical),]
#dimnames(ld.sumiter)[[1]] = 1:dim(ld.sumiter)[[1]]
dbeOutp@totalN$rep = ld.sumiter

# drop original estimates from ld.sumiter to calculate bootstrap mean & var
ld.sumiter$iter = As.num(ld.sumiter$iter)
ld.sumiter = ld.sumiter[ld.sumiter$iter > 0,]
# mean across iterations of total N
dbeOutp@totalN$estim = spdAgreg (list (value=ld.sumiter$value), BY = list(time=ld.sumiter$time, space=ld.sumiter$space, technical=ld.sumiter$technical), mean)
# variance across iterations of total N
dbeOutp@totalNvar = spdAgreg (list (value=ld.sumiter$value), BY = list(time=ld.sumiter$time, space=ld.sumiter$space, technical=ld.sumiter$technical), var)

# drop original estimates from ld.df to calculate bootstrap mean & var
ld.df = ld.df[ld.df$iter > 0,]
ld.mean = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, length=ld.df$length), mean)
ld.mean$length = As.num(ld.mean$length)
# should be in correct order, but sort anyway
ld.mean = ld.mean [order(ld.mean$time, ld.mean$space, ld.mean$technical, ld.mean$length),]
dimnames(ld.mean)[[1]] = 1:(dim(ld.mean)[1])

ld.var = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, length=ld.df$length), var)
ld.var$length = As.num(ld.var$length)
ld.var = ld.var [order(ld.var$time, ld.var$space, ld.var$technical, ld.var$length),]
dimnames(ld.var)[[1]] = 1:(dim(ld.var)[1])

dbeOutp@lenStruc$estim = ld.mean
dbeOutp@lenVar = ld.var

# Total weight, adjusted by landing multiplier to official landings weight
# taken from CL records not estimated
dbeOutp@totalW$estim = data.frame (CLagg[,c("time", "space", "technical")], value = CLagg$adjlwtOfAll)

# records where otoliths were added due to gaps in ALK
if (alkgaps.counter > 0){
  itxt = ifelse (alkgaps.counter > 1,"iterations","iteration")
  print(paste("In", alkgaps.counter, itxt, "otoliths were added due to gaps in the ALK. Full details are in the object alkgaps.list"))
  assign("alkgaps.list", alkgaps.list, envir = .GlobalEnv)
  }

# if there were length classes in ALK that were not in LD information, issue warning that length samples may not cover landed length range.
if (length.range.warn == T) print ("Note that length samples may not cover full length range. There were empty length classes in some LDs where the corresponding length class in the ALK contained ages.")

if (any (nSampLen$value < 5)) warning ("Strata present with fewer than 5 length samples. Low sample numbers reduce the accuracy and precision of bootstrap estimates.", call.=F)
if (any (nSampAge$value < 5)) warning ("Strata present with fewer than 5 age samples. Low sample numbers reduce the accuracy and precision of bootstrap estimates.", call.=F)


if (incl.precision) {  

  if (!all(is.na(dbeOutp@lenStruc$rep))) {
    
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="l",replicates=TRUE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="l",probs=probs,replicates=TRUE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@ageStruc$rep))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="a",replicates=TRUE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="a",probs=probs,replicates=TRUE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@totalN$rep))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="n",replicates=TRUE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="n",probs=probs,replicates=TRUE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@totalW$rep))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="w",replicates=TRUE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="w",probs=probs,replicates=TRUE,update=TRUE)
  }
}

return(dbeOutp)
  } ## END OF vesselRaise.boot FUNCTION
 ) ## End of setMethod
 
 
##################################################################

## Example

## load example data set
## library(COSTdbe)
#data("LEMexample")
## Setup object for output
## strat was defined when creating consolidated object
#LEM.dbeOut = dbeObject(desc="Example",species="Microstomus kitt",param="landings",strataDesc=LEM.strat, catchCat="LAN",methodDesc="bootstrap samples")
#
## Run vesselRaise function, B set to a very low number of iterations for demonstration only
#LEM.dbeOut = vesselRaise.boot (csObject = LEM.CScon, clObject = LEM.CLcon, dbeOutp = LEM.dbeOut, B = 30 )
#
#



