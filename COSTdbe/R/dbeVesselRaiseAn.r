# dbeVesselRaiseAn.r

# updated 05 Feb 2009
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

# Task list

# Issue Warning/error if no ALK for a strata with LD
# Check handling of NAs in strata definitions
# Label plus group in output? Can subsequent functions deal with character value in age vector?
# Methods when sex and/or species available in age data but are unsexed and taxon in length data and/or landings.
# Look to reduce duplication by sharing code used here and in other raising functions



## Requires hidden functions from COSTcore: see 'AAoutcomeObject.r' 
## resample, spdAgreg, extCatchCat, As.num
resample <- COSTcore:::resample
spdAgreg <- COSTcore:::spdAgreg
extCatchCat <- COSTcore:::extCatchCat
As.num <- COSTcore:::As.num
#


############### CALCULATIONS ##################


setGeneric("vesselRaise.an", function(csObject,        #consolidated CS table
                                clObject,                #consolidated CL table (same stratification as csObject)
                                dbeOutp,                 #'dbeOutput' object with descriptive fields
                                age.plus = -1,           # age of plus group, if < 0 then no plus group applied
                                incl.precision=TRUE,    ## added MM 26/07/2010
                                probs=c(0.025,0.975)
                                ){
  standardGeneric("vesselRaise.an")

})

setMethod("vesselRaise.an", signature(csObject="csDataCons", clObject="clDataCons", dbeOutp="dbeOutput"),

                            function(csObject,       #consolidated CS table
                            clObject,                #consolidated CL table (same stratification as csObject)
                            dbeOutp,                 #'dbeOutput' object with descriptive fields
                            age.plus = -1,           # age of plus group, if < 0 then no plus group applied
                            incl.precision=TRUE,    ## added MM 26/07/2010
                            probs=c(0.025,0.975)
                            )  {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

species <- dbeOutp@species
fraction <- dbeOutp@catchCat ; if ("ALL"%in%fraction) fraction <- c("LAN","DIS")    # modif MM 29/04/2010
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

if ( dbeOutp@methodDesc != "analytical" ) {
  warning(paste ("'dbeOutput' object methodDesc slot has been changed from", dbeOutp@methodDesc, "to 'analytical'"), call. = F)
  dbeOutp@methodDesc = "analytical" }

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
start.pos = c(1, (cumsum(nAge.vec)[-length(nAge.vec)]) +1 )
end.pos = cumsum(nAge.vec)

# PSUids for each strata combination
ageids = unique( CA [,c( "PSUid", "time","space")] )
ageids$Strat = paste (ageids$time, ageids$space, sep=":=:")
# tapply to match production of nAge.vec, may be an easier way?
uStrat = as.vector (tapply(ageids$Strat, list(time = ageids$time, space = ageids$space), function(x){unique(x)} ) )
uStrat = uStrat[!is.na (uStrat)]
# PSUid for each strata combination
AgePSUid = ageids$PSUid

# length sampling
HH = hh(csObject)

if (all(is.na(HH$time))) HH$time <- ""
if (all(is.na(HH$space))) HH$space <- ""
if (all(is.na(HH$technical))) HH$technical <- ""

# number of length primary sampling units
nLenSamples = tapply (HH$PSU, list(time = HH$time, space = HH$space, technical = HH$technical),
   function(x){length(unique(x))} )
nLen.vec = as.vector(nLenSamples)
# nLen.df used in analytical variance calculation
nLen.df = data.frame (expand.grid(dimnames(nLenSamples)), nLen = nLen.vec)
nLen.df = nLen.df [!is.na(nLen.df$nLen),]

nLen.vec = nLen.vec[!is.na(nLen.vec)]
#identify start and end positions for each strata
start.pos = c(1, (cumsum(nLen.vec)[-length(nLen.vec)]) +1 )
end.pos = cumsum(nLen.vec)

# PSUids for each strata combination
lenids = unique( HH [,c( "PSUid", "time","space","technical")] )
lenids$Strat = paste (lenids$time, lenids$space, lenids$technical, sep=":=:")
# may be an easier way?
uStrat = as.vector (tapply(lenids$Strat, list(time = lenids$time, space = lenids$space, technical = lenids$technical),
       function(x){unique(x)} ) )
uStrat = uStrat[!is.na (uStrat)]
# PSUid for each strata combination
LenPSUid = lenids$PSUid

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
hlsl$subSampCat = unlist(lapply (strsplit(as.character(hlsl$sort), split="-"), function(x) {x[4]}))                       ######### MM 03/04/2009  : 'commCat' is not in 'sort' anymore

#measured numbers are raised to sample-level
hlsl$raisedNum = hlsl$lenNum * hlsl$wt / hlsl$subSampWt

#raised numbers summed across subsamples ('subSampCat' field)  (sex still in variable list but should all be the same value)
varnames = c("PSUid","SSUid","TSUid","time","space","technical", "sampType" ,"landCtry", "vslFlgCtry", "proj",
 "trpCode", "staNum" , "sex", "lenCls", "lenCode" )
tb <- unique(hlsl[,varnames])

tb$raisedNum = spdAgreg(list(raisedNum = hlsl$raisedNum), BY=list(hlsl$PSUid, hlsl$SSUid, hlsl$TSUid, hlsl$time, hlsl$space, hlsl$technical, hlsl$sampType,
 hlsl$landCtry, hlsl$vslFlgCtry, hlsl$proj, hlsl$trpCode, hlsl$staNum, hlsl$spp, hlsl$lenCls, hlsl$lenCode), sum )[,"raisedNum"]

# Need to Check handling of NAs in strata definitions
if (all(is.na(tb$time))) tb$time <- ""
if (all(is.na(tb$space))) tb$space <- ""
if (all(is.na(tb$technical))) tb$technical <- ""

# sum across length groups to give Total raised numbers by PSUid
nbyPSU = spdAgreg(list(raisedNum=tb$raisedNum), BY=list( PSUid = tb$PSUid, time=tb$time, space=tb$space, technical=tb$technical), sum )


# Total Landed Weight All Landings
CL = cl(clObject)
# *** For now assume taxon = spp ***
CL = CL[CL$taxon %in% species,]
tblan = CL

# apply landing multiplier, unallocated and misreported weights to official landings weight following FishFrame definition
# Total Landings = Official Landings * Multiplier + Unallocated Catch + Misallocated Catch
# if values are NA assume multiplier is 1 and amount to add is 0
  tblan$landMult[is.na(tblan$landMult)] = 1
  tblan$unallocCatchWt [is.na(tblan$unallocCatchWt)] = 0
  tblan$misRepCatchWt [is.na(tblan$misRepCatchWt)] = 0
  tblan$adjlandWt = tblan$landWt * tblan$landMult + tblan$unallocCatchWt + tblan$misRepCatchWt

# 2) The Landings Value corresponds to the Official Landings, so we should find
#        Total Value = Total Landings * Landings Value / Official Landings
  tblan$adjlandValue = tblan$adjlandWt * tblan$landValue / tblan$landWt


# aggregrate landed weight (all vessels) by time, space and technical strata
CLagg = spdAgreg(list(adjlwtOfAll = tblan$adjlandWt, landValue = tblan$landValue),
 BY=list( time = tblan$time, space = tblan$space, technical = tblan$technical), sum )

ac.list = vector("list", 1) # current approach to store outputs
ld.list = vector("list", 1)

# may need to specify sequence of lenCls levels based on lenCode
alk = tapply(CA$age, list(lenCls = CA$lenCls, age = CA$age, time = CA$time, space = CA$space),length)
alk[is.na(alk)] <- 0

## converting alk array to data.frame
# rla number of aged fish at length class l, age a
# ra number of aged fish of age a
# pla proportion of fish aged a for length class l  , pla = rla/ra
# variance of pla, vpla = pla(1-pla)/ra

x.df = data.frame (expand.grid(dimnames(alk)), rla = as.vector(alk))
sumx = apply(alk, c(1,3,4), sum ) # sum for each length class (dimension 2 of alk array)
sumx.df = data.frame (expand.grid(dimnames(sumx)), ra = as.vector(sumx))
alk.df = merge(x.df, sumx.df)
alk.df$pla = alk.df$rla / alk.df$ra
alk.df$vpla = alk.df$pla * (1 - alk.df$pla) / alk.df$ra

### Length, tb contains numbers at length raised to individual vessel

# LANDED WEIGHT
# think about trpCode and staNum
# if take two genuine (not bootstrap) samples from same landing, may need to only take first occurence of landedwt to avoid double counting?

varnameswt = c("PSUid","SSUid","TSUid","time","space","technical", "sampType" ,"landCtry", "vslFlgCtry", "proj",
 "trpCode", "staNum" , "sex", "wt" )
tbwt <- unique(hlsl[,varnameswt])

# Landed weight for each of the sampled trips
lwtsam = spdAgreg(list(lwtsam=tbwt$wt), BY=list( PSUid = tbwt$PSUid), sum )
# convert landed weight of sampled vessels from g to kg to match total landed weight
lwtsam$lwtsam = lwtsam$lwtsam/1000

# merge landed weight with total numbers for each sampled vessel (to calculate totalNvar)
tbPSU = merge(nbyPSU, lwtsam)
# calculate numbers per unit weight by PSUid, m_i
tbPSU$numperkg = tbPSU$raisedNum / tbPSU$lwtsam
# calculate mean numbers per unit weight, m, for each strata as sum of numbers / sum of weight
# St in variable names for Strata
tbAll = spdAgreg(list(raisedNumSt=tbPSU$raisedNum, lwtsamSt=tbPSU$lwtsam),
  BY=list( time = tbPSU$time, space = tbPSU$space, technical = tbPSU$technical), sum )
tbAll$mnperkgSt = tbAll$raisedNumSt / tbAll$lwtsamSt
# combine m_i with m,
tbAllvar = merge(tbPSU, tbAll)
# contribution to variance
tbAllvar$contrib = (tbAllvar$numperkg - tbAllvar$mnperkgSt)^2
# sum within strata and length classes
ldAll.df = spdAgreg(list(raisedNum=tbAllvar$raisedNum, SSmi = tbAllvar$contrib),
 BY=list( time = tbAllvar$time, space = tbAllvar$space, technical = tbAllvar$technical), sum )
# divide total of Sum of squares contributions by n-1 to give variance
# NaN generated if only one sample in a stratum
ldAll.df = merge(ldAll.df, nLen.df)
ldAll.df$varmi = ldAll.df$SSmi / (ldAll.df$nLen - 1)
# will scale varmi up to varN in code below as same multipliers need to be calculated for VarNl


# merge landed weight with raised numbers by length for each sampled vessel (to calculate N & var at length)
tb = merge (tb, lwtsam)
# calculate numbers per unit weight by length group, m_il
tb$numperkg = tb$raisedNum / tb$lwtsam

# to calculate m_l the mean of m_il by strata including zeroes when a vessel caught none at that length,
# sum by strata then divide by number of vessels sampled

## Approach if use m_l unweighted mean of numbers per kg
#tbper = spdAgreg(list(sumperkg=tb$numperkg),
# BY=list( time = tb$time, space = tb$space, technical = tb$technical, lenCls = tb$lenCls), sum )
#tbper = merge(tbper, nLen.df)
#tbper$mnperkg = tbper$sumperkg / tbper$nLen

## Approach if use m_l weighted mean of numbers per kg, i.e. sum of raised numbers over samples / sum of sample weights
tbper = spdAgreg(list(sumN=tb$raisedNum),
 BY=list( time = tb$time, space = tb$space, technical = tb$technical, lenCls = tb$lenCls), sum )

tbper = merge(tbper, tbAll[,c("time","space","technical","lwtsamSt")])
tbper$mnperkg = tbper$sumN / tbper$lwtsamSt

# combine m_il with m_l,
tbvar = merge(tb, tbper)
# contribution to variance
tbvar$contrib = (tbvar$numperkg - tbvar$mnperkg)^2

# sum within strata and length classes
tmpld.df = spdAgreg(list(raisedNum=tbvar$raisedNum, varnlpart1 = tbvar$contrib),
 BY=list( time = tbvar$time, space = tbvar$space, technical = tbvar$technical, lenCls = tbvar$lenCls), sum )

# tbvar does not include zero records so need to include contribution to variance from samples with zero catch at length
# number of samples with values at each length class
nz.df = spdAgreg(list(nonzeroes =tbvar$raisedNum),
 BY=list( time = tbvar$time, space = tbvar$space, technical = tbvar$technical, lenCls = tbvar$lenCls), length )

nz.df = merge(nz.df, nLen.df)
nz.df$zeroes = nz.df$nLen - nz.df$nonzeroes
nz.df = merge(nz.df, tbper)
nz.df$zcontrib = nz.df$zeroes * nz.df$mnperkg^2

# combine contribution to variance for non-zero and zero catch at length
ld.df = merge(tmpld.df, nz.df)

# divide total of contributions by n-1 to give variance
# NaN generated if only one sample in a stratum
# ? could apply finite population correction ?, (1- number sampled / number landings)
# ? but not all fish have been measured in sampled landings
ld.df$varnl = (ld.df$varnlpart1 + ld.df$zcontrib) / (ld.df$nLen - 1)

ld.df = ld.df [, c("time", "space", "technical", "lenCls", "raisedNum", "nLen", "nonzeroes", "zeroes", "mnperkg", "varnl")]
names(ld.df)[names(ld.df)=="raisedNum"] <- "nl"

# Total landed weight all sampled trips by time, space and technical, converted to kg
#lwt = spdAgreg(list(lwtOfSampled=tbwt$wt / 1000),
# BY=list( time = tbwt$time, space = tbwt$space, technical = tbwt$technical), sum )

# Total landed weight of each sampled trip, converted to kg
#lwtbyPSU = spdAgreg(list(lwtOfSampled=tbwt$wt / 1000),
# BY=list( time = tbwt$time, space = tbwt$space, technical = tbwt$technical, PSUid = tbwt$PSUid), sum )

# squared total landed weight of each sampled trip, summed by time, space and technical
lwtSS = spdAgreg(list(lwtSS = tbPSU$lwtsam^2),
 BY=list( time = tbPSU$time, space = tbPSU$space, technical = tbPSU$technical), sum )

tbAll = merge(tbAll, lwtSS)
# combine landed weight all vessels with landed weight sampled vessels

# For now ignore proj field, may need to aggregate lwt across proj
# For now ignore sampType field and sex fields in lwt
# For now ignore landCat, commCatScl, commCat, landValue in CLagg

lwtBoth = merge (tbAll, CLagg)

# combine landed weights with total numbers
# (note two columns with same values raisedNum and raisedNumSt could drop one)
ldAll.df = merge (ldAll.df, lwtBoth, by=c("time", "space", "technical") )
# raise total numbers from sampled vessels to total numbers (N) for total landed weight
ldAll.df$N = ldAll.df$raisedNumSt * ldAll.df$adjlwtOfAll / ldAll.df$lwtsamSt
# raise variance of total numbers for a sampled vessel to variance (varN) of numbers for total landed weight
ldAll.df$varN = ldAll.df$varmi * ldAll.df$lwtSS * ldAll.df$adjlwtOfAll^2 / ldAll.df$lwtsamSt^2


# combine landed weights with length distribution
ld.df = merge (ld.df, lwtBoth)

# raise estimated numbers at length (nl) from sampled vessels to numbers (Nl) for total landed weight
ld.df$Nl = ld.df$nl * ld.df$adjlwtOfAll / ld.df$lwtsamSt

# raise variance (varnl) of numbers at length for a sampled vessel to variance (varNl) for numbers for total landed weight
ld.df$varNl = ld.df$varnl * ld.df$lwtSS * ld.df$adjlwtOfAll^2 / ld.df$lwtsamSt^2

# sum of technical strata for each time, space combination, leave NaN's in
ld.df2 = spdAgreg(list(Nl=ld.df$Nl, varNl=ld.df$varNl),  BY=list( time = ld.df$time, space = ld.df$space, lenCls = ld.df$lenCls), function(x){sum(x, na.rm=F)} )

# combine LD and ALK
ald = merge(alk.df, ld.df2, by = c("lenCls", "time", "space"), all=T)

# check for gaps in the ALK, currently only warning if a problem may want to stop processing
# lengths without age information
if (dim(ald[is.na(ald$pla) & !is.na(ald$Nl),])[1] > 0) {warning(paste("gap(s) in ALK, Numbers at age will be underestimated. Use function alkLgthRec to investigate and address this problem."))}
# alk information without numbers at length, does not stop processing but indicates length samples do not cover landed length range.
if (dim(ald[!is.na(ald$pla) & is.na(ald$Nl),])[1] > 0) {length.range.warn = T }

# for now keep only length classes with alk and length distribution information
ald = merge(alk.df, ld.df2)

# estimated numbers at length and age, raised to landings
ald$Nla = ald$pla * ald$Nl
# variance of Nla as Nl^2 * Var(pla) + Var(Nl) * pla^2
ald$varNla = ald$Nl^2 * ald$vpla + ald$varNl * ald$pla^2

# Sum across length classes to give estimated Numbers at Age by strata
ac = spdAgreg(list(NatAge = ald$Nla, varNatAge = ald$varNla),
    BY=list( time = ald$time, space = ald$space, age = ald$age ),  sum )
ac$age = as.numeric (as.character(ac$age))

ac.out = ac[order(ac$time, ac$space, ac$age),]
dimnames(ac.out)[[1]] = 1:(dim(ac.out)[1])

ld.out = ld.df [, c("time", "space", "technical", "lenCls", "Nl", "varNl") ]
ld.out = ld.out[order(ld.out$time, ld.out$space, ld.out$technical, ld.out$lenCls),] # ensure output is in consistent order
dimnames(ld.out)[[1]] = 1:(dim(ld.out)[1])

##### Output results and warnings #####

# Number of samples
# Only one slot for number of samples so including length and age samples by using technical strata to denote age samples.
#nSampLen = spdAgreg(list (value=lenids$PSUid), BY = list(time=lenids$time, space=lenids$space, technical=lenids$technical), length)
#nSampAge = spdAgreg(list (value=ageids$PSUid), BY = list(time=ageids$time, space=ageids$space, technical=rep("AgeSamples", length(ageids$PSUid)) ), length)
#dbeOutp@nSamp = rbind(nSampLen, nSampAge)

nSampLen = spdAgreg(list (value=lenids$PSUid), BY = list(time=lenids$time, space=lenids$space, technical=lenids$technical), length)             ######### MM 03/04/2009
nSampAge = spdAgreg(list (value=ageids$PSUid), BY = list(time=ageids$time, space=ageids$space), length)                                         ######### MM 03/04/2009
dbeOutp@nSamp$len = nSampLen                                                                                                                    ######### MM 03/04/2009
dbeOutp@nSamp$age = nSampAge                                                                                                                    ######### MM 03/04/2009

# Number measured or aged
#nMeasLen = spdAgreg(list (value=HL$lenNum), BY = list(time=HL$time, space=HL$space, technical=HL$technical), sum)
#nMeasAge = spdAgreg(list (value=CA$age), BY = list(time=CA$time, space=CA$space, technical=rep("Otoliths", length(CA$age))), length)
#dbeOutp@nMes = rbind(nMeasLen, nMeasAge)

nMeasLen = spdAgreg(list (value=HL$lenNum), BY = list(time=HL$time, space=HL$space, technical=HL$technical), sum)                               ######### MM 03/04/2009
nMeasAge = spdAgreg(list (value=CA$age), BY = list(time=CA$time, space=CA$space), length)                                                       ######### MM 03/04/2009
dbeOutp@nMeas$len = nMeasLen                                                                                                                    ######### MM 03/04/2009
dbeOutp@nMeas$age = nMeasAge                                                                                                                    ######### MM 03/04/2009



# Age structure
# technical set to NA as it's always NA in CScon@ca
dbeOutp@ageStruc$estim = data.frame  (time =  ac.out$time, space =  ac.out$space,
                                technical = NA, age = ac.out$age, value = ac.out$NatAge)

dbeOutp@ageVar = data.frame(time =  ac.out$time, space =  ac.out$space,
                                technical = NA, age = ac.out$age, value = ac.out$varNatAge)

# Length structure
#convert into data.frame matching dbeOutput format
# This output is by technical, unlike ageStruc
dbeOutp@lenStruc$estim =  data.frame  (time =  ld.out$time, space = ld.out$space,
                                      technical = ld.out$technical, length = ld.out$lenCls,
                                      value = ld.out$Nl  )

dbeOutp@lenVar = data.frame  (time =  ld.out$time, space = ld.out$space,
                                      technical = ld.out$technical, length = ld.out$lenCls,
                                      value = ld.out$varNl  )
# total N
dbeOutp@totalN$estim = data.frame  (time =  ldAll.df$time, space = ldAll.df$space,
                                      technical = ldAll.df$technical, value = ldAll.df$N  )
# Alternatively you can sum over length classes to give the same estimate of total N
#spdAgreg (list (value=ld.out$Nl), BY = list(time=ld.out$time, space=ld.out$space, technical=ld.out$technical), sum)

# variance of total N calculated from mean numbers per kg by trip
dbeOutp@totalNvar =  data.frame  (time =  ldAll.df$time, space = ldAll.df$space,
                                      technical = ldAll.df$technical, value = ldAll.df$varN  )

# Note it is different to the sum across length groups of the variance of numbers at length, assuming independene of length classes.
# spdAgreg (list (value=ld.out$varNl), BY = list(time=ld.out$time, space=ld.out$space, technical=ld.out$technical), sum)


# Total weight, adjusted by landing multiplier to official landings weight
# taken from CL records not estimated
dbeOutp@totalW$estim = data.frame (CLagg[,c("time", "space", "technical")], value = CLagg$adjlwtOfAll)

# if there were length classes in ALK that were not in LD information, issue warning that length samples may not cover landed length range.
if (length.range.warn == T) print ("Note that length samples may not cover full length range. There were empty length classes in some LDs where the corresponding length class in the ALK contained ages.")

if (any (nSampLen$value == 1)) warning ("Strata present with one length sample, variance cannot be estimated for these strata", call.=F)


if (incl.precision) {       #added MM 26/07/2010

  if (!all(is.na(dbeOutp@lenStruc$estim)) & !all(is.na(dbeOutp@lenVar))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="l",replicates=FALSE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="l",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@ageStruc$estim)) & !all(is.na(dbeOutp@ageVar))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="a",replicates=FALSE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="a",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@totalN$estim)) & !all(is.na(dbeOutp@totalNvar))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="n",replicates=FALSE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="n",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(dbeOutp@totalW$estim)) & !all(is.na(dbeOutp@totalWvar))) {
    dbeOutp <- dbeCalc(dbeOutp,type="CV",vrbl="w",replicates=FALSE,update=TRUE)
    dbeOutp <- dbeCalc(dbeOutp,type="CI",vrbl="w",probs=probs,replicates=FALSE,update=TRUE)
  }
}

return(dbeOutp)

  } ## END OF vesselRaise.an FUNCTION
 ) ## End of setMethod
 
 
##################################################################

## arguments to test the code line by line, without calling the function
##csObject = LEM.CScon
##clObject = LEM.CLcon
##dbeOutp = LEM.dbeOut.an
##age.plus = -1
#
## library(COSTdbe)
## load example data set
## data function should work when data set is part of COSTdbe package
## use load("pathname/LEMexample.rdata") if have the example file separately
#data("LEMexample")
## Setup object for output
## note strat defined above when creating consolidated object
#LEM.dbeOut.an = dbeObject(desc="Example",species="Microstomus kitt",param="landings",strataDesc=LEM.strat, catchCat="LAN",methodDesc="analytical")
#
## Run vesselRaise function
#LEM.dbeOut.an = vesselRaise.an (csObject = LEM.CScon, clObject = LEM.CLcon, dbeOutp = LEM.dbeOut.an)
#LEM.dbeOut.an
#
## CV, see function dbeCalc for full implementation of calculating CV and confidence intervals
#100*sqrt(LEM.dbeOut.an@ageVar$value)/LEM.dbeOut.an@ageStruc$estim$value
#
#



