
read.cost.data<-function(obsdata=NULL,mldata=NULL,species=NULL,wgl.eqn=NULL,season.definition="quarter"){
  if(is.null(obsdata)&is.null(mldata)){print("no data!");return}
obs.totlength<-ml.totlength<-obs.nFishBoat<-ml.nFishBoat<-obs.len.seas<-ml.len.seas<-
  obs.len.gear<-ml.len.gear<-obs.len.area<-ml.len.area<-
 obs.seas<-ml.seas<-obs.gear<-ml.gear<-obs.area<-ml.area<-obs.year<-ml.year<-l_disc<-
 l_land<-l_mland<-alk_l_disc<-alk_l_mland<-num_alk_disc<-num_alk_mland<-num_trip_obs<-
 num_haul_disc<-season_obs<-l_disc<-lfreq_disc<-num_alk_disc<-alk_l_disc<-alk_a_disc<-
 alk_lfreq_disc<-haulsize_disc<-sampsize_disc<-num_trip_land<-num_size_land<-l_land<-
 lfreq_land<-totsize_land<-sampsize_land<-num_trip_mland<-num_size_mland<-season_mland<-
 l_mland<-n_trip_mland<-lfreq_mland<-num_alk_mland<-alk_l_mland<-alk_a_mland<-alk_lfreq_mland<-
 totsize_mland<-sampsize_mland<-n_int_len_disc<-int_len_lim_disc<-int_len_vec_disc<-
 alk_w_mland<-alk_w_disc<-NULL  

n_trip_obs<-n_trip_mland<-0
  
  if(!is.null(obsdata)){
ca<-obsdata@ca[obsdata@ca$spp==species,]
hl<-obsdata@hl[obsdata@hl$spp==species,]
sl<-obsdata@sl[obsdata@sl$spp==species,]
tr<-obsdata@tr
if(!is.null(hl$catchCat)){
hl.disc<-hl[hl$catchCat=="DIS"&hl$spp==species,]
hl.land<-hl[hl$catchCat=="LAN"&hl$spp==species,]
ca.disc<-ca[ca$catchCat=="DIS"&ca$spp==species,]
ca.land<-ca[ca$catchCat=="LAN"&ca$spp==species,]
sl.disc<-sl[sl$catchCat=="DIS"&sl$spp==species,]
sl.land<-sl[sl$catchCat=="LAN"&sl$spp==species,]}
else {
ind<-grep("DIS",as.character(hl$sort))  
hl.disc<-hl[ind,][hl$spp[ind]==species,]
ind<-grep("DIS",as.character(ca$sort))
ca.disc<-ca[ind,][ca$spp[ind]==species,]
ind<-grep("DIS",as.character(sl$sort))
sl.disc<-sl[ind,][sl$spp[ind]==species,]
ind<-grep("LAN",as.character(hl$sort))
hl.land<-hl[ind,][hl$spp[ind]==species,]
ind<-grep("LAN",as.character(sl$sort))
sl.land<-sl[ind,][sl$spp[ind]==species,]
ind<-grep("LAN",as.character(ca$sort))
ca.land<-ca[ind,][ca$spp[ind]==species,]}

hh<-obsdata@hh
##################
obs.trips<-unique(c(ca.land$trpCode,ca.disc$trpCode,sl.land$trpCode,sl.disc$trpCode,
                   hl.land$trpCode,hl.disc$trpCode))
if(sum(!is.na(hh$date))>0){
date<-as.character(hh$date)
long.obs.year<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[1,])
obs.month<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[2,])
obs.day<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[3,])}
else {
  obs.trips<-unique(intersect(c(ca.land$trpCode,ca.disc$trpCode),c(hl.land$trpCode,hl.disc$trpCode)))
  obs.month<-obs.day<-long.obs.year<-NULL
  for(t in obs.trips){
    obs.month<-c(obs.month,getmode(ca$month[ca$trpCode==t&ca$age<99]))
    obs.day<-c(obs.day,15)
    long.obs.year<-c(long.obs.year,getmode(ca$year[ca$trpCode==t&ca$age<99]))}}
n_trip_obs<-length(obs.trips)

num_trip_obs<-tr$foNum
num_trip_land<-num_haul_disc<-l_disc<-lfreq_disc<-
	num_alk_disc<-alk_l_disc<-alk_a_disc<-
	haulsize_disc<-sampsize_disc<-totsize_land<-sampsize_land<-
	num_size_land<-l_land<-lfreq_land<-
	obs.seas<-obs.gear<-obs.area<-obs.year<-
	obs.totlength<-obs.nFishBoat<-obs.len.seas<-obs.len.gear<-obs.len.area<-NULL
useTSU<-F
if(!is.null(sl.land$TSUid)){if(sum(!is.na(sl.land$TSUid))>0){useTSU<-T}}
if(useTSU){sl.land.tsu<-sl.land$TSUid;hl.land.tsu<-hl.land$TSUid}
else if(sum(!is.na(sl.land$commCat))>0){sl.land.tsu<-sl.land$commCat;hl.land.tsu<-hl.land$commCat}
else if(sum(!is.na(sl.land$subSampCat))>0){sl.land.tsu<-sl.land$subSampCat;hl.land.tsu<-hl.land$subSampCat}
for(t in obs.trips){

nhaul<-tr$foNum[as.character(tr$trpCode)==t]

nsize<-sum(as.character(sl.land$trpCode)==t)
num_trip_land<-c(num_trip_land,nsize)

hh.use<-as.character(hh$trpCode)==t
obs.seas<-c(obs.seas,getmode(obs.month[obs.trips==t]))
if(!is.null(hh$foCatEu5))obs.gear<-c(obs.gear,getmode(hh$foCatEu5[hh.use]))
else obs.gear<-c(obs.gear,1)
if(!is.null(hh$area))obs.area<-c(obs.area,getmode(hh$area[hh.use]))
else obs.area<-c(obs.area,1)
obs.year<-c(obs.year,getmode(long.obs.year[hh.use]))

ca.use<-as.character(ca$trpCode)==t&ca$age<99
n<-sum(ca.use)
if(n>0){obs.len.seas<-c(obs.len.seas,rep(getmode(obs.month[hh.use]),sum(ca.use)))
        obs.len.gear<-c(obs.len.gear,rep(getmode(obs.gear[hh.use]),sum(ca.use)))
        obs.len.area<-c(obs.len.area,rep(getmode(obs.area[hh.use]),sum(ca.use)))
      }

for(i in 1:nhaul){
hl.disc.use<-as.character(hl.disc$trpCode)==t&hl.disc$staNum==i
n<-sum(hl.disc.use)
num_haul_disc<-c(num_haul_disc,n)
if(n>0){
l_disc<-c(l_disc,hl.disc$lenCls[hl.disc.use])
lfreq_disc<-c(lfreq_disc,hl.disc$lenNum[hl.disc.use])}}

ca.disc.use<-as.character(ca.disc$trpCode)==t&ca.disc$age<99
n<-sum(ca.disc.use)
num_alk_disc<-c(num_alk_disc,n)
if(n>0){
alk_w_disc<-c(alk_w_disc,ca.disc$indWt[ca.disc.use])
alk_l_disc<-c(alk_l_disc,ca.disc$lenCls[ca.disc.use])
alk_a_disc<-c(alk_a_disc,ca.disc$age[ca.disc.use])
}

for(i in 1:nhaul){
sl.disc.use<-as.character(sl.disc$trpCode)==t&sl.disc$staNum==i
n<-sum(sl.disc.use)
if(n>0){
haulsize_disc<-c(haulsize_disc,sl.disc$wt[sl.disc.use])
sampsize_disc<-c(sampsize_disc,sl.disc$subSampWt[sl.disc.use])}
else {
haulsize_disc<-c(haulsize_disc,NA)
sampsize_disc<-c(sampsize_disc,NA)}}

for(itsu in sl.land.tsu[as.character(sl.land$trpCode)==t]){
sl.land.use<-as.character(sl.land$trpCode)==t&sl.land.tsu==itsu
hl.land.use<-as.character(hl.land$trpCode)==t&hl.land.tsu==itsu
n1<-sum(sl.land.use)
if(n1>0){
totsize_land<-c(totsize_land,sl.land$wt[sl.land.use])
sampsize_land<-c(sampsize_land,sl.land$subSampWt[sl.land.use])}


n2<-sum(hl.land.use)
if(n2>0){
num_size_land<-c(num_size_land,n2)
l_land<-c(l_land,hl.land$lenCls[hl.land.use])
lfreq_land<-c(lfreq_land,hl.land$lenNum[hl.land.use])}
}
}
alk_lfreq_disc<-rep(1,length(alk_a_disc))
}

#############################
  if(!is.null(mldata)){
ca<-mldata@ca[mldata@ca$spp==species,]
hl<-mldata@hl[mldata@hl$spp==species,]
sl<-mldata@sl[mldata@sl$spp==species,]
tr<-mldata@tr

if(!is.null(hl$catchCat)){
hl.disc<-hl[hl$catchCat=="DIS"&hl$spp==species,]
hl.land<-hl[hl$catchCat=="LAN"&hl$spp==species,]
ca.disc<-ca[ca$catchCat=="DIS"&ca$spp==species,]
ca.land<-ca[ca$catchCat=="LAN"&ca$spp==species,]
sl.disc<-sl[sl$catchCat=="DIS"&sl$spp==species,]
sl.land<-sl[sl$catchCat=="LAN"&sl$spp==species,]}
else {
ind<-grep("DIS",as.character(hl$sort))  
hl.disc<-hl[ind,][hl$spp[ind]==species,]
ind<-grep("DIS",as.character(ca$sort))
ca.disc<-ca[ind,][ca$spp[ind]==species,]
ind<-grep("DIS",as.character(sl$sort))
sl.disc<-sl[ind,][sl$spp[ind]==species,]
ind<-grep("LAN",as.character(hl$sort))
hl.land<-hl[ind,][hl$spp[ind]==species,]
ind<-grep("LAN",as.character(sl$sort))
sl.land<-sl[ind,][sl$spp[ind]==species,]
ind<-grep("LAN",as.character(ca$sort))
ca.land<-ca[ind,][ca$spp[ind]==species,]}
hh<-mldata@hh
##########
num_trip_mland<-num_size_mland<-l_mland<-lfreq_mland<-
	num_alk_mland<-alk_l_mland<-alk_a_mland<-
	totsize_mland<-sampsize_mland<-ml.year<-
	ml.seas<-ml.gear<-ml.area<-ml.nFishBoat<-
	ml.totlength<-ml.len.seas<-ml.len.gear<-ml.len.area<-NULL
useTSU<-F
if(!is.null(sl.land$TSUid)){if(sum(!is.na(sl.land$TSUid))>0){useTSU<-T}}
if(useTSU){sl.land.tsu<-sl.land$TSUid;hl.land.tsu<-hl.land$TSUid}
else if(sum(!is.na(sl.land$commCat))>0){sl.land.tsu<-sl.land$commCat;hl.land.tsu<-hl.land$commCat}
else if(sum(!is.na(sl.land$subSampCat))>0){sl.land.tsu<-sl.land$subSampCat;hl.land.tsu<-hl.land$subSampCat}

ml.trips<-unique(c(ca.land$trpCode,ca.disc$trpCode,sl.land$trpCode,sl.disc$trpCode,
                   hl.land$trpCode,hl.disc$trpCode))
if(sum(!is.na(hh$date))>0){
date<-as.character(hh$date)
long.ml.year<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[1,])
ml.month<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[2,])
ml.day<-as.integer(matrix(unlist(strsplit(date,'-')),nrow=3)[3,])}
else {
  ml.trips<-unique(intersect(c(ca.land$trpCode,ca.disc$trpCode),c(hl.land$trpCode,hl.disc$trpCode)))
  ml.month<-ml.day<-long.ml.year<-NULL
  for(t in ml.trips){
    ml.month<-c(ml.month,getmode(ca$month[ca$trpCode==t&ca$age<99]))
    ml.day<-c(ml.day,15)
    long.ml.year<-c(long.ml.year,getmode(ca$year[ca$trpCode==t&ca$age<99]))}}
n_trip_mland<-length(ml.trips)
for(t in ml.trips){
  nhaul<-tr$foNum[as.character(tr$trpCode)==t]
nsize<-sum(as.character(sl.land$trpCode)==t)
num_trip_mland<-c(num_trip_mland,nsize)

hh.use<-as.character(hh$trpCode)==t
ml.seas<-c(ml.seas,getmode(ml.month[ml.trips==t]))
if(!is.null(hh$foCatEu5))ml.gear<-c(ml.gear,getmode(hh$foCatEu5[hh.use]))
else ml.gear<-c(ml.gear,1)
if(!is.null(hh$area))ml.area<-c(ml.area,getmode(hh$area[hh.use]))
else ml.area<-c(ml.area,1)
ml.year<-c(ml.year,getmode(long.ml.year[hh.use]))

ca.use<-(as.character(ca$trpCode)==t)&(ca$age<99)
n<-sum(ca.use)
if(n>0){ml.len.seas<-c(ml.len.seas,rep(getmode(ml.month[ml.trips==t]),sum(ca.use)))
        ml.len.gear<-c(ml.len.gear,rep(getmode(ml.gear[ml.trips==t]),sum(ca.use)))
        ml.len.area<-c(ml.len.area,rep(getmode(ml.area[ml.trips==t]),sum(ca.use)))
      }


for(itsu in sl.land.tsu[as.character(sl.land$trpCode)==t]){
sl.land.use<-as.character(sl.land$trpCode)==t&sl.land.tsu==itsu
hl.land.use<-as.character(hl.land$trpCode)==t&hl.land.tsu==itsu
n1<-sum(sl.land.use)
if(n1>0){
totsize_mland<-c(totsize_mland,sl.land$wt[sl.land.use])
sampsize_mland<-c(sampsize_mland,sl.land$subSampWt[sl.land.use])}
else {
totsize_mland<-c(totsize_mland,NA)
sampsize_mland<-c(sampsize_mland,NA)}

n2<-sum(hl.land.use)
num_size_mland<-c(num_size_mland,n2)
if(n2>0){
l_mland<-c(l_mland,hl.land$lenCls[hl.land.use])
lfreq_mland<-c(lfreq_mland,hl.land$lenNum[hl.land.use])}
}
ca.land.use<-(as.character(ca.land$trpCode)==t)&(ca.land$age<99)
n<-sum(ca.land.use)
num_alk_mland<-c(num_alk_mland,n)
ml.nFishBoat<-c(ml.nFishBoat,n)
if(n>0){
alk_w_mland<-c(alk_w_mland,ca.land$indWt[ca.land.use])  
alk_l_mland<-c(alk_l_mland,ca.land$lenCls[ca.land.use])
alk_a_mland<-c(alk_a_mland,ca.land$age[ca.land.use])}
}
alk_lfreq_mland<-rep(1,length(alk_a_mland))

season_mland<-ml.month[hh$staNum==1]
}
###
if(season.definition=="quarter"){obs.seas<-1+floor((obs.seas-1)/3)    
ml.seas<-1+floor((ml.seas-1)/3)
obs.len.seas<-1+floor((obs.len.seas-1)/3)
ml.len.seas<-1+floor((ml.len.seas-1)/3)}                                 
  totlength<-c(alk_l_disc,alk_l_mland)
  totweight<-c(alk_w_disc,alk_w_mland)
  if(!is.null(totweight))totweight<-log(totweight)
  replength<-rep(1,length(totlength))
  nFishBoat<-c(num_alk_disc,num_alk_mland) 
  len.seas<-c(obs.len.seas,ml.len.seas)
  len.gear<-c(obs.len.gear,ml.len.gear)
  len.area<-c(obs.len.area,ml.len.area)
  seas<-c(obs.seas,ml.seas)
  gear<-c(obs.gear,ml.gear)
  area<-c(obs.area,ml.area)
  year<-c(obs.year,ml.year)

  
  if(!is.null(wgl.eqn)){
    int<-wgl.eqn$int$const
    if(!is.null(wgl.eqn$int$seas))int<-int+wgl.eqn$int$seas[len.seas]
    if(!is.null(wgl.eqn$int$gear))int<-int+wgl.eqn$int$gear[len.gear]
    if(!is.null(wgl.eqn$int$area))int<-int+wgl.eqn$int$area[len.area]
    slope<-wgl.eqn$slope$const
    if(!is.null(wgl.eqn$slope$seas))slope<-slope+wgl.eqn$slope$seas[len.seas]
    if(!is.null(wgl.eqn$slope$gear))slope<-slope+wgl.eqn$slope$gear[len.gear]
    if(!is.null(wgl.eqn$slope$area))slope<-slope+wgl.eqn$slope$area[len.area]
    totweight<-int+slope*log(totlength)}
###
totlength<-log(totlength)
if(!is.null(l_disc))l_disc<-log(l_disc)
if(!is.null(l_land))l_land<-log(l_land)
if(!is.null(l_mland))l_mland<-log(l_mland)
if(!is.null(alk_l_disc))alk_l_disc<-log(alk_l_disc)
if(!is.null(alk_l_mland))alk_l_mland<-log(alk_l_mland)
  # Common parameters
  int_len=1
  l_min=min(l_disc,l_land,l_mland)
  l_max=max(l_disc,l_land,l_mland)
  r_len = exp(c(l_min,l_max))
  
  n_int_len = 1+round((r_len[2]-r_len[1])/int_len)
int_len_lim = log((r_len[1]-0.5+c(1:n_int_len))*int_len)
#  int_len_lim = round(log((r_len[1]-0.5+c(1:n_int_len))*int_len),3)
  int_len_lim = c(int_len_lim,max(r_len[2]+100.0,99999.9))
#  int_len_vec = round(log(seq(r_len[1],r_len[2],int_len)),3)
  tmp = seq(r_len[1],r_len[2],int_len)
  int_len_vec = (log(tmp+0.5)+log(tmp-0.5))/2

 # int_len_vec = log(seq(r_len[1],r_len[2],int_len))

  r_len_disc = c(1,r_len[2])
  n_int_len_disc = 1+round((r_len_disc[2]-r_len_disc[1])/int_len)
  #int_len_lim_disc = round(log((r_len_disc[1]-0.5+c(1:n_int_len_disc))*int_len),3)
  int_len_lim_disc = log((r_len_disc[1]-0.5+c(1:n_int_len_disc))*int_len)
  int_len_lim_disc = c(int_len_lim_disc,max(r_len_disc[2]+100.0,99999.9))
  tmp = seq(r_len_disc[1],r_len_disc[2],int_len)
  int_len_vec_disc = (log(tmp+0.5)+log(tmp-0.5))/2

  num_fish = (n_trip_obs+n_trip_mland)*(n_int_len+1)
  num_fish = num_fish + sum(num_alk_disc)+ sum(num_alk_mland)
  num_par = (n_trip_obs+n_trip_mland)+2 # r censoring parameter for all trips, k and m common
  num_par = num_par + 6 # 2 hyperparameters for all 3 censoring parameters

list(COST=1,
        num_par=num_par,
	num_fish=num_fish,
	n_trip_obs=n_trip_obs,
	num_trip_obs=num_trip_obs,
	num_haul_disc=num_haul_disc,
	season_obs=obs.seas,
	l_disc=l_disc,
	lfreq_disc=lfreq_disc,
	num_alk_disc=num_alk_disc,
	alk_l_disc=alk_l_disc,
	alk_a_disc=alk_a_disc,
	alk_w_disc=alk_w_disc,
	alk_lfreq_disc=alk_lfreq_disc,
	haulsize_disc=haulsize_disc,
	sampsize_disc=sampsize_disc,
	num_trip_land=num_trip_land,
	num_size_land=num_size_land,
	l_land=l_land,
	lfreq_land=lfreq_land,
	totsize_land=totsize_land,
	sampsize_land=sampsize_land,
	n_trip_mland=n_trip_mland,
	num_trip_mland=num_trip_mland,
	num_size_mland=num_size_mland,
	season_mland=ml.seas,
	l_mland=l_mland,
	lfreq_mland=lfreq_mland,
	num_alk_mland=num_alk_mland,
	alk_l_mland=alk_l_mland,
	alk_a_mland=alk_a_mland,
	alk_w_mland=alk_w_mland,
	alk_lfreq_mland=alk_lfreq_mland,
	totsize_mland=totsize_mland,
	sampsize_mland=sampsize_mland,
	n_int_len=n_int_len,
	int_len_lim=int_len_lim,
	int_len_vec=int_len_vec,
                n_int_len_disc=as.integer(n_int_len_disc),
                int_len_lim_disc=as.double(int_len_lim_disc),
                int_len_vec_disc=as.double(int_len_vec_disc),    
	totlength=totlength,replength=replength,nFishBoat=nFishBoat,
	totweight=totweight,seas=seas,gear=gear,area=area,year=year)
}

