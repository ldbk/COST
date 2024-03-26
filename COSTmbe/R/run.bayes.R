run.bayes<-function(csdata,cldata,fit=NULL,do.predict=T,species,season.definition="quarter",burnin=2000,
                    thin=1,nmcmc=1000,ageMin=0,ageMax=20,
                    wgl.eqn=NULL,age.covariates=NULL,weight.covariates=NULL,
                    prediction.cells=NULL,arealist=NULL,length.list,adj=NULL){

obsdata<-mldata<-NULL
if(sum(csdata@tr$sampType=="M")>0)mldata<-subset(csdata,subset=(sampType=="M"))
if(sum(csdata@tr$sampType=="S")>0)obsdata<-subset(csdata,subset=(sampType=="S")) 
if(is.null(mldata)&(is.null(obsdata))){print("no data of type M or S");return}
nseas<-0
if(season.definition=="quarter")nseas<-4
if(season.definition=="month")nseas<-12
if(nseas==0&age.covariates$season){print("invalid time strata");return}
models<-make.reg.models(age.covariates,weight.covariates)
l.int  <- log(seq(length.list$minl,length.list$maxl,length.list$int))
if(!is.null(wgl.eqn)){
  if(!is.null(wgl.eqn$int$seas)){
if(nseas==4&length(wgl.eqn$int$seas)==12)wgl.eqn$int$seas<-c(mean(wgl.eqn$int$seas[1:3]),
      mean(wgl.eqn$int$seas[4:6]),mean(wgl.eqn$int$seas[7:9]),mean(wgl.eqn$int$seas[10:12]))}}

input.data<-read.cost.data(obsdata,mldata,species,wgl.eqn,season.definition)
  trip.alk<-rep(1:input.data$n_trip_mland,input.data$num_alk_mland)
if(!age.covariates$season){input.data$season_mland<-rep(1,length(input.data$season_mland))
input.data$seas<-rep(1,length(input.data$seas))}
trip.l1<-rep(1:input.data$n_trip_mland,input.data$num_trip_mland)
trip.l<-rep(trip.l1,input.data$num_size_mland)
input.data$trip.l<-trip.l
input.data$trip.alk<-trip.alk

if(!is.null(input.data$alk_a_disc)){input.data$alk_a_disc[input.data$alk_a_disc<ageMin]<-ageMin
            input.data$alk_a_disc[input.data$alk_a_disc>ageMax]<-ageMax}
if(!is.null(input.data$alk_a_mland)){input.data$alk_a_mland[input.data$alk_a_mland<ageMin]<-ageMin
            input.data$alk_a_mland[input.data$alk_a_mland>ageMax]<-ageMax}


if(!is.null(input.data$sampsize_disc))input.data$sampsize_disc[is.na(input.data$sampsize_disc)]<-0
if(!is.null(input.data$haulsize_disc))input.data$haulsize_disc[is.na(input.data$haulsize_disc)]<-0
input.data$cens.mu=c(1,30.0,3.4)
input.data$cens.tau=c(1.0,1.0,1.0)
input.data$cens.pri=c(3.4,0.001,0.0001,0.001)
  seas.code<-sim.codecov(input.data$seas)
  gear.code<-sim.codecov(input.data$gear)
  area.code<-sim.codecov(input.data$area,arealist)
  if(is.null(adj))adj<-get.adj(area.code$covlist)
if(is.null(adj)){print("need area adjacency information");return}
aobs = NULL
  acov = list(year=input.data$year,seas=seas.code$cov,
	gear=gear.code$cov,area=area.code$cov)

  neigh = list(num=adj$num,adj=adj$adj) 

  simobj=list(aobs=aobs,acov=acov,neigh=neigh)
  class(simobj) <- "caa.data"
  input.data<<-input.data
 if(is.null(fit)){

mat<-NULL
   for(i in 1:length(acov))mat<-cbind(mat,acov[[i]])
   nsamp.cell<-nrow(unique(mat))
   nparam<-length(unique(acov$year))+length(unique(acov$seas))+
     length(unique(acov$gear))+length(arealist)-3
   if((nsamp.cell-nparam)<20)models$agemodel$Int$cell<-models$lgamodel$Int$cell<-
     models$wglmodel$Int$cell<-models$lgamodel$Slp$cell<-models$wglmodel$Slp$cell<-FALSE
# if(1){  
fit = cost.fit(simobj,input.data,burnin=burnin,numit.inner=thin,numit.outer=nmcmc,constr=1,seed=3421,
               ageMin=ageMin,ageMax=ageMax,nSeason=nseas,
  agemodel=models$agemodel,lgamodel=models$lgamodel,wglmodel=models$wglmodel,
  lgarel="log-linear",model1=T,model2=is.null(wgl.eqn))
nHaul = input.data$n_trip_obs+input.data$n_trip_mland
fit$aobs=list(data=list(nBoats=nHaul))
if(!is.null(wgl.eqn)){

Int <- list(eff = list(const = wgl.eqn$int$const, year = NULL, seas = wgl.eqn$int$seas, gear = wgl.eqn$int$gear,
              area =wgl.eqn$int$area, cell =NULL),
            tau.area = NULL, ar = NULL, tau.cell = NULL, tau.haul = NULL)
Slp <- list(eff = list(const = wgl.eqn$slope$const, year = NULL, seas = wgl.eqn$slope$seas,
  gear =wgl.eqn$slope$gear, area =wgl.eqn$slope$area, cell = NULL),
            tau.area = NULL, ar = NULL, tau.cell = NULL, tau.haul = NULL)
fit<-insert.wgl.param(fit,Int,Slp,tau.obs=1000000) 
}
fit$age$Int<-make.tau(fit$age$Int,models$agemodel$Int)
fit$lga$Int<-make.tau(fit$lga$Int,models$lgamodel$Int)
fit$wgl$Int<-make.tau(fit$wgl$Int,models$wglmodel$Int)

}
predict<-dbeObject.list<-NULL
if(do.predict){
seas.pred<-sim.codecov(prediction.cells$seas,seas.code$covlist)
gear.pred<-sim.codecov(prediction.cells$gear,gear.code$covlist)
area.pred<-sim.codecov(prediction.cells$area,area.code$covlist)

if(length(prediction.cells$seas)==1)if(prediction.cells$seas=="ALL")prediction.cells$seas<-seas.code$covlist
if(length(prediction.cells$gear)==1)if(prediction.cells$gear=="ALL")prediction.cells$gear<-gear.code$covlist
if(length(prediction.cells$area)==1)if(prediction.cells$area=="ALL")prediction.cells$area<-area.code$covlist
newpred<-expand.grid(1,prediction.cells$seas,prediction.cells$gear,prediction.cells$area)

tseas<-code.cov(newpred[,2],seas.code$covlist)
tgear<-code.cov(newpred[,3],gear.code$covlist)
tarea<-code.cov(newpred[,4],area.code$covlist)
tyear<-rep(1,length(tseas))
if(!models$agemodel$Int$area)tarea<-rep(1,length(tseas))
if(!models$agemodel$Int$seas)tseas<-rep(1,length(tseas))
if(!models$agemodel$Int$gear)tgear<-rep(1,length(tseas))
                                        
  landings<-NULL
  if(season.definition=="quarter")land.seas<-cldata@cl$quarter
  if(season.definition=="month")land.seas<-cldata@cl$month
if(season.definition!="quarter"&season.definition!="month"&age.covariates$season)
  {print("invalid season.definition");return} 
  for(i in 1:length(newpred[,1])){ind<-cldata@cl$taxon==species
    if(models$agemodel$Int$seas)ind<-ind&(land.seas==newpred[i,2])
    if(models$agemodel$Int$gear)ind<-ind&(cldata@cl$foCatEu5==newpred[i,3])                           
    if(models$agemodel$Int$area)ind<-ind&(cldata@cl$area==newpred[i,4])
    landings<-c(landings,sum(cldata@cl$landWt[ind]))}
landings<-1000*landings
predict <-  predict.fit.COST(fit,fit$COST.list,tyear,tseas,tgear,tarea,
                             landings,tseas,
          t2.year=NULL,t2.seas=NULL,t2.gear=NULL,t2.area=NULL,
          burnin=0,nMC=100,l.int=l.int,
          par.haulsize=NULL)
dbeObject.list<-mbe2dbe(predict,species)
}
#}
list(fit=fit,predict=predict,dbeObject.list=dbeObject.list)
}

#########################################################################
make.reg.models<-function(age.covariates,weight.covariates){
agemodel<-lgamodel<-wglmodel<-vector("list")
agemodel$Int<-lgamodel$Int<-wglmodel$Int<-lgamodel$Slp<-wglmodel$Slp<-vector("list")
agemodel$Int$year<-F
agemodel$Int$seas<-age.covariates$season
agemodel$Int$gear<-age.covariates$gear
agemodel$Int$area<-age.covariates$area
agemodel$Int$cell<-F
agemodel$Int$haul<-T
agemodel$Hsz<-NULL
lgamodel$Int$year<-F
lgamodel$Int$seas<-age.covariates$season
lgamodel$Int$gear<-age.covariates$gear
lgamodel$Int$area<-age.covariates$area
lgamodel$Int$cell<-F
lgamodel$Int$haul<-F
lgamodel$Hsz<-NULL
lgamodel$Slp$year<-F
lgamodel$Slp$seas<-F
lgamodel$Slp$gear<-F
lgamodel$Slp$area<-F
lgamodel$Slp$cell<-F
lgamodel$Slp$haul<-F
wglmodel$Int$year<-F
wglmodel$Int$seas<-weight.covariates$season
wglmodel$Int$gear<-weight.covariates$gear
wglmodel$Int$area<-weight.covariates$area
wglmodel$Int$cell<-F
wglmodel$Int$haul<-F
wglmodel$Hsz<-NULL
wglmodel$Slp$year<-F
wglmodel$Slp$seas<-F
wglmodel$Slp$gear<-F
wglmodel$Slp$area<-F
wglmodel$Slp$cell<-F
wglmodel$Slp$haul<-F
if((age.covariates$season+age.covariates$gear+age.covariates$area)>1)agemodel$Int$cell<-T
if((age.covariates$season+age.covariates$gear+age.covariates$area)>1)lgamodel$Int$cell<-T
if((weight.covariates$season+weight.covariates$gear+weight.covariates$area)>1)wglmodel$Int$cell<-T
list(agemodel=agemodel,lgamodel=lgamodel,wglmodel=wglmodel)
}
#########################################################################

get.adj<-function(areanames,cheat=F)
{
  newnames<-roman.sub(unique(areanames))
  adj<-num<-NULL
  areas.ok<-unique(unlist(adjacent.areas))
  if(sum(is.element(newnames,areas.ok))==length(newnames)){
    for(a in newnames){
    adj1<-adjacent.areas[[a]]
    adj<-c(adj,intersect(adj1,newnames))
    num<-c(num,length(intersect(adj1,newnames)))}
  newadj<-rep(NA,length(adj))
  for(i in 1:length(unique(areanames)))newadj[adj==((unique(newnames)[i]))]<-i
adj<-newadj
  if(sum(num==0)>0)adj<-num<-NULL}
  else if(cheat==T){
n<-length(areanames)
    num<-rep(2,n)
adj<-c(n,as.vector(matrix(c(2:n,1:(n-1)),nrow=2,byrow=T)),1)
  }
list(adj=adj,num=num,areanames=unique(areanames))
}
#########################################################################

roman.sub<-function(name){
  name<-sub("XIV","27.14.",name)
  name<-sub("VIIII","27.9.",name)
  name<-sub("VIII","27.8.",name)
  name<-sub("VII","27.7.",name)
  name<-sub("VI","27.6.",name)
  name<-sub("XIIII","27.14.",name)
  name<-sub("XIII","27.13.",name)
  name<-sub("XII","27.12.",name)
  name<-sub("XI","27.11.",name)
  name<-sub("IX","27.9.",name)
  name<-sub("IV","27.4.",name)
  name<-sub("V","27.5.",name)
  name<-sub("b1","b.1",name)
  name<-sub("b2","b.2",name)
  name<-tolower(name)
  name
}
    
#########################################################################
sim.codecov<-function(cov,covlist=NULL){
  newcov<-rep(NA,length(cov))
  if(is.null(covlist))covlist<-unique(cov)
  for (i in 1:length(covlist))newcov[cov==covlist[i]]<-i
  list(cov=newcov,covlist=covlist)
}
###################################################################
make.tau<-function(params,model){
area.tau<-cell.tau<-haul.tau<-NULL
taulist<-c(model$area,model$cell,model$haul)
if(sum(taulist)==1){
  if(model$area)area.tau<-params$tau
  if(model$cell)cell.tau<-params$tau
  if(model$haul)haul.tau<-params$tau}
if(sum(taulist)==3){
if(!is.null(dim(params$tau))){
  area.tau<-params$tau[1,]
  cell.tau<-params$tau[2,]
  haul.tau<-params$tau[3,]}
else {
  area.tau<-params$tau[1]
  cell.tau<-params$tau[2]
  haul.tau<-params$tau[3]}
}
if(sum(taulist)==2){
  id<-cumsum(taulist)
if(!is.null(dim(params$tau))){  
 if(model$area)area.tau<-params$tau[1,]
 if(model$cell) cell.tau<-params$tau[id[2],]
 if(model$haul)haul.tau<-params$tau[id[3],]}
  else {
 if(model$area)area.tau<-params$tau[1]
 if(model$cell) cell.tau<-params$tau[id[2]]
 if(model$haul)haul.tau<-params$tau[id[3]]}
}
params$area.tau<-area.tau
params$cell.tau<-cell.tau
params$haul.tau<-haul.tau
params
}
###################################################################
getmode<-function(x){
if(is.factor(x))x<-as.character(x)
mode<-NA
if(sum(!is.na(x))>0){
tab<-as.integer(table(x))
m<-max(tab)
mode<-unique(x)[tab==m]
mode<-mode[1]}
mode
}
###################################################################
paste0 <- function(...) paste(...,sep="")
catn <- function(...) cat(..., fill=T)
printf<-function(...) print(...)

###################################################################
code.cov<-function(cov,covlist){
cov<-as.character(cov)
cov.lev<-unique(cov)
new.cov<-rep(0,length(cov))
for (c in cov.lev)new.cov[cov==c]<-(1:length(covlist))[covlist==c]
new.cov
}
