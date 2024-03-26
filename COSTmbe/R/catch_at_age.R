caa.fit =
  function(obj,obj2=NULL,age.errors=FALSE,A2A=NULL,coastal.cod=0,
           ageMin=2,ageMax=13,nSeason=4,int.len=1,
           burnin=0,numit.inner=1,numit.outer=5,constr=1,seed=213421,
            agemodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE),
                         Hsz=NULL),
           lgamodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE,haul=TRUE),
                         Slp=list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE),
                         Hsz=NULL),
           lgarel="log-linear", 
           lgafixed=NULL,
           wglmodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE,haul=TRUE),
                         Slp=list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE),
                         Hsz=NULL),
           wglfixed=NULL,
           class.error=NULL,
           prior.par=list(age=list(Int=NULL,Hsz=NULL),lga=list(Int=NULL,Slp=NULL,Hsz=NULL),
                          wgl=list(Int=NULL,Slp=NULL,Hsz=NULL)),
           pl.init.g=FALSE,
           model1=TRUE,model2=TRUE)
  {
    foo <<- list(obj=obj,ageMin=ageMin,ageMax=ageMax,int.len=int.len,
                 burnin=burnin,numit.inner=numit.inner,numit.outer=numit.outer,
                 constr=constr,seed=seed,
                 agemodel=agemodel,lgamodel=lgamodel,wglmodel=wglmodel,
                 lgarel=lgarel,lgafixed=lgafixed,wglfixed=wglfixed,
                 class.error=class.error,
                 prior.par=prior.par,pl.init.g=pl.init.g,
                 model1=model1,model2=model2)
    #dump(foo)
    if(class(obj)!="caa.data")
      {
        cat(paste("Input object of wrong class",class(obj),"\n"))
        return(NULL)
      }
    if(!is.null(obj2) && class(obj2)!="caa.data")
      {
        cat(paste("Input object2 of wrong class",class(obj2),"\n"))
        return(NULL)
      }
    if(is.null(obj2))
      {
        fit = caa(obj$aobs$age,obj$aobs$length,obj$aobs$weight,
          obj$aobs$realseason,obj$aobs$type,obj$aobs$nFishBoat,
          obj$acov$year,obj$acov$seas,obj$acov$gear,obj$acov$area,
          num=obj$neigh$num,adj=obj$neigh$adj,obj$aobs$haulsize,
          age.errors=age.errors,A2A=A2A,
          coastal.cod=coastal.cod,ageMin=ageMin,ageMax=ageMax,nSeason=nSeason,
          int.len=int.len,replength=obj$aobs$replength,
          burnin=burnin,numit.inner=numit.inner,numit.outer=numit.outer,
          constr=constr,prior.par=prior.par,seed=seed,
          agemodel=agemodel,lgamodel=lgamodel,lgarel=lgarel,lgafixed=lgafixed,
          wglmodel=wglmodel,wglfixed=wglfixed,class.error=class.error,
          pl.init.g=pl.init.g,model1=model1,model2=model2)
      }
    else
      {
        fit.lga = caa(obj$aobs$age,obj$aobs$length,obj$aobs$weight,
          obj$aobs$realseason,obj$aobs$type,obj$aobs$nFishBoat,
          obj$acov$year,obj$acov$seas,obj$acov$gear,obj$acov$area,
          num=obj$neigh$num,adj=obj$neigh$adj,obj$aobs$haulsize,
          age.errors=age.errors,A2A=A2A,
          coastal.cod=coastal.cod,ageMin=ageMin,ageMax=ageMax,nSeason=nSeason,
          int.len=int.len,replength=obj$aobs$replength,
          burnin=burnin,numit.inner=numit.inner,numit.outer=numit.outer,
          constr=constr,prior.par=prior.par,seed=seed,
          agemodel=agemodel,lgamodel=lgamodel,lgarel=lgarel,lgafixed=lgafixed,
          wglmodel=wglmodel,wglfixed=wglfixed,class.error=class.error,
          pl.init.g=pl.init.g,
          model1=model1,model2=FALSE)
        fit.wgl = caa(obj2$aobs$age,obj2$aobs$length,obj2$aobs$weight,
          obj$aobs$realseason,obj$aobs$type,obj2$aobs$nFishBoat,
          obj2$acov$year,obj2$acov$seas,obj2$acov$gear,obj2$acov$area,
          num=obj2$neigh$num,adj=obj2$neigh$adj,obj2$aobs$haulsize,
          age.errors=age.errors,A2A=A2A,
          coastal.cod=coastal.cod,ageMin=ageMin,ageMax=ageMax,nSeason=nSeason,
          int.len=int.len,replength=obj2$aobs$replength,
          burnin=burnin,numit.inner=numit.inner,numit.outer=numit.outer,
          constr=constr,prior.par=prior.par,seed=seed,
          agemodel=agemodel,lgamodel=lgamodel,lgarel=lgarel,lgafixed=lgafixed,
          wglmodel=wglmodel,wglfixed=wglfixed,class.error=class.error,
          pl.init.g=pl.init.g,
          model1=FALSE,model2)
        fit = caa.merge.fit.lga.wgl(fit.lga,fit.wgl)
      }
    fit.save <<- fit
    return(fit)
  }
caa =
  function(totage,totlength,totweight,totseason,tottype,nFishBoat,f.year,f.seas,f.gear,f.area,
           num,adj,haulsize=NULL,
           age.errors=FALSE,A2A=NULL,coastal.cod=0,
           ageMin=2,ageMax=13,nSeason=4,int.len=1,
           replength=NULL,
           burnin=0,numit.inner=1,numit.outer=5,constr=1,seed=213421,
           agemodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE),
                         Hsz=NULL),
           lgamodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE,haul=TRUE),
                         Slp=list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE),
                         Hsz=NULL),
           lgarel="log-linear",lgafixed=NULL,
           wglmodel=list(Int=list(year=TRUE,seas=TRUE,gear=TRUE,area=TRUE,cell=TRUE,haul=TRUE),
                         Slp=list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE),
                         Hsz=FALSE),
           wglfixed=NULL,class.error=NULL,
           prior.par=list(age=list(Int=NULL,Hsz=NULL),lga=list(Int=NULL,Slp=NULL,Hsz=NULL),
                          wgl=list(Int=NULL,Slp=NULL,Hsz=NULL)),
           pl.init.g=FALSE,
           model1=TRUE,model2=TRUE)
{
  agemodel$Int$haul = TRUE    #Always include haul effect in age int model
  if(is.null(replength))
    replength = rep(1,length(totlength))

  #Checking if input is consistent
  check = caa.check.input(totage,totlength,totweight,haulsize,nFishBoat,
    replength,
    f.year,f.seas,f.gear,f.area,
    num,adj,
    age.errors,A2A,ageMin,ageMax,
    burnin,numit.inner,numit.outer,constr,seed,
    agemodel,lgamodel,lgarel,wglmodel,model1,model2)
  if(!check)
    {
      print("Jumping out of caa")
      return(1)
    }
  #rm(check)
  nHaul = length(nFishBoat)

  #Check if any of covariates have only one value,
  #if so this part of the model is removed.
  if(length(unique(f.year))==1)
    {
      agemodel$Int$year = FALSE
      if(!is.null(agemodel$Hsz))
        agemodel$Hsz$year = FALSE
      lgamodel$Int$year = FALSE
      lgamodel$Slp$year = FALSE
      if(!is.null(lgamodel$Hsz))
        lgamodel$Hsz$year = FALSE
      wglmodel$Int$year = FALSE
      wglmodel$Slp$year = FALSE
      if(!is.null(wglmodel$Hsz))
        wglmodel$Hsz$year = FALSE
    }
  if(length(unique(f.seas))==1)
    {
      agemodel$Int$seas = FALSE
      if(!is.null(agemodel$Hsz))
        agemodel$Hsz$seas = FALSE
      lgamodel$Int$seas = FALSE
      lgamodel$Slp$seas = FALSE
      if(!is.null(lgamodel$Hsz))
        lgamodel$Hsz$seas = FALSE
      wglmodel$Int$seas = FALSE
      wglmodel$Slp$seas = FALSE
      if(!is.null(wglmodel$Hsz))
        wglmodel$Hsz$seas = FALSE
    }
  if(length(unique(f.gear))==1)
    {
      agemodel$Int$gear = FALSE
      if(!is.null(agemodel$Hsz))
        agemodel$Hsz$gear = FALSE
      lgamodel$Int$gear = FALSE
      lgamodel$Slp$gear = FALSE
      if(!is.null(lgamodel$Hsz))
        lgamodel$Hsz$gear = FALSE
      wglmodel$Int$gear = FALSE
      wglmodel$Slp$gear = FALSE
      if(!is.null(wglmodel$Hsz))
        wglmodel$Hsz$gear = FALSE
    }
  if(length(unique(f.area))==1)
    {
      agemodel$Int$area = FALSE
      if(!is.null(agemodel$Hsz))
        agemodel$Hsz$area = FALSE
      lgamodel$Int$area = FALSE
      lgamodel$Slp$area = FALSE
      if(!is.null(lgamodel$Hsz))
        lgamodel$Hsz$area = FALSE
      wglmodel$Int$area = FALSE
      wglmodel$Slp$area = FALSE
      if(!is.null(wglmodel$Hsz))
        wglmodel$Hsz$area = FALSE
    }
  #If fixed lga model, then change
  if(!is.null(lgafixed))
    {
      lgamodel$Int$year = FALSE
      lgamodel$Int$seas = FALSE
      lgamodel$Int$gear = FALSE
      lgamodel$Int$area = FALSE
      lgamodel$Int$cell = FALSE
      lgamodel$Int$haul = FALSE
    }
  #If fixed wgl model, then change
  if(!is.null(wglfixed))
    {
      wglmodel$Int$year = FALSE
      wglmodel$Int$seas = FALSE
      wglmodel$Int$gear = FALSE
      wglmodel$Int$area = FALSE
      wglmodel$Int$cell = FALSE
      wglmodel$Int$haul = FALSE
    }
  #Change length and weight to log-scale
  if(!is.null(totlength))
      totlength = (log(totlength+0.5*int.len)+log(totlength-0.5*int.len))/2 
  if(!is.null(totweight))
      totweight = log(totweight)
  #Intervals for lenghts used for age-simulations
  l = totlength;l[l < -100] = NA
  r.len = exp(range(l,na.rm=TRUE))
  #rm(l)
  n.int.len = 1+as.integer((r.len[2]-r.len[1])/int.len)
  int.len.lim = log((r.len[1]-0.5+c(1:n.int.len))*int.len)
  int.len.lim = c(int.len.lim,max(r.len[2]+100.0,99999.9))
  int.len.vec = log(seq(r.len[1],r.len[2],int.len))
  #rm(r.len)
  #Convert NA to -99999
  if(!is.null(totage))
    {
     totage[is.na(totage)] = -99999
     totlength[is.na(totlength)] = -99999.0
     tottype[is.na(tottype)] = -99999
     if(model2)
       totweight[is.na(totweight)] = -99999.0
   }
  mcmc.par = c(burnin,numit.inner,numit.outer)

  prior = list(age=NULL,lga=NULL,wgl=NULL)

  if(model1)
    {
      if(is.null(nFishBoat))
        {
          nHaul = 0
          totage = 0
          totlength = 0.1
          totseason = 0
          tottype = 0
          nFishBoat = 0
        }
      
      #Find structure of missing ages
      struct.miss.ages =
        caa.find.struct.miss.ages(totage,totlength,totweight,
                                  replength,nFishBoat)
      if(is.null(struct.miss.ages))
        return(NULL)
      num.noAge=struct.miss.ages$num.noAge
      start.noAge=struct.miss.ages$start.noAge
      
      a.vec = c(ageMin:ageMax)
      nAges = length(a.vec)
      #cat("nHaul=",nHaul,"\n",sep="")
      if(0) # if not continuous age
        {
          totseason = rep(1,nHaul)
          nSeason = 1	
        }	
      lgarel.nSeason = nSeason
      lgarel.nAges = nAges*nSeason
      lgarel.a.vec = seq(a.vec[1]+1/nSeason,a.vec[nAges]+1,1/nSeason)
      lgarel.a2Age.vec = seq(0,lgarel.nAges-1,nSeason)
      if(coastal.cod)
        {
          a.vec = c(a.vec,a.vec)
          nAges = 2*nAges
          lgarel.a2Age.vec = c(lgarel.a2Age.vec,lgarel.a2Age.vec)
        }
      #catn("season:",totseason)
      #catn("a.vec:",a.vec)
      #catn("lga.a2Age:",lgarel.a2Age.vec)
      #catn("lga.avec:",lgarel.a.vec)
      if(is.null(class.error))
        {
          ptype1.CC = rep(1,nAges)
          ptype1.S  = rep(0,nAges)
          ptype2.CC = rep(1,nAges)
          ptype2.S  = rep(0,nAges)
          ptype4.CC = rep(0,nAges)
          ptype4.S  = rep(1,nAges)
          ptype5.CC = rep(0,nAges)
          ptype5.S  = rep(1,nAges)
        }
      else
        {
          ptype1.CC = class.error$ptype1.CC
          ptype1.S  = class.error$ptype1.S
          ptype2.CC = class.error$ptype2.CC
          ptype2.S  = class.error$ptype2.S
          ptype4.CC = class.error$ptype4.CC
          ptype4.S  = class.error$ptype4.S
          ptype5.CC = class.error$ptype5.CC
          ptype5.S  = class.error$ptype5.S
        }
      
      if(nHaul>0)
        d.age = data.frame(year=f.year,seas=f.seas,gear=f.gear,area=f.area)
      else
        d.age = NULL

      #Combine covariates for Amigo and length-only data
      d.agelen = d.age
      #rm(d.age)
      dimnames(d.agelen)[[2]] = c("year","seas","gear","area")
      #Make covariate structure for age model
      a.I.mcov = make.cov(agemodel$Int,d.agelen,nAges,length(num),nHaul)
      if(!is.null(agemodel$Hsz))
        a.H.mcov = make.cov(agemodel$Hsz,d.agelen,nAges,length(num),nHaul)
      else
        a.H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0,
          Sigma.cell=matrix(1),constr.cell=matrix(1),n.constr.cell=0)

      #Assumes covariates for age and lga models are the same
      lgalen.fac = d.agelen
      #rm(d.agelen)
      dimnames(lgalen.fac)[[2]] = c("year","seas","gear","area")

      #Make covariate structure for lga intercept model
      l.I.mcov = make.cov(lgamodel$Int,lgalen.fac,1,length(num),nHaul)
      l.I.mcov$cov = as.matrix(l.I.mcov$cov)
 
      #Make covariate structure for lga slope model
      l.S.mcov = make.cov(lgamodel$Slp,lgalen.fac,1,length(num),nHaul)
      l.S.mcov$cov = as.matrix(l.S.mcov$cov)
      #Make covariate structure for lga haulsize model
      if(!is.null(lgamodel$Hsz))
        {
          l.H.mcov = make.cov(lgamodel$Hsz,lgalen.fac,1,length(num),nHaul)
          l.H.mcov$cov = as.matrix(l.H.mcov$cov)
        }
      else
        l.H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0,
          Sigma.cell=matrix(1),constr.cell=matrix(1),n.constr.cell=0)
        #g function in relation log(length)=intercept+slope*g(age)+noise
      if(lgarel=="log-linear")
        {
          lgarelInt = 0
          lgarelnpar = 0
          init.gpar = 0
        }
      else if(lgarel=="Schnute-Richards")
        {
          lgarelInt = 1
          lgarelnpar = 3
          init.gpar = get.init.gpar.R(totage,totlength,a.vec,pl=pl.init.g)
          #        init.gpar = c(2,1e8,0.5) # c,theta,gamma
          #        cat(paste("Init g par: c=",init.gpar[1],"theta=",
          #                  init.gpar[2],"gamma=",init.gpar[3],"\n",sep=" "))
        }
      else if(lgarel=="polyn3")
        {
          lgarelInt = 2
          lgarelnpar = 2
          init.gpar = c(1,0)
        }
      else
        return(NULL)  #Unknown relation

      #Calculation of number of parameters in age and lga models
      age.numpar = calc.numpar.noHaul(nAges,a.I.mcov)
      if(!is.null(agemodel$Hsz))
        age.numpar = age.numpar+calc.numpar(nAges,a.H.mcov)
      age.numpar = age.numpar+1   #loglikelihood
      lga.numpar = calc.numpar(1,l.I.mcov)+calc.numpar(1,l.S.mcov)
      if(!is.null(lgamodel$Hsz))
        lga.numpar = lga.numpar+calc.numpar(1,l.H.mcov)
      lga.numpar = lga.numpar+1   #Precision for observation effect
      lga.numpar = lga.numpar+1   #Log-likelihood 
      numpar1 = c(age.numpar,lga.numpar,lgarelnpar,0,0)
      if(coastal.cod) 
        numpar1 = c(age.numpar,lga.numpar,lgarelnpar,lga.numpar,lgarelnpar)
 
      #Parameters for cencored lengths, currently not used
      cens.model = 0
      cens.par = c(1.0,5.0,3.6,1.0)


      #Merge all ncov and all ispat variables to common vectors
      age.ncov = c(a.I.mcov$ncov,a.H.mcov$ncov)
      age.ispat = c(a.I.mcov$ispat,a.H.mcov$ispat)
      age.icell = c(a.I.mcov$icell,a.H.mcov$icell)
      lga.ncov = c(l.I.mcov$ncov,l.S.mcov$ncov,l.H.mcov$ncov)
      lga.ispat = c(l.I.mcov$ispat,l.S.mcov$ispat,l.H.mcov$ispat)
      lga.icell = c(l.I.mcov$icell,l.S.mcov$icell,l.H.mcov$icell)
      if(!is.null(agemodel$Hsz$quad))
        quad.hsz = as.integer(agemodel$Hsz$quad)
      else
        quad.hsz = 0
      prior$age$Int = caa.make.prior.vec(agemodel$Int,a.I.mcov,nAges,prior.par$age$Int)
      if(!is.null(agemodel$Hsz))
        prior$age$Hsz = caa.make.prior.vec(agemodel$Hsz,a.H.mcov,nAges,prior.par$age$Hsz)
      else
        prior$age$Hsz = list(eff=list(mean=NULL,prec=NULL),prec=list(par=NULL),ar=NULL)
      prior$lga$Int = caa.make.prior.vec(lgamodel$Int,l.I.mcov,1,prior.par$lga$Int)
      prior$lga$Slp = caa.make.prior.vec(lgamodel$Slp,l.S.mcov,1,prior.par$lga$Slp)
      if(!is.null(lgamodel$Hsz))
        prior$lga$Hsz = caa.make.prior.vec(lgamodel$Hsz,l.S.mcov,1,prior.par$lga$Hsz)
      else
        prior$lga$Hsz = list(eff=list(mean=NULL,prec=NULL),prec=list(par=NULL),ar=NULL)
      
      nFish = length(totage)
      n.miss.sim = nHaul*lgarel.nAges*(n.int.len+1)
      #Put input variables into lists
      common.par <- list(nFish=as.integer(nFish),n_miss_sim=as.integer(n.miss.sim))
      dataList <- list(totage=as.integer(totage),totlength=as.double(totlength),
                       haulweight=as.double(haulsize),totseason=as.integer(totseason),
                       coastal_cod=as.integer(coastal.cod),tottype=as.integer(tottype),
                       nFishBoat=as.integer(nFishBoat),
                       replength=as.integer(replength),start_noAge=as.integer(start.noAge),
                       num_noAge=as.integer(num.noAge),n_int_len=as.integer(n.int.len),
                       int_len_lim=as.double(int.len.lim),int_len_vec=as.double(int.len.vec))
      ageList <- list(nAges=as.integer(nAges),a_vec=as.integer(a.vec),
                      n_cov=as.integer(age.ncov),
                      ispat=as.integer(age.ispat),icell=as.integer(age.icell),
                      int_nFac=as.integer(a.I.mcov$nFac),int_fix=as.integer(a.I.mcov$fix),
                      int_c_cov=as.integer(t(a.I.mcov$cov)),
                      int_Sigma_cell=as.double(t(a.I.mcov$Sigma.cell)),
                      int_constr_cell=as.double(t(a.I.mcov$constr.cell)),
                      int_nconstr_cell=as.integer(a.I.mcov$n.constr.cell),
                      hsz_nFac=as.integer(a.H.mcov$nFac),hsz_fix=as.integer(a.H.mcov$fix),
                      hsz_c_cov=as.integer(t(a.H.mcov$cov)),
                      hsz_Sigma_cell=as.double(t(a.H.mcov$Sigma.cell)),
                      hsz_constr_cell=as.double(t(a.H.mcov$constr.cell)),
                      hsz_nconstr_cell=as.integer(a.H.mcov$n.constr.cell),
                      age_errors=as.integer(age.errors),A2A=A2A,hsz_quad=as.integer(quad.hsz),
                      num_adj_area=as.integer(num),adj_area=as.integer(adj),
                      ptype1.CC=as.double(ptype1.CC),ptype1.S=as.double(ptype1.S),
                      ptype2.CC=as.double(ptype2.CC),ptype2.S=as.double(ptype2.S),
                      ptype4.CC=as.double(ptype4.CC),ptype4.S=as.double(ptype4.S),
                      ptype5.CC=as.double(ptype5.CC),ptype5.S=as.double(ptype5.S))
      lgaList <- list(n_cov=as.integer(lga.ncov),
                      ispat=as.integer(lga.ispat),icell=as.integer(lga.icell),
                      int_nFac=as.integer(l.I.mcov$nFac),int_fix=as.integer(l.I.mcov$fix),
                      int_c_cov=as.integer(t(l.I.mcov$cov)),
                      int_Sigma_cell=as.double(t(l.I.mcov$Sigma.cell)),
                      int_constr_cell=as.double(t(l.I.mcov$constr.cell)),
                      int_nconstr_cell=as.integer(l.I.mcov$n.constr.cell),
                      slp_nFac=as.integer(l.S.mcov$nFac),slp_fix=as.integer(l.S.mcov$fix),
                      slp_c_cov=as.integer(t(l.S.mcov$cov)),
                      slp_Sigma_cell=as.double(t(l.S.mcov$Sigma.cell)),
                      slp_constr_cell=as.double(t(l.S.mcov$constr.cell)),
                      slp_nconstr_cell=as.integer(l.S.mcov$n.constr.cell),
                      hsz_nFac=as.integer(l.H.mcov$nFac),hsz_fix=as.integer(l.H.mcov$fix),
                      hsz_c_cov=as.integer(t(l.H.mcov$cov)),
                      hsz_Sigma_cell=as.double(t(l.H.mcov$Sigma.cell)),
                      hsz_constr_cell=as.double(t(l.H.mcov$constr.cell)),
                      hsz_nconstr_cell=as.integer(l.H.mcov$n.constr.cell),
                      g_a_model=as.integer(lgarelInt),g_a_par_init=init.gpar,
                      g_a_ncat=as.integer(lgarel.nAges),g_a_nSeason=as.integer(lgarel.nSeason),
                      g_a_avec=as.double(lgarel.a.vec),
                      g_a_a2Age_vec=as.integer(lgarel.a2Age.vec),
                      fixed_model=as.integer(!is.null(lgafixed)),
                      fixed_int=lgafixed$Int,fixed_slp=lgafixed$Slp,fixed_tau=lgafixed$tau,
                      fixed_g_a_c=lgafixed$c,fixed_g_a_theta=lgafixed$theta,fixed_g_a_gamma=lgafixed$gamma,
                      cens_model=as.integer(cens.model),cens_par=cens.par,
                      num_adj_area=as.integer(num),adj_area=as.integer(adj))
      priorList <- list(age_eff_mean=c(prior$age$Int$eff$mean,prior$age$Hsz$eff$mean),
                        age_eff_prec=c(prior$age$Int$eff$prec,prior$age$Hsz$eff$prec),
                        age_prec_par=c(prior$age$Int$prec$par,prior$age$Hsz$prec$par),
                        age_ar=c(prior$age$Int$ar,prior$age$Hsz$ar),
                        lga_eff_mean=c(prior$lga$Int$eff$mean,prior$lga$Slp$eff$mean,prior$lga$Hsz$eff$mean),
                        lga_eff_prec=c(prior$lga$Int$eff$prec,prior$lga$Slp$eff$prec,prior$lga$Hsz$eff$prec),
                        lga_prec_par=c(prior$lga$Int$prec$par,prior$lga$Slp$prec$par,prior$lga$Hsz$prec$par),
                        lga_ar=c(prior$lga$Int$ar,prior$lga$Slp$ar,prior$lga$Hsz$ar))
      data.COST <- list(COST=0)
      data.COST$n_miss_sim = nHaul*lgarel.nAges*(n.int.len+1)
      
      
      numMCMC1 = (numit.outer)*c(age.numpar,lga.numpar,lgarelnpar,0,0)
      if(coastal.cod)
        numMCMC1 = (numit.outer)*c(age.numpar,lga.numpar,lgarelnpar,lga.numpar,lgarelnpar)
      

    
      #mcmc1 = rnorm(sum(numMCMC1))
      #loglik.mean = rep(1.1,2)
      #res.lga = rep(0.0,length(totage))
      #mod1.mean.inv.lik = rep(0.0,nHaul)
      #age.mean.inv.lik = rep(0.0,nHaul)
      #lga.mean.inv.lik = rep(0.0,nHaul)
      #ppp <- rep(0.0,10)

      #Start MCMC runs
      catn("Start MCMC runs")
      res1 = .Call("caa_main_model1",as.integer(mcmc.par),constr,seed,
        as.integer(numpar1),as.integer(nHaul),common.par,dataList,ageList,
        lgaList,priorList,data.COST,PACKAGE="COSTmbe")
      
      #catn("Finished calling caa_main_model1")
      
      res1$errflag=res1$err
      if(res1$errflag)
        {
          catn("Error fitting age and lga model")
          model1=NULL
          return(NULL)
        }
      else
        catn("Success fitting age and lga model")
      #Converting back to missing values
      catn("Continuing \n")
      res1$resid_lga[res1$resid_lga < -1000] = NA
      #Picking out relevant output from the C-routine
      res1 = list(mcmc=res1$mcmc,errflag=res1$errflag,
        loglik.mean=res1$loglik_mean,res.lga=res1$resid_lga,
        mod1.mean.inv.lik=res1$mod1_mean_inv_lik,
        age.mean.inv.lik=res1$age_mean_inv_lik,
        lga.mean.inv.lik=res1$lga_mean_inv_lik,
        ppp=res1$ppp)
      mcmc1 = res1$mcmc
      #Pick out parameters corresponding to different parts of the models
      age.mcmc = conv.mcmc.age(mcmc1[1:numMCMC1[1]],numit.outer,nAges,agemodel,
        a.I.mcov,a.H.mcov)
      lga.mcmc = conv.mcmc.lin(mcmc1[numMCMC1[1]+1:numMCMC1[2]],numit.outer,lgamodel,
        l.I.mcov,l.S.mcov,l.H.mcov)
      g.a.mcmc = conv.mcmc.g.a(mcmc1[numMCMC1[1]+numMCMC1[2]+1:numMCMC1[3]],numit.outer,
        lgarel)
      if(coastal.cod)
        {
          lga.CC.mcmc = conv.mcmc.lin(mcmc1[(numMCMC1[1]+numMCMC1[2]+numMCMC1[3]
            +1):(numMCMC1[1]+numMCMC1[2]+numMCMC1[3]
                 +numMCMC1[4])],
            numit.outer,lgamodel,l.I.mcov,l.S.mcov,l.H.mcov)
          g.a.CC.mcmc = conv.mcmc.g.a(mcmc1[(numMCMC1[1]+numMCMC1[2]+numMCMC1[3]
            +numMCMC1[4]+1):(numMCMC1[1]+numMCMC1[2]
                             +numMCMC1[3]+numMCMC1[4]+numMCMC1[5])],
            numit.outer,lgarel)
        }
      age.list = c(age.mcmc,loglik.mean=res1$loglik.mean[1],
                   mean.inv.lik=list(res1$age.mean.inv.lik),
                   list(model=agemodel,
                   data=list(nBoats=nHaul,nFishBoat=nFishBoat,
                   mcov=list(Int=a.I.mcov,Hsz=a.H.mcov),avec=a.vec,
                   neigh=list(num= num,adj=adj))),
                   ppp=list(res1$ppp))
      #rm(age.mcmc,age.mean.inv.lik,a.I.mcov,a.vec,numMCMC1)
      class(age.list) = "fit.age.caa"
      len = totlength
      len[len < -1000] = NA
      r = range(len,na.rm=TRUE)
      lga.list = c(lga.mcmc,loglik.mean=res1$loglik.mean[2],
                  mean.inv.lik=list(res1$lga.mean.inv.lik),
                  mean.inv.lik.mod1=list(res1$mod1.mean.inv.lik),
                  list(model=lgamodel,res.lga=res1$res.lga,
                       data=list(nBoats=nHaul,
                                 mcov=list(Int=l.I.mcov,Slp=l.S.mcov,Hsz=l.H.mcov),
                                 lgarel.nSeason=lgarel.nSeason,lgarel.avec=lgarel.a.vec,
                                 lgarel.a2Age.vec=lgarel.a2Age.vec),
                       rangelen=r))
      #rm(r,res1,res.lga)
      lga.list = c(lga.list,list(g.a=g.a.mcmc,lgarel=lgarel))
      if(coastal.cod)
        lga.list = c(lga.list,list(lga.CC.mcmc=lga.CC.mcmc,g.a.CC=g.a.CC.mcmc))
      #rm(len,g.a.mcmc,lga.mcmc,lga.mean.inv.lik,l.I.mcov,l.S.mcov)
      class(lga.list) = "fit.lga.caa"
    }#end if(model1)
  else
    { #No fitting of age and lga model
      age.list = NULL
      lga.list = NULL
      mcmc1 = NULL
      age.numpar = NULL
      lga.numpar = NULL
      lgarelnpar = NULL
      numpar1 = NULL
    }
  if(model2)
    {
      #print("Covariates wgl model")
      wgl.fac = data.frame(year=f.year,seas=f.seas,gear=f.gear,area=f.area)
      dimnames(wgl.fac)[[2]] = c("year","seas","gear","area")
      w.I.mcov = make.cov(wglmodel$Int,wgl.fac,1,length(num),nHaul)
      w.I.mcov$cov = as.matrix(w.I.mcov$cov)
      w.S.mcov = make.cov(wglmodel$Slp,wgl.fac,1,length(num),nHaul)
      w.S.mcov$cov = as.matrix(w.S.mcov$cov)
      #Make covariate structure for wgl haulsize model
      if(!is.null(wglmodel$Hsz))
        {
          w.H.mcov = make.cov(wglmodel$Hsz,wgl.fac,1,length(num),nHaul)
          w.H.mcov$cov = as.matrix(w.H.mcov$cov)
        }
      else
        w.H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0,
          Sigma.cell=matrix(1),constr.cell=matrix(1),n.constr.cell=0)
      #rm(wgl.fac)
      #print("Calculate number of parameters to be simulated")
      wgl.numpar = calc.numpar(1,w.I.mcov)+calc.numpar(1,w.S.mcov)
      if(!is.null(wglmodel$Hsz))
        wgl.numpar = wgl.numpar+calc.numpar(1,w.H.mcov)
      wgl.numpar = wgl.numpar+1   #Precision for observation effect
      wgl.numpar = wgl.numpar+1   #Log-likelihood
      
      if(coastal.cod)
        wgl.numpar = c(wgl.numpar,wgl.numpar)
      #print("Start MCMC runs")
      numMCMC2 = (numit.outer)*wgl.numpar
      mcmc2 = rnorm(sum(numMCMC2))
      res.wgl = rep(3.0,length(totlength))
      wgl.mean.inv.lik = rep(0.0,length(nFishBoat))
      ncov = c(w.I.mcov$ncov,w.S.mcov$ncov,w.H.mcov$ncov)
      ispat = c(w.I.mcov$ispat,w.S.mcov$ispat,w.H.mcov$ispat)
      icell = c(w.I.mcov$icell,w.S.mcov$icell,w.H.mcov$icell)
  
      prior$wgl$Int = caa.make.prior.vec(wglmodel$Int,w.I.mcov,1,prior.par$wgl$Int)
      prior$wgl$Slp = caa.make.prior.vec(wglmodel$Slp,w.S.mcov,1,prior.par$wgl$Slp)
      if(!is.null(wglmodel$Hsz))
        prior$wgl$Hsz = caa.make.prior.vec(wglmodel$Hsz,w.S.mcov,1,prior.par$wgl$Hsz)
      else
        prior$wgl$Hsz = list(eff=list(mean=NULL,prec=NULL),prec=list(par=NULL),ar=NULL)
      #print(dim(w.I.mcov$Sigma.cell))
      #print(dim(w.I.mcov$constr.cell))
      #print(w.I.mcov$nFac)
      res2 = .C("caa_main_model2",
            #MCMC parameters
            as.integer(mcmc.par),           #Burnin,additional iterations, thinning
            as.integer(constr),             #=1 gives sum-constraint, 
                                            #=2 gives treatment constraint
            as.integer(seed),               #Positive integer giving seed number
            as.integer(coastal.cod),        #=1 if coastal cod, 0 otherwise
            #Number of boats
            as.integer(length(nFishBoat)),  #Number of boats for wgl data 
            #Length and weight data
            as.double(totlength),           #Vector of lengths
            as.double(totweight),           #Vector of weights
            as.double(haulsize),            #Vector of haul sizes
            as.integer(replength),          #Repetitions of lengths
            as.integer(tottype),            #If cod, type of cod
            as.integer(nFishBoat),          #Number of fish pr boat
            as.integer(ncov),               #Number of explanatory (factor) variables for submodels
            as.integer(ispat),              #Ind for spat fact (-1 if missing spat fact) for submodels
            as.integer(icell),              #Ind for cell fact (-1 if missing cell fact) for submodels
            #Model for weight given length
            as.integer(w.I.mcov$nFac),      #Vector giving number of categories for factors
            as.integer(w.I.mcov$fix),       #Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(w.I.mcov$cov)),    #Factors for intercept in wgl model
            as.double(t(w.I.mcov$Sigma.cell)),
            as.double(t(w.I.mcov$constr.cell)),
            as.integer(w.I.mcov$n.constr.cell),
            as.integer(w.S.mcov$nFac),      #Vector giving number of categories for factors
            as.integer(w.S.mcov$fix),       #Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(w.S.mcov$cov)),    #Factors for intercept in wgl model
            as.double(t(w.S.mcov$Sigma.cell)),
            as.double(t(w.S.mcov$constr.cell)),
            as.integer(w.S.mcov$n.constr.cell),
            as.integer(w.H.mcov$nFac),      #Vector giving number of categories for factors
            as.integer(w.H.mcov$fix),       #Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(w.H.mcov$cov)),    #Factors for intercept in wgl model
            as.double(t(w.H.mcov$Sigma.cell)),
            as.double(t(w.H.mcov$constr.cell)),
            as.integer(w.H.mcov$n.constr.cell),
            as.integer(!is.null(wglfixed)), #Fixed wgl model
                                            #0 is equal to simulating weight
                                            #1 is equal to fixed weight values
            as.double(wglfixed$Int),         #Values of fixed intercept lga parameters
            as.double(wglfixed$Slp),         #Values of fixed slope lga parameters
            as.double(wglfixed$tau),         #Values of fixed precision lga parameters
            #Spatial structure 
            as.integer(num),                #Number of neighbours for each region
            as.integer(adj),                #List of neighbours for each region
            as.integer(wgl.numpar),         #Number of parameters to be simulated in age,lga,wgl model
            as.double(c(prior$wgl$Int$eff$mean,prior$wgl$Slp$eff$mean,prior$wgl$Hsz$eff$mean)),
            as.double(c(prior$wgl$Int$eff$prec,prior$wgl$Slp$eff$prec,prior$wgl$Hsz$eff$prec)),
            as.double(c(prior$wgl$Int$prec$par,prior$wgl$Slp$prec$par,prior$wgl$Hsz$prec$par)),
            as.double(c(prior$wgl$Int$ar,prior$wgl$Slp$ar,prior$wgl$Hsz$ar)),   
            mcmc=as.double(mcmc2),          #Simulated parameters
	    loglik.mean=as.double(2),       #Loglikelihood of mean-parameters
            res.wgl=as.double(res.wgl),     #Residuals from wgl model
            wgl.mean.inv.lik=as.double(wgl.mean.inv.lik),
            errflag=integer(1),
            PACKAGE="caa")
      #rm(mcmc.par,wgl.mean.inv.lik)
      if(res2$errflag)
        {
          catn("Error fitting wgl model")
          return(NULL)
        }
      else
        catn("Success fitting wgl model")
      res2$res.wgl[res2$res.wgl < -1000] = NA
      res2 = list(mcmc=res2$mcmc,errflag=res2$errflag,
               loglik.mean=res2$loglik.mean,
               wgl.mean.inv.lik=res2$wgl.mean.inv.lik,res.wgl=res2$res.wgl)
      mcmc2 = res2$mcmc
      #  wgl.mcmc = mcmc[1:numMCMC2]
      wgl.mcmc = conv.mcmc.lin(mcmc2[1:numMCMC2[1]],numit.outer,wglmodel,w.I.mcov,w.S.mcov,w.H.mcov)
      if(coastal.cod)
        {
          wgl.CC.mcmc = conv.mcmc.lin(mcmc2[(numMCMC2[1]+1):(numMCMC2[1]+numMCMC2[2])],
            numit.outer,wglmodel,w.I.mcov,w.S.mcov,w.H.mcov)
        }
      wgl.list = c(wgl.mcmc,loglik.mean=res2$loglik.mean[1],
                mean.inv.lik=list(res2$wgl.mean.inv.lik),
                list(model=wglmodel,res.wgl=res2$res.wgl,
                data=list(nBoats=length(nFishBoat),
                          mcov=list(Int=w.I.mcov,Slp=w.S.mcov,Hsz=w.H.mcov))))
      if(coastal.cod)
        wgl.list = c(wgl.list,list(wgl.CC.mcmc=wgl.CC.mcmc))
      #rm(res2,res.wgl,wgl.mcmc,w.I.mcov,w.S.mcov,numMCMC2)
      class(wgl.list) = "fit.wgl.caa"
    }#end if(model2)
  else
    {
      wgl.list = NULL
      mcmc2 = NULL
      wgl.numpar = NULL
    }
  res = list(age=age.list,lga=lga.list,wgl=wgl.list,
              mcmc=list(samples1=mcmc1,samples2=mcmc2,nMCMC=numit.outer,
              numpar1=numpar1,
              numpar2=wgl.numpar))
  class(res) = "fit.caa"
  #rm(age.list,age.numpar,lga.list,
  #   lga.numpar,lgarelnpar,mcmc1,mcmc2,
  #   nAges,nHaul,numpar1,wgl.list,wgl.numpar)
  res
}

caa.check.input = function(totage,totlength,totweight,haulsize,nFishBoat,
                            replength,
                            f.year,f.seas,f.gear,f.area,
                            num,adj,
                            age.errors,A2A,ageMin,ageMax,
                            burnin,numit.inner,numit.outer,constr,seed,
                            agemodel,lgamodel,lgarel,wglmodel,model1,model2)
{
  if(!model1 && !model2)
    {
      print("At least one of model1 and model2 needs to be true")
      return(FALSE)
    }
  if(sum(is.na(totlength))>0)
    {
      print("Missing lengths not allowed")
      print("The routine caa.rm.na can be used for removing fish with missing lengths")
      return(FALSE)
    }
  if(model1)
    {
      if(ageMax<ageMin)
        {
          print(c(ageMin,ageMax))
          print("ageMax is less than ageMin")
          return(FALSE)
        }
      if(min(totage,na.rm=TRUE) < ageMin)
        {
          print("ageMin is larger than smallest observed age")
          return(FALSE)
        }
      if(max(totage,na.rm=TRUE) > ageMax)
        {
          print("ageMax is smaller than largest observed age")
          return(FALSE)
        }
      nAges = ageMax-ageMin+1
      # Check input for age and lga model
      if(!is.null(nFishBoat))
        {
          N1 = length(totage)
          N2 = sum(replength)
          N = sum(nFishBoat)
          
          if(length(totlength)!=N1)
            {
              print("Length of totlength do not correspond to length of totage")
              return(FALSE)
            }
          if(N1!=N)
            {
              cat(paste("Number of fish given by totlength and replength",N1,
                        "do not correspond to sum of nFishBoat",N,"\n"))
              return(FALSE)
            }
          if(min(totage,na.rm=TRUE)<ageMin)
            {
              print("Minimum age in totage smaller than ageMin")
              return(FALSE)
            }
          nHaul = length(nFishBoat)
          if(length(f.year)!=nHaul)
            {
              print("Length of f.year do not corresopond with length of nFishBoat")
              return(FALSE)
            }
          if(length(f.seas)!=nHaul)
            {
              print("Length of f.seas do not corresopond with length of nFishBoat")
              return(FALSE)
            }
          if(length(f.gear)!=nHaul)
            {
              print("Length of f.gear do not corresopond with length of nFishBoat")
              return(FALSE)
            }
          if(length(f.area)!=nHaul)
            {
              print("Length of f.area do not corresopond with length of nFishBoat")
              return(FALSE)
            }
        }
      if(age.errors)
        {
          if(dim(A2A)[1]!=nAges || dim(A2A)[2]!=nAges)
            {
              print("Dimension of A2A do not correspond to number of agegroups")
              return(FALSE)
            }
        }
      if(sum(is.na(pmatch(c("year","seas","gear","area","cell"),names(agemodel$Int))))>0)
        {
          print("age int model is missing some of year,seas,gear,area,cell")
          return(FALSE)
        }
      if(!is.null(agemodel$Hsz))
        {
          if(is.null(haulsize))
            {
              print("haulsize sepcified in age model but no haulsize data")
              return(FALSE)
            }
          if(sum(is.na(pmatch(c("year","seas","gear","area","cell"),names(agemodel$Hsz))))>0)
            {
              print("age hsz model is missing some of year,seas,gear,area,cell")
              return(FALSE)
            }
        }
      if(sum(is.na(pmatch(c("Int","Slp"),names(lgamodel))))>0)
        {
          print("lga int model is missing some of year,seas,gear,area,cell,haul")
          return(FALSE)
        }
      if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(lgamodel$Int))))>0)
        {
          print("lga int model is missing some of year,seas,gear,area,cell,haul")
          return(FALSE)
        }
      if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(lgamodel$Slp))))>0)
        {
          print("lga slp model is missing some of year,seas,gear,area,cell,haul")
          return(FALSE)
        }
      if(!is.null(lgamodel$Hsz))
        {
          if(is.null(haulsize))
            {
              print("haulsize sepcified in lga model but no haulsize data")
              return(FALSE)
            }
          if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(lgamodel$Hsz))))>0)
            {
              print("lga hsz model is missing some of year,seas,gear,area,cell,haul")
              return(FALSE)
            }
        }
      if(is.na(pmatch(lgarel,c("log-linear","Schnute-Richards"))))
        {
          print("lgarel needs to be log-linear or Schnute-Richards")
          return(FALSE)
        }
    }
  if(model2)
    {
      if(is.null(nFishBoat))
        {
          print("Need amigo data to fit wgl model")
          return(FALSE)
        }
      N = sum(nFishBoat)
      if(length(totweight)!=length(totlength))
        {
          print("Length of totweight do not correspond to length of totlength")
          return(FALSE)
        }
      nHaul = length(nFishBoat)
      if(length(f.year)!=nHaul)
        {
          print("Length of f.year do not corresopond with length of nFishBoat")
          return(FALSE)
        }
      if(length(f.seas)!=nHaul)
        {
          print("Length of f.seas do not corresopond with length of nFishBoat")
          return(FALSE)
        }
      if(length(f.gear)!=nHaul)
        {
          print("Length of f.gear do not corresopond with length of nFishBoat")
          return(FALSE)
        }
      if(length(f.area)!=nHaul)
        {
          print("Length of f.area do not corresopond with length of nFishBoat")
          return(FALSE)
        }
      if(sum(is.na(pmatch(c("Int","Slp"),names(wglmodel))))>0)
        {
          print("wgl int model is missing some of year,seas,gear,area,cell","haul")
          return(FALSE)
        }
      if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(wglmodel$Int))))>0)
        {
          print("wgl int model is missing some of year,seas,gear,area,cell","haul")
          return(FALSE)
        }
      if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(wglmodel$Slp))))>0)
        {
          print("wgl slp model is missing some of year,seas,gear,area,cell,haul")
          return(FALSE)
        }
      if(!is.null(wglmodel$Hsz))
        {
          if(is.null(haulsize))
            {
              print("haulsize sepcified in wgl model but no haulsize data")
              return(FALSE)
            }
          if(sum(is.na(pmatch(c("year","seas","gear","area","cell","haul"),names(wglmodel$Hsz))))>0)
            {
              print("wgl hsz model is missing some of year,seas,gear,area,cell","haul")
              return(FALSE)
            }
        }
    }
  #Check if neighbor structure is symmetric
  nA = length(num)
  A = matrix(0,ncol=nA,nrow=nA)
  ind = 0
  for(i in 1:nA)
    {
      A[i,adj[ind+1:num[i]]] = 1
      ind = ind + num[i]
    }
  if((max(A-t(A))-min(A-t(A)))>0)
    {
      print("Neighborstructure is not symmetric")
      return(FALSE)
    }
  
  if((round(burnin)-burnin)!=0)
    {
      print("burnin needs to be an integer")
      return(FALSE)
    }
  if((round(numit.inner)-numit.inner)!=0)
    {
      print("numit.inner needs to be an integer")
      return(FALSE)
    }
  if((round(numit.outer)-numit.outer)!=0)
    {
      print("numit.outer needs to be an integer")
      return(FALSE)
    }
  if((round(seed)-seed)!=0)
    {
      print("seed needs to be an integer")
      return(FALSE)
    }
  if(constr!=0 && constr!=1)
    {
      print("constr needs to be either 0 or 1")
      return(FALSE)
    }
  return(TRUE)
}

caa.find.struct.miss.ages =
  function(totage,totlength,totweight,replength,nFishBoat)
{
  #Routine for finding first index of missing age at each haul
  #and also number of missing ages for each haul
  #Check if fish with observed ages are given first and that
  #rest of fish are sorted accoring to increasing length
  ind = 0
  nBoats = length(nFishBoat)
  start.noAge = rep(NA,nBoats)
  num.noAge = rep(NA,nBoats)
  for(h in 1:nBoats)
    {
      r = replength[ind+1:nFishBoat[h]]
      a = totage[ind+1:nFishBoat[h]]
      #a = rep(a,r)
      a[a < -1000] = NA
      num.noAge[h] = sum(is.na(a))
      l = totlength[ind+1:nFishBoat[h]]
      #l = rep(l,r)
      l[l < -1000] = NA
      #N.h = sum(r)
      N.h = length(a)
      if(num.noAge[h]>0)
        {
          i = min(c(1:N.h)[is.na(a)])
          if(num.noAge[h]!=(N.h-i+1))
            {
              print(paste("nFishBoat=",nFishBoat[h]))
              print(ind)
              print(cbind(a,l))
              print(paste("h=",h,"num.noAge=",num.noAge[h],"i=",i))
              print("Aged fish must be stored first inside each haul")
              return(NULL)
            }
          l2 = l[i:N.h]
          if(max(abs(l2[!is.na(l2)]-sort(l2)))>0)
            {
              print("Non-aged fish must be sorted in increasing length")
              return(NULL)
            }
          start.noAge[h] = ind+i-1
        }
      else
        start.noAge[h] = 0
      ind = ind + nFishBoat[h]
    }
  list(num.noAge=num.noAge,start.noAge=start.noAge)
}


make.cov = function(model,factors,ncat,nArea,nHaul)
 {
  cov = rep(1,dim(factors)[1])
  ncov = 1
  nFac = 1
  fix = 1
  if(model$year)
    {
      cov = cbind(cov,factors$year)
      ncov = ncov+1
      nFac = c(nFac,length(unique(factors$year)))
      fix = c(fix,1)
    }
  if(model$seas)
    {
      cov = cbind(cov,factors$seas)
      ncov = ncov+1
      nFac = c(nFac,length(unique(factors$seas)))
      fix = c(fix,1)
    }
  if(model$gear)
    {
      cov = cbind(cov,factors$gear)
      ncov = ncov+1
      nFac = c(nFac,length(unique(factors$gear)))
      fix = c(fix,1)
    }
  if(model$area)
    {
      cov = cbind(cov,factors$area)
      ncov = ncov+1
      ispat = ncov
      nFac = c(nFac,nArea)
      fix = c(fix,0)
    }
  else
     ispat = -1
  if(model$cell)
    {
      res = make.cell.new(factors,model,ncat,nArea)
      cell = res$cell
      cov = cbind(cov,cell)
      ncov = ncov+1
      icell = ncov
      nFac = c(nFac,length(unique(cell)))
      fix = c(fix,0)
    }
  else
    {
      res = list(cell=0,Sigma=matrix(1),constr=matrix(1),n.constr.cell=0)
      icell = -1
    }
  if(model$haul)
    {
      haul = 1:nHaul
      cov = cbind(cov,haul)
      ncov = ncov+1
      nFac = c(nFac,nHaul)
      fix = c(fix,0)
    }
  list(cov=cov,ncov=ncov,ispat=ispat,nFac=nFac,fix=fix,icell=icell,
       Sigma.cell=res$Sigma,constr.cell=res$constr,V.o=res$V.o,
       n.constr.cell=res$n.constr.cell)
}
      
make.cell = function(data,model,nArea)
{
 d = data
 d$year = as.numeric(d$year)
 d$seas = as.numeric(d$seas)
 d$gear = as.numeric(d$gear)
 d$area = as.numeric(d$area)
 if(model$year)
   n.year = length(unique(d$year))
 else
   {
    d$year = 1
    n.year = 1
  }
 if(model$seas)
   n.seas = length(unique(d$seas))
 else
   {
    d$seas = 1
    n.seas = 1
  }
 if(model$gear)
   n.gear = length(unique(d$gear))
 else
   {
    d$gear = 1
    n.gear = 1
  }
 if(model$area)
   n.area = nArea
 else
   {
    d$area = 1
    n.area = 1
  }
 #Calculate cell
 cell = n.area*(n.gear*(n.seas*(d$year-1)+d$seas-1)+d$gear-1)+d$area-1
 #Convert cells to 1,...,nCell
 u.cell = unique(cell)
 r.cell = rank(u.cell)
 n.cell = length(u.cell)
 c.cell = cell
 for(j in 1:n.cell)
   c.cell[cell==u.cell[j]] = r.cell[j]
 c.cell
}


make.cell.new = function(data,model,ncat,nArea,cond.u=FALSE)
{
   #Calculate observed cells
   n = calc.numb.fact(data,model,nArea)
   res = make.cell.number(n,data,data)
   cell = res$cell
   
   #Constraints before conversion
   d = as.numeric(n)
   d = c(ncat,d)
   # Removing those only containing one level
   d = d[d>1]

   ncell = prod(d)
   d.e = c(1,d,1)
   ld = length(d)
   df = prod(d-1)
   nconstr = 0
   for(i in 1:ld)
     nconstr = nconstr + prod(d[-i])
   if(ld==2)
     A = make.constr.matrix2(d)
   else if(ld==3)
     A = make.constr.matrix3(d)
   else if(ld==4)
     A = make.constr.matrix4(d)
   else if(ld==5)
     A = make.constr.matrix5(d)
   else
     print("Not valid number of factors for use of cell effects")
   #A <<- A   
   #Rearrange also constraint matrix (within each age category)
   e = eigen(A%*%t(A),symmetric=TRUE)
   n.o = length(res$ind.o);n.u=length(res$ind.u);n.f=n.o+n.u
   #cat("Number of unobserved cells is",n.u,"\n")
   if(n.u>0)
     {
       A2 = A
       for(a in 1:ncat)
         {
           A2[,(a-1)*n.o+1:n.o] = A[,(a-1)*n.f+res$ind.o]
           A2[,ncat*n.o+(a-1)*n.u+1:n.u] = A[,(a-1)*n.f+res$ind.u]
         }
       A = A2
     }
   #Calculate conditional distribution for observed cells
   #In case the conditional covariance matrix is singular, it
   #must, in order to use the GMRFLib library,
   #be represented as a combination of a non-singular covariance
   #matrix in combination with a constraint matrix
   n.o2 = ncat*n.o
   A.o = A[,1:n.o2]
   V.o = diag(1,n.o2)-t(A.o)%*%solve(A%*%t(A),A.o)
   #Modify V.o to become non-singular
   m = min(eigen(V.o,symmetric=TRUE)$val)
   if(m<0)
     V.o = V.o - 2*diag(rep(m,dim(V.o)[1]))
   #e = eigen(A%*%t(A),symmetric=TRUE)
   #nn = length(e$val)
   #r = sum(abs(e$val)>0.0001)
   e = eigen(V.o,symmetric=TRUE)
   r = sum(abs(e$val)>0.0000001)
   n.constr = n.o2-r
   if(n.constr>0)
     {
       e$val[(r+1):n.o2] = e$val[r]
       W = e$vec%*%diag(e$val)%*%t(e$vec)
       CC = t(e$vec[,(r+1):n.o2])
     }
   else
     {
       W = V.o
       CC = matrix(1)
     }
   n.u2 = ncat*n.u
   if(cond.u & n.u2>0)
     {
       #Calculate conditional distribution for unobserved cells
       #given constraints and given observed cells
       A.u = A[,n.o2+1:n.u2]
       B = t(A.u)%*%ginv(A.u%*%t(A.u))
       E.u = -B%*%A.o
       V.u = diag(1,n.u2)-B%*%A.u
       #Modify V.o to become non-singular
       m = min(eigen(V.u,symmetric=TRUE)$val)
       if(m<0)
         V.u = V.u - 2*diag(rep(m,dim(V.u)[1]))
       #A.svd = svd(A.u)
       #r = sum(abs(A.svd$d)>0.0000001)
       #A.svd$u = A.svd$u[,1:r]
       #A.svd$v = A.svd$v[,1:r]
       #A.svd$d = A.svd$d[1:r]
       #A.u.tilde = t(A.svd$u)%*%A.svd$u%*%diag(A.svd$d)%*%t(A.svd$v)
       #A.o.tilde = t(A.svd$u)%*%A.o
       #B.tilde = t(solve(A.u.tilde%*%t(A.u.tilde),A.u.tilde))
       #E.u.tilde = -B.tilde%*%A.o.tilde
       #V.u.tilde = diag(1,n.u2)-B.tilde%*%A.u.tilde
     }
   else
     {
       E.u = matrix(1)
       V.u = matrix(1)
       #A.u.tilde = NULL
       #E.u.tilde = NULL
       #V.u.tilde = NULL
     }
   
   return(list(cell=cell,Sigma=W,constr=CC,n.constr.cell=n.constr,
               V.o=V.o,E.u=E.u,V.u=V.u,num.cell.u = n.u2))
}



calc.numb.fact = function(d,model,nArea)
  {
    if(model$year)
      n.year = length(unique(as.numeric(d$year)))
    else
      {
        d$year = 1
        n.year = 1
      }
    if(model$seas)
      n.seas = length(unique(as.numeric(d$seas)))
    else
      {
        d$seas = 1
        n.seas = 1
      }
    if(model$gear)
      n.gear = length(unique(as.numeric(d$gear)))
    else
      {
        d$gear = 1
        n.gear = 1
      }
    if(model$area)
      n.area = nArea
    else
      {
        d$area = 1
        n.area = 1
      }
    return(list(n.year=n.year,n.seas=n.seas,n.gear=n.gear,n.area=n.area))
  }

make.cell.number = function(n,d.o,d)
  {
    #Calculate cell numbers
    ncell = prod(as.numeric(n))
    cell = 1:ncell
    cell.o = enumerate.cell(n,d.o)
    u.cell.o = sort(unique(cell.o))
    ind.o = pmatch(u.cell.o,cell)
    if(length(ind.o)<ncell)
      ind.u = cell[-ind.o]
    else
      ind.u = NULL
    #Convert all cells such that observed are given first.
    cell = cell[c(ind.o,ind.u)]

    #Convert cells in d to new number
    cell.d = enumerate.cell(n,d)
    cell.d.c = cell.d
    for(i in 1:length(ind.o))
      cell.d.c[cell.d==ind.o[i]] = i
    for(i in 1:length(ind.u))
      cell.d.c[cell.d==ind.u[i]] = length(ind.o)+i
    return(list(cell=cell.d.c,ind.o=ind.o,ind.u=ind.u))
  }

enumerate.cell = function(n,d)
  {
    pr = 1
    cell = 0
    if(n$n.area>1)
      {
        cell = cell + pr * (d$area-1)
        pr = pr*n$n.area
      }
    if(n$n.gear>1)
      {
        cell = cell + pr * (d$gear-1)
        pr = pr*n$n.gear
      }
    if(n$n.seas>1)
      {
        cell = cell + pr * (d$seas-1)
        pr = pr*n$n.seas
      }
    if(n$n.year>1)
      {
        cell = cell + pr * (d$year-1)
        pr = pr*n$n.year
      }
    cell = cell+1
    cell
  }

make.constr.matrix2 = function(d)
{
  ncell = prod(d)
  df = prod(d-1)
  nconstr = ncell - df
  A = matrix(0,nrow=nconstr,ncol=ncell)
  cc = 0
  ind = 1:d[2]
  for(j in 1:d[1])
    {
      cc = cc+1
      A[cc,(j-1)*d[2]+ind] = 1
    }
  ind = (1:d[1])*d[2]-d[2]+1
  for(j in 1:(d[2]-1))
    {
      cc = cc+1
      A[cc,j-1+ind] = 1
    }
  A
}

make.constr.matrix3 = function(d)
{
  ncell = prod(d)
  df = prod(d-1)
  nconstr = ncell - df
  A = matrix(0,nrow=nconstr,ncol=ncell)
  cc = 0
  ind = 1:d[3]
  for(j in 1:d[1])
  for(k in 1:d[2])
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]+(k-1)*d[3]+ind] = 1
    }
  ind = (1:d[2])*d[3]-d[3]+1
  for(j in 1:d[1])
  for(k in 1:(d[3]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]+k-1+ind] = 1
    }
  ind = (1:d[1])*d[2]*d[3]-d[2]*d[3]+1
  for(j in 1:(d[2]-1))
  for(k in 1:(d[3]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[3]+k-1+ind] = 1
    }
  A
}

make.constr.matrix4 = function(d)
{
  ncell = prod(d)
  df = prod(d-1)
  nconstr = ncell - df
  A = matrix(0,nrow=nconstr,ncol=ncell)
  cc = 0
  ind = 1:d[4]
  for(j in 1:d[1])
  for(k in 1:d[2])
  for(l in 1:d[3])
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]+(k-1)*d[3]*d[4]+(l-1)*d[4]+ind] = 1
    }
  ind = (1:d[3])*d[4]-d[4]+1
  for(j in 1:d[1])
  for(k in 1:d[2])
  for(l in 1:(d[4]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]+(k-1)*d[3]*d[4]+l-1+ind] = 1
    }
  ind = (1:d[2])*d[3]*d[4]-d[3]*d[4]+1
  for(j in 1:d[1])
  for(k in 1:(d[3]-1))
  for(l in 1:(d[4]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]+(k-1)*d[4]+l-1+ind] = 1
    }
  ind = (1:d[1])*d[2]*d[3]*d[4]-d[2]*d[3]*d[4]+1
  for(j in 1:(d[2]-1))
  for(k in 1:(d[3]-1))
  for(l in 1:(d[4]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[3]*d[4]+(k-1)*d[4]+l-1+ind] = 1
    }
  A
}

make.constr.matrix5 = function(d)
{
  ncell = prod(d)
  df = prod(d-1)
  nconstr = ncell - df
  A = matrix(0,nrow=nconstr,ncol=ncell)
  cc = 0
  ind = 1:d[5]
  for(j in 1:d[1])
  for(k in 1:d[2])
  for(l in 1:d[3])
  for(m in 1:d[4])
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]*d[5]+(k-1)*d[3]*d[4]*d[5]+(l-1)*d[4]*d[5]+(m-1)*d[5]+ind] = 1
    }
  ind = (1:d[4])*d[5]-d[5]+1
  for(j in 1:d[1])
  for(k in 1:d[2])
  for(l in 1:d[3])
  for(m in 1:(d[5]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]*d[5]+(k-1)*d[3]*d[4]*d[5]+(l-1)*d[4]*d[5]+m-1+ind] = 1
    }
  ind = (1:d[3])*d[4]*d[5]-d[4]*d[5]+1
  for(j in 1:d[1])
  for(k in 1:d[2]-1)
  for(l in 1:(d[4]-1))
  for(m in 1:(d[5]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]*d[5]+(k-1)*d[3]*d[4]*d[5]+(l-1)*d[5]+m-1+ind] = 1
    }
  ind = (1:d[2])*d[3]*d[4]*d[5]-d[3]*d[4]*d[5]+1
  for(j in 1:d[1])
  for(k in 1:(d[3]-1))
  for(l in 1:(d[4]-1))
  for(m in 1:(d[5]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[2]*d[3]*d[4]*d[5]+(k-1)*d[4]*d[5]+(l-1)*d[5]+m-1+ind] = 1
    }
  ind = (1:d[1])*d[2]*d[3]*d[4]*d[5]-d[2]*d[3]*d[4]*d[5]+1
  for(j in 1:(d[2]-1))
  for(k in 1:(d[3]-1))
  for(l in 1:(d[4]-1))
  for(m in 1:(d[5]-1))
    {
      cc = cc+1
      A[cc,(j-1)*d[3]*d[4]*d[5]+(k-1)*d[4]*d[5]+(l-1)*d[5]+m-1+ind] = 1
    }
  A
}

reduce.matrix = function(d,A)
{
  #Reduce the number of rows of A without changing the rank
  #Can probably be done more elegant...
  more = TRUE
  r = sum(abs(svd(A)$d)>0.00001)
  while(more)
    {
      more = FALSE
      nc = dim(A)[1]
      for(i in nc:1)
        {
          A2 = A[-i,]
          r2 = sum(abs(svd(A2)$d)>0.00001)
          if(r2==r)
            {
              A = A2
              more = TRUE
            }
        }
    }
  A
}

conv.mcmc.age = function(mcmc.age,nMCMC,ncat,agemodel,a.I.mcov,a.H.mcov)
{
  mcmc = matrix(mcmc.age,ncol=nMCMC)
  i = 1
  ind = 0
  haul.save = agemodel$Int$haul
  agemodel$Int$haul = FALSE
  foo = get.eff.age(agemodel$Int,ncat,mcmc,nMCMC,a.I.mcov,ind)
  agemodel$Int$haul = haul.save
  rm(haul.save)
  r = list(Int=list(eff=foo$eff))
  ind = foo$ind
  if(!is.null(agemodel$Hsz))
    {
      foo = get.eff.age(agemodel$Hsz,ncat,mcmc,nMCMC,a.H.mcov,ind)
      r$Hsz = list(eff=foo$eff)
      ind = foo$ind
    }
  if(a.I.mcov$ispat > -1)
    {
      ar = mcmc[ind+1,]
      r$Int = c(r$Int,list(ar=ar))
      ind = ind+1
    }
  if(!is.null(agemodel$Hsz))
    {
      if(a.H.mcov$ispat > -1)
        {
          ar = mcmc[ind+1,]
          r$Hsz = c(r$Hsz,list(ar=ar))
          ind = ind+1
        }
    }
  nran = sum(a.I.mcov$fix==0)
  if(nran>0)
    {
      tau = mcmc[ind+1:nran,]
      if(nran==1)
        tau = t(as.matrix(tau))
      r$Int = c(r$Int,list(tau=tau))
      ind = ind+nran
    }
  if(!is.null(agemodel$Hsz))
    {
      nran = sum(a.H.mcov$fix==0)
      if(nran>0)
        {
          tau = mcmc[ind+1:nran,]
          if(nran==1)
            tau = t(as.matrix(tau))
          r$Hsz = c(r$Hsz,list(tau=tau))
          ind = ind+nran
        }
    }
  loglik = mcmc[ind+1,]
  r = c(r,list(loglik=loglik))
  ind = ind+1
  r
}

get.eff.age = function(model,ncat,mcmc,nMCMC,mcov,ind)
  {
  eff = list(Const=array(NA,c(ncat,nMCMC)))
  i = 1
  if(model$year)
   {
     i = i+1
     eff = c(eff,list(year=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
   }
  if(model$seas)
    {
      i = i+1
      eff = c(eff,list(seas=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
    }
  if(model$gear)
    {
      i = i+1
      eff = c(eff,list(gear=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
    }
  if(model$area)
    {
      i = i+1
      eff = c(eff,list(area=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
    }
  if(model$cell)
    {
      i = i+1
      eff = c(eff,list(cell=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
    }
  if(model$haul)
    {
      i = i+1
      eff = c(eff,list(haul=array(NA,c(ncat,mcov$nFac[i],nMCMC))))
    }
  for(a in 1:ncat)
   {
     i = 1
     eff$Const[a,] = mcmc[ind+1:mcov$nFac[i],]
     ind = ind + mcov$nFac[i]
     if(model$year)
       {
         i = i+1
         eff$year[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
     if(model$seas)
       {
         i = i+1
         eff$seas[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
     if(model$gear)
       {
         i = i+1
         eff$gear[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
     if(model$area)
       {
         i = i+1
         eff$area[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
     if(model$cell)
       {
         i = i+1
         eff$cell[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
     if(model$haul)
       {
         i = i+1
         eff$haul[a,,] = mcmc[ind+1:mcov$nFac[i],]
         ind = ind + mcov$nFac[i]
       }
   }
  list(eff=eff,ind=ind)
}


conv.mcmc.lin = function(mcmc.lin,nMCMC,linmodel,lin.I.mcov,lin.S.mcov,lin.H.mcov)
{
  mcmc = matrix(mcmc.lin,ncol=nMCMC)
  eff = list(Int=NULL,Slp=NULL)
  ind = 0
  foo = get.eff(linmodel$Int,mcmc,lin.I.mcov,ind)
  Int = foo$eff
  ind = foo$ind
  foo = get.eff(linmodel$Slp,mcmc,lin.S.mcov,ind)
  Slp = foo$eff
  ind = foo$ind
  if(!is.null(linmodel$Hsz))
    {
      foo = get.eff(linmodel$Hsz,mcmc,lin.H.mcov,ind)
      Hsz = foo$eff
      ind = foo$ind
      r = list(Int=list(eff=Int),Slp=list(eff=Slp),Hsz=list(eff=Hsz))
    }
  else
    r = list(Int=list(eff=Int),Slp=list(eff=Slp))
  if(lin.I.mcov$ispat > -1)
   {
    ar = mcmc[ind+1,]
    r$Int = c(r$Int,list(ar=ar))
    ind = ind+1
   }
  if(lin.S.mcov$ispat > -1)
   {
    ar = mcmc[ind+1,]
    r$Slp = c(r$Slp,list(ar=ar))
    ind = ind+1
   }
  if(!is.null(linmodel$Hsz))
    {
      if(lin.H.mcov$ispat > -1)
        {
          ar = mcmc[ind+1,]
          r$Hsz = c(r$Slp,list(ar=ar))
          ind = ind+1
        }
    }
  nran = sum(lin.I.mcov$fix==0)
  if(nran>0)
   {
    tau = mcmc[ind+1:nran,]
    if(nran==1)
      tau = t(as.matrix(tau))
    r$Int = c(r$Int,list(tau=tau))
    ind = ind+nran
  }
  nran = sum(lin.S.mcov$fix==0)
  if(nran>0)
   {
    tau = mcmc[ind+1:nran,]
    if(nran==1)
      tau = t(as.matrix(tau))
    r$Slp = c(r$Slp,list(tau=tau))
    ind = ind+nran
  }
  if(!is.null(linmodel$Hsz))
    {
      nran = sum(lin.H.mcov$fix==0)
      if(nran>0)
        {
          tau = mcmc[ind+1:nran,]
          if(nran==1)
            tau = t(as.matrix(tau))
          r$Hsz = c(r$Hsz,list(tau=tau))
          ind = ind+nran
        }
    }
 tau.obs = mcmc[ind+1,]
 r = c(r,list(tau.obs=tau.obs))
 ind = ind+1
 loglik = mcmc[ind+1,]
 r = c(r,list(loglik=loglik))
 ind = ind+1
 r
}

get.eff = function(model,mcmc,lin.mcov,ind)
  {
      eff = list(Const=NULL)
      if(model$year)
        eff = c(eff,list(year=NULL))
      if(model$seas)
        eff = c(eff,list(seas=NULL))
      if(model$gear)
        eff = c(eff,list(gear=NULL))
      if(model$area)
        eff = c(eff,list(area=NULL))
      if(model$cell)
        eff = c(eff,list(cell=NULL))
      if(model$haul)
        eff = c(eff,list(haul=NULL))
      i = 1
      eff$Const = matrix(mcmc[ind+1:lin.mcov$nFac[i],],ncol=dim(mcmc)[2])
      ind = ind + lin.mcov$nFac[i]
      if(model$year)
       {
         i = i+1
         eff$year = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      if(model$seas)
       {
         i = i+1
         eff$seas = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      if(model$gear)
       {
         i = i+1
         eff$gear = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      if(model$area)
       {
         i = i+1
         eff$area = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      if(model$cell)
       {
         i = i+1
         eff$cell = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      if(model$haul)
       {
         i = i+1
         eff$haul = mcmc[ind+1:lin.mcov$nFac[i],]
         ind = ind + lin.mcov$nFac[i]
       }
      list(eff=eff,ind=ind)
    }

conv.mcmc.g.a = function(mcmc.g.a,nMCMC,lgarel)
{
  r = NULL
  if(lgarel=="Schnute-Richards")
    {
      mcmc = matrix(mcmc.g.a,ncol=nMCMC)
      cc = mcmc[1,]
      theta = mcmc[2,]
      gamma = mcmc[3,]
      r = c(r,list(model=lgarel,theta=theta,gamma=gamma,c=cc))
    }
 r
}

calc.numpar = function(ncat,mcov)
  {
    return(ncat*sum(mcov$nFac)+(mcov$ispat!=-1)+sum(mcov$fix==0))
  }
calc.numpar.noHaul = function(ncat,mcov)
  {
    return(ncat*sum(mcov$nFac[-length(mcov$nFac)])+(mcov$ispat!=-1)+sum(mcov$fix==0))
  }
caa.merge.fit.lga.wgl = function(obj1,obj2)
  {
    if(!match("fit.caa",class(obj1)))
      {
        print("obj1 must be of class fit.caa")
        return(1)
      }
    if(!match("fit.caa",class(obj2)))
      {
        print("obj2 must be of class fit.caa")
        return(1)
      }
    if(obj1$mcmc$nMCMC!=obj2$mcmc$nMCMC)
      {
        print("Number of MCMC iterations needs to be equal in order to merge two caa objects")
        return(1)
      }
    fit = obj1
    fit$wgl = obj2$wgl
    fit$mcmc$samples2 = obj2$mcmc$samples2
    fit$mcmc$numpar2 = obj2$mcmc$numpar2
    fit
  } 
