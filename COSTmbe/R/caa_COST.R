cost.fit = 
  function(obj,objCOST,
           burnin=0,numit.inner=1,numit.outer=10,constr=1,seed=213421,
           ageMin=0,ageMax=12,nSeason=12,
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
           prior.par=list(age=list(Int=NULL,Hsz=NULL),lga=list(Int=NULL,Slp=NULL,Hsz=NULL),
                          wgl=list(Int=NULL,Slp=NULL,Hsz=NULL)),
           pl.init.g=FALSE,
           model1=TRUE,model2=TRUE)
{
  f.year = obj$acov$year
  f.seas = obj$acov$seas
  f.gear = obj$acov$gear
  f.area = obj$acov$area
  num = obj$neigh$num
  adj = obj$neigh$adj

  age.errors = FALSE
  A2A = NULL
  int.len = 1

  a.vec = c(ageMin:ageMax)
  nAges = length(a.vec)

  nHaul = objCOST$n_trip_obs+objCOST$n_trip_mland
  #cat("nHaul=",nHaul,"\n")

  nFishBoat = c(objCOST$num_alk_disc,objCOST$num_alk_mland)+objCOST$n_int_len+1


  agemodel$Int$haul = TRUE    #Always include haul effect in age int model

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

  mcmc.par = c(burnin,numit.inner,numit.outer)
  prior = list(age=NULL,lga=NULL,wgl=NULL)

  if(model1)
  {
    #Assumes covariates for age and lga models are the same
    if(nHaul>0)
      agelga.fac = data.frame(year=f.year,seas=f.seas,gear=f.gear,area=f.area)
    else
      agelga.fac = NULL

    #Combine covariates for Amigo and length-only data
    dimnames(agelga.fac)[[2]] = c("year","seas","gear","area")

    #Make covariate structure for age model
    a.I.mcov = make.cov(agemodel$Int,agelga.fac,nAges,length(num),nHaul)
    if(!is.null(agemodel$Hsz))
      a.H.mcov = make.cov(agemodel$Hsz,agelga.fac,nAges,length(num),nHaul)
     else
      a.H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0,
                      Sigma.cell=matrix(1),constr.cell=matrix(1),n.constr.cell=0)

    #Make covariate structure for lga intercept model
    l.I.mcov = make.cov(lgamodel$Int,agelga.fac,1,length(num),nHaul)
    l.I.mcov$cov = as.matrix(l.I.mcov$cov)
    #Make covariate structure for lga slope model
    l.S.mcov = make.cov(lgamodel$Slp,agelga.fac,1,length(num),nHaul)
    l.S.mcov$cov = as.matrix(l.S.mcov$cov)
    #Make covariate structure for lga haulsize model
    if(!is.null(lgamodel$Hsz))
      {
        l.H.mcov = make.cov(lgamodel$Hsz,agelga.fac,1,length(num),nHaul)
        l.H.mcov$cov = as.matrix(l.H.mcov$cov)
      }
    else
      l.H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0,
        Sigma.cell=matrix(1),constr.cell=matrix(1),n.constr.cell=0)

    #g function in relation log(length)=intercept+slope*g(age)+noise
    if(lgarel=="log-linear")
      {
        ga = list(model=0,npar=0,init.par=0)
      }
    else if(lgarel=="Schnute-Richards")
      {
        #init.gpar = get.init.gpar.R(totage,totlength,a.vec,pl=pl.init.g)
        init.gpar = c(1.6,1e10,0.06) # c,theta,gamma
        cat(paste("Init g par: c=",init.gpar[1],"theta=",
                  init.gpar[2],"gamma=",init.gpar[3],"\n",sep=" "))
        ga = list(model=1,npar=3,init.par=init.gpar)
      }
    else if(lgarel=="polyn3")
      {
        ga = list(model=2,npar=2,init.par=c(1,0))
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
    numpar1 = c(age.numpar,lga.numpar,ga$npar,0,0)

    #Parameters for cencored lengths 
    cens.model = 1
    cens.par = c(1,50.0,3.5,1000) #k,m,r,Nlim
    objCOST$cens.mu = c(1,15.0,3.4) #k,m,r
    objCOST$cens.tau = c(1.0,1.0,1.0) #k,m,r
    objCOST$cens.pri = c(3.4,0.001,0.0001,0.001)#r priors:mu_mean,mu_prec,tau_a,tau_b
    
    ga$nSeason = nSeason
    ga$nAges = nAges*nSeason
    ga$a.vec = seq(a.vec[1]+1/nSeason,a.vec[nAges]+1,1/nSeason)
    ga$a2Age.vec = seq(0,ga$nAges-1,nSeason)
    ga <<- ga

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
     
    nFish = 1000
    n.miss.sim = nHaul*ga$nAges*(objCOST$n_int_len+1)
    #Put input variables into lists
    common.par <- list(nFish=as.integer(nFish),n_miss_sim=as.integer(n.miss.sim))
    dataList <- list(coastal_cod=0)
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
                    num_adj_area=as.integer(num),adj_area=as.integer(adj))
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
                    g_a_model=as.integer(ga$model),g_a_par_init=as.double(ga$init.par),
                    g_a_ncat=as.integer(ga$nAges),g_a_nSeason=as.integer(ga$nSeason),
                    g_a_avec=as.double(ga$a.vec),
                    g_a_a2Age_vec=as.integer(ga$a2Age.vec),
                    fixed_model=as.integer(!is.null(lgafixed)),
                    fixed_int=lgafixed$Int,fixed_slp=lgafixed$Slp,fixed_tau=lgafixed$tau,
                    fixed_g_a_c=lgafixed$c,fixed_g_a_theta=lgafixed$theta,fixed_g_a_gamma=lgafixed$gamma,
                    cens_model=as.integer(cens.model),cens_par=as.double(cens.par),
                    num_adj_area=as.integer(num),adj_area=as.integer(adj))
    priorList <- list(age_eff_mean=c(prior$age$Int$eff$mean,prior$age$Hsz$eff$mean),
                      age_eff_prec=c(prior$age$Int$eff$prec,prior$age$Hsz$eff$prec),
                      age_prec_par=c(prior$age$Int$prec$par,prior$age$Hsz$prec$par),
                      age_ar=c(prior$age$Int$ar,prior$age$Hsz$ar),
                      lga_eff_mean=c(prior$lga$Int$eff$mean,prior$lga$Slp$eff$mean,prior$lga$Hsz$eff$mean),
                      lga_eff_prec=c(prior$lga$Int$eff$prec,prior$lga$Slp$eff$prec,prior$lga$Hsz$eff$prec),
                      lga_prec_par=c(prior$lga$Int$prec$par,prior$lga$Slp$prec$par,prior$lga$Hsz$prec$par),
                      lga_ar=c(prior$lga$Int$ar,prior$lga$Slp$ar,prior$lga$Hsz$ar))

    numMCMC1 = (numit.outer)*c(age.numpar,lga.numpar,ga$npar)

    #mcmc1 = rnorm(sum(numMCMC1))
    #loglik.mean = rep(1.1,2)
    #res.lga = rep(0.0,length(totage))
    #mod1.mean.inv.lik = rep(0.0,nHaul)
    #age.mean.inv.lik = rep(0.0,nHaul)
    #lga.mean.inv.lik = rep(0.0,nHaul)
    #ppp <- rep(0.0,10)


    #Start MCMC runs
    catn("Start MCMC runs - age and lga model")
    res1 = .Call("caa_main_model1",as.integer(mcmc.par),constr,seed,as.integer(numpar1),nHaul,
                 common.par,dataList,ageList,lgaList,priorList,objCOST,PACKAGE="COSTmbe")

    #print("Finished calling caa_main_model1")
    
    res1$errflag=res1$err
    if(res1$errflag)
      {
        catn("Error fitting age and lga model")
        model1=NULL
        return(NULL)
      }
    else
      catn("Success fitting age and lga model")
    
    #cat("Continuing \n")
    
    #Converting back to missing values
    res1$resid_lga[res1$resid_lga < -1000] = NA
    
    #Picking out relevant output from the C-routine
    mcmc1 = res1$mcmc

    #Pick out parameters corresponding to different parts of the models
    age.mcmc = conv.mcmc.age(mcmc1[1:numMCMC1[1]],numit.outer,nAges,agemodel,
                              a.I.mcov,a.H.mcov)
    lga.mcmc = conv.mcmc.lin(mcmc1[numMCMC1[1]+1:numMCMC1[2]],numit.outer,lgamodel,
                              l.I.mcov,l.S.mcov,l.H.mcov)
    g.a.mcmc = conv.mcmc.g.a(mcmc1[numMCMC1[1]+numMCMC1[2]+1:numMCMC1[3]],numit.outer,
                              lgarel)

    age.list = c(age.mcmc,loglik.mean=res1$loglik.mean[1],
                  mean.inv.lik=list(res1$age.mean.inv.lik),
                  list(model=agemodel,
                       data=list(nBoats=nHaul,nFishBoat=nFishBoat,
                            mcov=list(Int=a.I.mcov,Hsz=a.H.mcov),avec=a.vec,
                            neigh=list(num= num,adj=adj))),
                  ppp=list(res1$ppp))
    class(age.list) = "fit.age.caa"

    lga.list = c(lga.mcmc,loglik.mean=res1$loglik.mean[2],
                  mean.inv.lik=list(res1$lga.mean.inv.lik),
                  mean.inv.lik.mod1=list(res1$mod1.mean.inv.lik),
                  list(model=lgamodel,res.lga=res1$res.lga,
                       data=list(nBoats=nHaul,
                                 mcov=list(Int=l.I.mcov,Slp=l.S.mcov,Hsz=l.H.mcov),
                                 lgarel.nSeason=nSeason,lgarel.avec=ga$a.vec,
                                 lgarel.a2Age.vec=ga$a2Age.vec)))
    lga.list = c(lga.list,list(g.a=g.a.mcmc,lgarel=lgarel))
    class(lga.list) = "fit.lga.caa"
    
    numMCMC_COST = (numit.outer)*objCOST$num_par
    cens.mcmc = conv.mcmc.cens(res1$mcmc_COST[1:numMCMC_COST[1]],numit.outer)
    COST.list = list(num.par=objCOST$num_par,cens.mcmc=cens.mcmc,
                     ntrip.obs=objCOST$n_trip_obs,ntrip.ml=objCOST$n_trip_mland,
                     mcmc=list(samples=res1$mcmc_COST,numpar=objCOST$num_par))

    #rm(agelga.fac,a.I.mcov,a.H.mcov,l.I.mcov,l.S.mcov,l.H.mcov)
    #rm(cens.model,cens.par,quad.hsz)
    #rm(age.ncov,age.ispat,age.icell,lga.ncov,lga.ispat,lga.icell)
    #rm(common.par,dataList,ageList,lgaList,priorList)
    #rm(numMCMC1,age.mcmc,lga.mcmc,g.a.mcmc,res1)
  }
  else
  { #No fitting of age and lga model
    age.list = NULL
    lga.list = NULL
    mcmc1 = NULL
    numpar1 = NULL
    ga=list(npar = NULL)
    COST.list = list(ntrip.obs=objCOST$n_trip_obs,ntrip.ml=objCOST$n_trip_mland)
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
  
    #Number of parameters to be simulated
    wgl.numpar = calc.numpar(1,w.I.mcov)+calc.numpar(1,w.S.mcov)
    if(!is.null(wglmodel$Hsz))
      wgl.numpar = wgl.numpar+calc.numpar(1,w.H.mcov)
    wgl.numpar = wgl.numpar+1   #Precision for observation effect
    wgl.numpar = wgl.numpar+1   #Log-likelihood


    totlength = objCOST$totlength
    replength = objCOST$replength
    totweight = objCOST$totweight
    nFishBoat = objCOST$nFishBoat
    haulsize = NULL
    coastal.cod = 0
    tottype = NULL
  
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

    catn("Start MCMC runs - wgl model")
    res2 = .C("caa_main_model2",
            #MCMC parameters
            as.integer(mcmc.par),           #Burnin,additional iterations, thinning
            as.integer(constr),             #=1 gives sum-constraint, 
                                            #=2 gives treatment constraint
            as.integer(seed),               #Positive integer giving seed number
            as.integer(coastal.cod),         #1 if coastal cod, 0 otherwise
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
            PACKAGE="COSTmbe")
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
    wgl.mcmc = conv.mcmc.lin(mcmc2[1:numMCMC2],numit.outer,wglmodel,w.I.mcov,w.S.mcov,w.H.mcov)
    wgl.list = c(wgl.mcmc,loglik.mean=res2$loglik.mean[1],
                mean.inv.lik=list(res2$wgl.mean.inv.lik),
                list(model=wglmodel,res.wgl=res2$res.wgl,
                data=list(nBoats=length(nFishBoat),
                          mcov=list(Int=w.I.mcov,Slp=w.S.mcov,Hsz=w.H.mcov))))
    class(wgl.list) = "fit.wgl.caa"

    #rm(wgl.fac,w.I.mcov,w.S.mcov,w.H.mcov)
    #rm(wgl.numpar)
    #rm(ncov,ispat,icell)
    #rm(numMCMC2,mcmc2,res.wgl,wgl.mean.inv.lik,res2,wgl.mcmc)
  }
  else
  {
    wgl.list = NULL
    mcmc2 = NULL
    wgl.numpar = NULL
  }

  res = list(age=age.list,lga=lga.list,wgl=wgl.list,
             mcmc=list(samples1=mcmc1,samples2=mcmc2,nMCMC=numit.outer,
                       numpar1=numpar1,numpar2=wgl.numpar),
             COST.list=COST.list)
  class(res) = "fit.caa"

  #rm(age.list,lga.list,wgl.list)
  #rm(f.year,f.seas,f.gear,f.area,num,adj)
  #rm(age.errors,A2A,int.len,nSeason,a.vec,nAges,nHaul,nFishBoat)

  cat("Finished cost.fit\n")
  fit.save <<- res
  res  
}

conv.mcmc.cens = function(mcmc.cens,nMCMC)
{
  mcmc = matrix(mcmc.cens,ncol=nMCMC)
  mu = mcmc[c(1,3,5),]
  tau = mcmc[c(2,4,6),]
  k = mcmc[7,]
  m = mcmc[8,]
  r = mcmc[9:dim(mcmc)[1],]
  cens = list(k=k,m=m,r=r,mu=mu,tau=tau)
  cens
}

predict.fit.COST = 
  function(obj,obj.COST,t.year,t.seas,t.gear,t.area,catch,season,
           t2.year=NULL,t2.seas=NULL,t2.gear=NULL,t2.area=NULL,
           burnin=0,nMC=100,l.int=NULL,
           par.haulsize=NULL)
{
  foo <<- list(obj=obj,t.year=t.year,t.seas=t.seas,t.gear=t.gear,t.area=t.area,catch=catch,
               burnin=burnin,nMC=nMC,l.int=l.int)
  check = predict.caa.check.input(obj,t.year,t.seas,t.gear,t.area,catch,
    t2.year,t2.seas,t2.gear,t2.area,burnin,nMC,l.int,par.haulsize)

  if(!check)
    {
      print("Jumping out of caa.predict")
      return(1)
    }
  oa = obj$age
  ol = obj$lga
  ow = obj$wgl
  if(ol$lgarel=="log-linear")
    {
     lgarelInt = 0
     lgarelnpar = 0
   }
  else if(ol$lgarel=="Schnute-Richards")
   {
     lgarelInt = 1
     lgarelnpar = 2
   }
  else
   {
     print("Unknown age-length relation model")
     return(NULL)
   }
  tfac = make.tot.factors(oa,ol,ow,t.year,t.seas,t.gear,t.area,
    t2.year,t2.seas,t2.gear,t2.area)
  
  if(is.null(l.int))
    {
      N.lint = 1
      l.int =0
    }
  else
    N.lint = length(l.int)

  nAges = length(oa$data$avec)
  nCell = dim(tfac$factors)[1]
  nMCMC = obj$mcmc$nMCMC
  a.vec = oa$data$avec
  lgarel.nSeason = ol$data$lgarel.nSeason
  lgarel.a.vec = ol$data$lgarel.avec
  lgarel.nAges = length(lgarel.a.vec)
  lgarel.a2Age.vec = ol$data$lgarel.a2Age.vec

  totmcmc = rep(0,length(oa$data$avec)*N.lint*(obj$mcmc$nMCMC-burnin))
#  mean.l = rep(0,length(t.year)*(obj$mcmc$nMCMC-burnin))
  mean.l = rep(0,length(oa$data$avec)*(obj$mcmc$nMCMC-burnin))
  mean.w = rep(0,length(oa$data$avec)*(obj$mcmc$nMCMC-burnin))

  ncov = c(oa$data$mcov$Int$ncov,oa$data$mcov$Hsz$ncov,
           ol$data$mcov$Int$ncov,ol$data$mcov$Slp$ncov,ol$data$mcov$Hsz$ncov,
           ow$data$mcov$Int$ncov,ow$data$mcov$Slp$ncov,ow$data$mcov$Hsz$ncov)
  ispat = c(oa$data$mcov$Int$ispat,oa$data$mcov$Hsz$ispat,
            ol$data$mcov$Int$ispat,ol$data$mcov$Slp$ispat,ol$data$mcov$Hsz$ispat,
            ow$data$mcov$Int$ispat,ow$data$mcov$Slp$ispat,ow$data$mcov$Hsz$ispat)
  icell = c(oa$data$mcov$Int$icell,oa$data$mcov$Hsz$icell,
            ol$data$mcov$Int$icell,ol$data$mcov$Slp$icell,ol$data$mcov$Hsz$icell,
            ow$data$mcov$Int$icell,ow$data$mcov$Slp$icell,ow$data$mcov$Hsz$icell)
  if(is.null(oa$model$Hsz$haul))
    a.H.haul = 0
  else
    a.H.haul = oa$model$Hsz$haul
  if(is.null(ol$model$Hsz$haul))
    l.H.haul = 0
  else
    l.H.haul = ol$model$Hsz$haul
  if(is.null(ow$model$Hsz$haul))
    w.H.haul = 0
  else
    w.H.haul = ow$model$Hsz$haul
  inchaul = c(oa$model$Int$haul,a.H.haul,
              ol$model$Int$haul,ol$model$Slp$haul,l.H.haul,
              ow$model$Int$haul,ow$model$Slp$haul,w.H.haul)
  foo = tfac$cell.u.dist
  num.cell.u = c(foo$age$Int$n.u,foo$age$Hsz$n.u,
                 foo$lga$Int$n.u,foo$lga$Slp$n.u,foo$lga$Hsz$n.u,
                 foo$wgl$Int$n.u,foo$wgl$Slp$n.u,foo$wgl$Hsz$n.u)
  num.cell.o = c(foo$age$Int$n.o,foo$age$Hsz$n.o,
                 foo$lga$Int$n.o,foo$lga$Slp$n.o,foo$lga$Hsz$n.o,
                 foo$wgl$Int$n.o,foo$wgl$Slp$n.o,foo$wgl$Hsz$n.o)

  if(is.null(par.haulsize))
    par.haulsize = matrix(0,ncol=1,nrow=1)


  lga.cens.model = 1
  lga.cens = c(1.0,10.0,3.4,1000) #k,m,r,Nlim
  
  catn("Start calling caa_predict")
  
  data.COST = list(
    COST=1,
    numpar=as.integer(obj.COST$mcmc$numpar),
    samples=as.double(obj.COST$mcmc$samples))
  mcmc.samp = list(
    nMCMC=as.integer(nMCMC),burnin=as.integer(burnin),
    samples1=as.double(obj$mcmc$samples1),samples2=as.double(obj$mcmc$samples2),
    numpar1=as.integer(obj$mcmc$numpar1),numpar2=as.integer(obj$mcmc$numpar2))
  common.par = list(
    ncov=as.integer(ncov),ispat=as.integer(ispat),icell=as.integer(icell),
    neigh.num=as.integer(oa$data$neigh$num),
    neigh.adj=as.integer(oa$data$neigh$adj),
    inchaul=as.integer(inchaul),
    coastal.cod=0)
  data.age = list(
    nBoats=as.integer(oa$data$nBoats),nAges=as.integer(nAges),
    avec=as.integer(oa$data$avec),
    season=as.integer(season),
    int.nfac=as.integer(oa$data$mcov$Int$nFac),
    int.fix=as.integer(oa$data$mcov$Int$fix),   
    int.cov=as.integer(t(oa$data$mcov$Int$cov)),
    hsz.nfac=as.integer(oa$data$mcov$Hsz$nFac),
    hsz.fix=as.integer(oa$data$mcov$Hsz$fix),
    hsz.cov=as.integer(t(oa$data$mcov$Hsz$cov)))
  data.lga = list(
    nBoats=as.integer(ol$data$nBoats),  
    int.nfac=as.integer(ol$data$mcov$Int$nFac),
    int.fix=as.integer(ol$data$mcov$Int$fix),
    int.cov=as.integer(t(ol$data$mcov$Int$cov)),
    slp.nfac=as.integer(ol$data$mcov$Slp$nFac),
    slp.fix=as.integer(ol$data$mcov$Slp$fix),
    slp.cov=as.integer(t(ol$data$mcov$Slp$cov)),
    hsz.nfac=as.integer(ol$data$mcov$Hsz$nFac),
    hsz.fix=as.integer(ol$data$mcov$Hsz$fix),
    hsz.cov=as.integer(t(ol$data$mcov$Hsz$cov)),
    lgarelInt=as.integer(lgarelInt),
    ga.ncat=as.integer(lgarel.nAges),
    ga.nSeason=as.integer(lgarel.nSeason),
    ga.avec=as.double(lgarel.a.vec),
    ga.a2Age.vec=as.integer(lgarel.a2Age.vec),
    lga.cens.model=as.integer(lga.cens.model),
    lga.cens=as.double(lga.cens))
  data.wgl = list(
    nBoats=as.integer(ow$data$nBoats),  
    int.nfac=as.integer(ow$data$mcov$Int$nFac),
    int.fix=as.integer(ow$data$mcov$Int$fix),
    int.cov=as.integer(t(ow$data$mcov$Int$cov)),
    slp.nfac=as.integer(ow$data$mcov$Slp$nFac),
    slp.fix=as.integer(ow$data$mcov$Slp$fix),
    slp.cov=as.integer(t(ow$data$mcov$Slp$cov)),
    hsz.nfac=as.integer(ow$data$mcov$Hsz$nFac),
    hsz.fix=as.integer(ow$data$mcov$Hsz$fix),
    hsz.cov=as.integer(t(ow$data$mcov$Hsz$cov)))
  data.catch = list(
    nCell=as.integer(nCell),                  
    nfactors=as.integer(dim(tfac$factors)[2]),
    fac.age.int=as.integer(tfac$fac.age$Int), 
    fac.age.hsz=as.integer(tfac$fac.age$Hsz), 
    fac.lga.int=as.integer(tfac$fac.lga$Int), 
    fac.lga.slp=as.integer(tfac$fac.lga$Slp), 
    fac.lga.hsz=as.integer(tfac$fac.lga$Hsz), 
    fac.wgl.int=as.integer(tfac$fac.wgl$Int), 
    fac.wgl.slp=as.integer(tfac$fac.wgl$Slp), 
    fac.wgl.hsz=as.integer(tfac$fac.wgl$Hsz), 
    factors=as.integer(t(tfac$factors)),      
    catch=as.double(catch))
  dist.cell = list(
    num.cell.o=as.integer(num.cell.o),     #Number of observed cells for each submodel
    num.cell.u=as.integer(num.cell.u),     #Number of unobserved cells for each submodel
    age.int.E=as.double(t(tfac$cell.u.dist$age$Int$E)),
    age.hsz.E=as.double(t(tfac$cell.u.dist$age$Hsz$E)),
    lga.int.E=as.double(t(tfac$cell.u.dist$lga$Int$E)),
    lga.slp.E=as.double(t(tfac$cell.u.dist$lga$Slp$E)),
    lga.hsz.E=as.double(t(tfac$cell.u.dist$lga$Hsz$E)),
    wgl.int.E=as.double(t(tfac$cell.u.dist$wgl$Int$E)),
    wgl.slp.E=as.double(t(tfac$cell.u.dist$wgl$Slp$E)),
    wgl.hsz.E=as.double(t(tfac$cell.u.dist$wgl$Hsz$E)),
    age.int.C=as.double(t(tfac$cell.u.dist$age$Int$C)),
    age.int.nC=as.integer(dim(tfac$cell.u.dist$age$Int$C)[2]),
    age.hsz.C=as.double(t(tfac$cell.u.dist$age$Hsz$C)),
    age.hsz.nC=as.integer(dim(tfac$cell.u.dist$age$Hsz$C)[2]),
    lga.int.C=as.double(t(tfac$cell.u.dist$lga$Int$C)),
    lga.int.nC=as.integer(dim(tfac$cell.u.dist$lga$Int$C)[2]),
    lga.slp.C=as.double(t(tfac$cell.u.dist$lga$Slp$C)),
    lga.slp.nC=as.integer(dim(tfac$cell.u.dist$lga$Slp$C)[2]),
    lga.hsz.C=as.double(t(tfac$cell.u.dist$lga$Hsz$C)),
    lga.hsz.nC=as.integer(dim(tfac$cell.u.dist$lga$Hsz$C)[2]),
    wgl.int.C=as.double(t(tfac$cell.u.dist$wgl$Int$C)),
    wgl.int.nC=as.integer(dim(tfac$cell.u.dist$wgl$Int$C)[2]),
    wgl.slp.C=as.double(t(tfac$cell.u.dist$wgl$Slp$C)),
    wgl.slp.nC=as.integer(dim(tfac$cell.u.dist$wgl$Slp$C)[2]),
    wgl.hsz.C=as.double(t(tfac$cell.u.dist$wgl$Hsz$C)),
    wgl.hsz.nC=as.integer(dim(tfac$cell.u.dist$wgl$Hsz$C)[2]))
 dist.cell<<-dist.cell
  res = .Call("caa_predict",mcmc.samp,common.par,
    data.age,data.lga,data.wgl,data.catch,
    as.double(t(par.haulsize)), #Mean and var for haulsize in each cell
    dist.cell,                  #Cell parameters
    as.integer(N.lint),         #Length intervals
    as.double(l.int),           
    as.integer(nMC),            #Number of Monte Carlo simulations for p_a
    data.COST,
    PACKAGE="COSTmbe")

  res$errflag = res$err
  if(res$errflag)
    {
      catn("Error calling caa_predict")
      return(NULL)
    }
  else
    catn("Finished calling caa_predict")

  if(data.COST$COST)
    {
      totcatch_land = read.mcmc.totcatch(nMCMC-burnin,nAges,N.lint,res$mcmc_totcatch_land)
      totcatch_disc = read.mcmc.totcatch(nMCMC-burnin,nAges,N.lint,res$mcmc_totcatch_disc)
      cov = data.frame(year=tfac$factors[,2],seas=tfac$factors[,3],
                       gear=tfac$factors[,4],area=tfac$factors[,5])
      if(obj.COST$ntrip.obs==0)
        res = list(totcatch.land=totcatch_land,cov=cov,avec=oa$data$avec,
                   l.int=l.int,
                   mean.lga.land=matrix(res$mcmc_mean_l_land,ncol=nMCMC-burnin),
                   mean.wga.land=matrix(res$mcmc_mean_w_land,ncol=nMCMC-burnin),
                   totcatch.disc=NULL,
                   mean.lga.disc=NULL,
                   mean.wga.disc=NULL)
      else 
        res = list(totcatch.land=totcatch_land,cov=cov,avec=oa$data$avec,
                   l.int=l.int,
                   mean.lga.land=matrix(res$mcmc_mean_l_land,ncol=nMCMC-burnin),
                   mean.wga.land=matrix(res$mcmc_mean_w_land,ncol=nMCMC-burnin),
                   totcatch.disc=totcatch_disc,
                   mean.lga.disc=matrix(res$mcmc_mean_l_disc,ncol=nMCMC-burnin),
                   mean.wga.disc=matrix(res$mcmc_mean_w_disc,ncol=nMCMC-burnin))
                   #planded=matrix(res$mcmc_planded,ncol=nMCMC-burnin))
    }
  else
    {
      totcatch = read.mcmc.totcatch(nMCMC-burnin,nAges,N.lint,res$mcmc_totcatch)
      cov = data.frame(year=tfac$factors[,2],seas=tfac$factors[,3],
                       gear=tfac$factors[,4],area=tfac$factors[,5])
      res = list(totcatch=totcatch,cov=cov,avec=oa$data$avec,
                 l.int=l.int,
                 mean.lga=matrix(res$mcmc_mean_l,ncol=nMCMC-burnin),
                 mean.wga=matrix(res$mcmc_mean_w,ncol=nMCMC-burnin))
    }

  class(res) = "caa.predict"
  
  #rm(catch,t.cell.age,t.cell.lga.Int,t.cell.lga.Slp,t.cell.wgl.Int,t.cell.wgl.Slp,
  #   cov,fac.age,fac.lga$Int,fac.wgl$Int,fac.lga$Slp,fac.wgl$Slp)
     
  res
}

