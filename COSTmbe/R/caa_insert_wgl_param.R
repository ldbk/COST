insert.wgl.param =
  function(fit,
           Int=list(eff=list(const,year=NULL,seas=NULL,gear=NULL,area=NULL,cell=NULL),
                    tau.area=NULL,ar=NULL,tau.cell=NULL,tau.haul=NULL),
           Slp=list(eff=list(const,year=NULL,seas=NULL,gear=NULL,area=NULL,cell=NULL),
                    tau.area=NULL,ar=NULL,tau.cell=NULL,tau.haul=NULL),
           Hsz=NULL,tau.obs=1)
{
  nMCMC = fit$mcmc$nMCMC 
  tau.large = 10000000000000000.00

  model = list()

  if(is.null(Slp$year) && !is.null(Int$year))
    {
      print("This routine so far assumes all effects included in Slp model")
      print("also must be within the intercept model")
      return(1)
    }
  if(is.null(Slp$seas) && !is.null(Int$seas))
    {
      print("This routine so far assumes all effects included in Slp model")
      print("also must be within the intercept model")
      return(1)
    }
  if(is.null(Slp$gear) && !is.null(Int$gear))
    {
      print("This routine so far assumes all effects included in Slp model")
      print("also must be within the intercept model")
      return(1)
    }
  if(is.null(Slp$area) && !is.null(Int$area))
    {
      print("This routine so far assumes all effects included in Slp model")
      print("also must be within the intercept model")
      return(1)
    }

  make.samples.eff = function(obj,nMCMC,nHaul)
    {
      model = list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE)
      samples.eff = rep(obj$eff$const,nMCMC)
      samples.ar = NULL
      samples.tau = NULL
      ncov = 1
      nFac = 1
      fix = 1
      factors = list(const=1)
      #Make just one category of each covariate, possible?
      if(!is.null(obj$eff$year))
        {
          ncov = ncov+1
          samples.eff = rbind(samples.eff,matrix(rep(obj$eff$year,nMCMC),ncol=nMCMC))
          model$year = TRUE
          nFac = c(nFac,length(obj$eff$year))
          factors$year = 1:length(obj$eff$year)
          fix = c(fix,1)
        }
      if(!is.null(obj$eff$seas))
        {
          ncov = ncov+1
          samples.eff = rbind(samples.eff,matrix(rep(obj$eff$seas,nMCMC),ncol=nMCMC))
          model$seas = TRUE
          nFac = c(nFac,length(obj$eff$seas))
          factors$seas=1:length(obj$eff$seas)
          fix = c(fix,1)
        }
      if(!is.null(obj$eff$gear))
        {
          ncov = ncov+1
          samples.eff = rbind(samples.eff,matrix(rep(obj$eff$gear,nMCMC),ncol=nMCMC))
          model$gear = TRUE
          nFac = c(nFac,length(obj$eff$gear))
          factors$gear = 1:length(obj$eff$gear)
          fix = c(fix,1)
        }
      if(!is.null(obj$eff$area))
        {
          ncov = ncov+1
          samples.eff = rbind(samples.eff,matrix(rep(obj$eff$area,nMCMC),ncol=nMCMC))
          model$area = TRUE
          if(is.null(obj$ar))
            {
              print("obj$ar must be present when obj$eff$area is present")
              return(1)
            }
          if(is.null(obj$tau.area))
            {
              print("obj$tau.area must be present when obj$eff$area is present")
              return(1)
            }
          samples.ar = rbind(samples.ar,rep(obj$ar,nMCMC))
          samples.tau = rbind(samples.tau,rep(obj$tau.area,nMCMC))
          nArea = length(obj$eff$area)
          nFac = c(nFac,length(obj$eff$area))
          factors$area = 1:length(obj$eff$area)
          fix = c(fix,0)
          ispat = ncov
        }
      else
        {
          nArea = 1
          ispat = -1
        }
      if(!is.null(obj$eff$cell))
        {
          ncov = ncov+1
          icell = ncov
        }
      else
        {
          icell = -1
        }
      if(!is.null(obj$tau.cell))
        {
          ncov = ncov + 1
          #Assume one cell
          samples.eff = rbind(samples.eff,rep(0,nMCMC))
          samples.tau = rbind(samples.tau,rep(obj$tau.cell,nMCMC))
          model$cell = TRUE
          fix = c(fix,0)
        }
      if(!is.null(obj$tau.haul))
        {
          ncov = ncov + 1
          #Assume one haul
          samples.eff = rbind(samples.eff,rep(0,nMCMC))
          samples.tau = rbind(samples.tau,rep(obj$tau.haul,nMCMC))
          model$cell = TRUE
          fix = c(fix,0)
        }
      cov = expand.grid(factors)
      if(!is.null(nHaul))
        {
          while(dim(cov)[1] < nHaul)
            cov = rbind(cov,cov)
          cov = cov[1:nHaul,]
        }
                
      list(samples.eff=samples.eff,samples.ar=samples.ar,
           samples.tau=samples.tau,model=model,
           mcov=list(cov=as.matrix(cov),ncov=ncov,ispat=ispat,icell=icell,nFac=nFac,fix=fix))
    }
  #eff-int
  foo = make.samples.eff(Int,nMCMC,NULL)
  samples.eff = foo$samples.eff
  samples.ar = foo$samples.ar
  samples.tau = foo$samples.tau
  model$Int = foo$model
  I.mcov = foo$mcov
  nHaul = dim(foo$mcov$cov)[1]

  #eff-slp
  foo = make.samples.eff(Slp,nMCMC,nHaul)
  samples.eff = rbind(samples.eff,foo$samples.eff)
  samples.ar = rbind(samples.ar,foo$samples.ar)
  samples.tau = rbind(samples.tau,foo$samples.tau)
  model$Slp = foo$model
  S.mcov = foo$mcov

  #eff-hsz
  if(!is.null(Hsz))
    {
      foo = make.samples.eff(Hsz,nMCMC,nHaul)
      samples.eff = rbind(samples.eff,foo$samples.eff)
      samples.ar = rbind(samples.ar,foo$samples.ar)
      samples.tau = rbind(samples.tau,foo$samples.tau)
      model$Hsz = foo$model
      H.mcov = foo$mcov
    }
  else
    H.mcov = list(ncov=0,ispat=-1,icell=-1,nFac=0,fix=0,cov=0)
  samples2 = rbind(samples.eff,samples.ar,samples.tau,
    rep(tau.obs,nMCMC),rep(0,nMCMC))
  samples2 = as.vector(samples2)
  wgl.mcmc = conv.mcmc.lin(samples2,nMCMC,model,I.mcov,S.mcov,H.mcov)
  fit$wgl = c(wgl.mcmc,loglik.mean=1,
                mean.inv.lik=1,
                list(model=model,res.wgl=0,
                data=list(nBoats=nHaul,
                          mcov=list(Int=I.mcov,Slp=S.mcov,Hsz=H.mcov))))
  class(fit$wgl) = "fit.wgl.caa"
  fit$mcmc$samples2 = samples2
  fit$mcmc$numpar2 = length(samples2)/nMCMC

  if(1)
    return(fit)
  wgl.fac = data.frame(year=f$year,seas=f$seas,gear=f$gear,area=f$area)
  dimnames(wgl.fac)[[2]] = c("year","seas","gear","area")
  nBoats = dim(f)[1]

  fit$wgl = list(Int=NULL,Slp=NULL)
  
  find.eff = function(obj)
    {
      model =list(year=FALSE,seas=FALSE,gear=FALSE,area=FALSE,cell=FALSE,haul=FALSE)
      eff = list()
      eff$Const = rep(obj$const,nMCMC)
      nFac = 1
      if(!is.null(obj$year))
        {
          eff$year = matrix(obj$year,ncol=nMCMC,nrow=length(obj$year))
          model$year = TRUE
          nFac = c(nFac,length(obj$year))
        }
      if(!is.null(obj$seas))
        {
          eff$seas = matrix(obj$seas,ncol=nMCMC,nrow=length(obj$seas))
          model$seas = TRUE
          nFac = c(nFac,length(obj$seas))
        }
      if(!is.null(obj$gear))
        {
          eff$gear = matrix(obj$gear,ncol=nMCMC,nrow=length(obj$gear))
          model$gear = TRUE
          nFac = c(nFac,length(obj$gear))
        }
      if(!is.null(obj$area))
        {
          eff$area = area=matrix(obj$area,ncol=nMCMC,nrow=length(obj$area))
          model$area = TRUE
          nFac = c(nFac,length(obj$area))
        }
      if(!is.null(obj$cell))
        {
          eff$cell = matrix(obj$cell,ncol=nMCMC,nrow=length(obj$cell))
          model$cell = TRUE
          nFac = c(nFac,length(obj$cell))
        }
      tau = NULL
      if(!is.null(obj$tau.area))
        tau = cbind(tau,rep(obj$tau.area,nMCMC))
      if(!is.null(obj$tau.cell))
        tau = cbind(tau,rep(obj$tau.cell,nMCMC))
      ar = NULL
      if(!is.null(obj$ar))
        ar = rep(obj$ar,nMCMC)
      Int = list(eff=eff,ar=ar,tau=tau)
      list(eff=eff,ar=ar,tau=tau,nFac=nFac,model=model)
    }

  eff.Int = find.eff(Int)
  fit$wgl$Int = list(eff=eff.Int$eff,ar=eff.Int$ar,tau=eff.Int$tau)
  fit$wgl$model = list(Int=eff.Int$model)
  eff.Slp = find.eff(Slp)
  fit$wgl$Slp = list(eff=eff.Slp$eff,ar=eff.Slp$ar,tau=eff.Slp$tau)
  fit$wgl$model$Slp = eff.Slp$model
  if(!is.null(Hsz))
    {
      eff.Hsz = find.eff(Slp)
      fit$wgl$Hsz = list(eff=eff.Hsz$eff,ar=eff.Hsz$ar,tau=eff.Hsz$tau)
      fit$wgl$model$Hsz = eff.Hsz$model
    }

  fit$wgl$tau.obs = rep(tau.obs,nMCMC)
  
  fit$wgl$loglik = rep(1,nMCMC)
  fit$wgl$loglik.mean = 1
  fit$wgl$loglik.mean.inv.lik = 1
  fit$wgl$data = list(nBoats=nBoats)
  w.I.mcov = make.cov(Intmodel,wgl.fac,1,length(num),fit$aobs$data$nBoats)
  w.I.mcov$nFac = nFac

  nFac = c(nFac,0)
  w.S.mcov = make.cov(Slpmodel,wgl.fac,length(num),fit$aobs$data$nBoats)
  w.S.mcov$nFac = nFac

  #eff = as.list(eff)
  tau = NULL
  if(!is.null(Slp.tau.area))
    tau = cbind(tau,rep(Slp.tau.area,nMCMC))
  if(!is.null(Slp.tau.cell))
    tau = cbind(tau,rep(Slp.tau.cell,nMCMC))
  ar = NULL
  if(!is.null(Slp.ar))
    ar = rep(Slp.ar,nMCMC)
  Slp = list(eff=eff,ar=ar,tau=tau)


  obj = list(Int=Int,Slp=Slp,tau.haul=rep(tau.large,nMCMC),
              tau.obs=rep(tau.large,nMCMC),
              loglik=rep(1,nMCMC),loglik.mean=1,
              model=list(Int=Intmodel,Slp=Slpmodel),
              data=list(nBoats=nBoats,mcov=list(Int=w.I.mcov,Slp=w.S.mcov)))


  class(fit$wgl) = "fit.wgl.caa"

  fit$wgl = obj
  fit
}

