predict.fit.caa = 
  function(obj,t.year,t.seas,t.gear,t.area,catch,season,
           t2.year=NULL,t2.seas=NULL,t2.gear=NULL,t2.area=NULL,
           burnin=0,nMC=100,l.int=NULL,
           par.haulsize=NULL,coastal.cod=0)
{
  foo <<- list(obj=obj,t.year=t.year,t.seas=t.seas,t.gear=t.gear,t.area=t.area,catch=catch,
               burnin=burnin,nMC=nMC,l.int=l.int,coastal.cod=coastal.cod)
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
  if(coastal.cod)
    {
      ncov = c(oa$data$mcov$Int$ncov,oa$data$mcov$Hsz$ncov,
               ol$data$mcov$Int$ncov,ol$data$mcov$Slp$ncov,ol$data$mcov$Hsz$ncov, #lga skrei
               ow$data$mcov$Int$ncov,ow$data$mcov$Slp$ncov,ow$data$mcov$Hsz$ncov, #wgl skrei
               ol$data$mcov$Int$ncov,ol$data$mcov$Slp$ncov,ol$data$mcov$Hsz$ncov, #lga coastal cod
               ow$data$mcov$Int$ncov,ow$data$mcov$Slp$ncov,ow$data$mcov$Hsz$ncov) #wgl coastal cod
      ispat = c(oa$data$mcov$Int$ispat,oa$data$mcov$Hsz$ispat,
                ol$data$mcov$Int$ispat,ol$data$mcov$Slp$ispat,ol$data$mcov$Hsz$ispat, #lga skrei
                ow$data$mcov$Int$ispat,ow$data$mcov$Slp$ispat,ow$data$mcov$Hsz$ispat, #wgl skrei
                ol$data$mcov$Int$ispat,ol$data$mcov$Slp$ispat,ol$data$mcov$Hsz$ispat, #lga coastal cod
                ow$data$mcov$Int$ispat,ow$data$mcov$Slp$ispat,ow$data$mcov$Hsz$ispat) #wgl coastal cod
      icell = c(oa$data$mcov$Int$icell,oa$data$mcov$Hsz$icell,
                ol$data$mcov$Int$icell,ol$data$mcov$Slp$icell,ol$data$mcov$Hsz$icell, #lga skrei
                ow$data$mcov$Int$icell,ow$data$mcov$Slp$icell,ow$data$mcov$Hsz$icell, #wgl skrei
                ol$data$mcov$Int$icell,ol$data$mcov$Slp$icell,ol$data$mcov$Hsz$icell, #lga coastal cod
                ow$data$mcov$Int$icell,ow$data$mcov$Slp$icell,ow$data$mcov$Hsz$icell) #wgl coastal cod
    }
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
  inchaul = as.numeric(c(oa$model$Int$haul,a.H.haul,
                         ol$model$Int$haul,ol$model$Slp$haul,l.H.haul,
                         ow$model$Int$haul,ow$model$Slp$haul,w.H.haul))
  if(coastal.cod)
    inchaul = as.numeric(c(oa$model$Int$haul,a.H.haul,
                           ol$model$Int$haul,ol$model$Slp$haul,l.H.haul, #lga skrei
                           ow$model$Int$haul,ow$model$Slp$haul,w.H.haul, #wgl skrei
                           ol$model$Int$haul,ol$model$Slp$haul,l.H.haul, #lga coastal cod
                           ow$model$Int$haul,ow$model$Slp$haul,w.H.haul))#wgl coastal cod
  foo = tfac$cell.u.dist
  num.cell.u = c(foo$age$Int$n.u,foo$age$Hsz$n.u,
                 foo$lga$Int$n.u,foo$lga$Slp$n.u,foo$lga$Hsz$n.u,
                 foo$wgl$Int$n.u,foo$wgl$Slp$n.u,foo$wgl$Hsz$n.u)
  num.cell.o = c(foo$age$Int$n.o,foo$age$Hsz$n.o,
                 foo$lga$Int$n.o,foo$lga$Slp$n.o,foo$lga$Hsz$n.o,
                 foo$wgl$Int$n.o,foo$wgl$Slp$n.o,foo$wgl$Hsz$n.o)

  if(coastal.cod)
    {
      num.cell.u = c(foo$age$Int$n.u,foo$age$Hsz$n.u,
                     foo$lga$Int$n.u,foo$lga$Slp$n.u,foo$lga$Hsz$n.u,
                     foo$wgl$Int$n.u,foo$wgl$Slp$n.u,foo$wgl$Hsz$n.u,
                     foo$lga$Int$n.u,foo$lga$Slp$n.u,foo$lga$Hsz$n.u,
                     foo$wgl$Int$n.u,foo$wgl$Slp$n.u,foo$wgl$Hsz$n.u)
      num.cell.o = c(foo$age$Int$n.o,foo$age$Hsz$n.o,
                     foo$lga$Int$n.o,foo$lga$Slp$n.o,foo$lga$Hsz$n.o,
                     foo$wgl$Int$n.o,foo$wgl$Slp$n.o,foo$wgl$Hsz$n.o,
                     foo$lga$Int$n.o,foo$lga$Slp$n.o,foo$lga$Hsz$n.o,
                     foo$wgl$Int$n.o,foo$wgl$Slp$n.o,foo$wgl$Hsz$n.o)
    }
  
  lga.cens.model = 0
  lga.cens = c(1.0,10.0,3.4,1000) #k,m,r,Nlim
  if(is.null(par.haulsize))
    par.haulsize = matrix(0,ncol=1,nrow=1)


  catn("Start calling caa_predict")
  mcmc.samp = list(
    nMCMC=as.integer(nMCMC),burnin=as.integer(burnin),
    samples1=as.double(obj$mcmc$samples1),samples2=as.double(obj$mcmc$samples2),
    numpar1=as.integer(obj$mcmc$numpar1),numpar2=as.integer(obj$mcmc$numpar2))
  common.par = list(
    ncov=as.integer(ncov),ispat=as.integer(ispat),icell=as.integer(icell),
    neigh.num=as.integer(oa$data$neigh$num),
    neigh.adj=as.integer(oa$data$neigh$adj),
    inchaul=as.integer(inchaul),
    coastal.cod=as.integer(coastal.cod))
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
      num.cell.o=as.integer(num.cell.o),                    #Number of observed cells for each submodel
      num.cell.u=as.integer(num.cell.u),                    #Number of unobserved cells for each submodel
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
  data.COST = list(COST=0)

  res = .Call("caa_predict",mcmc.samp,common.par,
    data.age,data.lga,data.wgl,data.catch,
    as.double(t(par.haulsize)), #Mean and var for haulsize in each cell
    dist.cell,                                            #Cell parameters
    as.integer(N.lint),
    as.double(l.int),  #Length intervals   
    as.integer(nMC), #Number of Monte Carlo simulations for p_a
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
  totcatch = read.mcmc.totcatch(nMCMC-burnin,nAges,N.lint,res$mcmc_totcatch)
  cov = data.frame(year=tfac$factors[,2],seas=tfac$factors[,3],
                    gear=tfac$factors[,4],area=tfac$factors[,5])
  res = list(totcatch=totcatch,cov=cov,avec=oa$data$avec,
              l.int=l.int,
              mean.lga=matrix(res$mcmc_mean_l,ncol=nMCMC-burnin),
              mean.wga=matrix(res$mcmc_mean_w,ncol=nMCMC-burnin))
  class(res) = "caa.predict"
  #rm(catch,t.cell.age,t.cell.lga.Int,t.cell.lga.Slp,t.cell.wgl.Int,t.cell.wgl.Slp,
  #   cov,fac.age,fac.lga$Int,fac.wgl$Int,fac.lga$Slp,fac.wgl$Slp)
     
  res
}

predict.caa.check.input =
  function(obj,t.year,t.seas,t.gear,t.area,catch,
           t2.year,t2.seas,t2.gear,t2.area,burnin,nMC,l.int,par.haulsize)
  {
    Hsz = FALSE
    if(!(class(obj)=="fit.caa"))
      {
        print("First input variable to predict.fit.caa must be of class fit.caa")
        return(FALSE)
      }
    nCell = length(catch)
    if(length(t.year)!=nCell)
      {
        print("Length of t.year do not match length of catch")
        return(FALSE)
      }
    if(length(t.seas)!=nCell)
      {
        print("Length of t.seas do not match length of catch")
        return(FALSE)
      }
    if(length(t.gear)!=nCell)
      {
        print("Length of t.gear do not match length of catch")
        return(FALSE)
      }
    if(length(t.area)!=nCell)
      {
        print("Length of t.area do not match length of catch")
        return(FALSE)
      }
    if(!check.fixed.in.model(obj$age$model$Int,obj$age$data$mcov$Int$cov,dim(obj$age$Int$eff$area)[2],
                             t.year,t.seas,t.gear,t.area,"age-int model"))
      return(FALSE)
    if(!is.null(obj$age$model$Hsz))
      {
        Hsz = TRUE
        if(!check.fixed.in.model(obj$age$model$Hsz,obj$age$data$mcov$Hsz$cov,dim(obj$age$Hsz$eff$area)[2],
                             t.year,t.seas,t.gear,t.area,"age-hsz model"))
          return(FALSE)
      }
    if(is.null(dim(obj$lga$Int$eff$area)))nArea<-length(obj$lga$Int$eff$area)
    else nArea<-dim(obj$lga$Int$eff$area)[1]
    if(!check.fixed.in.model(obj$lga$model$Int,obj$lga$data$mcov$Int$cov,nArea,
                             t.year,t.seas,t.gear,t.area,"lga-int model"))
      return(FALSE)
    if(!check.fixed.in.model(obj$lga$model$Slp,obj$lga$data$mcov$Slp$cov,dim(obj$lga$Slp$eff$area)[1],
                             t.year,t.seas,t.gear,t.area,"lga-slp model"))
      return(FALSE)
    if(!is.null(obj$lga$model$Hsz))
      {
        Hsz = TRUE
    if(is.null(dim(obj$lga$Hsz$eff$area)))nArea<-length(obj$lga$Hsz$eff$area)
    else nArea<-dim(obj$lga$Hsz$eff$area)[1]
        if(!check.fixed.in.model(obj$lga$model$Hsz,obj$lga$data$mcov$Hsz$cov,nArea,
                             t.year,t.seas,t.gear,t.area,"lga-hsz model"))
          return(FALSE)
      }
    if(is.null(dim(obj$wgl$Int$eff$area)))nArea<-length(obj$wgl$Int$eff$area)
    else nArea<-dim(obj$wgl$Int$eff$area)[1]
    if(!check.fixed.in.model(obj$wgl$model$Int,obj$wgl$data$mcov$Int$cov,nArea,
                             t.year,t.seas,t.gear,t.area,"wgl-int model"))
      return(FALSE)
    if(!check.fixed.in.model(obj$wgl$model$Slp,obj$wgl$data$mcov$Slp$cov,dim(obj$wgl$Slp$eff$area)[1],
                             t.year,t.seas,t.gear,t.area,"wgl-slp model"))
      return(FALSE)
    if(!is.null(obj$wgl$model$Hsz))
      {
        Hsz = TRUE
    if(is.null(dim(obj$wgl$Hsz$eff$area)))nArea<-length(obj$wgl$Hsz$eff$area)
    else nArea<-dim(obj$wgl$Hsz$eff$area)[1]
        if(!check.fixed.in.model(obj$wgl$model$Hsz,obj$wgl$data$mcov$Hsz$cov,nArea,
                             t.year,t.seas,t.gear,t.area,"wgl-hsz model"))
          return(FALSE)
      }
    if(Hsz)
      {
        if(is.null(par.haulsize))
          {
            print("parameters for haulsize missing")
            return(FALSE)
          }
        if((dim(par.haulsize)[1]!=length(catch)) || (dim(par.haulsize)[2]!=2))
          {
            print("par.haulsize of wrong dimension")
            return(FALSE)
          }
      }
    if(!is.null(t2.seas))
      {
        if(is.null(t2.year) || is.null(t2.gear) || is.null(t2.area))
          {
            print("If t2.seas is present, so must t2.year, t2.gear, t2.area")
            return(FALSE)
          }
        if(length(t2.year)!=nCell)
          {
            print("Length of t2.year do not match length of catch")
            return(FALSE)
          }
        if(length(t2.seas)!=nCell)
          {
            print("Length of t2.seas do not match length of catch")
            return(FALSE)
          }
        if(length(t2.gear)!=nCell)
          {
            print("Length of t2.gear do not match length of catch")
            return(FALSE)
          }
        if(length(t2.area)!=nCell)
          {
            print("Length of t2.area do not match length of catch")
            return(FALSE)
          }
      }
    return(TRUE)
  }
check.fixed.in.model = function(model,fit.cov,nArea,
                                 t.year,t.seas,t.gear,t.area,name.model)
  {
    i = 1
    if(model$year)
      {
        i = i+1
        u.acov = unique(fit.cov[,i])
        u.tcov = unique(t.year)
        if(sum(is.na(match(u.tcov,u.acov)))>0)
          {
            print(u.acov)
            print(u.tcov)
            print(paste("Value in t.year not found in fitted",name.model))
            return(FALSE)
          }
      }
    if(model$seas)
      {
        i = i+1
        u.acov = unique(fit.cov[,i])
        u.tcov = unique(t.seas)
        if(sum(is.na(match(u.tcov,u.acov)))>0)
          {
            print(u.acov)
            print(u.tcov)
            print(paste("Value in t.seas not found in fitted",name.model))
            return(FALSE)
          }
      }
    if(model$gear)
      {
        i = i+1
        u.acov = unique(fit.cov[,i])
        u.tcov = unique(t.gear)
        if(sum(is.na(match(u.tcov,u.acov)))>0)
          {
            print(u.acov)
            print(u.tcov)
            print(paste("Value in t.gear not found in fitted",name.model))
            return(FALSE)
          }
      }
    if(model$area)
      {
        i = i+1
        u.acov = 1:nArea
        u.tcov = unique(t.area)
        if(sum(is.na(match(u.tcov,u.acov)))>0)
          {
            print(u.acov)
            print(u.tcov)
            print(paste("Value in t.area not found in fitted",name.model))
            return(FALSE)
          }
      }
    return(TRUE)
  }


read.mcmc.totcatch = function(Nmcmc,nAges,Nint,mcmc)
{
 totcatch = array(mcmc,c(Nint,nAges,Nmcmc))
 totcatch
}


matching.cell = function(model,ncat,xcov,t.year,t.seas,t.gear,t.area,nArea,ind.cell,add.prev=FALSE)
{
  if(model$cell)
   {
     ind.main = c(1:4)[as.numeric(model[1:4])]
     nh = dim(xcov)[1]
     k = 1
     if(model$year)
       {
         k = k+1
         year = xcov[,k]
       }
     else
       year = rep(1,nh)
     if(model$seas)
       {
         k = k+1
         seas = xcov[,k]
       }
     else
       seas = rep(1,nh)
     if(model$gear)
       {
         k = k+1
         gear = xcov[,k]
       }
     else
       gear = rep(1,nh)
     if(model$area)
       {
         k = k+1
         area = xcov[,k]
       }
     else
       area = rep(1,nh)
     d.o = data.frame(year=year,seas=seas,gear=gear,area=area)
     k = k+1
     cell.o = xcov[,k]
     n.o = max(cell.o)
     n = calc.numb.fact(d.o,model,nArea)
     d = data.frame(year=t.year,seas=t.seas,gear=t.gear,area=t.area)
     #Calculate cell numbers for all cells observed in catch data
     t.cell = make.cell.number(n,d.o,d)$cell
     #Indices for unobserved cells, translated to start at 1
     ind.u = unique(t.cell[t.cell>n.o])-n.o
     n.u = length(ind.u)
     #if age model, take into account cells at different age-groups as well
     ind.u.a = ind.u
     if(ncat>1)
       {
         for(a in 2:ncat)
           ind.u.a = c(ind.u.a,(a-1)*n.u+ind.u)
       }
     n.u.a = length(ind.u.a)
     #cat("Number of unique observed cells within each age-group=",n.o,"\n")
     #cat("Number of unique unobserved cells within each age-group=",n.u,"\n")
     #Calculate conditional distribution for all unobserved cells given observed cells
     #Reduce to only those unobserved cells in catch data
     if(n.u>0)
       {
         res = make.cell.new(d.o,model,ncat,nArea,cond.u=TRUE)
         E.u = res$E.u[ind.u.a,]
         V.u = res$V.u[ind.u.a,ind.u.a]
         #Find C.u such that V.u=C.u%*%t(C.u) utilzing that V.u can be singular
         V.u.eig = eigen(V.u,symmetric=TRUE)
         r = sum(V.u.eig$val> (dim(V.u)[1]*V.u.eig$val[1]*1e-10))
         C.u = V.u.eig$vec[,1:r]%*%diag(sqrt(V.u.eig$val[1:r]))
         print("##########################################")
         print(r)
         print("##########################################")
         #Translate cell numbers such that unobserved cells in catch data
         #are numbered n.o+1,...,n.o+n.u
         t.cell[t.cell>n.o] = n.o + match(t.cell[t.cell>n.o],unique(t.cell[t.cell>n.o]))
       }
     else
       {
         ind.u = 0
         E.u=matrix(1);V.u=matrix(1);C.u=matrix(1)
       }
     fac = make.fac.ind(model,ind.cell,add.prev)
   }
  else
   {
     
     t.cell = rep(-1,length(t.year))
     fac = make.fac.ind(model,0,add.prev)
     ind.u = 0
     n.u = 0
     n.o = 0
     E.u=matrix(1);V.u=matrix(1);C.u=matrix(1)
   }
  list(t.cell=t.cell,fac=fac,
       cell.u.dist=list(n.u=n.u*ncat,n.o=n.o*ncat,ind.u=ind.u,
                        E=E.u,C=C.u))
}
    
make.fac.ind = function(mod,ind.cell=0,add=FALSE)
  {
    if(add)
      add.prev = 4
    else
      add.prev = 0
    fac = 1   # Constant term
    if(mod$year)
       fac = c(fac,add.prev+2)
    if(mod$seas)
       fac = c(fac,add.prev+3)
    if(mod$gear)
       fac = c(fac,add.prev+4)
    if(mod$area)
       fac = c(fac,add.prev+5)
    if(mod$cell)
       fac = c(fac,6+add.prev+ind.cell)
    fac
  }

make.tot.factors = function(oa,ol,ow,t.year,t.seas,t.gear,t.area,t2.year,t2.seas,t2.gear,t2.area)
  {
    if(is.null(t2.year))
    tot.factors = cbind(1,t.year,t.seas,t.gear,t.area)
  else
    tot.factors = cbind(1,t.year,t.seas,t.gear,t.area,t2.year,t2.seas,t2.gear,t2.area)
  foo = list(n.u=0,n.o=0,E=matrix(1),C=matrix(1))
  cell.u.dist =list(age=list(Int=foo,Hsz=foo),
                    lga=list(Int=foo,Slp=foo,Hsz=foo),
                    wgl=list(Int=foo,Slp=foo,Hsz=foo))
  #Find matching cells 
  ind.cell = 0
  ncat = dim(oa$Int$eff$Const)[1]
  #Age-Int model
  fac.age = list()
  nArea = length(oa$data$neigh$num)
  foo = matching.cell(oa$model$Int,ncat,oa$data$mcov$Int$cov,t.year,t.seas,t.gear,t.area,
                      nArea,ind.cell,FALSE)
  fac.age$Int = foo$fac
  if(oa$model$Int$cell)
    {
      tot.factors = cbind(tot.factors,foo$t.cell)
      ind.cell = ind.cell +1
      cell.u.dist$age$Int = foo$cell.u.dist
    }
  #Age-Hsz model
  if(!is.null(oa$model$Hsz))
    {
      foo = matching.cell(oa$model$Hsz,ncat,oa$data$mcov$Hsz$cov,t.year,t.seas,t.gear,t.area,
                          nArea,ind.cell,FALSE)
      fac.age$Hsz = foo$fac
      if(oa$model$Hsz$cell)
        {
          tot.factors = cbind(tot.factors,foo$t.cell)
          ind.cell = ind.cell +1
          cell.u.dist$age$Hsz = foo$cell.u.dist
        }
    }
  else
    {
      oa$data$mcov$Hsz = list(nFac=0,fix=0,cov=0,ncov=0,ispat=-1)
      fac.age$Hsz = -1
    }
    
  #Lga-Int model
  fac.lga = list()
  #nArea = length(ol$data$neigh$num)
  foo = matching.cell(ol$model$Int,1,ol$data$mcov$Int$cov,t.year,t.seas,t.gear,t.area,
                      nArea,ind.cell,FALSE)
  fac.lga$Int = foo$fac
  if(ol$model$Int$cell)
    {
      tot.factors = cbind(tot.factors,foo$t.cell)
      ind.cell = ind.cell +1
      cell.u.dist$lga$Int = foo$cell.u.dist
    }
  #Lga-Slp model
  foo = matching.cell(ol$model$Slp,1,ol$data$mcov$Slp$cov,t.year,t.seas,t.gear,t.area,
                      nArea,ind.cell,FALSE)
  fac.lga$Slp = foo$fac
  if(ol$model$Slp$cell)
    {
      tot.factors = cbind(tot.factors,foo$t.cell)
      ind.cell = ind.cell +1
      cell.u.dist$lga$Slp = foo$cell.u.dist
    }
  if(!is.null(ol$model$Hsz))
    {
      foo = matching.cell(ol$model$Hsz,1,ol$data$mcov$Hsz$cov,t.year,t.seas,t.gear,t.area,
                          nArea,ind.cell,FALSE)
      fac.lga$Hsz = foo$fac
      if(ol$model$Hsz$cell)
        {
          tot.factors = cbind(tot.factors,foo$t.cell)
          ind.cell = ind.cell +1
          cell.u.dist$lga$Hsz = foo$cell.u.dist
        }
    }
  else
    {
      ol$data$mcov$Hsz = list(nFac=0,fix=0,cov=0,ncov=0,ispat=-1)
      fac.lga$Hsz = -1
    }

                                        #Wgl-Int model
  fac.wgl = list()
  #nArea = length(ow$data$neigh$num)
  if(is.null(t2.year))
    foo = matching.cell(ow$model$Int,1,ow$data$mcov$Int$cov,t.year,t.seas,t.gear,t.area,
                        nArea,ind.cell,FALSE)
  else
    foo = matching.cell(ow$model$Int,1,ow$data$mcov$Int$cov,t2.year,t2.seas,t2.gear,t2.area,
                        nArea,ind.cell,TRUE)
  fac.wgl$Int = foo$fac
  if(ow$model$Int$cell)
    {
      tot.factors = cbind(tot.factors,foo$t.cell)
      ind.cell = ind.cell +1
      cell.u.dist$wgl$Int = foo$cell.u.dist
    }
  #Wgl-Slp model
  if(is.null(t2.year))
    foo = matching.cell(ow$model$Slp,1,ow$data$mcov$Slp$cov,t.year,t.seas,t.gear,t.area,
                        nArea,ind.cell,FALSE)
  else
    foo = matching.cell(ow$model$Slp,1,ow$data$mcov$Slp$cov,t2.year,t2.seas,t2.gear,t2.area,
                        nArea,ind.cell,TRUE)
  fac.wgl$Slp = foo$fac
  if(ow$model$Slp$cell)
    {
      tot.factors = cbind(tot.factors,foo$t.cell)
      ind.cell = ind.cell +1
      cell.u.dist$wgl$Slp = foo$cell.u.dist
    }
  if(!is.null(ow$model$Hsz))
    {
      if(is.null(t2.year))
        foo = matching.cell(ow$model$Hsz,1,ow$data$mcov$Hsz$cov,t.year,t.seas,t.gear,t.area,
                            nArea,ind.cell,FALSE)
      else
        foo = matching.cell(ow$model$Hsz,1,ow$data$mcov$Hsz$cov,t2.year,t2.seas,t2.gear,t2.area,
                            nArea,ind.cell,TRUE)
      fac.wgl$Hsz = foo$fac
      if(ow$model$Hsz$cell)
        {
          tot.factors = cbind(tot.factors,foo$t.cell)
          ind.cell = ind.cell +1
          cell.u.dist$wgl$Hsz = foo$cell.u.dist
        }
    }
  else
    {
      ow$data$mcov$Hsz = list(nFac=0,fix=0,cov=0,ncov=0,ispat=-1)
      fac.wgl$Hsz = -1
    }
  #Including haul as covariate, but missing
  tot.factors = cbind(tot.factors,-1)
  ind.haul = dim(tot.factors)[2]
  fac.age$Int = c(fac.age$Int,ind.haul)  # Haul always included in age-Int model
  if(ol$model$Int$haul)
    fac.lga$Int = c(fac.lga$Int,ind.haul)
  if(ol$model$Slp$haul)
    fac.lga$Slp = c(fac.lga$Slp,ind.haul)
  if(!is.null(ol$model$Hsz))
    {
      if(ol$model$Hsz$haul)
        fac.lga$Hsz = c(fac.lga$Hsz,ind.haul)
    }
  if(ow$model$Int$haul)
    fac.wgl$Int = c(fac.wgl$Int,ind.haul)
  if(ow$model$Slp$haul)
    fac.wgl$Slp = c(fac.wgl$Slp,ind.haul)
  if(!is.null(ow$model$Hsz))
    {
      if(ow$model$Hsz$haul)
        fac.wgl$Hsz = c(fac.wgl$Hsz,ind.haul)
    }
  list(factors=tot.factors,fac.age=fac.age,fac.lga=fac.lga,fac.wgl=fac.wgl,
       cell.u.dist=cell.u.dist)
}
