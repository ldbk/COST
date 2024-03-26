caa.make.prior.vec = function(model,mcov,ncat,prior.par)
  {
    prior = list(eff=list(mean=NULL,prec=NULL),prec=list(par=NULL),ar=NULL)

    if(is.null(prior.par))
      {
        prior$eff$mean=rep(0,ncat)
        prior$eff$prec=0.0001
        ind=1
        if(model$year)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,rep(0,mcov$nFac[ind]*ncat))
            prior$eff$prec=c(prior$eff$prec,0.1)
          }
        if(model$seas)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,rep(0,mcov$nFac[ind]*ncat))
            prior$eff$prec=c(prior$eff$prec,0.1)
          }
        if(model$gear)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,rep(0,mcov$nFac[ind]*ncat))
            prior$eff$prec=c(prior$eff$prec,0.1)
          }
        if(model$area)
          {
            ind=ind+1
            prior$prec$par=c(0.01,0.01)
            prior$ar = c(1,1)
          }
        if(model$cell)
          {
            ind=ind+1
            prior$prec$par=c(prior$prec$par,0.01,0.01)
          }
        #Haul effect, always included
        prior$prec$par=c(prior$prec$par,0.01,0.01)
        prior$ar = c(1,1)
      }
    else
      {
        prior$eff$mean=prior.par$mu.const
        prior$eff$prec=1/prior.par$sd.const^2
        ind=1
        if(model$year)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,prior.par$mu.year)
            prior$eff$prec=c(prior$eff$prec,1/prior.par$sd.year^2)
          }
        if(model$seas)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,prior.par$mu.seas)
            prior$eff$prec=c(prior$eff$prec,1/prior.par$sd.seas^2)
          }
        if(model$gear)
          {
            ind=ind+1
            prior$eff$mean=c(prior$eff$mean,prior.par$mu.gear)
            prior$eff$prec=c(prior$eff$prec,1/prior.par$sd.gear^2)
          }
        if(model$area)
          {
            ind=ind+1
            prior$prec$par=prior.par$area.gam.par
            prior$ar = prior.par$ar
          }
        if(model$cell)
          {
            ind=ind+1
            prior$prec$par=c(prior$prec$par,prior.par$cell.gam.par)
          }
        if(model$haul)
          {
            ind=ind+1
            prior$prec$par=c(prior$prec$par,prior.par$haul.gam.par)
          }
      }
     
    prior
  }


marginal.caa <- 
  function(data,fit,age.errors=FALSE,A2A=NULL,
           ageMin=2,ageMax=13,burnin=0)
{
  oa <- fit$age
  ol <- fit$lga
  ow <- fit$wgl
  if(ol$lgarel=="log-linear")
    {
     lgarelInt <- 0
     lgarelnpar <- 0
   }
  else if(ol$lgarel=="Schnute-Richards")
   {
     lgarelInt <- 1
     lgarelnpar <- 2
   }
  else
   {
     print("Unknown age-length relation model")
     return(NULL)
   }

  n.length.only <- length(Length)       #Number of length-only fish
  n.length.a.only <- length(ageLength)  #Number of age-stratified-by-length fish

  nAges <- length(oa$data$avec)
  nMCMC <- fit$mcmc$nMCMC
  loglik = c(0,0)
  logprior = c(0,0)
  logpost = c(0,0)
  ncov = c(oa$data$mcov$Int$ncov,oa$data$mcov$Hsz$ncov,
           ol$data$mcov$Int$ncov,ol$data$mcov$Slp$ncov,ol$data$mcov$Hsz$ncov,
           ow$data$mcov$Int$ncov,ow$data$mcov$Slp$ncov,ow$data$mcov$Hsz$ncov)
  ispat = c(oa$data$mcov$Int$ispat,oa$data$mcov$Hsz$ispat,
            ol$data$mcov$Int$ispat,ol$data$mcov$Slp$ispat,ol$data$mcov$Hsz$ispat,
            ow$data$mcov$Int$ispat,ow$data$mcov$Slp$ispat,ow$data$mcov$Hsz$ispat)
  res <- .C("caa_marg_dens",
            #MCMC parameters
            as.integer(nMCMC),             #1 Number of MCMC iterations
            as.integer(burnin),
            as.double(fit$mcmc$samples1),  #2 MCMC-samples
            as.double(fit$mcmc$samples2),  #3 MCMC-samples
            as.integer(fit$mcmc$numpar1),  #4
            as.integer(fit$mcmc$numpar2),  #5
            #Age data
            as.integer(oa$data$nBoats),     #6 Number of boats for age data
            as.integer(totage),             #Vector of ages
            as.double(totlength),           #Vector of lengths
            as.double(totweight),           #Vector of weights
            as.integer(nFishBoat),          #Number of fish pr boat
            as.integer(start.noAge),        #Index at which no-Aged fish start for each haul
            as.integer(num.noAge),          #Number of no-Aged fish for each haul
            as.integer(ncov),               #Number of explanatory (factor) variables
            as.integer(ispat),              #Index for spatial factor (-1 if spatial factor)
            as.integer(nAges),               #8 Number of age categories
            as.integer(oa$data$avec),        #9 Vector giving ages (on original scale)
            as.integer(oa$data$mcov$Int$nFac),   #11 Vector giving number of categories for factors
            as.integer(oa$data$mcov$Int$fix),    #13 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(oa$data$mcov$Int$cov)), #14 Factors for age model
            as.integer(oa$data$mcov$Hsz$nFac),   #11 Vector giving number of categories for factors
            as.integer(oa$data$mcov$Hsz$fix),    #13 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(oa$data$mcov$Hsz$cov)), #14 Factors for age model
            #Length given age data
            as.integer(ol$data$nBoats),   #15 Number of boats for lga data
            as.integer(ol$data$mcov$Int$nFac),#18 Vector giving number of categories for factors
            as.integer(ol$data$mcov$Int$fix),  #20 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ol$data$mcov$Int$cov)),#21 Factors for intercept in lga model
            as.integer(ol$data$mcov$Slp$nFac),  #23 Vector giving number of categories for factors
            as.integer(ol$data$mcov$Slp$fix),   #25 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ol$data$mcov$Slp$cov)),#26 Factors for intercept in lga model
            as.integer(ol$data$mcov$Hsz$nFac),  #23 Vector giving number of categories for factors
            as.integer(ol$data$mcov$Hsz$fix),   #25 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ol$data$mcov$Hsz$cov)),#26 Factors for intercept in lga model
            as.integer(lgarelInt),          #27 Model for age-length relationship
                                            #0 is equal to log-linear model
                                            #1 is equal to Schnute-Richards model
            #Weight given length data
            as.integer(ow$data$nBoats),     #28 Number of boats for wgl data
            as.integer(ow$data$mcov$Int$nFac),      #30 Vector giving number of categories for factors
            as.integer(ow$data$mcov$Int$fix),       #32 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ow$data$mcov$Int$cov)),    #33 Factors for intercept in wgl model
            as.integer(ow$data$mcov$Slp$nFac),      #35 Vector giving number of categories for factors
            as.integer(ow$data$mcov$Slp$fix),       #37 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ow$data$mcov$Slp$cov)),    #38 Factors for intercept in wgl model
            as.integer(ow$data$mcov$Hsz$nFac),      #35 Vector giving number of categories for factors
            as.integer(ow$data$mcov$Hsz$fix),       #37 Vector =1 if factor is fixed, 0 otherwise
            as.integer(t(ow$data$mcov$Hsz$cov)),    #38 Factors for intercept in wgl model
            #Spatial structure 
            as.integer(oa$data$neigh$num),          #39 Number of neighbours for each region
            as.integer(oa$data$neigh$adj),          #40 List of neighbours for each region
            #Output
            loglik=as.double(loglik),               #Log-likelihoods for models 1 and 2
            logprior=as.double(logprior),           #Log-priors for models 1 and 2
            logposterior=as.double(logpost),        #Log-posteriors for models 1 and 2
            errflag=integer(1),
            PACKAGE="COSTmbe")
  if(res$errflag)
    {
      print("Error calling caa_marg_dens")
      return(NULL)
    }
  else
      print("Finished calling caa_predict")
  list(loglik=res$loglik,logprior=res$logprior,logpost=res$logpost)
}

DIC <- function(object) UseMethod("DIC")

DIC.fit.caa <- function(object)
  {
    tab <- NULL
    nam <- NULL
    if(!is.null(object$age))
     {
      tab <- rbind(tab,DIC(object$age))
      nam <- c(nam,"Age model")
     }
    if(!is.null(object$lga))
     {
      tab <- rbind(tab,DIC(object$lga))
      nam <- c(nam,"Lga model")
     }
    if(!is.null(object$wgl))
     {
      tab <- rbind(tab,DIC(object$wgl))
      nam <- c(nam,"Wgl model")
     }
    tab <- rbind(tab,colSums(tab))
    nam <- c(nam,"Total")
    row.names(tab) <- nam
    tab
  }
DIC.default <- function(object)
{
    Dbar <- mean(-2*object$loglik)
    Dhat <- -2*object$loglik.mean
    pD <- Dbar-Dhat
    DIC <- Dbar+pD
    tab <- c(Dbar,Dhat,pD,DIC)
    names(tab) <- c("Dbar","Dhat","pD","DIC")
    tab
}
BF.caa <- function(fit1,fit2)
{
  #Age and LGA model
  x <- min(fit1$age$loglik+fit1$lga$loglik)
  B.lga <- mean(1/(exp(fit2$age$loglik+fit2$lga$loglik-x)))/
           mean(1/(exp(fit1$age$loglik+fit1$lga$loglik-x)))
  #WGL model
  x <- min(fit1$wgl$loglik)
  B.wgl <- mean(1/exp(fit2$wgl$loglik-x))/
           mean(1/exp(fit1$wgl$loglik-x))
  B <- rbind(B.lga,B.wgl)
  row.names(B) <- c("Age and lga models","Wgl model")
  B
}

PsBF.caa <- function(fit1,fit2)
{
  #Age and LGA model
  
  B.lga <- exp(sum(log(fit2$lga$mean.inv.lik.mod1))-
               sum(log(fit1$lga$mean.inv.lik.mod1)))
  #wgl model
  B.wgl <- exp(sum(log(fit2$wgl$mean.inv.lik))-
               sum(log(fit1$wgl$mean.inv.lik)))
  B <- rbind(B.lga,B.wgl)
  row.names(B) <- c("Age and lga models","Wgl model")
  B
}

