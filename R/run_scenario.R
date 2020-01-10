#' Simulate Test
#'
#' Performs a Monte Carlo simulation of the BACI design based on the
#' posterior sample obtained from the fit of the pilot data and tests for
#' significant effects (via model probabilities).
#'
#' @param x A matrix of scenario specificiations to be run
#' @param scenario The scenario parameters as specified in scenarioParams (generated
#' by a call to fitData
#'
#' @details
#' This runs statistical testing across the posterior sample (number of requested
#' iterations). An approximate type I or type II (i.e. 1-power) error rate
#' is calculated based on the proportion of tests failing the criteria
#' set for model probabilities (>0.5).
#'
#' Note that Type I error is simply the proportion of tests identified as
#' significantly different when the effect size is zero.
#'
#' This power calculation includes uncertainty of parameter estimates
#' via use of the posterior sample obtained through INLA.
#'
#' @export
#' @return 	a list of realised model probabilities for each scenario, with each
#' scenario having as many as required by the number of iterations. Also returns
#' a list of the simulated datasets used in testing.
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (2019) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution.
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' #scenarioRun<-run.scenario(scenario.matrix[2,], scenarioParams)
#' scenarioRun<-run.scenario(x1)
run.scenario<-function(x,scenario=scenarioParams){
	#scenario.matrix - a matrix of scenario specifications to be run
	#tests - a character vector indicating the statistical tests to be applied
  dat<-scenario$dat
	effect.type<-scenario$effect.type
	mod1.formula.use<-scenario$mod1.formula.use
	variableType<-scenario$variableType

	locations.impact.i<-as.numeric(unlist(x["locations.impact"]))
	locations.control.i<-as.numeric(unlist(x["locations.control"]))
	times.before.i<-as.numeric(unlist(x["times.before"]))
	times.after.i<-as.numeric(unlist(x["times.after"]))
	replicates.i<-as.numeric(unlist(x["replicates"]))
	sublocations.within.locations.i<-as.numeric(unlist(x["sublocations.within.locations"]))
	subtimes.within.times.i<-as.numeric(unlist(x["subtimes.within.times"]))
  trials.i<-as.numeric(unlist(x["trials"]))
  if(is.na(trials.i) & length(unique(dat$Response))>2 & variableType=="binomial"){
       cat("You have specified a binomial distribution for proportional or \n",
           "percentage data, without indicating how many trials to simulate \n")
       return()}

  if(is.na(trials.i)==F){
    if(trials.i>1 & length(unique(dat$Response))==2){trials.i=NA}}
  fixed.sample<-scenario$fixed.sample
  fixed.levels<-scenario$fixed.levels
  hyperpar.sample<-scenario$hyperpar.sample


  BIeta<-fixed.sample[,fixed.levels[intersect(grep("Impact", fixed.levels),
                                             grep("Before",fixed.levels))]]
  BCeta=fixed.sample[,fixed.levels[intersect(grep("Control", fixed.levels),
                                             grep("Before",fixed.levels))]]

  # calculate observed before states based on the appropriate link functions
  BIp=xformEta(BIeta,variableType,"response")

	# Calculate the After Impact site state based on the specified effect size
	effect.i=unlist(x["effect"])
    # effect size
  if(effect.type=="Multiplicative"){
     AI.p=BIp+(BIp*effect.i)}
  if(effect.type=="Fixed"){
     AI.p=BIp+effect.i
     # check the effect specification against the family
     # gaussian = no issues
     # poisson
     if(variableType=="poisson"){
      # check if the AI.P is less than zero. If so return to zero.
      if(min(AI.p)<0){AI.p[which(AI.p<0)]=0}
     }
     # gamma
     if(variableType=="gamma"){
      # check if AI.p is equal to or less than zero. If so, return to a very small
      # positive value (1/1000 of min value observed in the response data
      if(min(AI.p)<=0){AI.p[which(AI.p<=0)]=min(dat$Response)/1000}
     }
     # nbinomial
     if(variableType=="nbinomial"){
      # check if the AI.P is less than zero. If so return to zero.
      if(min(AI.p)<0){AI.p[which(AI.p<0)]=0}
     }
     #binomial
     if(variableType=="binomial"){
      # check the fixed effect.i is between 0 and 1
      if(abs(effect.i)>1){
       cat("Your effect size is out of bounds on the probability scale\n")
       return()}
      # check if the AI.P is less than zero or greater than 1.
      # If so return to zero or 1.
      if(min(AI.p)<0){AI.p[which(AI.p<0)]=0}
      if(max(AI.p)>1){AI.p[which(AI.p>1)]=1}
      }
     # beta
     if(variableType=="beta"){
      # check the fixed effect.i is between 0 and 1
      if(abs(effect.i)>1){
       cat("Your effect size is out of bounds on the probability scale\n")
       return()}
      # check if the AI.P is less than or equal to zero or greater than or equal to 1.
      # If so return to nearly zero or nearly 1.
      if(min(AI.p)<=0){AI.p[which(AI.p<=0)]=0+1e-5}
      if(max(AI.p)>=1){AI.p[which(AI.p>=1)]=1-1e-5}
      }
    }

  # Back transform the After Impact mean onto the scale of the linear predictor
  AIeta<-xformEta(AI.p,variableType,"link")

	# calculate the number of sites and time samples required
	n.locations=locations.impact.i+locations.control.i
	n.times=times.before.i+times.after.i
	n.sublocations=n.locations*sublocations.within.locations.i
	n.subtimes=n.times*subtimes.within.times.i

	# create a dataframe for the sampling design
	sim.dat=as.data.frame(expand.grid(Location=1:n.locations, Time=1:n.times)) # every locations measured every time
	sim.dat$BvA="B"; sim.dat$BvA[which(match(sim.dat$Time,times.before.i+1:times.after.i)>0)]="A"
	sim.dat$CvI="C"; sim.dat$CvI[which(match(sim.dat$Location,locations.control.i+1:locations.impact.i)>0)]="I"

	sim.dat$Time=as.factor(sim.dat$Time)
	sim.dat$Location=as.factor(sim.dat$Location)
	sim.dat$T.L=as.factor(paste(sim.dat$Time,sim.dat$Location,sep="_"))
	sim.dat$BvA=as.factor(sim.dat$BvA)
	sim.dat$CvI=as.factor(sim.dat$CvI)
  sim.dat$E=1 # for poisson
  if(variableType=="binomial" & is.null(dat$Trials)==F) {
    sim.dat$trials.i=trials.i}
	sim.dat$BvAxCvI=as.factor(paste(sim.dat$BvA,sim.dat$CvI,sep="._."))

	# ensure unique coding of relevant factors
	sim.dat$Time.unique=as.factor(paste(sim.dat$BvA,sim.dat$Time, sep="_"))
	sim.dat$Location.unique=as.factor(paste(sim.dat$CvI,sim.dat$Location, sep="_"))

	# replicate the sampling design by the number of sublocations.i
	# can change this to a straight apply
	sim.dat=do.call(rbind,lapply(1:sublocations.within.locations.i, function(reps,data) {data$sublocation <- reps;data}, data = sim.dat))
	# replicate the sampling design by the number of subtimes.i
	sim.dat=do.call(rbind,lapply(1:subtimes.within.times.i, function(reps,data) {data$subtime <- reps;data}, data = sim.dat))
	# replicate the sampling design by the number of replicates.i
	sim.dat=do.call(rbind,lapply(1:replicates.i, function(reps,data) {data$reps <- reps;data}, data = sim.dat))
  sim.dat$repID=(1:nrow(sim.dat)) + nrow(dat)

	# ensure they are factor variables and generate the necessary interaction terms
	sim.dat$sublocation.unique=as.factor(paste(sim.dat$CvI,sim.dat$Location,sim.dat$sublocation, sep="_"))
	sim.dat$subtime.unique=as.factor(paste(sim.dat$BvA,sim.dat$LTime,sim.dat$subtime, sep="_"))
	sim.dat$T.subL=as.factor(paste(sim.dat$Time.unique,sim.dat$sublocation.unique,sep="_"))
	sim.dat$L.subT=as.factor(paste(sim.dat$Location.unique,sim.dat$subtime.unique,sep="_"))
	sim.dat$subL.subT=as.factor(paste(sim.dat$sublocation.unique,sim.dat$subtime.unique,sep="_"))

  # create a matrix for the simulated response data of size nrow sim.dat, ncol = n.its
  response.mat=matrix(ncol=scenario$n.its,nrow=nrow(sim.dat))

	# start with the appropriate means
  #BC - use actual mean estimate
  index.BC=which(sim.dat$BvA=="B" & sim.dat$CvI=="C")
	response.mat[index.BC,]<-matrix(rep(BCeta,length(index.BC)),ncol=scenario$n.its,byrow=T)
  #BI - use actual mean estimate
  index.BI=which(sim.dat$BvA=="B" & sim.dat$CvI=="I")
	response.mat[index.BI,]<-matrix(rep(BIeta,length(index.BI)),ncol=scenario$n.its,byrow=T)
  #AI - use mean estimate modified by specified effect
  index.AI=which(sim.dat$BvA=="A" & sim.dat$CvI=="I")
	response.mat[index.AI,]<-matrix(rep(AIeta,length(index.AI)),ncol=scenario$n.its,byrow=T)
  #AC  - use BC mean estimate
  index.AC=which(sim.dat$BvA=="A" & sim.dat$CvI=="C")
	response.mat[index.AC,]<-matrix(rep(BCeta,length(index.AC)),ncol=scenario$n.its,byrow=T)

  # get the random standard deviations from the precision estimates
  random.structure=scenario$random.structure
  random.effects=sqrt(1/scenario$hyperpar.sample[,which(match(
                gsub("Precision for ","",colnames(scenario$hyperpar.sample)),
                random.structure)>0)])
  colnames(random.effects)=gsub("Precision for ","",colnames(random.effects))

  # use rnorm to sample deviations for each random effect for each iteration
  random.deviations=lapply(random.structure,FUN=function(tt){
               do.call("cbind",lapply(random.effects[,tt],FUN=function(x){
               rnorm(length(unique(sim.dat[,tt])),0,x)[
                        match(sim.dat[,tt],unique(sim.dat[,tt]))]}))})
  names(random.deviations)=random.structure

  # substitute Location and sublocation deviations with any orginal data
  dat.times=unique(dat[,c("BvA","Time.unique")])
  dat.times=split(as.character(dat.times$Time.unique),list(dat.times$BvA))
  dat.locations=unique(dat[,c("CvI","Location.unique")])
  dat.locations=split(as.character(dat.locations$Location.unique),list(dat.locations$CvI))
  dat.sublocations=unique(dat[c("CvI","Location.unique","sublocation.unique")])
  dat.sublocations=split(as.character(dat.sublocations$sublocation.unique),
                    list(dat.sublocations$Location.unique))
  dat.subtimes=unique(dat[,c("BvA","Time.unique","subtime.unique")])
  dat.subtimes=split(as.character(dat.subtimes$subtime.unique),
                    list(dat.subtimes$Time.unique))
  dat.times.before=dat.times$Before
  dat.subtimes.before=dat.subtimes[dat.times.before]
  dat.locations.impact=dat.locations$Impact
  dat.locations.control=dat.locations$Control
  dat.sublocations.impact=dat.sublocations[dat.locations.impact]
  dat.sublocations.control=dat.sublocations[dat.locations.control]

  sim.dat.times=unique(sim.dat[,c("BvA","Time.unique")])
  sim.dat.times=split(as.character(sim.dat.times$Time.unique),list(sim.dat.times$BvA))
  sim.dat.locations=unique(sim.dat[,c("CvI","Location.unique")])
  sim.dat.locations=split(as.character(sim.dat.locations$Location.unique),list(sim.dat.locations$CvI))
  sim.dat.sublocations=unique(sim.dat[c("CvI","Location.unique","sublocation.unique")])
  sim.dat.sublocations=split(as.character(sim.dat.sublocations$sublocation.unique),
                    list(sim.dat.sublocations$Location.unique))
  sim.dat.subtimes=unique(sim.dat[,c("BvA","Time.unique","subtime.unique")])
  sim.dat.subtimes=split(as.character(sim.dat.subtimes$subtime.unique),
                    list(sim.dat.subtimes$Time.unique))
  sim.dat.times.before=sim.dat.times$B
  sim.dat.subtimes.before=sim.dat.subtimes[sim.dat.times.before]
  sim.dat.locations.impact=sim.dat.locations$I
  sim.dat.locations.control=sim.dat.locations$C
  sim.dat.sublocations.impact=sim.dat.sublocations[sim.dat.locations.impact]
  sim.dat.sublocations.control=sim.dat.sublocations[sim.dat.locations.control]

  sim.dat$sublocation.unique=as.character(sim.dat$sublocation.unique)
  sim.dat$Location.unique=as.character(sim.dat$Location.unique)


  sim.dat.list=list()
  for(n in 1:scenario$n.its){
    # subsamples locations and sublocations for each iteration
    dat.locations.impact=dat.locations$Impact
    dat.locations.control=dat.locations$Control
    dat.sublocations.impact=dat.sublocations[dat.locations.impact]
    dat.sublocations.control=dat.sublocations[dat.locations.control]

    # extract the posterior sample estimates
    sample.estimates=scenario$post.sample[[n]]$latent
    Location.sample.estimates=sample.estimates[grep("Location.unique",
                                      rownames(sample.estimates))]
    names(Location.sample.estimates)=levels(dat$Location.unique)
    sublocation.sample.estimates=sample.estimates[grep("sublocation.unique",
                                      rownames(sample.estimates))]
    if(length(sublocation.sample.estimates)>0){
     names(sublocation.sample.estimates)=levels(dat$sublocation.unique)
    }

    # copy the simulated data and replace with the existing elements
    sim.dat.use=sim.dat

    # subststitute existing impact locations
    # if the number of existing impact locations is more than the number required subsample
    if(length(dat.sublocations.impact)>locations.impact.i){
      dat.sublocations.impact=sample(dat.sublocations.impact,locations.impact.i)}
    for(h in 1:length(dat.sublocations.impact)){
         index.h=which(sim.dat.use$Location.unique==names(sim.dat.sublocations.impact)[h])
         sim.dat.use$Location.unique[index.h]=names(dat.sublocations.impact)[h]
         random.deviations$Location.unique[index.h,n]=
                  Location.sample.estimates[names(dat.sublocations.impact)[h]]
         sublocations.h=dat.sublocations.impact[[h]]
           # if the number of existing subocations is more than the number required subsample
         if(length(sublocations.h)>sublocations.within.locations.i){
           sublocations.h=sample(sublocations.h,sublocations.within.locations.i)}
         for(l in 1:length(sublocations.h)){
             index.l=which(sim.dat.use$sublocation.unique==sim.dat.sublocations.impact[[h]][l])
             sim.dat.use$sublocation.unique[index.l]=sublocations.h[l]
             if(length(random.deviations$sublocation.unique)>0){
              random.deviations$sublocation.unique[index.l,n]=
                  sublocation.sample.estimates[sublocations.h[l]]
             }
             }
         }
    # subststitute existing control locations
    # if the number of existing control locations is more than the number required subsample
    if(length(dat.sublocations.control)>locations.control.i){
      dat.sublocations.control=sample(dat.sublocations.control,locations.control.i)}
    for(h in 1:length(dat.sublocations.control)){
         index.h=which(sim.dat.use$Location.unique==names(sim.dat.sublocations.control)[h])
         sim.dat.use$Location.unique[index.h]=names(dat.sublocations.control)[h]
         random.deviations$Location.unique[index.h,n]=
                  Location.sample.estimates[names(dat.sublocations.control)[h]]
         sublocations.h=dat.sublocations.control[[h]]
           # if the number of existing subocations is more than the number required subsample
         if(length(sublocations.h)>sublocations.within.locations.i){
           sublocations.h=sample(sublocations.h,sublocations.within.locations.i)}
         for(l in 1:length(sublocations.h)){
             index.l=which(sim.dat.use$sublocation.unique==sim.dat.sublocations.control[[h]][l])
             sim.dat.use$sublocation.unique[index.l]=sublocations.h[l]
             if(length(random.deviations$sublocation.unique)>0){
              random.deviations$sublocation.unique[index.l,n]=
                  sublocation.sample.estimates[sublocations.h[l]]
             }
             }
         }
    sim.dat.list=c(sim.dat.list,list(sim.dat.use))
  }

  # sum all the deviations and add to the response matrix
  response.mat=Reduce("+",random.deviations)+response.mat

  # now transform from eta
  response.mat=apply(response.mat, MARGIN=2,FUN=xformEta,
                      variableType=variableType,direction="response")

    # for each row, sample from the appropriate distribution
    if(variableType=="gaussian"){
     hyper.var=sqrt(1/scenario$hyperpar.sample[,"Precision for the Gaussian observations"])
     response.mat=do.call("cbind",lapply(1:ncol(response.mat),FUN=function(x){
                  rnorm(nrow(response.mat),mean=response.mat[,x],sd=hyper.var[x])}))
                  }
    if(variableType=="poisson"){
     response.mat=do.call("cbind",lapply(1:ncol(response.mat),FUN=function(x){
                  rpois(nrow(response.mat),lambda=response.mat[,x])}))
                  }
    if(variableType=="gamma"){
     hyper.var=scenario$hyperpar.sample[,"Precision parameter for the Gamma observations"]
     response.mat=do.call("cbind",lapply(1:ncol(response.mat),FUN=function(x){
                  rgamma(nrow(response.mat),shape=hyper.var[x],
                  scale=response.mat[,x]/hyper.var[x])}))
    }
    if(variableType=="nbinomial"){
     hyper.var=scenario$hyperpar.sample[,"size for the nbinomial observations (1/overdispersion)"]
     response.mat=do.call("cbind",lapply(1:ncol(response.mat),FUN=function(x){
                  rnbinom(nrow(response.mat),size=hyper.var[x], mu=response.mat[,x])}))
    }
    if(variableType=="binomial"){
     if(is.na(trials.i)){trials.i=1}
     response.mat=do.call("cbind",lapply(1:ncol(response.mat),FUN=function(x){
                  rbinom(nrow(response.mat),prob=response.mat[,x],size=trials.i)}))
    }
    if(variableType=="beta"){
     hyper.var=scenario$hyperpar.sample[,"precision parameter for the beta observations"]
     response.mat=do.call("cbind",lapply(1:length(hyper.var),FUN=function(x){
                  vec=round(rbeta(nrow(response.mat),shape1=response.mat[,x]*hyper.var[x],
                         shape2=(-response.mat[,x]*hyper.var[x])+hyper.var[x]),3)
                  vec[which(vec==1)]=0.999
                  vec[which(vec==0)]=0.001
                  return(vec)}))
    }

  # Now add any existing "Before" data to the simulated data
  for(i in 1:length(sim.dat.list)){
      sim.dat.i=sim.dat.list[[i]][,c("BvA","CvI",scenario$random.structure)]
      sim.dat.i$Response=response.mat[,i]
      sim.dat.i$BvA=as.character(sim.dat.i$BvA)
      sim.dat.i$CvI=as.character(sim.dat.i$CvI)
      sim.dat.i$BvA[which(sim.dat.i$BvA=="B")]="Before"
      sim.dat.i$BvA[which(sim.dat.i$BvA=="A")]="After"
      sim.dat.i$CvI[which(sim.dat.i$CvI=="C")]="Control"
      sim.dat.i$CvI[which(sim.dat.i$CvI=="I")]="Impact"
      sim.dat.i$Trials=trials.i

      # add the pilot data to the simulated data
      #sim.dat.i=rbind(dat[which(dat$BvA=="Before"),colnames(sim.dat.i)],sim.dat.i)

      sim.dat.i$BvA=as.factor(sim.dat.i$BvA)
      sim.dat.i$CvI=as.factor(sim.dat.i$CvI)
      sim.dat.list[[i]]=sim.dat.i
  }


  # Model formula
	baci.formula.inla=as.formula(
           paste0(c("Response~BvA*CvI",
                    paste("f(",scenario$random.structure,", model='iid')",sep="")),
                                collapse="+"))
  no.baci.formula.inla=as.formula(
           paste0(c("Response~BvA+CvI",
                    paste("f(",scenario$random.structure,", model='iid')",sep="")),
                                collapse="+"))
	## run the test using dopar
	require(doParallel,quietly=TRUE)
	require(foreach,quietly=TRUE)

	cl <- makeCluster(as.numeric(dataComponents$ncores)) #use 6 cores, ie for an 8-core machine
	registerDoParallel(cl)
	system.time(
			if(as.numeric(dataComponents$ncores)==1){
      	scen.out<-foreach(i=1:length(sim.dat.list),
        .export=c("scenarioParams","scenario.matrix"),
				.packages="INLA")%do% {
          require(INLA,quietly=TRUE)
          dat.n=sim.dat.list[[i]]
          mod.baci=try(inla(baci.formula.inla,data=dat.n,
                          family=variableType,
                          Ntrials=dat.n$Trials, verbose = TRUE,
                          control.compute=list(dic=TRUE, config=TRUE,
                          waic= TRUE, cpo = TRUE, mlik = TRUE),
                          control.predictor=list(compute=T)),silent=T)
          mod.no.baci=try(inla(no.baci.formula.inla,data=dat.n,
                          family=variableType,
                          Ntrials=dat.n$Trials, verbose = TRUE,
                          control.compute=list(dic=TRUE, config=TRUE,
                          waic= TRUE, cpo = TRUE, mlik = TRUE),
                          control.predictor=list(compute=T)),silent=T)
          scen.out <- as.numeric(NA)
          if(class(mod.baci)!="try-error" &
             class(mod.no.baci)!="try-error"){  # if the models did not fail
          lmls <- c(mod.baci$mlik[1],mod.no.baci$mlik[1])
          ind <- which(max(lmls) == lmls)
          lmls <- lmls - lmls[ind]          # For stable estimation of pmps
          pmps <- exp(lmls)/sum(exp(lmls))  # Estimated posterior model probabilies
          scen.out <- pmps[2]}else{scen.out<-NA}    # Probability that 'null' model is preferred....when only comparing two models
         }
        }else{
      	scen.out<-foreach(i=1:length(sim.dat.list),
        .export=c("scenarioParams","scenario.matrix"),
				.packages="INLA")%dopar% {
          require(INLA,quietly=TRUE)
          dat.n=sim.dat.list[[i]]
          mod.baci=try(inla(baci.formula.inla,data=dat.n,
                          family=variableType,
                          Ntrials=dat.n$Trials, verbose = TRUE,
                          control.compute=list(dic=TRUE, config=TRUE,
                          waic= TRUE, cpo = TRUE, mlik = TRUE),
                          control.predictor=list(compute=T)),silent=T)
          mod.no.baci=try(inla(no.baci.formula.inla,data=dat.n,
                          family=variableType,
                          Ntrials=dat.n$Trials, verbose = TRUE,
                          control.compute=list(dic=TRUE, config=TRUE,
                          waic= TRUE, cpo = TRUE, mlik = TRUE),
                          control.predictor=list(compute=T),),silent=T)
          scen.out <- as.numeric(NA)
          if(class(mod.baci)!="try-error" &
             class(mod.no.baci)!="try-error"){  # if the models did not fail
          lmls <- c(mod.baci$mlik[1],mod.no.baci$mlik[1])
          ind <- which(max(lmls) == lmls)
          lmls <- lmls - lmls[ind]           # For stable estimation of pmps
          pmps <- exp(lmls)/sum(exp(lmls))   # Estimated posterior model probabilies
          scen.out <- pmps[2]}else{scen.out<-NA}               # Probability that 'null' model is preferred....when only comparing two models
          }
         }
        )
  stopCluster(cl)
	return(list(model.probs=scen.out,
              sim.factor.data=sim.dat.list))
}
