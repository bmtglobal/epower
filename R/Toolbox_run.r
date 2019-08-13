

#' \tabular{ll}{
#' Package: \tab epower\cr
#' Type: \tab Package\cr
#' Title: \tab Power analysis for beyond BACI sampling designs\cr
#' Version: \tab 1.3\cr
#' Date: \tab 2018-10-30\cr
#' Author: \tab BMT, AIMS and QUT (Rebecca Fisher, Glenn Shiell, Rohan Sadler, James McGree)\cr
#' Maintainer: \tab Rebecca Fisher\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' Depends: \tab XLConnect, doParallel, INLA\cr
#' }
#'
#' @details
#' epower conducts a power analysis on beyond BACI sampling
#' designs through INLA and Monte Carlo simulation.
#'
#' Currently epower considers only univariate responses. The analysis allows for
#' crossed random effects between \code{Time} and \code{Site} in the design.
#'
#' The epower toolbox has been designed for use by ecologists and environmental practioners
#' with a sound understanding of the statistical principles of
#' BACI designs. To assist the non-R user, BACI parameters and pilot data are
#' defined in an Excel workbook. The template epower_interface_V1.3.xlsx should
#' have been supplied with the package tar.gz file.
#'
#' To run the program, simply adjust your BACI design
#' in a copy of epower_interface_V1.3.xlsx, load the
#' package and then call the function fitData(), which extracts the relevant information
#' from the excel interface, followed by assessPower(), which carries out the power analysis
#' based on the scenario information extracted.
#' Alternatively, the program can be run entirely within R, using the function supplyData
#' and calling power Scenario manually before running assessPower().

#' The epower (V1.3) package is a BMT product which has been developed in
#' collaboration with the Australian Institute of Marine Science (AIMS),
#' Queensland University of Technology (QUT) and Pink Lake Analytics.
#' The method/code supporting epower v1.3 is described in Fisher R et al. (2019, Methods in Ecology and Evolution).
#' Detailed instructions for operating the package can be found in the user manual (BMT 2019), available as Appendix 1 (Fisher et al 2019), or from BMTglobal.
#' epower is freely available to users under licence.
#' The licence explicitly precludes the incorporation of the epower code into
#' other programs which are subsequently sold or used for commercial purposes.
#' Users are asked to please include the following citation text:
#' Analyses were carried out using the software epower V1.3 (BMT 2019)
#' as described in Fisher et al (2018) and (BMT 2019), based on the
#' statistical programming platform R (R-Core Team, 2019).

#' Acknowledgements:
#'
#' BMT funded the development
#' of this package with Glenn Shiells as project leader. Rebecca
#' Fisher (AIMS, Perth, Western Australia) encoded the toolbox and
#' developed the formulation of the BACI design. James McGree (QUT, Brisbane,
#' Queensland) provided advice on implementation of the toolbox using INLA,
#' the use of Bayes factors for statistical hypothesis testing (model selection)
#' and the use of a posterior sample as the basis for monte-carlo simulation.
#' Rohan Sadler (Pink Lake Analytics, Perth, Western Australia) restructured
#' the code as the epower package, added parallelization and
#' assisted in the design of a beta cost optimisation solution (not currently implemented).
#'
#' Discussion with Marti Anderson (affiliation), Carl Schwarz (Simon Fraser
#' University, British Columbia, Canada), Tim Langlois (University of Western
#' Australia, WA, Australia) and Kim Freidman(previously at the Department of Parks
#' and Wildlife, WA, Australia) during development of epower was highly valued.
#'
#' @name epower-package
#' @aliases epower epower-package
#' @docType package
#' @author
#' Rebecca Fisher (Australian Institue of Marine Science)
#'
#' Glenn Shiell (BMT)
#'
#' Rohan Sadler (Pink Lake Analytics)
#'
#' James McGree (Queensland University of Technology)
#'
#' Maintainer: Rebecca Fisher \email{r.fisher@@aims.gov.au}

#' @keywords package
#' @seealso \code{\link{fitData}}
#' @seealso \code{\link{assessPower}}
#' @seealso \code{\link{powerScenario}}
#' @seealso \code{\link{supplyData}}
#' @references Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM
#' (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @references BMT (2018). epower: Statistical Power Analysis for BACI designs; Guide to Software and Statistical Methods.
#' @examples
#' install.packages("epower",dependencies=TRUE)
#' library(epower)
#' # Set the working directory in R to the folder containg the
#' # excel workbook. This can be done by clicking
#' # File -> Change dir...
#'
NULL
# building options
## library(roxygen2)
## setwd("/home/rohan/workspace/POWER TOOLBOX V1.1")
# package.skeleton("epower",code_files=c("Toolbox_run.r"),force=TRUE)
## roxygenize("epower",overwrite=TRUE,copy.package=FALSE,unlink.target=FALSE)
## R CMD build epower
## R CMD check epower
## rm -R epower*
# detach("package:epower",unload=TRUE)
## install.packages("epower",repos=NULL)
## library(epower)

#' fitData
#'
#' Unpacks the power analysis data and scenarios from the epower template and fits
#' an INLA mixed model to the supplied pilot data. Generates the necessary R objects
#' to run assessPower(). See ?assessPower
#'
#' This function wraps the other functions within the epower
#' package to extract the necessary scenario data and peform a Bayesin mixed model fit
#' using inla(). It generates the required R objects dataComponents and
#' scenarioParams for running the function assessPower().
#'
#' @param excelInFile the excel file in which the power
#' analysis scenario is described; exclude the directory
#' name (see dir.name), but ensure the working directory has been set to that
#' containing the workbook file.
#' @param ncores the number of cores required for the analysis
#' @param dir.name the directory in which the excel file
#' is located.
#'
#' @details The function requires INLA for estimation
#' of a mixed effects model used to parameterize the monte carlo simulation
#' and for model fits with and without that BvA * CvI interaction term that
#' are used to calculate model probabilities.
#'
#' The function fitData() starts by checking that the excel interface workbook
#' specified actually exists in the current working directory, loads the required
#' packages, and then calls the supporting function designFactors(epower
#' package.interface.file = excelInFile), which is responsible for importing and
#' unpacking all of the data supplied in the excel interface workbook file.
#' During the unpacking process the original pilot data is imported, the
#' specified design fields are formatted to ensure they are factors and are
#' unique in terms of the coding of factor levels (i.e. Sites 1-3 within
#' Locations are given unique names across different locations), and then
#' relabelled to the uniform field names indicated in the excel interface.
#' The factor levels for BvA and CvI are also re-labelled to Before, After, Control
#' and Impact from whatever was originally contained within the pilot data. The
#' resulting outputs are converted to a list that is returned globally as the
#' object designComponents that is then available for use in subsequent function calls.
#' The use of the global assignment means that the designComponents object is
#' available outside the fitData() call, and can be used by  the assessPower()
#' function if this later called by the user.
#' Once the relevant data has been extracted, fitData() calls the function
#' powerScenario(), which takes the dataComponents list as an input and uses the
#' design information to construct an appropriate random and fixed effects model
#' structure, and fits the pilot data using a generalised mixed model via a
#' call to inla(). Once the pilot data is fitted, a posterior sample is obtained
#' (with the size equivalent to the number of simulation iterations specified by
#' the user) using the function inla.posterior.sample(n = n.its,pilot.data.fit)
#' supplied by the INLA package. Relevant statistical outputs are extracted and
#' formatted such that they can be written to file for the user's perusal (
#' saved as ...Model_fit_stats.csv), along with PIT values (obtained by
#' setting cpo=TRUE and calling pilot.data.fit$cpo$pit), which are plotted as a
#' histogram and saved as ...Pit_histograme.pdf so the user can evaluate the
#' quality of the model fit. A good model fit is indicated by an even frequency
#' distribution of values (i.e., the PIT histogram should be largely flat).
#' If the pilot data contains Before, After, Control and Impact data,
#' the powerScenario() function automatically fits inla() mixed models with and
#' without the BA x CI interaction term, calculates model probabilities, and returns
#' model fit statistical for both models, along with the calculated model
#' probabilities in the file ...Model_fit_stats.csv. This functionality can be
#' used to test for a significant impact in the supplied data.
#'
#' XLConnect is used to
#' read the excel workbook file.
#'
#' A template for the excel workbook file should have been made available with the package
#' tar.gz file.
#'
#' You will need to check the current approximate CPU load
#' on the available cores before implementing ncores > 1
#'
#' @export
#' @return Fitted model results - Including:  model fit statistics for the INLA analysis peformed
#' on the supplied pilot data as a .csv file written to the working directory
#' and a pdf showing the histogram of PIT values obtained
#' from the INLA fit.
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' install.packages("epower",dependencies=TRUE)
#' library(epower)
#' # Set the working directory in R to the folder containg the
#' # excel workbook. This can be done by clicking
#' # File -> Change dir...
#'
#' fitData(excelInFile="",dir.name="data_examples")

fitData<-function(
		excelInFile="",
		dir.name=getwd()
    ){

  if(nchar(excelInFile)>0){
	 # catch for existence of file name
	 fMatch<-match(excelInFile,list.files(dir.name))
	 if(is.na(fMatch)) {
		cat("Your file does not exist, please check the spelling of \n",
				"    either your excel file name or directory name\n")
		return()
	 }
 }
  if(nchar(excelInFile)==0){
   file.path.val=file.choose()
   excelInFile <- basename(file.path.val)
   dir.name=gsub(excelInFile,"",file.path.val,fixed=T)
   setwd(dir.name)
  }

	endChar<-substr(dir.name,nchar(dir.name),nchar(dir.name))
	if(endChar == "/") {
		excelInFile<-paste(dir.name,excelInFile,sep="")
	}else{
		endChar<-substr(dir.name,nchar(dir.name)-1,nchar(dir.name))
		if( endChar == "\\") {
			excelInFile<-paste(dir.name,excelInFile,sep="")
		}else{
			excelInFile<-paste(dir.name,excelInFile,sep="/")
		}
	}

	#packageRequired<-c("XLConnect","INLA","doParallel")
	#packageMatch<-match(packageRequired,installed.packages()[,1])
	#packageNA<-which(is.na(packageMatch))
	#if(length(packageNA)>0) {
	#	cat("Please allow installation of the following packages ",
	#			paste(packageRequired[packageNA],collapse=" and "),"\n")
	#	install.packages(packageRequired[packageNA])
	#}
  #excelInFile<<-excelInFile
#  'require' only used at debugging; required packages moved to DESCRIPTION
#	packageRequired<-c("XLConnect","INLA","doParallel")
#	packageMatch<-match(packageRequired,installed.packages()[,1])
#	packageNA<-which(is.na(packageMatch))
#	if(length(packageNA)>0) {
#		cat("Please allow installation of the following packages ",
#				paste(packageRequired[packageNA],collapse=" and "),"\n")
#		install.packages(packageRequired[packageNA])
#	}

	packageRequired<-c("XLConnect","INLA","doParallel")
	packageMatch<-match(packageRequired,installed.packages()[,1])
	packageNA<-which(is.na(packageMatch))
	if(length(packageNA)>0) {
		cat("Please allow installation of the following packages ",
				paste(packageRequired[packageNA],collapse=" and "),"\n")
		install.packages(packageRequired[packageNA])
	}

	excelInFile<<-excelInFile
	dataComponents<<-do.call(designFactors,list(toolbox.interface.file=excelInFile),quote=TRUE)

	require(INLA,quietly=TRUE)
	scenarioParams<<-do.call(powerScenario,list(inputData=dataComponents))

 #return(list(scenarioParams,scenario.matrix))
}

#' Assess Power
#'
#' Run the power toolbox following a call to fitData(). See ?fitData
#'
#' This function wraps the other functions within the epower
#' package to perform the power analysis given as a
#' scenario within the excel file supplied to the companion function fitData().
#' fitData() must be run prior to running assessPower() in order to generate
#' the required model objects dataComponents and scenarioParams.
#'
#' @param NA the function takes no arguments, but instead uses the objects contained
#' in the global environment generated by the function fitData()
#'
#' @details The function assessPower() allows the user to assess power across a range of
#' scenarios as specified in the excel interface workbook and unpacked in the call to fitData().
#' The function is directly
#' called by the user and has no arguments that need to be specified, but will only
#' run if the function fitData() has already been called by the user during that R
#' session, because it relies on global variables generated during the execution
#' of fitData(). Initially assessPower() calls the function buildScenarioMatrix()
#' which takes the information supplied on the excel interface file and generates
#' a matrix of all requested scenario combinations. Each row of this matrix is
#' then passed to the function run.scenario(), which is responsible for building the
#' monte-carlo datasets based on the specifications of that scenario (including
#' the specified effect size) and the posterior sample generated by powerScenario()
#' from the pilot data model fit; combining this with the original pilot data;
#' and then calculating posterior model probabilities for a model with and without
#' the BA*CI interaction term. The returned model probabilities are collated such
#' that those <0.5 are assigned a 1 (representing a successful detection of impact)
#' for that iteration of that scenario, and those >0.5 are assigned a 0
#' (no detection of impact). Where no effect is applied in a given scenario,
#' the proportion of successful detections represents type 1 error, whereas if
#' an effect was applied, the proportion of successful detections represents
#' statistical power for that scenario. The proportion of successful detections
#' is combined with the generated scenario matrix, and output as a csv
#' file ...scenario_power_summary.csv.
#'
#' @export
#' @return A power analysis - Including: a saved R workspace containing all
#' simulated scenario data and simulation results exported as a .csv file
#' labelled with the string: ...scenario_power_summary.csv
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' install.packages("epower",dependencies=TRUE)
#' library(epower)
#' # Set the working directory in R to the folder containg the
#' # excel workbook. This can be done by clicking
#' # File -> Change dir...
#'
#' fitData(excelInFile="epower_interface_V1.3.xlsx")
#' assessPower()
#'
assessPower<-function(){
  scenario.matrix<<-do.call(buildScenarioMatrix,list(params=scenarioParams))
	require(doParallel,quietly=TRUE)
	require(foreach,quietly=TRUE)

  # now run the Monte-carlo simulation for each scenario
  scen.out<<-apply(scenario.matrix,MARGIN=1,FUN=run.scenario,scenario=scenarioParams)

  ## now get the model probabilities from the list
  model.probs<-do.call("rbind",lapply(scen.out,FUN=function(x){unlist(x$model.probs)}))
  model.prob.success<-matrix(0,nrow(model.probs),ncol(model.probs))
  model.prob.success[which(model.probs<0.5)]<-1

	# aggreate results for each scenario to calculate power
	sim.dat<-scenario.matrix
	sim.dat$sig.outcomes<-rowMeans(model.prob.success)
  summary.sim<-sim.dat

  random.vars<-intersect(c("Location","sublocation","Time","subtime"),
                        gsub(".unique","",scenarioParams$random.structure))
  factor.vars.all<-c("BvA","CvI",random.vars)

  save(list=c("scen.out","scenarioParams","dataComponents","scenario.matrix"),
       file=paste("Saved_workspace_",gsub(".xlsx","",basename(excelInFile)),"_",fileName,
                ".RData",sep=""))
  write.csv(summary.sim,file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
                                   "_scenario_power_summary.csv",sep=""))

 #return(scen.out)
#	 rm(wb,sim.dat,summary.sim,scen.out,dataComponents,scenario.matrix,scenarioParams,c1,
#            endChar,fMatch, excelInFile,dir.name,dr1Result,modelSummary,ff,newExcelName)
}
#system.time(
#		runToolbox(excelInFile="epower_interface_V1.3.xlsx")
#)

#' Data Object Extraction
#'
#' Read excel scenario workbook and convert to R data objects
#'
#' This function uses XLConnect to read the excel scenario
#' workbook, and then processes that information into
#' a series of R objects.
#'
#' @param toolbox.interface.file the name of the excel file
#'
#' @export
#' @return A list of design specification parameters
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' dataComponents<-designFactors()
designFactors<-function(toolbox.interface.file=excelInFile) {
# load workbook excelInFile
	require(XLConnect,quietly=TRUE)
	wb <- loadWorkbook(toolbox.interface.file, create = FALSE)

	## define components of data for analysis
	dat=readWorksheet(wb,"pilot_data")
	variableType<-readWorksheet(wb,"design_specification",startRow=3,startCol=1,endRow=4,endCol=2)
	design.matrix=readWorksheet(wb,"design_specification",startRow=6,startCol=1,endRow=15,endCol=2)
	levels.dat=readWorksheet(wb,"design_specification",startRow=16,startCol=1,endRow=20,endCol=2)
	scenario.data=readWorksheet(wb,"design_specification", startRow=23, startCol=1, endRow=34, endCol=2)
	effect.info=readWorksheet(wb,"design_specification",startRow=38,startCol=1,endRow=43,endCol=2)
	ncores<-readWorksheet(wb,"design_specification",startRow=46,startCol=1,endRow=48,endCol=2)
	costResponsePars<-readWorksheet(wb,"design_specification",startRow=50,startCol=1,endRow=57,endCol=2)
	paramCostBounds<-readWorksheet(wb,"design_specification",startRow=26,startCol=4,endRow=34,endCol=5)
  keep.sim.dat<-unlist(readWorksheet(wb,"design_specification",startRow=59,startCol=2,endRow=60,endCol=2))=="Yes"

	## re-write the pilot.dat colnames using the standard factor names
	colnames(dat)=design.matrix$Factor[match(colnames(dat),design.matrix$Name)]

  if(length(which(colnames(dat)=="CvI"))==0){  # would be good to add some more error catching
		 cat("You have not specified a valid column indicating the control \n",
         "and impact locations \n")
		return()
	  }else{
  ## define the response variable
  if(variableType$Value=="binomial" & length(unique(dat$Response))>2){
	  if(is.null(dat$Trials)) {
		 cat("you have specified a binomial response variable \n",
				 "outside the bounds of 0 and 1, without indicating \n",
         "the number of trials for each row. Please indicate which column \n",
         "in the response data contains the number of trials. \n",
         "If your data are between zero and one and you have no information on trials, \n",
         "a beta distribution might be appopropriate. \n"
         )
		return()
	  }else{
	   if(max(dat$Response)<=1) { # data in range 0-1, assume response is a proportion
                                # and convert to frequencies. This is required by INLA ##? add popup here with
                                # warning?
      dat$Response=dat$Response*dat$Trials}
    }
  }

	## define the factor variables
	factor.vars=design.matrix$Factor[-(1:2)]
	factor.vars.current=factor.vars[which(match(factor.vars,colnames(dat))>0)]
	factor.vars.missing=factor.vars[which(is.na(match(factor.vars,colnames(dat))))]

	## append the missing factor variables to the dat dataframe for simplicty
	missing.mat=matrix(1,nrow=nrow(dat),ncol=length(factor.vars.missing))
	colnames(missing.mat)=factor.vars.missing
	dat=cbind(dat,missing.mat)

	## ensure the factor variables are coded as factors
	## sapply(1:length(factor.vars), function(x, dat){ dat[,factor.vars[x]]<<-as.factor(dat[,factor.vars[x]])}, dat=dat)

	## ensure unique coding of relevant factors
	dat$Time.unique<-as.factor(paste(dat$BvA,dat$Time, sep="_"))
	dat$subtime.unique<-as.factor(paste(dat$BvA,dat$Time, dat$subtime, sep="_"))
	dat$Location.unique<-as.factor(paste(dat$CvI,dat$Location, sep="_"))
	dat$sublocation.unique<-as.factor(paste(dat$CvI,dat$Location,dat$sublocation, sep="_"))
#	dat[,Time.unique]=as.factor(paste(dat[,BvA],dat[,Time], sep="_"))
#	dat[,subtime.unique]=as.factor(paste(dat$CvI,dat[,Time], dat[,subtime], sep="_"))
#	dat[,Location.unique]=as.factor(paste(dat[,CvI],dat[,Location], sep="_"))
#
	## generate the necesary interactions
	dat$T.L=as.factor(paste(dat$Time.unique,dat$Location.unique,sep="_"))
	dat$T.subL=as.factor(paste(dat$Time.unique,dat$sublocation.unique,sep="_"))
	dat$L.subT=as.factor(paste(dat$Location.unique,dat$subtime.unique,sep="_"))
	dat$subL.subT=as.factor(paste(dat$sublocation.unique,dat$subtime.unique,sep="_"))

	after.code=levels.dat[which(levels.dat$Levels=="After"),"Code"]

	detach("package:XLConnect", unload=TRUE) # rjava interface sometimes corrupts parallel processing

  # re-write level data
  dat$BvA[which(dat$BvA==levels.dat$Code[1])]=levels.dat$Levels[1]
  dat$BvA[which(dat$BvA==levels.dat$Code[3])]=levels.dat$Levels[3]
  dat$CvI[which(dat$CvI==levels.dat$Code[2])]=levels.dat$Levels[2]
  dat$CvI[which(dat$CvI==levels.dat$Code[4])]=levels.dat$Levels[4]

	costResponseNames<-costResponsePars[,1]
	costResponsePars<-as.list(costResponsePars[,2])
	names(costResponsePars)<-costResponseNames

	variableType<-variableType[2]

	ncores<-ncores[2]

	# returned object is list
	dcomps<-list("dat"=dat,
               "design.matrix"=design.matrix,
               "levels.dat"=levels.dat,
               "scenario.data"=scenario.data,
               "effect.info"=effect.info,
			         "factor.vars"=factor.vars,
               "factor.vars.current"=factor.vars.current,
               "factor.vars.missing"=factor.vars.missing,
               "after.code"=after.code,
               "ncores"=ncores,
               "costResponsePars"=costResponsePars,
               "variableType"=variableType,
               "paramCostBounds"=paramCostBounds,
               "keep.sim.dat"=keep.sim.dat)

	return(dcomps)
 }
}


#' Scenario Parameters
#'
#' Defines the power analysis scenario parameters and objects
#'
#' Parameters controlling the power analysis to be undertaken
#' are returned as an object, along with model specificiations.
#'
#' @param inputData the data objects extracted from the excel
#' file defining the power analysis scenario; by default it
#' inherits dataComponents from \code{\link{designFactors}}.
#'
#' @details Inla is used to fit models, and a posterior sample
#' (of size n.its) is obtained that includes posterior samples for each strata of
#' the BACI design, each level of the fixed structure available in the pilot data
#' (Before v After * Control v Impact), and any other hyperparameters
#' specific to the statistical distribution used (e.g. random error estimates
#' for a Gaussian fit, estimate of theta for a negative binomial).
#
#' Also defined are variances at each strata of the BACI design
#' for Monte Carlo simulation if required and desired effect sizes.
#'
#' @export
#' @return A list with scenario defining parameters and objects, including
#' a posterior sample, along with separate components
#' for the hyper parameters (hyperpar.sample, note this includes samples for
#' each BACI strata) and fixed parameter estimates (fixed.sample).
#' The fitted inla model is also returned, along with the
#' random structure of the design specified.
#'
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' scenarioParams<-powerScenario()
powerScenario<-function(inputData=dataComponents){
	#autoload("VarCorr",lme4)
	scenario.data<-inputData$scenario.data
    dat<-inputData$dat
	design.matrix<-inputData$design.matrix
	levels.dat<-inputData$levels.dat
	variance.data<-inputData$variance.data
	tests.selected<-inputData$tests.selected
	effect.info<-inputData$effect.info
	factor.vars<-inputData$factor.vars
	factor.vars.current<-inputData$factor.vars.current
	factor.vars.missing<-inputData$factor.vars.missing
	after.code<-inputData$after.code
	variableType<-as.character(inputData$variableType)

	n.its<-as.numeric(scenario.data[which(scenario.data$Factor=="Number of iterations"),"Value"])
	fileName<<-scenario.data[which(scenario.data$Factor=="Filename"),"Value"]

# design
	locations.impact=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of Impact Locations"),"Value"]),split=";")))#1
	locations.control=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of Control Locations"),"Value"]),split=";")))#c(2,6)
	sublocations.within.locations=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of sublocations within Location"),"Value"]),split=";")))
	times.before=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of sample times Before"),"Value"]),split=";")))
	times.after=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of sample times After"),"Value"]),split=";")))
	subtimes.within.times=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of subtimes within Time"),"Value"]),split=";")))
  trials=dat$Trials#as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of trials"),"Value"]),split=";")))
  # if Trials is blank, assume 1
  if(is.null(trials)){dat$Trials=1}else{
  # if trials contains values more than 1, but data consits of only 0 and 1, assume 1
  if(max(trials)>1 & length(unique(dat$Response))==2){dat$Trials=1}}

	replicates=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of replicate measurements"),"Value"]),split=";")))

# effects
	effect=as.numeric(unlist(strsplit(as.character(effect.info[which(effect.info[,1]=="Effect values"),2]),split=";")))
	effect.type=c("Multiplicative","Fixed")[which(effect.info[1:2,2]=="1")]

# alpha value for significance testing
	alpha=as.numeric(effect.info[5,2])

	### write model formulas based on the given factor variables
  # all allowed random terms
	random.terms=list(
     "Location.unique"="Location",
     "L.subT"=c("Location","subtime"),
     "T.L"=c("Time","Location"),
     "sublocation.unique"="sublocation",
     "T.subL"=c("Time","sublocation"),
     "subL.subT"=c("sublocation","subtime"),
     "Time.unique"="Time",
     "subtime.unique"="subtime")

  # random terms specified by the user as being in this design
  include.terms=names(random.terms)[unlist(lapply(random.terms,FUN=function(x){
                      max(is.na(match(x,factor.vars.current)))==0}))]
  if(variableType=="binomial" & max(dat$Trials)>1){
   dat$repID=1:nrow(dat)
   include.terms=c(include.terms,"repID")}

  # concatenate the BvA and CvI data to create the most complete interaction term given the data
  # and if so create the appropriate model formula
  dat$BvAxCvI=paste(dat$BvA,dat$CvI,sep="._.")
  fixed.levels=unique(dat$BvAxCvI)
  n.fixed=length(fixed.levels)
  dat$E=1

# 1. full design INLA
	mod1.formula.inla=as.formula(
           paste0(c("Response~-1+BvAxCvI",
                    paste("f(",include.terms,", model='iid')",sep="")),
                                collapse="+"))
  dz.val=1.909-(0.002101*n.its) # may need to alter this, needs testing.
  if(dz.val<1){dz.val==1}
  result=inla(mod1.formula.inla,data=dat[,c("Response","BvAxCvI","E",include.terms)],
                  family=variableType,
                  Ntrials=dat$Trials, verbose = FALSE,
                  control.inla=list(int.strategy='grid', dz=dz.val),
                  control.compute=list(dic=TRUE, config=TRUE,
                  waic= TRUE, cpo = TRUE, mlik = TRUE),
                  control.predictor=list(compute=T))

  post.sample=inla.posterior.sample(n=n.its,result)
  hyperpar.sample=do.call("rbind",lapply(post.sample, FUN=function(x){x$hyperpar}))
  fixed.sample=do.call("rbind",lapply(post.sample, FUN=function(x){
        n.latent=length(x$latent)
        out=x$latent[(n.latent-(n.fixed-1)):n.latent]
        names(out)=fixed.levels
        return(out)}))

  if(nrow(unique(dat[,c("BvA","CvI")]))==4){  # if there is before and after data
                                              # at control and impact locations
                                              # run the analysis
    # Model formula
  	baci.formula.inla=as.formula(
             paste0(c("Response~BvA*CvI",
                      paste("f(",include.terms,", model='iid')",sep="")),
                                  collapse="+"))
    no.baci.formula.inla=as.formula(
             paste0(c("Response~BvA+CvI",
                      paste("f(",include.terms,", model='iid')",sep="")),
                                  collapse="+"))

    require(INLA,quietly=TRUE)
    mod.baci=inla(baci.formula.inla,data=dat[,c("Response","BvA","CvI","E",include.terms)],
                    family=variableType,
                    Ntrials=dat$Trials, verbose = FALSE,
                    control.compute=list(dic=TRUE, config=TRUE,
                    waic= TRUE, cpo = TRUE, mlik = TRUE),
                    control.predictor=list(compute=T))
    mod.no.baci=inla(no.baci.formula.inla,data=dat[,c("Response","BvA","CvI","E",include.terms)],
                    family=variableType,
                    Ntrials=dat$Trials, verbose = FALSE,
                    control.compute=list(dic=TRUE, config=TRUE,
                    waic= TRUE, cpo = TRUE, mlik = TRUE),
                    control.predictor=list(compute=T))

    lmls <- c(mod.baci$mlik[1],mod.no.baci$mlik[1])
    ind <- which(max(lmls) == lmls)
    lmls <- lmls - lmls[ind]          # For stable estimation of pmps
    pmps <- exp(lmls)/sum(exp(lmls))  # Estimated posterior model probabilies
                                      # Probability that 'null' model is preferred....
                                      # when only comparing two models

    # extract the results for the fitted inla models
    mod.summary=summary(mod.baci)
    out.table=mod.summary$hyperpar
    out.table=rbind(mod.summary$fixed[,colnames(out.table)],out.table)
    out.table.no.baci=rbind(summary(mod.no.baci)$fixed[,colnames(out.table)],
                             summary(mod.no.baci)$hyperpar)

    rownames.tab=rownames(out.table)
    tt=gsub(".unique","",gsub("L.subT"," Location by subtime",
                        gsub("T.subL"," Time by sublocation",gsub("T.L","Time x Location",
                        gsub("._."," - ",rownames.tab)))))
    out.table=data.frame("Model"=NA,"Parameter"=tt,out.table)
    out.table$Parameter=as.character(out.table$Parameter)
    out.table$Model[1]="BA*CI"

    rownames.tab=rownames(out.table.no.baci)
    tt=gsub(".unique","",gsub("L.subT"," Location by subtime",
                        gsub("T.subL"," Time by sublocation",gsub("T.L","Time x Location",
                        gsub("._."," - ",rownames.tab)))))
    out.table.no.baci=data.frame("Model"=NA,"Parameter"=tt,out.table.no.baci)
    out.table.no.baci$Parameter=as.character(out.table.no.baci$Parameter)
    out.table.no.baci$Model[1]="BA+CI"

    mod.stats=data.frame(summary(mod.baci)$dic[c("dic","p.eff","mean.deviance","deviance.mean","family.dic","family.p.eff")])
    mod.stats.waic=data.frame(summary(mod.baci)$waic[c("waic","p.eff")])
    colnames(mod.stats.waic)=c("waic","waic.p.eff")
    mod.stats=cbind(mod.stats,mod.stats.waic)

    mod.stats.no.baci=data.frame(summary(mod.no.baci)$dic[c("dic","p.eff","mean.deviance","deviance.mean","family.dic","family.p.eff")])
    mod.stats.no.baci.waic=data.frame(summary(mod.no.baci)$waic[c("waic","p.eff")])
    colnames(mod.stats.no.baci.waic)=c("waic","waic.p.eff")
    mod.stats.no.baci=cbind(mod.stats.no.baci,mod.stats.no.baci.waic)

    output.dat=
    rbind(# baci model
          out.table,
          rep("",ncol(out.table)),
          colnames(mod.stats),
          unlist(mod.stats[1,]),
          # no baci model
          rep("",ncol(out.table.no.baci)),
          colnames(out.table.no.baci),
          out.table.no.baci,
          rep("",ncol(out.table.no.baci)),
          colnames(mod.stats.no.baci),
          unlist(mod.stats.no.baci[1,]),
          rep("",ncol(out.table.no.baci)),
          c("Estimated posterior model probabilities:","BA*CI=",round(pmps[1],3),
                                                       "BA+CI=",round(pmps[2],3),
            "","Impact detected=",pmps[2]<0.5))
    output.dat=as.matrix(output.dat)
    output.dat[is.na(output.dat)]=""
    # write out the pilot data model fit statistics before the the power analysis is run
    write.csv(output.dat,file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
       "_Impact_Assessment.csv",sep=""),row.names=F)
    pdf(file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
       "_Pit_histogram.pdf",sep=""))
    hist(mod.baci$cpo$pit,xlab="PIT values",main="")
    dev.off()
  }else{
    # extract the results for the fitted inla models
    mod.summary=summary(result)
    out.table=mod.summary$hyperpar
    out.table=rbind(mod.summary$fixed[,colnames(out.table)],out.table)

    rownames.tab=rownames(out.table)
    tt=gsub(".unique","",gsub("L.subT"," Location by subtime",
                        gsub("T.subL"," Time by sublocation",gsub("T.L","Time x Location",
                        gsub("._."," - ",rownames.tab)))))
    out.table=data.frame("Model"=NA,"Parameter"=tt,out.table)
    out.table$Parameter=as.character(out.table$Parameter)
    out.table$Model[1]="Fitted model"

    mod.stats=data.frame(summary(result)$dic[c("dic","p.eff","mean.deviance","deviance.mean","family.dic","family.p.eff")])
    mod.stats.waic=data.frame(summary(result)$waic[c("waic","p.eff")])
    colnames(mod.stats.waic)=c("waic","waic.p.eff")
    mod.stats=cbind(mod.stats,mod.stats.waic)

    output.dat=
    rbind(# fitted model
          out.table,
          rep("",ncol(out.table)),
          colnames(mod.stats),
          unlist(mod.stats[1,]))
    output.dat=as.matrix(output.dat)
    output.dat[is.na(output.dat)]=""
    # write out the pilot data model fit statistics before the the power analysis is run
    write.csv(output.dat,file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
       "_Model_fit_stats.csv",sep=""),row.names=F)
    pdf(file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
                   "_Pit_histogram.pdf",sep=""))
    hist(result$cpo$pit,xlab="PIT values",main="")
    dev.off()
  }
	scenarioTerms<-list("dat"=dat,
      "post.sample"=post.sample,
      "random.structure"=include.terms,
      "hyperpar.sample"=hyperpar.sample,
      "fixed.levels"=fixed.levels,
      "fixed.sample"=fixed.sample,
      "mod1.formula.use"=mod1.formula.inla,
			"locations.impact"=locations.impact,
      "locations.control"=locations.control,
      "sublocations.within.locations"=sublocations.within.locations,
			"times.before"=times.before,
      "times.after"=times.after,
      "subtimes.within.times"=subtimes.within.times,
      "trials"=as.numeric(unlist(strsplit(as.character(scenario.data[which(scenario.data$Factor=="Number of trials"),"Value"]),split=";"))),
      "replicates"=replicates,
			"effect"=effect,
      "effect.type"=effect.type,
      "alpha"=alpha,
      "n.its"=n.its,
      "fileName"=fileName,
			"modelEstimates"=result,
      "variableType"=variableType)
	return(scenarioTerms)

}

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
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
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
    names(sublocation.sample.estimates)=levels(dat$sublocation.unique)

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
             random.deviations$sublocation.unique[index.l,n]=
                  sublocation.sample.estimates[sublocations.h[l]]
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
             random.deviations$sublocation.unique[index.l,n]=
                  sublocation.sample.estimates[sublocations.h[l]]
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
#' Build scenario matrix
#'
#' Build matrix of sampling scenarios for which power is to be calculated
#'
#' @param params scenarioParams (as generated by fitData)
#'
#' @export
#' @return
#' A simple matrix of scenarios is built
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' scenario.matrix<-buildScenarioMatrix(params=scenarioParams)
buildScenarioMatrix<-function(params=scenarioParams){
	scenarioMatrix=expand.grid(
      params$locations.impact,
			params$locations.control,
			params$sublocations.within.locations,
			params$times.before,
			params$times.after,
			params$subtimes.within.times,
			params$replicates,
      params$trials,
			params$effect)

	colnames(scenarioMatrix)=c(
      "locations.impact",
			"locations.control",
			"sublocations.within.locations",
			"times.before",
			"times.after",
			"subtimes.within.times",
			"replicates",
      "trials",
			"effect")

  scenarioMatrix=data.frame(apply(scenarioMatrix,MARGIN=2, FUN=function(x){
                    col.vals=x
                    if(max(is.na(x))==1){col.vals=1}
                    return(col.vals)}))

	return(scenarioMatrix)
}
#' Calculates the cost of the design.
#' Supports costPowerOptimise which is in Beta and not currently implemented
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' dataComponents<-do.call(designFactors,list(toolbox.interface.file=excelInFile),quote=TRUE)
#' initialCost<-calcCost(scenario.matrix,TRUE)
#' calcCost(as.numeric(startScenario[c(1,2,5,7)]))  # should equal 0
calcCost<-function(x){

	locations.impact.i=as.numeric(unlist(x["locations.impact"]))
	locations.control.i=as.numeric(unlist(x["locations.control"]))
	times.before.i=as.numeric(unlist(x["times.before"]))
	times.after.i=as.numeric(unlist(x["times.after"]))
	replicates.i=as.numeric(unlist(x["replicates"]))
	sublocations.within.locations.i=as.numeric(unlist(x["sublocations.within.locations"]))
	subtimes.within.times.i=as.numeric(unlist(x["subtimes.within.times"]))
  trials.i=as.numeric(unlist(x["trials"]))

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

  sim.dat$trials.i=trials.i
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
  sim.dat$repID=1:nrow(sim.dat)
	# ensure they are factor variables and generate the necessary interaction terms
	sim.dat$sublocation.unique=as.factor(paste(sim.dat$CvI,sim.dat$Location,sim.dat$sublocation, sep="_"))
	sim.dat$subtime.unique=as.factor(paste(sim.dat$BvA,sim.dat$LTime,sim.dat$subtime, sep="_"))
	sim.dat$T.subL=as.factor(paste(sim.dat$Time.unique,sim.dat$sublocation.unique,sep="_"))
	sim.dat$L.subT=as.factor(paste(sim.dat$Location.unique,sim.dat$subtime.unique,sep="_"))
	sim.dat$subL.subT=as.factor(paste(sim.dat$sublocation.unique,sim.dat$subtime.unique,sep="_"))

  design.attributes=list(Trials=sum(sim.dat$trials.i),
                         Replicate=nrow(sim.dat),
                         Sublocation=length(unique(sim.dat$sublocation.unique)),
                         Location=length(unique(sim.dat$Location.unique)),
                         Subtime=length(unique(sim.dat$subtime.unique)),
                         Time=length(unique(sim.dat$Time.unique)))


	totalCost<- sum(unlist(design.attributes)*unlist(dataComponents$costResponsePars[1:6]))
	return(totalCost)
}

#' Evaluate Power of Design.
#'  Supports costPowerOptimise which is in Beta and not currently implemented
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' excelInFile<-"epower_interface_V1.3.xlsx"
#' ncores<-1
#' dir.name=getwd()
#' require(INLA)
#' require(doParallel,quietly=TRUE)
#' require(foreach,quietly=TRUE)
#' dataComponents<-do.call(designFactors,list(toolbox.interface.file=excelInFile),quote=TRUE)
#' scenarioParams<-do.call(powerScenario,list(inputData=dataComponents))
#' scenario.matrix<-do.call(buildScenarioMatrix,list(params=scenarioParams))
#' startScenario<-scenario.matrix[1,]
#' scenarioEvaluation<-evaluatePower(as.numeric(startScenario[c(1:2,5,7)]))#,missingPars=startScenario)
evaluatePower<-function(x,ncores=dataComponents$ncores){
	# missingPars is simply startScenario
	#xNames<-names(x)
   #	x<-ceiling(x)
	#x<-data.frame(matrix(x,nrow=1))
	#names(x)<-xNames
	#x<-cbind(x,scenario.matrix[scenarioIndex,8:ncol(scenario.matrix)])

	require(INLA,quietly=TRUE)
	require(doParallel,quietly=TRUE)
	require(foreach,quietly=TRUE)

  ## note might be good to make this updateably re number of its....
  probs.out=run.scenario(x=x,scenario=scenarioParams)

  ## now get the model probabilities from the list
  model.probs=unlist(probs.out$model.probs)
  model.success=rep(0,length(model.probs))
  model.success[which(model.probs<0.5)]=1

	scenPower<- mean(model.success)

	return(scenPower)
}
#evaluatePower(as.numeric(startScenario[c(1:2,5,7)]))#,missingPars=startScenario)
# evaluatePower(scenario.matrix[1,])

# simple solution
# this solution entails:
# cost evaluating every combination in a simple apply

# then finding all those 'close' to the budget frontier
#   i.e. all those that have at least one link in the
#   3d hypercube to a point beyond the budget frontier

#  evaluate all these points

# as power is a monotonic function of each parameter axis
# so you can only belong to one of these frontier solutions,
#  given you have only integer valued points.

# return all solutions, and their power, ranked from highest to lowest

# note: treat control vs impact as pooled; and step by 2

##########################################################
# need to generalise for all possible nestings of sampling

#' Optimise Power for Cost
#'
#' Currently in Beta and not implemented
#' @examples
#' excelInFile<-"epower_interface_V1.3.xlsx"
#' ncores<-1
#' dir.name=getwd()
#' require(doParallel,quietly=TRUE)
#' require(foreach,quietly=TRUE)
#' dataComponents<-do.call(designFactors,list(toolbox.interface.file=excelInFile),quote=TRUE)
#' scenarioParams<-do.call(powerScenario,list(inputData=dataComponents))
#' scenario.matrix<-do.call(buildScenarioMatrix,list(params=scenarioParams))
#' scenarioEvaluation<-costPowerOptimise(startScenario)
costPowerOptimise<-function(scenarioIndex=1){
	startScenarioCost<-calcCost(scenario.matrix[scenarioIndex,])

	paramBounds<-dataComponents$paramCostBounds

	paramVec<-list(
			locations.impact=paramBounds[1,1]:paramBounds[1,2],
			locations.control=paramBounds[2,1]:paramBounds[2,2],
			sublocations.within.locations=paramBounds[3,1]:paramBounds[3,2],
			times.before=paramBounds[4,1]:paramBounds[4,2],
			times.after=paramBounds[5,1]:paramBounds[5,2],
			subtimes.within.times=paramBounds[6,1]:paramBounds[6,2],
			trials=paramBounds[7,1]:paramBounds[7,2],
			replicates=paramBounds[8,1]:paramBounds[8,2])

	# re-sample possible solutions on exponential scale
	paramVec=lapply(paramVec,FUN=function(x){
				if(length(x)>5){x=x[unique(round(exp(seq(0,(log(length(x))),length=5))))]}else{x}})

	## define all possible parameter combinations
	paramGrid<-expand.grid(paramVec)
	paramLength<-lapply(paramVec,length)
	paramCumProd<-cumprod(unlist(paramLength))
	paramIterWhich<-which(paramLength>1)

	paramCost<-apply(paramGrid,1,calcCost)  # calcCost(paramGrid[1,],startScenarioCost)
	budget<-dataComponents$costResponsePars$Budget
	lessBudgetWhich<-which(paramCost<=budget)
	## 943 solutions are less than budget

	# -------------------------------
	### if length(lessBudgetWhich)==0 then exit

	paramCostNext<-rep(NA,length(paramCost))

	## error catch if max(lessBudgetWhich+1)
	## out of bounds on first axis - first column in paramGrid
	if(max(lessBudgetWhich+1) > length(paramCost)){   # if there are no
		cat("For some reason you have more candidate frontier solutions than parameter combinstaions\n")
		if(max(lessBudgetWhich)!= length(paramCost)) {
			paramCostNext[max(lessBudgetWhich+1)]<-budget
		}else{
			# only one solution , the maxium rep scenario considered
			frontierSolution<-data.frame(paramGrid[nrow(paramGrid),],effect=scenario.matrix[scenarioIndex,9:ncol(scenario.matrix)])
			solutionValues<-evaluatePower(frontierSolution)
			frontierSolution$Estimated.power=solutionValues
			# evaluate all the solutions for type 1 error
			frontierSolution.type1=frontierSolution
			frontierSolution.type1$effect=0
			solutionType1Values<-evaluatePower(frontierSolution.type1)
			frontierSolution$Estimated.typeI=solutionType1Values
			frontierSolution$Estimated.cost=calcCost(frontierSolution[,names(paramVec)])
			frontierSolution<-frontierSolution[order(frontierSolution$Estimated.power,
							decreasing=TRUE),]
			write.csv(solutionValues,file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
							"_orderedSolutions.csv",sep=""),row.names=F)
			return(solutionValues)

		}
		paramCostNext[lessBudgetWhich[-length(lessBudgetWhich)]]<-paramCost[lessBudgetWhich[-length(lessBudgetWhich)]+1]
	} else{
		paramCostNext[lessBudgetWhich]<-paramCost[lessBudgetWhich+1]
	}

	# frontier solution will have the next row as
    #   due to how expand.grid structures its results
    #     perhaps excludes those solutions that within budget at the end of an expand grid block,
    #     but not within budget at the start of the next block - hence paramIterWhich loop
#	frontier<-ifelse((paramCostNext>budget & !is.na(paramCostNext)) |   # either beyond budget
#					(c(1:length(paramCost))%%paramCumProd[paramIterWhich[1]] == 0 & paramCost<=budget),   # is at maximum number of locations - axis1
#			1,NA)
#
#	frontierSolution<-cbind(paramGrid,paramCost)[which(frontier==1),]

   # remove frontier solutions within bounds of larger solutions
   paramCostMatrix<-cbind(paramCost,paramCostNext,matrix(rep(NA,length(paramCost)),ncol=length(paramIterWhich)-1,nrow=length(paramCost)))
   for(i in 1:(length(paramIterWhich)-1)){
		## out of bounds on second axis - second term of paramIterWhich
		#axisIterWhich<-which((c(1:length(paramCost))%%paramCumProd[paramIterWhich[i+1]]==0))
		lessWhich<-which((1:length(paramCost)) <= (length(paramCost)-paramCumProd[paramIterWhich[i]]))
		paramCostMatrix[lessWhich,i+2]<-paramCost[lessWhich+paramCumProd[paramIterWhich[i]]]
	}
	nIter<-length(paramIterWhich)+1
	paramMax<-apply(paramCostMatrix,1,function(x) sum(is.na(x[2:nIter]) |
						x[2:nIter]>=budget | x[1] > x[2:nIter]))
	frontier<-ifelse(paramMax==length(paramIterWhich) & paramCost<=budget,1,0)
	frontierSolution<-cbind(paramGrid,paramCost)[which(frontier==1),]

	# add other parameters to mix
	ff<-matrix(rep(scenario.matrix[scenarioIndex,9:ncol(scenario.matrix)],
					each=nrow(frontierSolution)),ncol=length(9:ncol(scenario.matrix)))
	ff<-as.data.frame(ff)
	names(ff)<-names(scenario.matrix)[9:ncol(scenario.matrix)]

	frontierSolution<-cbind(frontierSolution,ff)

	cat("number of solutions = ", sum(frontier,na.rm=TRUE),"\n")
	cat("try to get this below 500 by resetting initial parameter bounds","\n")

	# evaluate all the solutions for power for the given effect size
	solutionValues<- apply(frontierSolution,1,evaluatePower)  #evaluatePower(frontierSolution[1,])
	frontierSolution$Estimated.power=solutionValues

	# evaluate all the solutions for type 1 error
	frontierSolution.type1=frontierSolution
	frontierSolution.type1$effect=0
	solutionType1Values<- apply(frontierSolution.type1,1,evaluatePower)  #evaluatePower(frontierSolution[1,])
	frontierSolution$Estimated.typeI=solutionType1Values

	frontierSolution$Estimated.cost=apply(frontierSolution[,names(paramVec)],1,calcCost)
	frontierSolution<<-frontierSolution[order(frontierSolution$Estimated.power,
					decreasing=TRUE),]
	write.csv(frontierSolution,file=paste(gsub(".xlsx","",basename(excelInFile)),"_",fileName,
					"_orderedSolutions.csv",sep=""),row.names=F)
	#return(frontierSolution)
}

#' Transform ETA
#'
#' A function to transform between the response and link scales as required
#'
#' @param x - a number or numeric vector on which to apply the transformation
#'
#' @param variableType - the statsitical distirbution being used
#'
#' @param direction - whether to go from the "link" to the "response" scale
#'
#' @export
#' @return
#' A number or numeric vector of the transformed result
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}
#' @examples
#' xformEta(c(0.2,0.5,0.8),"binomial","link")
#' xformEta(c(-1,0,1),"binomial","response")

xformEta<-function(x,variableType,direction){

 if(direction=="response"){ # from the scale of the link function to the response
  eta=x
  if(variableType=="gaussian"){P=eta}
  if(variableType=="poisson"){P=exp(eta)}
  if(variableType=="gamma"){P=exp(eta)}
  if(variableType=="nbinomial"){P=exp(eta)}
  if(variableType=="binomial"){P=exp(eta) / (1 + exp(eta))}
  if(variableType=="beta"){P=exp(eta) / (1 + exp(eta))}
  return(P)
  }

 if(direction=="link"){  # from the response to the scale of the link function
  P=x
  if(variableType=="gaussian"){
    eta=P}
  if(variableType=="poisson"){
    eta=log(P)}
  if(variableType=="gamma"){
    eta=log(P)}
  if(variableType=="nbinomial"){
    eta=log(P)}
  if(variableType=="binomial"){
   eta=qlogis(P)}
  if(variableType=="beta"){
   eta=qlogis(P)}
  return(eta)
  }

}

#' supplyData
#'
#' An alternative to fitData() that allows the power analysis to be set up and run
#' entirely through R, without using the excel interface.
#' It generates the necessary dataComponents object required to run powerScenario,
#' which generates the necessary object scenarioParams to run function assessPower.
#'
#' @param dat A data.frame containing the pilot data to use in the analysis
#' @param variableType A character string indicating the distribution to use
#' for the respon variable. One "gaussian", "gamma", "poisson", "nbinomial", "binomial" or "beta"
#' @param   design.matrix A named list specifying the column headings to use from data that
#' correspond to different elements of the BACI design. Must contain:
#' Response (the column (heading) name in dat corresponding to the variable for which power is the be examined),
#' Trials (For a binomial response the column in dat indicating the number of trials),
#' Location, sublocation , Time, subtime,
#' BvA (the column (factor) in dat indicating data from "before" versus "after" the impact),
#' CvI (the column (factor) in dat indicating data from "control" (or reference) versus "impact" sites).
#' Elements not relevant to the design
#' must be included in the list, but may be specified as NA (e.g. subtime, sublocation).
#' @param levels.dat A named list containing the elements:
#' Before (the factor level in the BvA column of dat used to indicate the before data),
#' Control (the factor level in the CvI column of dat used to indicate data
#' for the control or reference location(s)),
#' After (the factor level in the BvA column of dat used to indicate the after data), and
#' Impact (the factor level in the CvI column of dat used to indicate data
#' for the impact location(s)).
#' @param scenario.data A named list containing the elements:
#' Number.of.iterations (the number of iterations to perform.
#' Note: It is recommended to run a small number (~5) initially
#' to confirm the simulation is working. Preliminary results
#' can be examined using ~100 iterations,
#' but a minimum of ~500 should be performed for final results),
#' filename="test" (a name for the simulation data to be saved as),
#' Number.of.Impact.Locations (the number of Impact Locations in the design),
#' Number.of.Control.Locations (the number of Control Locations in the design),
#' Number.of.sublocations.within.Location (the number of sublocations in the design.
#' Sublocation is nested within Location),
#' Number.of.sample.times.Before (the number of Before Times in the design),
#' Number.of.sample.times.After (the number of After Times in the design),
#' Number.of.subtimes.within.Time (the number of subtimes () in the design.
#' Subtime () is nested within Time, if there are no subtimes specify as 1),
#' Number.of.trials (the number of trials where data are a proportion or frequency,
#' and a binomial distribution is used),
#' Number.of.replicate.measurements (the number of replicates to use.
#' Note that a fixed replicate design is specified with replication = 1,
#' and the replicate ID assigned at "sublocation" above).
#' Separate multiple values to test using ";". E.g. 3;6;12.
#' @param effect.info A names list containing the elements:
#' Multiplicative (1, if a multiplicative effect is desired, otherwise 0),
#' Fixed.change (1, if a Fixed (additive) effect is desired, otherwise 0),
#' Effect.values=-0.3 (For a multiplicative effect: 0 = no effect; -0.1= a 10% decline;
#' +0.1 = a 10% increase etc.
#' For an absolute change in the fixed effect: 0 = no effect; +10 = an increase of 10 units;
#' and -10 = a decrease of 10 units).
#' Separate multiple effect size values to test using ";".
#' Note: "Multiplicative"
#' will multiply the response variable by the supplied value and then add this as the
#' "effect"; "Fixed" will add the supplied value to the response variable directly.
#' For either case the "impact" is applied only to "after" "impact" samples.
#' @param ncores the number of cores required for the analysis
#'
#' Unlike fitData(), the function supplyData() only generates the dataComponents
#' object and does not automatically call powerScenario, this step must be done
#' manually by the user.
#'
#' You will need to check the current approximate CPU load
#' on the available cores before implementing ncores > 1
#'
#' @export
#' @return A dataComponents list containing all the information required to run powerScenario.
#' @references
#' Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (in review) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution
#' @author Rebecca Fisher \email{r.fisher@@aims.gov.au}

supplyData <- function(dat,
                       variableType="gaussian",
                       design.matrix,
                       levels.dat,
                       scenario.data,
                       effect.info=list(Multiplicative=1,Fixed.change=0,Effect.values="0;-0.3"),
                       ncores=1) {
  # convert all factors to strings to be consistent with the excel interface extraction
  #dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)
  indx <- sapply(dat, is.factor)
  dat[indx] <- lapply(dat[indx], function(x) as.character(x))

  # variableType re format
  variableType<- data.frame(Factor="Response Distribution",
                            Value=variableType,stringsAsFactors=F)
  # design.matrix re format
  tt=do.call("rbind",design.matrix)
  design.matrix=data.frame(Factor=rownames(tt),Name=tt,stringsAsFactors=F)
  rownames(design.matrix)=1:nrow(design.matrix)

  # levels.dat re format
  tt=do.call("rbind",levels.dat)
  levels.dat=data.frame(Levels=rownames(tt),Code=tt,stringsAsFactors=F)
  rownames(levels.dat)=1:nrow(levels.dat)

  # get a filename variable from scenario.data
  excelInFile<<-scenario.data$filename

  # scenario.data re format
  scenario.data=c(list(Global.mean=NA),scenario.data,stringsAsFactors=F)
  tt=do.call("rbind",scenario.data)
  scenario.data=data.frame(Factor=rownames(tt),Value=tt,stringsAsFactors=F)
  rownames(scenario.data)=1:nrow(scenario.data)
  scenario.data$Factor=gsub("."," ",scenario.data$Factor,fixed=T)

  # effect.info re format
  effect.info=c(effect.info,list(alpha=NA))
  tt=do.call("rbind",effect.info)
  effect.info=data.frame(Effect.Type=rownames(tt),yes.1.no.0=tt,stringsAsFactors=F)
  rownames(effect.info)=1:nrow(effect.info)
  effect.info$Effect.Type=gsub("."," ",effect.info$Effect.Type,fixed=T)

  # ncores re format
  ncores<- data.frame(Cores="Number of cores",
                      n=ncores,stringsAsFactors=F)
 	#costResponsePars<-readWorksheet(wb,"design_specification",startRow=50,startCol=1,endRow=57,endCol=2)
	#paramCostBounds<-readWorksheet(wb,"design_specification",startRow=26,startCol=4,endRow=34,endCol=5)
  #keep.sim.dat<-unlist(readWorksheet(wb,"design_specification",startRow=59,startCol=2,endRow=60,endCol=2))=="Yes"
   keep.sim.dat<-FALSE
   names(keep.sim.dat)="Col1"



	## re-write the pilot.dat colnames using the standard factor names
	colnames(dat)=design.matrix$Factor[match(colnames(dat),design.matrix$Name)]


  ## define the response variable
  if(variableType$Value=="binomial" & length(unique(dat$Response))>2){
	  if(is.null(dat$Trials)) {
		 cat("you have specified a binomial response variable \n",
				 "outside the bounds of 0 and 1, without indicating \n",
         "the number of trials for each row. Please indicate which column \n",
         "in the response data contains the number of trials. \n",
         "If your data are between zero and one and you have no information on trials, \n",
         "a beta distribution might be appopropriate. \n"
         )
		return()
	  }else{
	   if(max(dat$Response)<=1) { # data in range 0-1, assume response is a proportion
                                # and convert to frequencies. This is required by INLA ##? add popup here with
                                # warning?
      dat$Response=dat$Response*dat$Trials}
    }
  }

	## define the factor variables
	factor.vars=design.matrix$Factor[-(1:2)]
	factor.vars.current=factor.vars[which(match(factor.vars,colnames(dat))>0)]
	factor.vars.missing=factor.vars[which(is.na(match(factor.vars,colnames(dat))))]

	## append the missing factor variables to the dat dataframe for simplicty
	missing.mat=matrix(1,nrow=nrow(dat),ncol=length(factor.vars.missing))
	colnames(missing.mat)=factor.vars.missing
	dat=cbind(dat,missing.mat)

	## ensure the factor variables are coded as factors
	## sapply(1:length(factor.vars), function(x, dat){ dat[,factor.vars[x]]<<-as.factor(dat[,factor.vars[x]])}, dat=dat)

	## ensure unique coding of relevant factors
	dat$Time.unique<-as.factor(paste(dat$BvA,dat$Time, sep="_"))
	dat$subtime.unique<-as.factor(paste(dat$BvA,dat$Time, dat$subtime, sep="_"))
	dat$Location.unique<-as.factor(paste(dat$CvI,dat$Location, sep="_"))
	dat$sublocation.unique<-as.factor(paste(dat$CvI,dat$Location,dat$sublocation, sep="_"))
#	dat[,Time.unique]=as.factor(paste(dat[,BvA],dat[,Time], sep="_"))
#	dat[,subtime.unique]=as.factor(paste(dat$CvI,dat[,Time], dat[,subtime], sep="_"))
#	dat[,Location.unique]=as.factor(paste(dat[,CvI],dat[,Location], sep="_"))
#
	## generate the necesary interactions
	dat$T.L=as.factor(paste(dat$Time.unique,dat$Location.unique,sep="_"))
	dat$T.subL=as.factor(paste(dat$Time.unique,dat$sublocation.unique,sep="_"))
	dat$L.subT=as.factor(paste(dat$Location.unique,dat$subtime.unique,sep="_"))
	dat$subL.subT=as.factor(paste(dat$sublocation.unique,dat$subtime.unique,sep="_"))

	after.code=levels.dat[which(levels.dat$Levels=="After"),"Code"]

	#detach("package:XLConnect", unload=TRUE) # rjava interface sometimes corrupts parallel processing

  # re-write level data
  dat$BvA[which(dat$BvA==levels.dat$Code[1])]=levels.dat$Levels[1]
  dat$BvA[which(dat$BvA==levels.dat$Code[3])]=levels.dat$Levels[3]
  dat$CvI[which(dat$CvI==levels.dat$Code[2])]=levels.dat$Levels[2]
  dat$CvI[which(dat$CvI==levels.dat$Code[4])]=levels.dat$Levels[4]

	costResponseNames<-NA#costResponsePars[,1]
	costResponsePars<-NA#as.list(costResponsePars[,2])
	#names(costResponsePars)<-costResponseNames

	variableType<-variableType[2]

	ncores<-ncores[2]

	# returned object is list
	dataComponents<-list("dat"=dat,
               "design.matrix"=design.matrix,
               "levels.dat"=levels.dat,
               "scenario.data"=scenario.data,
               "effect.info"=effect.info,
			         "factor.vars"=factor.vars,
               "factor.vars.current"=factor.vars.current,
               "factor.vars.missing"=factor.vars.missing,
               "after.code"=after.code,
               "ncores"=ncores,
               "costResponsePars"=costResponsePars,
               "variableType"=variableType,
               "paramCostBounds"=NA,#paramCostBounds,
               "keep.sim.dat"=keep.sim.dat)
  return(dataComponents)
 }

