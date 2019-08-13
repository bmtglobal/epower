# TODO: Add comment
# 
# Author: rohan
###############################################################################


#######
# This script includes the basic steps required to create and 
#    build the epower package from the file Toolbox_run.r

#install.packages(c("devtools","roxygen2","testthat","knitr"))
#install.packages(c("lintr","formatR"))
#install.packages("R.rsp")

# bleeding edge devtools
#devtools::install_github("hadley/devtools")
library(devtools)
#has_devel()  # check that devtools is correct
library(roxygen2)
#library(testhat)
library(knitr)
library(R.rsp)

# -----------------------
# for tidying up formatting in R
#lintr::lint_package()
#http://yihui.name/formatR/
#formatR::tidy_dir("R")

#devtools::session_info()

# -----------------------
# create  a new package
#devtools::create("version1.3/epower")
#hg add epower/*
#hg commit -m 'Added newly created package template'

# copy source code to R directory
#cp Toolbox_run.r epower/R"
#hg add epower/R/Toolbox_run.r 
#hg commit -m 'Added Toolbox_run.r to package R directory'

# -----------------------
#  build documents for the package
#setwd("/mnt/business/workspace/Clients/BMTOceania/epower/version1.3/epower")
setwd("C:/Users/rfisher/OneDrive - Australian Institute of Marine Science/Documents/AIMS/EcologicalRiskModelling/EPower/epower/version1.3/epower")

# copy toolbox_run.r over to epower/R

devtools::document()
#hg add epower/man/*
#hg commit -m 'R help files added'

devtools::use_package("doParallel")
devtools::use_package("XLConnect")
devtools::use_package("INLA")
#http://www.math.ntnu.no/inla/R/testing/src/contrib/
#sudo R CMD INSTALL INLA_0.0-1480869339.tar.gz 

# -------------------------
# add static vignette pdf
#pico vignette/E-Power_ManualV1.3_JM_rf.pdf.asis
#%\VignetteIndexEntry{epower}
#%\VignetteEngine{R.rsp::asis}

#hg pull https://bitbucket.org/rebeccafisher76/bmtoceanica
#hg update
#hg push https://bitbucket.org/rebeccafisher76/bmtoceanica

# ---------------
# manually update the DESCRIPTION file 
#   see DESCRIPTION template below.

# ----------------
# build packages
devtools::build()

# ---------
# STEPS
# 1) edit Toolbox_run.r
# 2) copy Toolbox_run.r to epower/R
# 3) devtools::document()
# 4) devtools::build()
# 5) copy built package to dropbox

# ---------
# windows
#  setup: https://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html
# R CMD INSTALL --build epower_1.3.tar.gz
# ensure correct java, compatible with R 64 bit version is installed
# ensure Rtools is in path, after download

# ----------------
## DESCRIPTION v 1.3
#Package: epower
#Title: Power analysis for beyond BACI sampling designs
#Version: 1.3
#Authors@R: c(person("Rebecca", "Fisher", email = "r.fisher@@aims.gov.au", role = c("aut", "cre")),person("Glenn","Shiell",role="aut"),person("Rohan","Sadler",role="aut"),person("James","McGree",role="aut"))
#Description: epower conducts a power analysis on beyond BACI sampling
#designs through INLA based inference and Monte Carlo simulation. Currently epower considers only univariate responses. The analysis allows for cross random effects between \code{Time} and \code{Site} in the design. The epow toolbox has been designed for use by ecologists with a sound understanding of the statistical principles of BACI designs. To assist the non-R user BACI parameters are defined in a Excel spreadsheet located in the package DOC folder: E-Power_interface_V1.3.xlsx
#Depends:
#		R (>= 3.3.2)
#License: TBA
#Encoding: UTF-8
#LazyData: true
#RoxygenNote: 5.0.1
#Imports: foreach,
#doParallel,
#XLConnect,
#INLA
#Suggests: R.rsp
#VignetteBuilder: R.rsp

