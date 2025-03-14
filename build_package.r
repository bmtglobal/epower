# TODO: Add comment
#
# Author: rohan - later modified by becky
###############################################################################


#######
# This script includes the basic steps required to create and
#    build the epower package from the file Toolbox_run.r

library(devtools)
library(roxygen2)
library(knitr)
library(R.rsp)


# -----------------------
#  build documents for the package
#setwd("/mnt/business/workspace/Clients/BMTOceania/epower/version1.3/epower")
#setwd("C:/Users/rfisher/OneDrive - Australian Institute of Marine Science/Documents/AIMS/EcologicalRiskModelling/EPower/epower")

devtools::document()

use_package("doParallel")
use_package("XLConnect", type="Suggests")
use_package("openxlsx", type="Suggests")
use_package("INLA")

# ----------------
# build packages
devtools::build()
