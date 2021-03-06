% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Toolbox_run.r
\name{buildScenarioMatrix}
\alias{buildScenarioMatrix}
\title{Build scenario matrix}
\usage{
buildScenarioMatrix(params = scenarioParams)
}
\arguments{
\item{params}{scenarioParams (as generated by fitData)}
}
\value{
A simple matrix of scenarios is built
}
\description{
Build matrix of sampling scenarios for which power is to be calculated
}
\examples{
scenario.matrix<-buildScenarioMatrix(params=scenarioParams)
}
\references{
Fisher R, Shiell GR, Sadler RJ, Inostroza K, Shedrawi G, Holmes TH, McGree JM (2019) epower: an R package for power analysis of Before-After-Control-Impact (BACI) designs. Methods in Ecology and Evolution.
}
\author{
Rebecca Fisher \email{r.fisher@aims.gov.au}
}
