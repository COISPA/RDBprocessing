% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LAND_MEDBS.R
\name{LAND_MEDBS}
\alias{LAND_MEDBS}
\title{Landing by length (LANDINGS) table - MED & BS data call}
\usage{
LAND_MEDBS(datacs, datacl, verbose = FALSE)
}
\arguments{
\item{datacs}{Detailed data in RCG CS format}

\item{datacl}{Landings aggregated data in RCG CL format}

\item{verbose}{boolean. If TRUE a message is printed.}
}
\value{
LANDINGS table
}
\description{
Landing by length (LANDINGS) table - MED & BS data call
}
\examples{
LAND_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL)
}
