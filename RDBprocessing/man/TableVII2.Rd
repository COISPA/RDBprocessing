% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TableVII2.R
\name{TableVII2}
\alias{TableVII2}
\title{TableVII2 - GFCM DCRF datacall}
\usage{
TableVII2(datacs, datacl, verbose = F)
}
\arguments{
\item{datacs}{Detailed data in RCG CS format}

\item{datacl}{Landings aggregated data in RCG CL format}

\item{verbose}{boolean. If TRUE a message is printed.}
}
\value{
TableVII2
}
\description{
TableVII2 - GFCM DCRF datacall
}
\examples{
library(COSTcore)
TableVII2(RDBprocessing::data_ex,RDBprocessing::data_exampleCL)
}
