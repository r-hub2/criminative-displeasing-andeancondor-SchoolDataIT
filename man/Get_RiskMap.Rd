% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Get_RiskMap.R
\name{Get_RiskMap}
\alias{Get_RiskMap}
\title{Download the Map of Risks of Italian Municipality}
\source{
<https://www.istat.it/mappa-rischi/getReport.php>
}
\usage{
Get_RiskMap(verbose = TRUE, metadata = FALSE, autoAbort = FALSE)
}
\arguments{
\item{verbose}{Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.}

\item{metadata}{Logical. If \code{TRUE}, the function returns also the list of variables with the relevant explanations. \code{FALSE} by default.}

\item{autoAbort}{Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.}
}
\value{
By default, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
}
\description{
This functions downloads a broad set of indicators covering several categories of risks to which Italian municipalities are exposed. It includes mostly geographical, demographic and urbanistic information. The dataset is static and it dates back to Jan 1st 2018.
}
\examples{



\donttest{
  Get_RiskMap(autoAbort = TRUE)[-c(1:5)]
}

}
