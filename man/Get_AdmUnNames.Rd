% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Get_AdmUnNames.R
\name{Get_AdmUnNames}
\alias{Get_AdmUnNames}
\title{Download the names and codes of Italian LAU and NUTS-3 administrative units}
\source{
<https://www.istat.it/it/archivio/6789>
}
\usage{
Get_AdmUnNames(Year = 2023, date = "01_01_", autoAbort = FALSE)
}
\arguments{
\item{Year}{Numeric or character value. Last available is 2024.
For coherence with school data, it is also in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}. \code{2023} by default.}

\item{date}{Character. The reference date, in format \code{"dd_mm_"}. Usually, the administrative codes are available at 01/01, at 06/30 and 12/31 of every year. \code{"01_01_"} by default.}

\item{autoAbort}{Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.}
}
\value{
An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, including: NUTS-3 code, NUTS-3 abbreviation,
LAU code, LAU name (description) and cadastral code. All variables are characters except for the NUTS-3 code.
}
\description{
This function downloads a file provided by the Italian National Institute of Statistics including all the codes of administrative units in Italy. As of today, it is the easiest way to map directly cadastral codes to municipality codes.
}
\examples{


\donttest{
  Get_AdmUnNames(2024, autoAbort = TRUE)
}

}
