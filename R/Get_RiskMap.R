#' Download the Map of Risks of Italian Municipality
#'
#' @description  This functions downloads a broad set of indicators covering several categories of risks to which Italian municipalities are exposed. It includes mostly geographical, demographic and urbanistic information. The dataset is static and it dates back to Jan 1st 2018.
#'
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param metadata Logical. If \code{TRUE}, the function returns also the list of variables with the relevant explanations. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#' @examples
#'
#'
#'
#' \donttest{
#'   Get_RiskMap(autoAbort = TRUE)[-c(1:5)]
#' }
#'
#' @source <https://www.istat.it/mappa-rischi/getReport.php>
#' @return By default, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @export
#'
#'

Get_RiskMap <- function(verbose=TRUE, metadata = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)


  start.zero <- Sys.time()
  url <- "https://www.istat.it/mappa-rischi/getReport.php"
  status <- 0
  while(status != 200){

    response <- tryCatch({
      httr::GET(url, query = list(
        "paramsSelected[dateFrom]" = "01/01/2018","paramsSelected[fileType]" = "xlsx"))
    }, error = function(e) {
      message("Error occurred during scraping, attempt repeated ... \n")
      NULL
    })
    status <- response$status_code
    if(is.null(response)){
      status <- 0
    }
    if(status != 200){
      message("Operation exited with status: ", status, "; operation repeated")
    }
  }
  temp <- tempdir()
  if(!dir.exists(temp)){
    dir.create(temp)
  }

  excel <- paste0(temp, "/tempdata.xlsx")
  writeBin(as.vector(response$content), excel)
  DB <- readxl::read_xlsx(excel, sheet = 1)

  colsnumeric <- c(9, 13:26, 28, 30, 32, 33, 35, 37:134, 136, 138, 139, 141, 143:393, 395, 396, 397)
  names(DB)[c(1:8)] <-  stringr::str_to_title(names(DB)[c(1:8)])
  DB[,colsnumeric] <- DB[,colsnumeric] %>% apply(MARGIN = 2, FUN = function(x){
    x <- x %>% stringr::str_replace_all("--", "0") %>% stringr::str_replace_all(",", ".") %>% as.numeric()
  })

  DB <- DB %>%  dplyr::rename(Region_code = .data$Id_regione, Region_description = .data$Dzreg,
                        Province_code = .data$Codpro, Province_description = .data$Dzpro,
                        Municipality_code = .data$Procom, Municipality_description = .data$Dzcom) %>%
    dplyr::mutate(dplyr::across(.data$Data_rif, ~ as.Date("2018-01-01")))

  if(metadata){
    metadata.df <- c(readxl::read_xlsx(excel, sheet = 2))
    res <- list(data = DB, metadata = metadata.df)
  } else res <- DB

  unlink(excel, recursive = TRUE, force = TRUE)

  endtime <- Sys.time()
  if(verbose){
    cat(difftime(endtime, start.zero, units = "secs"), "Seconds needed to retrieve the municipalities risk map \n")
  }
  return(res)
}
