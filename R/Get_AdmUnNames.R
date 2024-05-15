#' Download the names and codes of Italian LAU and NUTS-3 administrative units
#'
#' @description This function downloads a file provided by the Italian National Institute of Statistics including all the codes of administrative units in Italy. As of today, it is the easiest way to map directly cadastral codes to municipality codes.
#'
#' @param Year Numeric or character value. Last available is 2024.
#' For coherence with school data, it is also in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}. \code{2023} by default.
#' @param date Character. The reference date, in format \code{"dd_mm_"}. Usually, the administrative codes are available at 01/01, at 06/30 and 12/31 of every year. \code{"01_01_"} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, including: NUTS-3 code, NUTS-3 abbreviation,
#' LAU code, LAU name (description) and cadastral code. All variables are characters except for the NUTS-3 code.
#'
#' @examples
#'
#'
#' \donttest{
#'   Get_AdmUnNames(2024, autoAbort = TRUE)
#' }
#'
#' @source <https://www.istat.it/it/archivio/6789>
#'
#' @export



Get_AdmUnNames <- function(Year = 2023, date = "01_01_", autoAbort = FALSE){

  #utils::globalVariables(".", package = "SchoolDataIT", add = FALSE)

  if(!Check_connection(autoAbort = autoAbort)) return(NULL)

  pattern0 <- "Archivio-elenco-comuni-codici-e-denominazioni_Anni_"
  pattern1<- ifelse(year.patternA(Year) %in% c(
    "202122", "202223", "202324"), "2022-2024.zip", ifelse(year.patternA(Year) %in% c(
      "201415", "201516","201617", "201718", "201819", "201920", "202021"),
      "2015-2021.zip", ifelse(year.patternA(Year) %in% c(
        "200708", "200809", "200910", "201011", "201112", "201213", "201314"),
        "2008-2014.zip",  NA ) ) )
  if(is.na(pattern1)) {
    message("It seems there are no data for year: ", Year, "; We apologise for the inconvenience")
    return(NULL)
  }

  pattern <- paste0(pattern0, pattern1)

  home.ISTAT <- "https://www.istat.it/it/archivio/6789"
  homepage <- NULL
  attempt <- 0
  while(is.null(homepage) && attempt <= 10){
    homepage <- tryCatch({
      xml2::read_html(home.ISTAT)
    }, error = function(e){
      message("Cannot read the html; ", 10 - attempt,
              " attempts left. If the problem persists, please contact the mantainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) return(NULL)
  link <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  link <- grep(pattern, link, value = TRUE)
  if(length(link) == 0L){
    message("Administrative units names not found for date: ", paste0(date, Year))
    return(NULL)
  }

  base.url <- dirname(home.ISTAT)
  file.url <- xml2::url_absolute(link, base.url)

  temp1 <- tempfile()
  temp2 <- tempfile()
  temp1 <- paste0(temp1, ".zip")
  x <- tryCatch({
    utils::download.file(url = file.url, destfile = temp1, mode = "wb")},
    error = function(e) {
      NULL
    })

  if(is.null(x)){
    unlink(temp1, recursive = TRUE, force = TRUE)
    unlink(temp2, recursive = TRUE, force = TRUE)
    return(NULL)
  }
  utils::unzip(zipfile = temp1, exdir = temp2)

  files.date <- paste0(date, as.numeric(substr(year.patternA(Year),1,4))+1)
  excel <- list.files(list.files(temp2, full.names = TRUE), full.names = TRUE)
  excel <- grep(files.date, excel, value = TRUE)

  sheetnum <- which(grepl("CODICI", readxl::excel_sheets(excel)))
  ext <- substr(excel, regexpr(files.date, excel)[[1]]+nchar(files.date)+1,
                nchar(excel))
  if(ext == "xlsx"){
    suppressWarnings(res <- readxl::read_xlsx(excel, sheet = sheetnum))
  } else if(ext == "xls"){
    suppressWarnings(res <- readxl::read_xls(excel, sheet = sheetnum))
  } else{
    message(paste("problematic file extension:"), ext)
    return(NULL)
  }


  names(res) <- stringr::str_remove_all(names(res), "\\s*\\(.*?$")

  res <- res %>%
    dplyr::select(.data$`Codice Provincia`, .data$`Sigla automobilistica`,
                  .data$`Progressivo del Comune`, .data$`Denominazione in italiano`,
                  .data$`Codice Catastale del comune`) %>%
    rename_by_idx(c(1:5), into = c("Province_code", "Province_initials",
                                   "Municipality_code","Municipality_description", "Cadastral_code"))
  if(is.numeric(res$Province_code)) {
    res$Province_code <- sprintf(res$Province_code, "%03d")
  }
  if(is.numeric(res$Municipality_code)){
    res$Municipality_code <- sprintf(res$Municipality_code, "%03d")
  }
  res <- res %>%  dplyr::mutate(Municipality_code =
    paste0(.data$Province_code, .data$Municipality_code))
  res$Province_code <- as.numeric(res$Province_code)

  unlink(temp1, recursive = TRUE, force = TRUE)
  unlink(temp2, recursive = TRUE, force = TRUE)

  return(res)
}
