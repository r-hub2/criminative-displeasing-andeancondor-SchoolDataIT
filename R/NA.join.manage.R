#' @keywords internal
#'
NA.join.manage <- function(data, lastcol, NA_autoRM = NULL){
  na.rm <- NULL
  if(!is.null(NA_autoRM)){
    if(NA_autoRM %in% c("Y", "y", "YES", "yes", "Yes", 1, "1", TRUE, "TRUE")){
      na.rm <- "Y"
    } else na.rm <- "N"
  } else {
    cat("Do you want to clean NA values? \n",
          "    - To remove NAs, please press 'Y'\n",
          "    - To abort the operation and cancel the join, please press 'A'\n",
          "    - To keep the NAs, press any other key \n",
          "(please, do not use quotes in the prompt) \n")
    na.rm <- readline(prompt = "  > ")
  }

  if (toupper(na.rm) == "Y") {
    cat("NAs deleted \n")
    data <- data[-which(apply(data[-c(1:lastcol)], MARGIN = 1, function(x) all(is.na(x)))), ]
  } else if (toupper(na.rm) == "A"){
    data <- data[,c(1:lastcol)]
    cat("You chose to abort the join \n")
  } else {
    cat("NAs kept in \n")
  }

  return(data)
}
