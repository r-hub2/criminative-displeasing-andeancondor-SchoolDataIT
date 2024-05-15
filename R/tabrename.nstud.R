#' @keywords internal
#'
tabrename.nstud <- function(){
  manualdf <- data.frame(
    Input = c("ANNOSCOLASTICO", "CODICESCUOLA", "ORDINESCUOLA", "ANNOCORSO", "FASCIAETA",
              "ALUNNI", "CLASSI", "ALUNNIMASCHI", "ALUNNIFEMMINE" ),
    Readable = c("Year", "School_code", "Order", "Grade", "Age_class",
                 "Students", "Classes", "Male_students", "Female_students") )
  return(manualdf)
}
