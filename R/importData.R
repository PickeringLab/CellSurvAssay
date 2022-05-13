#' Imports the data
#'
#' It helps import the data set, whether it's in a tsv, csv, txt, or excel
#' format.
#'
#' @param filepath Path of the file to be imported.
#' @param filetype The type of file to be imported. By default, it's
#'   \code{"excel"}, but also accepts \code{"tsv"}, \code{"csv"}, or
#'   \code{"txt"}.
#' @param separator The delimiter in your data set, if it's a \code{"txt"} file.
#' @param ... Any other argument that can be passed in the
#'   \code{\link[readr]{read_csv}}, \code{\link[readr]{read_tsv}},
#'   \code{\link[readr]{read_delim}}, or \code{\link[readxl]{read_excel}}
#'   functions, if your data set is a csv, tsv, txt, or an excel file,
#'   respectively.
#' @return A data frame
#' @examples
#' \dontrun{
#' # If it's an excel file
#' importData("path/to/file")
#'
#' # If it's a csv file
#' importData("path/to/file", filetype = "csv")
#'
#' # If it's a txt file with each observation separated by a "|"
#' importData("path/to/file", filetype = "txt", separator = "|")
#' }
#' @export
importData <- function(filepath, filetype = "excel", separator = NULL, ...) {
  if (filetype == "csv") {
    datatab <- readr::read_csv(filepath, ...)
  }else if (filetype == "tsv") {
    datatab <- readr::read_tsv(filepath, ...)
  } else if(filetype == "excel") {
    datatab <- readxl::read_excel(filepath, ...)
  } else {
    datatab <- readr::read_delim(filepath, delim = separator, ...)
  }

  j <- 0
  for (i in 1:5) {
    if (!c("cline", "Exp", "dose", "ncells", "ncolonies")[i] %in% colnames(datatab)) {
      j <- j + 1
    }
  }
  if (j > 0) {
    warning("Error! Your column names don't match with the requirements. Please refer to the help file or the vignette for more details.")
  }
  return(as.data.frame(datatab))
}
