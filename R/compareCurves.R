#' Compares two cell survival curves using ANOVA.
#'
#' It helps compare two cell survival curves using the
#' \code{\link[CFAssay:cellsurvLQdiff]{cellsurvLQdiff}} function of the CFAssay
#' package, without having the user go through the data wrangling steps
#' necessary for that. Visit
#' \url{https://bioconductor.org/packages/release/bioc/html/CFAssay.html} for
#' more details about the method.
#'
#' @param data A data frame containing at least the following five columns:
#'   "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param cline1 Name of the cell-line/group that you want to compare.
#' @param cline2 Name of another cell-line/group that you want to compare.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood)
#'   by default. Can be \code{"ls"} (least squares) or \code{"franken"}
#'   (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"}
#'   calculates fitted plating efficiencies as model parameters, \code{"fix"}
#'   uses fixed ones calculated from the observed zero dose data.
#' @return An object of class \code{cellsurvLQdiff}, as returned by
#'   \code{\link[CFAssay:cellsurvLQdiff]{cellsurvLQdiff}}.
#' @examples
#' datatab <- CASP8_data
#' compareCurves(datatab, "shCASP8-N", "shCASP8-B+Z+N")
#' compareCurves(datatab, "shCASP8-N", "shCASP8-B+Z+N", method = "ls", PEmethod = "fix")
#' @export
compareCurves <- function(data, cline1, cline2, method = "ml", PEmethod = "fit") {
  invisible(utils::capture.output(fit <- CFAssay::cellsurvLQdiff(data[c(which(data$cline == cline1), which(data$cline == cline2)),], curvevar = "cline", method = method, PEmethod = PEmethod)))
  intro <- paste0("****** Cell type 1: ", cline1, "   ||   Cell type 2: ", cline2, " ******\n")
  end <- paste("\n*** Analysis by CellSurvAssay v1.0.0 ***")
  cat(intro)
  print(fit)
  cat(end)
}
