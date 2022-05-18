#' Calculates the Dose Enhancement Ratio
#'
#' It helps you calculate the Dose Enhancement Ratio automatically without going
#' through any data wrangling steps or calculation.
#'
#' @param data A data frame containing at least the following five columns:
#'   "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param control Name of the cell-line/group that will act as control
#'   (numerator in the ratio).
#' @param treatment Name of the cell-line/group that will act as treatment
#'   (denominator in the ratio).
#' @param S The survival fraction value for which you want to calculate your
#'   DER.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood)
#'   by default. Can be \code{"ls"} (least squares) or \code{"franken"}
#'   (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"}
#'   calculates fitted plating efficiencies as model parameters, \code{"fix"}
#'   uses fixed ones calculated from the observed zero dose data.
#' @return The Dose Enhancement Ratio of Control:Treatment.
#' @examples
#' datatab <- CASP8_data
#' calculateDER(datatab, "shCASP8-NT", "shCASP8-N", 0.25)
#' calculateDER(datatab, "shCASP8-NT", "shCASP8-N", 0.25, method = "ls", PEmethod = "fix")
#' @export
calculateDER <- function(data, control, treatment, S, method = "ml", PEmethod = "fit") {


  control_data <- subset(data, cline == control)
  treatment_data <- subset(data, cline == treatment)
  invisible(utils::capture.output(fit_control <- CFAssay::cellsurvLQfit(control_data, method = method, PEmethod = PEmethod)))
  invisible(utils::capture.output(fit_treatment <- CFAssay::cellsurvLQfit(treatment_data, method = method, PEmethod = PEmethod)))
  alpha_control <- fit_control$coefficients[["dose"]] * (-1)
  beta_control <- fit_control$coefficients[["dose2"]] * (-1)
  alpha_treatment <- fit_treatment$coefficients[["dose"]] * (-1)
  beta_treatment <- fit_treatment$coefficients[["dose2"]] * (-1)
  D_control <- .quadraticFunction(beta_control, alpha_control, log(S))
  D_treatment <- .quadraticFunction(beta_treatment, alpha_treatment, log(S))
  DER <- D_control/D_treatment
  DER <- D_control/D_treatment
  results <- paste0("*** Dose enhancement Ratio ***\n", "control = ", control, "\ntreatment = ", treatment,
                    "\nsurvival fraction = ", S, "\nmethod = ", method, "\npe method = ", PEmethod,
                    "\nDER = ", DER,
                    "\n\n*** Analysis by CellSurvAssay v1.0.0 ***")
  cat(results)
}
