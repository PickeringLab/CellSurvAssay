utils::globalVariables(c("cline", "alpha", "d", "pts", "ctype", "pts_minus_sems", "pts_plus_sems", "yhat", "y_hat"))

.sfpmean_mod <- function (X, S0 = NULL)
{
  sf.mean <- function(X) {
    v <- X$ncells * X$S0
    fit <- stats::glm(X$ncolonies ~ v - 1, family = stats::quasipoisson(link = "identity"),
                      data = X)
    if (summary(fit)$dispersion < 1)
      fit <- stats::glm(X$ncolonies ~ v - 1, family = stats::poisson(link = "identity"),
                        data = X)
    summary(fit)$coef
  }
  dose <- NULL
  if (is.null(S0) & !("pe" %in% names(X)))
    stop("In function .sfpmean_mod: 'S0' has to be given in argument list or 'pe' has to be a column in the data frame! See help document.")
  doses <- unique(X$dose)
  if (is.null(S0)) {
    pmean <- sapply(1:length(doses), function(i) {
      X1 <- subset(data.frame(X, S0 = X$pe), dose == doses[i])
      sf.mean(X1)
    })
  }
  if (!is.null(S0)) {
    if (is.null(names(S0)))
      stop("S0 is not a named vector!")
    if (length(grep("(Exp)", names(S0))) == length(unique(X$Exp))) {
      ExpNames <- sapply(1:length(S0), function(i) strsplit(names(S0)[i],
                                                            ")")[[1]][2])
    }
    else {
      ExpNames <- as.numeric(names(S0))
    }
    if (!all(sort(unique(X$Exp)) == sort(ExpNames)))
      stop("Mismatch of experiment names in data frame and names of S0!")
    S01 <- sapply(1:nrow(X), function(i) S0[X$Exp[i]])
    X$S0 <- S01
    pmean <- sapply(1:length(doses), function(i) {
      X1 <- subset(X, dose == doses[i])
      sf.mean(X1)
    })
  }
  colnames(pmean) <- paste("dose_", doses, sep = "")
  rownames(pmean) <- 1:nrow(pmean)
  rownames(pmean)[1:2] <- c("SF", "stdev")
  pmean[1:2, ]
}




.dfforggPlot <- function(datatable, cell_type, method, PEmethod) {
  alpha <- c()
  beta <- c()
  pts_minus_sems <- c()
  pts_plus_sems <- c()
  points <- c()
  subset_data <- subset(datatable, cline == cell_type)
  invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset_data, method = method, PEmethod = PEmethod)))
  x <- fit
  data <- fit$data
  data$dose2 <- data$dose^2
  data$lcells <- log(data$ncells)
  uexp <- unique(data$Exp)
  doses <- unique(data$dose)
  maxd <- max(doses)
  b <- fit$coef[c("dose", "dose2")]
  if (0 %in% doses) {
    S0 <- CFAssay::pes(data)$S0
    names(S0) <- rownames(CFAssay::pes(data))
    meanSF <- .sfpmean_mod(data, S0)
  }
  if (!(0 %in% doses)) {
    data$pe <- exp(data$logPle)
    meanSF <- .sfpmean_mod(data)
  }
  pts <- meanSF[1, ]
  sems <- meanSF[2, ]
  points <- c(points, pts)
  pts_plus_sems <- c(pts + sems)
  pts_minus_sems <- c(pts - sems)
  alpha <- rep(b[1], times = length(doses))
  beta <- rep(b[2], times = length(doses))
  ctype <- rep(cell_type, times = length(doses))
  df <- data.frame(ctype, alpha, beta, doses, pts, pts_plus_sems, pts_minus_sems)
  rownames(df) <- NULL
  return(df)
}


.quadraticFunction <- function(a, b, c) {
  if(delta(a, b, c) > 0) {
    x_1 = (-b + sqrt(delta(a, b, c)))/(2*a)
    x_2 = (-b - sqrt(delta(a, b, c)))/(2*a)
    if (x_1 >= 0 & x_2 >= 0) {
      z <- readline(prompt = paste("There are two positive roots: a)", x_1, "& b)", x_2, ". Which one do you want the DER for? (please enter a/b): "))
      if (z == "a") {
        result <- x_1
      }else if (z == "b") {
        result <- x_2
      }else {
        print("Invalid entry! Please try again.")
      }
    }else if (x_1 >= 0 & x_2 < 0) {
      result <- x_1
    }else if(x_2 >= 0 & x_1 < 0) {
      result <- x_2
    }else {
      result <- NA
    }
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    result <- -b/(2*a)
  }
  else {result <- NA} # third case D<0
}


delta <- function(a,b,c) {
  b^2-4*a*c
}



