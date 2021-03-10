get_z_trans <- function(mean=NULL, sd=NULL, x=NULL, z=NULL, pop=F, symbol="i") {
  label_x       <- sprintf("x_{%s}", symbol)
  label_z       <- sprintf("z_{%s}", symbol)
  label_mean    <- ifelse(pop, "\\mu", "\\bar{x}")
  label_sd      <- ifelse(pop, "\\sigma", "s")
  result        <- list()
  result$formel <- "$%s = \\frac{%s - %s}{%s}$" %>%
    sprintf(label_z, label_x, label_mean, label_sd)
  if(length(c(mean, sd, x))==3) {
    result$einsetzen <- "$%s = \\frac{%s - %s}{%s}$" %>%
      sprintf(label_z, x, mean, sd) %>% fix_formula
    result$raw      <- round((x-mean)/sd, 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s\\approx%s$", label_z, result$fmt)
    result$quick    <- "$%s=\\frac{%s-%s}{%s}\\approx%s$" %>%
      sprintf(label_z, x, mean, sd, result$fmt) %>% fix_formula
  } else if (length(c(mean, sd, z))==3) {
    result$formel <- "$%s = %s \\cdot %s + %s$" %>%
      sprintf(label_x, label_z, label_sd, label_mean)
    result$einsetzen <- "$%s = %s \\cdot %s + %s$" %>%
      sprintf(label_x, z, sd, mean) %>%
      fix_formula
    result$raw <- round(z*sd + mean, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s\\approx%s$", label_x, result$fmt)
  }
  return(result)
}

get_p_over <- function(z) {
  result <- list()
  result$formel <- "$p = P(z \\gt %s)$" %>%
    sprintf(z) %>%
    fix_formula
  if(z > 0) {
    result$umformen <- "$p = 1-P(z \\leq %s)$" %>%
      sprintf(z) %>%
      fix_formula
    under <- round(pnorm(round(z,2)), 4)
    result$einsetzen <- "$p \\approx 1-%s$" %>%
      sprintf(under) %>%
      fix_formula
    result$raw <- 1-under
    result$fmt <- fmt(result$raw, 4)
    result$ergebnis <- sprintf("$p \\approx %s$", result$fmt)
  } else stop("z < 0 not yet implemented")
  return(result)
}

get_p_under <- function(z) {
  result <- list()
  result$formel <- "$p=P(z \\leq %s)$" %>%
    sprintf(z) %>%
    fix_formula
  if(z > 0) {
    result$raw <- round(pnorm(round(z,2)), 4)
    result$fmt <- fmt(result$raw, 4)
    result$ergebnis <- sprintf("$p \\approx %s$", result$fmt)
  } else {
    result$umformen <- "$p=1-P(z \\leq %s)$" %>%
      sprintf(-z) %>%
      fix_formula
    p_negative <- round(pnorm(round(-z,2)), 4)
    result$einsetzen <- "$p \\approx 1-%s$" %>%
      sprintf(p_negative) %>%
      fix_formula
    result$raw <- 1-p_negative
    result$fmt <- fmt(result$raw, 4)
    result$ergebnis <- sprintf("$p \\approx %s$", result$fmt)
  }
  return(result)
}

get_z_under <- function(p, symbol="i") {
  label <- sprintf("z_{%s}", symbol)
  result <- list()
  result$formel <- "$P(z \\leq %s) = %s$" %>%
    sprintf(label, p) %>%
    fix_formula
  if (p < 0.5) {
    result$umformen  <- "$P(z \\leq -%s) = 1-%s = %s$" %>%
      sprintf(label, p, 1-p) %>%
      fix_formula
    negative <- round(qnorm(round(1-p,4)), 2)
    result$einsetzen <- sprintf("$-%s \\approx %s$", label, negative) %>%
      fix_formula
    result$raw <- -negative
    result$fmt <- fmt(-negative)
    result$ergebnis <- sprintf("$%s \\approx %s$", label, result$fmt)
  } else {
    result$raw <- round(qnorm(round(p, 4)), 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s \\approx %s$", label, result$fmt)
  }
  return(result)
}

get_stderr <- function(data=NULL, sigma=NULL, n=NULL, stderr=NULL) {
  result <- list()
  result$formel <- "$\\sigma_{\\bar{x}}=\\frac{\\sigma}{\\sqrt{n}}$"
  if(!is.null(data)) {
    if(is.null(n)) n <- length(data)
  }
  if(length(c(n, sigma))==2) {
    result$einsetzen <- "$\\sigma_{\\bar{x}}=\\frac{%s}{\\sqrt{%s}}$" %>%
      sprintf(sigma, n) %>%
      fix_formula
    result$raw <- round(sigma/sqrt(n), 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- "$\\sigma_{\\bar{x}}\\approx%s$" %>%
      sprintf(result$fmt)
  } else if(length(c(sigma, stderr))==2) {
    result$umformen <- "$n=\\Big(\\frac{\\sigma}{\\sigma_{\\bar{x}}}\\Big)^2$"
    result$einsetzen <- "$n=\\Big(\\frac{%s}{%s}\\Big)^2$" %>%
      sprintf(sigma, stderr) %>%
      fix_formula
    result$raw <- round((sigma/stderr)^2,2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- "$n\\approx%s$" %>%
      sprintf(result$fmt)
  }
  return(result)
}

get_intervall <- function(stderr=NULL, kib2=NULL, alpha=NULL) {
  result <- list()
  result$formel <- "$\\frac{\\mathit{KIB}}{2} = z_{(1-\\alpha/2)} \\cdot \\sigma_{\\bar{x}}$"
  if(length(c(alpha, stderr)) == 2) {
    result$einsetzen <- "$\\frac{\\mathit{KIB}}{2} = z_{%s\\%%} \\cdot \\sigma_{\\bar{x}} \\approx %s \\cdot %s$" %>%
      sprintf(100-alpha/2*100,  round(qnorm(1-alpha/2),2), stderr) %>%
      fix_formula
    result$raw <- round(round(qnorm(1-alpha/2),2)*stderr,2)
    result$fmt <- (fmt(result$raw))
    result$ergebnis <- "$\\frac{\\mathit{KIB}}{2} \\approx %s$" %>%
      sprintf(result$fmt)
  } else if(length(c(kib2, stderr))==2) {
    stop("Getting alpha not implemented yet")
  } else if(length(c(kib2, alpha))==2) {
    result$umformen <- "$\\sigma_{\\bar{x}} = \\frac{\\mathit{KIB}}{2} \\cdot \\frac{1}{z_{(1-\\alpha/2)}}$"
    result$einsetzen <- "$\\sigma_{\\bar{x}}=\\frac{\\mathit{KIB}}{2}\\cdot \\frac{1}{z_{%s\\%%}} = %s \\cdot \\frac{1}{%s}$" %>% 
      sprintf(100-alpha/2*100, kib2, round(qnorm(1-alpha/2),2)) %>%
      fix_formula
    result$raw <- round(kib2 / round(qnorm(1-alpha/2), 2), 2)
    result$fmt <- (fmt(result$raw))
    result$ergebnis <- "$\\sigma_{\\bar{x}} \\approx %s$" %>%
      sprintf(result$fmt)
  }
  return(result)
}
