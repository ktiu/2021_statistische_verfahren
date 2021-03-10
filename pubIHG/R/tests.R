get_t1 <- function(data  = NULL,
                   barx  = NULL,
                   s     = NULL,
                   n     = NULL,
                   mu    = NULL,
                   alpha = 0.05,
                   mode  = c("ungerichtet", "aufwärts", "abwärts")) {
  result <- list()
  result$formel <- "$t=\\sqrt{n}\\cdot\\frac{\\bar{x}-\\mu_0}{s}$"
  result$wählen <- "Der Mittelwert einer Stichprobe soll auf signifikante Abweichung von der Grundgesamtheit geprüft werden. Die Standardabweichung der Population ist nicht bekannt. Deshalb wird ein $t$-Test durchgeführt."
  if(!is.null(data)) {
    if(is.null(barx)) barx <- get_mean(data)$raw
    if(is.null(s))    s    <- get_sd(data)$raw
    if(is.null(n))    n    <- length(data)
  }
  if(length(c(barx, mu, s, n, alpha, mode)) == 6) {
    df <- n-1
    result$nullhypothese       <- "$H_0: \\mu = \\mu_0$"
    result$alternativhypothese <- "$H_0: \\mu %s \\mu_0$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>%
      fix_formula
    result$df <- "$\\mathit{df} = n -1 = %s - 1 = %s$" %>%
      sprintf(n, df)
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$t \\leq t_{\\mathit{df};\\alpha}$",
        einsetzen  = "$t \\leq t_{%s;%s\\%%}$" %>%
          sprintf(df, alpha*100) %>%
          fix_formula,
        lower      = round(qt(alpha, df), 3),
        upper      = Inf,
        ergebnis   = sprintf("$z \\leq %s$", round(qt(alpha, df), 3)) %>%
          fix_formula
      ),
      aufwärts = list(
        formel     = "$t \\geq t_{\\mathit{df}; (1-\\alpha)}$",
        einsetzen  = "$t \\geq t_{%s; %s\\%%}$" %>%
          sprintf(df, 100-alpha*100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = round(qt(1-alpha, df),3),
        ergebnis   = sprintf("$t \\geq %s$", round(qt(1-alpha, df),3)) %>%
          fix_formula
      ),
      ungerichtet = list(
        formel     = "$t \\leq t_{\\mathit{df};\\alpha/2}\\quad \\textrm{oder} \\quad t \\geq t_{\\mathit{df};(1-\\alpha/2)}$",
        einsetzen  = "$t \\leq t_{%s; %s\\%%} \\quad \\textrm{oder} \\quad t \\geq t_{%s; %s\\%%}$" %>%
          sprintf(df, alpha/2*100, df, 100-alpha/2*100) %>%
          fix_formula,
        lower      = round(qt(alpha/2, df),3),
        upper      = round(qt(1-alpha/2, df),3),
        ergebnis   = "$t \\leq %s\\quad \\textrm{oder} \\quad t \\geq %s$" %>%
          sprintf(round(qt(alpha/2, df),3),
                  round(qt(1-alpha/2, df),3)) %>%
        fix_formula
      )
    )[[mode]]
    result$einsetzen <- "$t=\\sqrt{%s}\\cdot\\frac{%s-%s}{%s}$" %>%
      sprintf(n, barx, mu, s) %>%
      fix_formula
    result$raw      <- round(sqrt(n)*(barx-mu)/s, 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$t\\approx%s$", result$fmt)
    result$test     <- (result$raw <= result$ablehnungsbereich$lower | result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <- "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}
get_z1 <- function(data  = NULL,
                   barx  = NULL,
                   mu    = NULL,
                   sigma = NULL,
                   alpha = 0.05,
                   mode  = c("ungerichtet", "aufwärts", "abwärts")) {
  result <- list()
  result$formel <- "$z=\\sqrt{n}\\cdot\\frac{\\bar{x}-\\mu_0}{\\sigma}$"
  result$wählen <- "Der Mittelwert einer Stichprobe soll auf signifikante Abweichung von der Grundgesamtheit geprüft werden. Die Standardabweichung der Population ist bekannt. Deshalb wird ein $z$-Test durchgeführt."
  if(!is.null(data)) {
    if(is.null(barx)) barx <- get_mean(data)$raw
  }
  if(length(c(barx, mu, sigma, alpha, mode)) == 5) {
    result$nullhypothese       <- "$H_0: \\mu = \\mu_0$"
    result$alternativhypothese <- "$H_0: \\mu %s \\mu_0$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>%
      fix_formula
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$z \\leq z_{\\alpha}$",
        einsetzen  = "$z \\leq z_{%s\\%%}$" %>%
          sprintf(alpha*100) %>%
          fix_formula,
        lower      = floor(qnorm(alpha)*100)/100,
        upper      = Inf,
        ergebnis   = sprintf("$z \\leq %s$", fmt(floor(qnorm(alpha)*100)/100))
      ),
      aufwärts = list(
        formel     = "$z \\geq z_{(1-\\alpha)}$",
        einsetzen  = "$z \\geq z_{%s\\%%}$" %>%
          sprintf(100-alpha*100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = ceiling(qnorm(1-alpha)*100)/100,
        ergebnis   = sprintf("$z \\geq %s$", fmt(ceiling(qnorm(1-alpha)*100)/100))
      ),
      ungerichtet = list(
        formel     = "$z \\leq z_{\\alpha/2}\\quad \\textrm{oder} \\quad z \\geq z_{(1-\\alpha/2)}$",
        einsetzen  = "$z \\leq z_{%s\\%%} \\quad \\textrm{oder} \\quad z \\geq z_{%s\\%%}$" %>%
          sprintf(alpha/2*100, 100-alpha/2*100) %>%
          fix_formula,
        lower      = floor(qnorm(alpha/2)*100)/100,
        upper      = ceiling(qnorm(1-alpha/2)*100)/100,
        ergebnis   = "$z \\leq %s\\quad \\textrm{oder} \\quad z \\geq %s$" %>%
          sprintf(fmt(floor(qnorm(alpha/2)*100)/100),
                  fmt(ceiling(qnorm(1-alpha/2)*100)/100))
      )
    )[[mode]]
    result$einsetzen <- "$z=\\sqrt{%s}\\cdot\\frac{%s-%s}{%s}$" %>%
      sprintf(length(data), barx, mu, sigma) %>%
      fix_formula
    result$raw      <- round(sqrt(length(data))*(barx-mu)/sigma, 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$z\\approx%s$", result$fmt)
    result$test     <- (result$raw <= result$ablehnungsbereich$lower | result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <- "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}

get_f <- function(datalist=NULL, mode=c("ungerichtet", "aufwärts", "abwärts"), var1=NULL, var2=NULL, n1=NULL, n2=NULL, alpha=0.05) {
  result <- list()
  result$formel <- "$F = \\frac{s^2_1}{s^2_2}$"
  result$wählen <- "Zwei Stichproben sollen auf einen signifkanten Unterschied in der Varianz überprüft werden. Deshalb muss ein $F$-Test durchgeführt werden."
  if(!is.null(datalist)) {
    if(is.null(var1)) var1 <- get_var(datalist[[1]])$raw
    if(is.null(var2)) var2 <- get_var(datalist[[2]])$raw
    if(is.null(n1))   n1   <- length(datalist[[1]])
    if(is.null(n2))   n2   <- length(datalist[[2]])
  }
  if(length(c(var1, var2, n1, n2, mode, alpha))==6){
    df1 <- n1-1
    df2 <- n2-1
    result$nullhypothese <- "$H_0: \\sigma^2_1 = \\sigma^2_2$"
    result$alternativhypothese <- "$H_1: \\sigma^2_1 %s \\sigma^2_2$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>% fix_formula
    result$df1 <- "$\\mathit{df}_1 = %s-1 = %s$" %>%
      sprintf(n1, df1)
    result$df2 <- "$\\mathit{df}_2 = %s-1 = %s$" %>%
      sprintf(n2, df2)
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$F \\leq F_{\\mathit{df}_1;\\mathit{df}_2;\\alpha}$",
        einsetzen  = "$F \\leq F_{%s;%s;%s\\%%}$" %>%
          sprintf(df1, df2, alpha*100) %>%
          fix_formula,
        lower      = round(qf(alpha, df1, df2), 2),
        upper      = Inf,
        ergebnis   = sprintf("$F \\leq %s$", fmt(qf(alpha, df1, df2)))
      ),
      aufwärts = list(
        formel     = "$F \\geq F_{\\mathit{df}_1;\\mathit{df}_2;(1-\\alpha)}$",
        einsetzen  = "$F \\geq F_{\\mathit{%s};\\mathit{%s};%s\\%%}$" %>%
          sprintf(df1, df2, 100-alpha*100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = round(qf(1-alpha, df1, df2), 2),
        ergebnis   = sprintf("$F \\geq %s$", fmt(qf(1-alpha, df1, df2)))
      ),
      ungerichtet = list(
        formel     = "$F \\leq F_{\\mathit{df_1};\\mathit{df_2};\\alpha/2}\\quad \\textrm{oder} \\quad F \\geq F_{\\mathit{df_1};\\mathit{df_2};(1-\\alpha/2)}$",
        einsetzen  = "$F \\leq F_{%s;%s;{%s\\%%}} \\quad \\textrm{oder} \\quad F \\geq F_{%s;%s;{%s\\%%}}$" %>%
          sprintf(df1, df2, alpha/2*100, df1, df2, 100-alpha/2*100) %>%
          fix_formula,
        lower      = round(qf(alpha/2, df1, df2), 2),
        upper      = round(qf(1-alpha/2, df1, df2), 2),
        ergebnis   = "$F \\leq %s\\quad \\textrm{oder} \\quad F \\geq %s$" %>%
          sprintf(fmt(qf(alpha/2, df1, df2), 2),
                  fmt(qf(1-alpha/2, df1, df2), 2))
      )
    )[[mode]]
    result$einsetzen <- "$F = \\frac{%s}{%s}$" %>%
      sprintf(var1,  var2) %>% fix_formula
    result$raw       <- round(var1/var2, 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- "$F = %s$" %>% sprintf(result$fmt)
    result$test      <- (result$raw <= result$ablehnungsbereich$lower | result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <- "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}
