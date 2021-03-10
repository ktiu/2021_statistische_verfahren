fmt <- function(number, digits=2) {
  number %>%
    format(digits=digits, nsmall=digits) %>%
    str_replace(",", "{,}") %>%
    return()
}

solve_mean <- function(xs, fragment=F) {
  prefix = ifelse(fragment, "> ", "")
  n <- length(xs)
  zaehler <- sum(xs) %>% round(2)
  m <- round(zaehler/n, 2)
  lines = c(
    sprintf("%s- $\\bar{x}=\\frac{\\sum\\limits_{i=1}^{n}x_{i}}{n}$", prefix),
    sprintf("%s- $\\bar{x}=\\frac{%s}{%i}$\n", prefix, fmt(zaehler), n),
    sprintf("%s- $\\bar{x}=%s$", prefix, fmt(m))
  )
  cat(lines, sep="\n")
  return(m)
}

solve_var <- function(xs, mean=NA, fragment=F) {
  if(is.na(mean)){mean<- mean(xs)}
  prefix = ifelse(fragment, "> ", "")
  n <- length(xs)
  zaehler <- sum((xs-mean)^2)
  variance <- round(zaehler/(n-1), 2)
  c(
    sprintf("%s- $s^2=\\frac{\\sum\\limits_{i=1}^{n}(x_{i}-\\bar{x})^2}{n-1}$", prefix),
    sprintf("%s- $s^2=\\frac{%s}{%i-1}$", prefix, fmt(zaehler), n),
    sprintf("%s- $s^2=%s$", prefix, fmt(variance))
  ) %>% cat(sep="\n")
  return(variance)
}

solve_sd <- function(variance, mean=NA, fragment=F) {
  prefix <- ifelse(fragment, "> ", "")
  standard_deviation <- round(sqrt(variance), 2)
  c(
    sprintf("%s- $s=\\sqrt{s^2}$", prefix),
    sprintf("%s- $s=\\sqrt{%s}$", prefix, fmt(variance)),
    sprintf("%s- $s=%s$", prefix, fmt(standard_deviation))
  ) %>% cat(sep="\n")
  return(standard_deviation)
}

get_mean <- function(data = NULL, symbol = "x") {
  result <- list()
  result$formel <- sprintf("$\\bar{%s}=\\frac{\\sum\\limits_{i=1}^{n}%s_{i}}{n}$",
                           symbol,
                           symbol)
  if(!is.null(data)){
    result$einsetzen <- sprintf("$\\bar{%s}=\\frac{%s}{%s}$",
                                symbol, fmt(sum(data)),
                                length(data))
    result$raw       <- round(mean(data), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$\\bar{%s}=%s$", symbol, result$fmt)
  }
  return(result)
}

get_median <- function(data, alt = F, symbol = "x") {
  label  <- ifelse(alt, sprintf("\\textit{Md}_%s", symbol), "\\textit{Md}")
  sorted <- sort(data)
  result <- list()
  if(length(data) %% 2 == 0) {
    result$formel <- "$%s = \\frac{x_{(\\frac{n}{2})}+x_{(\\frac{n}{2}+1)}}{2}$" %>%
      sprintf(label)
    result$einsetzen <- "$%s=\\frac{x_{%s}+x_{%s}}{2}=\\frac{%s + %s}{2}$" %>%
      sprintf(label, length(sorted)/2, length(sorted)/2+1, sorted[length(sorted)/2], sorted[length(sorted)/2+1]) %>%
      fix_formula
    result$raw      <- round(median(data), 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  } else {
    result$formel <- "$%s=x_{(\\frac{n+1}{2})}$" %>%
      sprintf(label)
    result$einsetzen <- "$%s=x_{(%s)}$" %>%
      sprintf(label, (length(sorted)+1)/2)
    result$raw      <- round(median(data), 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

get_var <- function(data = NULL, alt = F, symbol = "x") {
  label <- ifelse(alt, sprintf("s^2_%s", symbol), "s^2")
  result <- list()
  result$formel <- sprintf("$%s=\\frac{\\sum\\limits_{i=1}^{n}(%s_{i}-\\bar{%s})^2}{n-1}$",
                           label,
                           symbol,
                           symbol)
  if(!is.null(data)) {
    result$einsetzen <- sprintf("$%s=\\frac{%s}{%s}$",
                                label,
                                fmt(sum((data-round(mean(data), 2))^2)),
                                length(data)-1)
    result$raw       <- round(sum(round((data-round(mean(data), 2))^2,2))/(length(data)-1), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

get_sd <- function(data = NULL, alt = F, symbol = "x", variance = NULL, pop = F) {
  base_label <- ifelse(pop, "\\sigma", "s")
  label      <- ifelse(alt, sprintf("%s_%s", base_label, symbol), base_label)
  labels2    <- ifelse(alt, sprintf("%s^2_%s", base_label, symbol), sprintf("%s^2", base_label))
  result     <- list()
  result$formel <- sprintf("$%s=\\sqrt{%s}$", label, labels2)
  if(!is.null(data)) {
    variance         <- get_var(data)$raw
  }
  if(!is.null(variance)) {
    result$einsetzen <- sprintf("$%s=\\sqrt{%s}$", label, fmt(variance))
    result$raw       <- round(sqrt(variance), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$%s\\approx%s$", label, result$fmt)
  }
  return(result)
}

get_range <- function(data=NULL, alt=F, symbol="x") {
  label  <- ifelse(alt, sprintf("R_%s", symbol), "R")
  result <- list()
  result$formel <- sprintf("$%s=x_{(n)}-x_{(1)}$", label)
  if(!is.null(data)) {
    sorted <- sort(data)
    first  <- sorted[1]
    last   <- sorted[length(data)]
    result$einsetzen <- sprintf("$%s=%s-%s$", label, last, first) %>% fix_formula
    result$raw <- last-first
    result$fmt <- str_replace(result$raw, ",", "{,}")
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

get_iqr <- function(data=NULL, alt=F, symbol="x") {
  label  <- ifelse(alt, sprintf("\\mathit{IQR}_%s", symbol), "\\mathit{IQR}")
  result <- list()
  result$formel <- sprintf("$%s=Q_3-Q_1$", label)
  if(!is.null(data)) {
    sorted <- sort(data)
    q1 <- median(sorted[1:ceiling(length(data)/2)])
    q3 <- median(sorted[floor(length(data)/2+1):length(data)])
    result$q1 <- "$%s=%s$" %>% sprintf("Q_1", q1) %>% fix_formula
    result$q3 <- "$%s=%s$" %>% sprintf("Q_3", q3) %>% fix_formula
    result$einsetzen <- sprintf("$%s=%s-%s$", label, q3, q1) %>% fix_formula
    result$raw <- q3-q1
    result$fmt <- str_replace(result$raw, ",", "{,}")
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

get_variationskoeffizient <- function(data=NULL, mean=NULL, sd=NULL, alt=F, symbol="x") {
   label <- ifelse(alt, sprintf("v_%s", symbol), "v")
   result <- list(
    formel = "$%s=\\frac{s}{|\\bar{x}|}\\cdot100\\%%\\quad$" %>%
      sprintf(label)
   )
   if(!is.null(data)) {
     if(is.null(mean)) mean <- get_mean(data)$raw
     if(is.null(sd))   sd   <- get_sd(data)$raw
   }
   if(length(c(mean, sd))==2) {
     result$einsetzen <- "$%s=\\frac{%s}{%s}\\cdot100\\%%$" %>%
      sprintf(label, sd, abs(mean)) %>%
      fix_formula
     result$raw <- round(sd/mean*100, 2)
     result$fmt <- fmt(result$raw)
     result$ergebnis <- "$%s=%s\\%%$" %>%
      sprintf(label, result$fmt)
   }
   return(result)
}

get_phi <- function(chisq=NULL, n=NULL) {
  result <- list()
  result$formel <- "$\\phi=\\sqrt{\\frac{\\chi^2}{n}}$"
  if(length(c(chisq, n)) == 2) {
    result$einsetzen <- "$\\phi\\approx\\sqrt{\\frac{%s}{%s}}$" %>%
      sprintf(chisq, n) %>%
      fix_formula
    result$raw <- round(sqrt(chisq/n), 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$\\phi\\approx%s$", result$fmt)
  }
  return(result)
}

fix_formula <- function(string){
  string %>%
    str_replace_all("(\\d),(\\d)", "\\1{,}\\2") %>%
    str_replace_all("- *-", "+") %>%
    str_replace_all("\\+ *-", "-") %>%
    str_replace_all("\\cdot *(-[0-9,{}]*)", "\\cdot \\(\\1\\)")
}

solution_table <- function(solution_data) {
  summe <- sum(solution_data$Punkte)
  summenzeile <- nrow(solution_data) + 1 
  solution_data %>%
    mutate(`Erreicht`=sprintf("<input type='checkbox' style='width:30px; height:30px' value='%s' />", str_replace(Punkte, ",", ".")),
           `Max. Punkte`=format(Punkte, nsmall=1),
           `&nbsp;` = ifelse(Implizit, "(auch&nbsp;implizit)", "")) %>%
    select(Schritt, Musterlösung, `&nbsp;`, `Max. Punkte`, `Erreicht`) %>%
    bind_rows(list(Schritt="", `Musterlösung`="", `&nbsp;`="$\\sum$", `Max. Punkte`=format(summe, nsmall=1), `Erreicht`="<div class='punkte-aufgabe'><input type='hidden' value='1'/><span></span>")) %>%
    kable("html", escape=F, align="lcccc") %>%
    kable_styling() %>%
    column_spec(c(2, 4), background = light_gray)
}
