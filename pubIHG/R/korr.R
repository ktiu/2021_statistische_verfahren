get_cov <- function(data=NULL, meanx=NULL, meany=NULL) {
  result <- list()
  result$formel <- "$s_{xy}=\\frac{\\displaystyle \\sum_{i=1}^{n}(x_{i}-\\bar{x})\\cdot(y_{i}-\\bar{y})}{n-1}$"
  if (!is.null(data)) {
    if(is.null(meanx)) meanx <- get_mean(data[1])$raw
    if(is.null(meany)) meany <- get_mean(data[2])$raw
    zaehler <- sum(round((data[1]-meanx)*(data[2]-meany), 2))
    nenner <- nrow(data)-1
    result$einsetzen <- sprintf("$s_{xy}=\\frac{%s}{%s}$", zaehler, nenner)
    result$raw <- round(zaehler/nenner, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$s_{xy}=%s$", result$fmt)
  }
  return(result)
}

get_corr <- function(cov=NULL, sdx=NULL, sdy=NULL) {
  result <- list()
  result$formel <- "$r=\\frac{s_{xy}}{s_x \\cdot s_y}$"
  if(length(c(cov, sdx, sdy)) == 3) {
    result$einsetzen <- sprintf("$r=\\frac{%s}{%s \\cdot %s}$",
                                fmt(cov), fmt(sdx), fmt(sdy))
    result$raw <- round(cov/sdx/sdy, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$r=%s$", result$fmt)
  }
  return(result)
}

get_b <- function(cov=NULL, varx=NULL, sdx=NULL) {
  result <- list()
  result$formel <- "$b=\\frac{s_{xy}}{s^2_x}$"
  if(!is.null(cov) & length(c(cov, sdx, varx) > 1)) {
    nennerfmt <- ifelse(is.null(varx),
                        sprintf("%s^2", fmt(sdx)),
                        str_replace(varx, ",", "{,}"))
    nenner <- ifelse(is.null(varx), round(sdx^2, 2), varx)
    result$einsetzen <- sprintf("$b=\\frac{%s}{%s}$",
                                str_replace(cov, ",", "{,}"),
                                nennerfmt)
    result$raw <- round(cov/nenner,3)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$b=%s$", result$fmt)
  }
  return(result)
}

get_a <- function(b=NULL, meanx=NULL, meany=NULL){
  result <- list()
  result$formel <- "$a=\\bar{y}-b\\cdot\\bar{x}$"
  if(length(c(b, meanx, meany)==3)){
    result$einsetzen <- sprintf("$a=%s-%s\\cdot%s$",
                                str_replace(meany, ",", "{,}"),
                                str_replace(b, ",", "{,}"),
                                str_replace(meanx, ",", "{,}")) %>%
                          fix_formula
    result$raw <- round(meany - b*meanx, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$a=%s$", result$fmt)
  }
  return(result)
}

get_reg <- function(a=NULL, b=NULL){
  result <- list()
  result$formel <- "$y=a + b \\cdot x$"
  if(length(c(a, b) == 2)){
    result$ergebnis <- sprintf("$y \\approx %s + %s \\cdot x$",
                               str_replace(a, ",", "{,}"),
                               str_replace(b, ",", "{,}")
                              ) %>% fix_formula
  }
  return(result)
}

get_pred <- function(a, b, x=NULL, y=NULL){
  result <- list()
  result$formel <- sprintf("$\\hat{y}= %s + %s \\cdot x$", a, b) %>%
    fix_formula
  if(length(c(a, b, x)) == 3){
    result$einsetzen <- "$\\hat{y}\\approx %s + %s \\cdot %s$" %>%
      sprintf(a, b, x) %>%
      fix_formula
    result$raw <- round(a+b*x, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$\\hat{y}\\approx%s$", result$fmt)
  } else if (length(c(a, b, y)) == 3) {
    result$formel <- sprintf("$x = \\frac{\\hat{y} - %s}{%s}$", a, b) %>%
      fix_formula
    result$einsetzen <-"$x \\approx \\frac{%s-%s}{%s}$" %>%
      sprintf(y, a, b) %>%
      fix_formula
    result$raw <- round((y-a)/b, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$x\\approx%s$", result$fmt)
  }
  return(result)
}
