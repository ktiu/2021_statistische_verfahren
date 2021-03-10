runden <- function(note){
  pre <- round(round((note * 3)-0.1) / 3, 1)
  if (pre > 4) pre <- 5
  if (pre < 1) pre <- 1
  return(pre)
}

punkte <- function(note){
  pre <- round(17 - note * 3)
  if (pre < 5) pre <- 0
  if (pre > 15) pre <- 15
  return(pre)
}
