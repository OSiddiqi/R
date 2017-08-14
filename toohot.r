toohot <-function(a, isSummer) {
  if (isSummer == T && a > 59 && a < 101) {
    return(TRUE)
  } else if (isSummer == F && a > 59 && a < 91) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}