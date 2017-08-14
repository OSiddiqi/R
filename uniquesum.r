uniquesum <-function(a,b,c) {
  if (a == b && a == c) {
    return(0)
  } else if (a == b) {
    return(a+c)
  } else if (b == c) {
    return(a+b)
  } else if (a != b && b != c) {
    return(a+b+c)
  }
}