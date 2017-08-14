addiftruezero <-function(d,e,bool) {
  if(d == 0) {
    return(e)
  } else if(e == 0) {
    return(d)  
  } else if(bool == TRUE) {
    d+e
  } else {
    d*e
  }
}
