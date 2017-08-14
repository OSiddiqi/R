leapyear <-function(a) {
  if (a %% 4 == 0 && (a %% 100 != 0 || a %% 400 == 0)) {
  return("It is a leap year!")
  } else {
    return("It is NOT a leap year!")
  }
}