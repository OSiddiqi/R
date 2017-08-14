blackjack <-function(n,o) {
  if (n > 21 && o > 21) {
    return("Both busted")
} else if (n > o && n <= 21) {
    return("Player n is the winner")
} else {
    return("Player o is the winner")
}
}