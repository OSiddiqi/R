evens <-c(2,4,6,8,10)
write.csv(evens, "evens.csv")
read.csv("C:/Users/Administrator/Documents/R Scripts/evens.csv", header = FALSE)

odds <-c(evens + 1)
write.csv(odds, "odds.csv")
read.csv("C:/Users/Administrator/Documents/R Scripts/odds.csv", header = FALSE)