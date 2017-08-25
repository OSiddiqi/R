getwd()
setwd("C:/Users/Administrator/Documents/R Scripts")
getwd()
rawBCD <- read.table("C:/Users/Administrator/Documents/R Scripts/BCD_Small.data", header = T, sep = ",", fill = TRUE)
BCD_NoID <- rawBCD[,-1]
normalise <- function(x)
  return((x - min(x)) / (max(x) - min(x)))
#normalise(c(1, 2, 3, 4, 5))
#normalise(c(100, 200, 300, 400, 500))
BCD_NoID$V28 <- as.numeric(as.character(BCD_NoID$V28))
BCD_Normal <- as.data.frame(lapply(BCD_NoID[3:32], normalise))
BCD_Train <- BCD_Normal[1:160,]
BCD_Test <- BCD_Normal[161:200,]
BCD_Train_Labels <- BCD_NoID[1:160, 2]
BCD_Test_Labels <- BCD_Test[161:200, 2]
knn(BCD_Train, BCD_Test,BCD_Train_Labels, k = 14)
summary(BCD_Train)
#cleanBCD <- BCD_Train[!is.na(BCD_Train$v3) && !is.na(BCD_Train$V9) && !is.na(BCD_Train$V13) && !is.na(BCD_Train$V20) && !is.na(BCD_Train$V21) && !is.na(BCD_Train$V26) && !is.na(BCD_Train$V28) && !is.na(BCD_Train$V29) && !is.na(BCD_Train$V32)]
BCD_Train$V3 = NULL
BCD_Train$V9 = NULL
BCD_Train$V20 = NULL
BCD_Train$V21 = NULL
BCD_Train$V26 = NULL
BCD_Train$V28 = NULL
BCD_Train$V29 = NULL
BCD_Train$V31 = NULL
BCD_Train$V32 = NULL
BCD_Train$V13 = NULL

BCD_Test$V3 = NULL
BCD_Test$V9 = NULL
BCD_Test$V20 = NULL
BCD_Test$V21 = NULL
BCD_Test$V26 = NULL
BCD_Test$V28 = NULL
BCD_Test$V29 = NULL
BCD_Test$V31 = NULL
BCD_Test$V32 = NULL
BCD_Test$V13 = NULL

BCD_Prediction <- knn(BCD_Train, BCD_Test,BCD_Train_Labels, k = 14)
result <- table(Predictions = BCD_Prediction, TrueLabels = BCD_Test_Labels)
result <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred3, prop.chisq = FALSE)
error <- c(results3[[4]][3]+results3[[4]][2])
