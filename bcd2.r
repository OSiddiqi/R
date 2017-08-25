getwd()
setwd("C:/Users/Administrator/Documents/R Scripts")
getwd()
rawBCD <- read.table("C:/Users/Administrator/Documents/R Scripts/BCD_Large.data", header = FALSE, sep = ",")
rawBCD <- rawBCD[-1]
table(rawBCD$V2)
rawBCD$V2 <- as.factor(rawBCD$V2)
summary(rawBCD)
normalise <- function(x)
  return((x - min(x)) / (max(x) - min(x)))
test_Norm <- normalise(c(1,2,3,4,5))
show(test_Norm)
BCD_N <- as.data.frame(lapply(rawBCD[2:31], normalise))
BCD_Train <- BCD_N[1:469,]
BCD_Test <- BCD_N[470:569,]
BCD_Train_Labels <- rawBCD [1:469, 1]
BCD_Test_Labels <- rawBCD [470:569, 1]

BCD_Pred <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 7)
BCD_Test_Pred <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 7)
results <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred, prop.chisq = FALSE)
error <- c(results[[4]][3]+results[[4]][2])

BCD_Pred2 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 1)
BCD_Test_Pred2 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 1)
results2<-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred2, prop.chisq = FALSE)
error2 <- c(results2[[4]][3]+results2[[4]][2])

BCD_Pred3 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 2)
BCD_Test_Pred3 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 2)
results3 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred3, prop.chisq = FALSE)
error3 <- c(results3[[4]][3]+results3[[4]][2])

BCD_Pred4 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 5)
BCD_Test_Pred4 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 5)
results4 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred4, prop.chisq = FALSE)
error4 <- c(results4[[4]][3]+results4[[4]][2])

BCD_Pred5 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 10)
BCD_Test_Pred5 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 10)
results5 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred5, prop.chisq = FALSE)
error5 <- c(results5[[4]][3]+results5[[4]][2])

BCD_Pred6 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 15)
BCD_Test_Pred6 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 15)
results6 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred6, prop.chisq = FALSE)
error6 <- c(results6[[4]][3]+results6[[4]][2])

BCD_Pred7 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 20)
BCD_Test_Pred7 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 20)
results7 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred7, prop.chisq = FALSE)
error7 <- c(results7[[4]][3]+results7[[4]][2])

k_values <- c(7,1,2,5,10,15,20)
error_df <- as.data.frame(c(error*100, error2*100, error3*100, error4*100, error5*100, error6*100, error7*100))
ggplot(error_df, aes(k_values,error_df)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess", color ="red") +
  xlab("k Values") +
  ylab("Error rate in %") +
  ggtitle("Error rate against k Values")

