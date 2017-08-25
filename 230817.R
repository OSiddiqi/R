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
rawBCD[["V2"]] = factor(rawBCD[["V2"]])
model <- svm(V2 ~ V3, rawBCD)

predicted <- predict(model, rawBCD)

plot(rawBCD, predicted, col = "red", pch=4)

#Based on http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(V2 ~., data = rawBCD, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(V2 ~., data = rawBCD, method = "rf",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = rawBCD)
test_pred

cfm<-confusionMatrix(test_pred, rawBCD$V2)
show(cfm)
